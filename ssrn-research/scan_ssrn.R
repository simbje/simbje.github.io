#!/usr/bin/env Rscript
# =============================================================================
# scan_ssrn.R
# Bi-weekly research-trends scanner.
#
# Phase A (deterministic): pull last 14 days of papers from arXiv (q-fin.*,
#   stat.ML, cs.LG) and best-effort SSRN. Produces a candidate-paper list.
# Phase B (Claude Haiku tool-use): cluster candidates into ~5-10 named topics
#   and pick the top-3 most-trending papers. Edges are derived deterministically
#   in the Quarto page from paper-id intersections — agent does NOT emit edges.
#
# Outputs ssrn-research/snapshots/YYYY-MM-DD.json
#
# Usage:
#   Rscript ssrn-research/scan_ssrn.R              # full run (needs ANTHROPIC_API_KEY)
#   Rscript ssrn-research/scan_ssrn.R --dry-run    # Phase A only, no LLM cost
# =============================================================================

suppressPackageStartupMessages({
  library(httr2)
  library(xml2)
  library(rvest)
  library(jsonlite)
  library(lubridate)
  library(dplyr)
  library(stringr)
  library(purrr)
})

# ── Args ─────────────────────────────────────────────────────────────────────
ARGS    <- commandArgs(trailingOnly = TRUE)
DRY_RUN <- "--dry-run" %in% ARGS

# ── Paths (resolve relative to this script so cwd doesn't matter) ────────────
.script_dir <- tryCatch({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[grep("--file=", args)]
  if (length(file_arg) > 0)
    dirname(normalizePath(sub("--file=", "", file_arg[1])))
  else
    normalizePath(getwd())
}, error = function(e) normalizePath(getwd()))

SNAPSHOT_DIR  <- file.path(.script_dir, "snapshots")
ERROR_LOG     <- file.path(.script_dir, "_last_error.txt")
TODAY         <- Sys.Date()
SNAPSHOT_FILE <- file.path(SNAPSHOT_DIR, paste0(format(TODAY, "%Y-%m-%d"), ".json"))

dir.create(SNAPSHOT_DIR, recursive = TRUE, showWarnings = FALSE)

if (file.exists(SNAPSHOT_FILE)) {
  message("Snapshot already exists for ", format(TODAY, "%Y-%m-%d"), " — skipping.")
  quit(save = "no", status = 0)
}

ANTHROPIC_API_KEY <- Sys.getenv("ANTHROPIC_API_KEY")

# ── Retry helper (matches ssb-daily/generate_post.R:28-45) ──────────────────
with_retry <- function(fn, max_attempts = 3L, base_wait = 2) {
  last_error <- NULL
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch(
      list(value = fn(), error = NULL),
      error = function(e) list(value = NULL, error = e)
    )
    if (is.null(result$error)) return(result$value)
    last_error <- result$error
    if (attempt < max_attempts) {
      wait <- base_wait * 2^(attempt - 1L)
      message("  Attempt ", attempt, " failed: ", conditionMessage(last_error),
              " — retrying in ", wait, "s...")
      Sys.sleep(wait)
    }
  }
  stop(last_error)
}

UA <- paste0(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
  "AppleWebKit/537.36 (KHTML, like Gecko) ",
  "Chrome/124.0.0.0 Safari/537.36"
)

# Null-coalesce
`%||%` <- function(a, b) if (is.null(a)) b else a

# =============================================================================
# Phase A — deterministic fetch
# =============================================================================

# arXiv categories of interest. q-fin.* covers all quantitative finance;
# stat.ML and cs.LG capture data-science / ML papers that are often
# applied to finance.
ARXIV_CATEGORIES <- c(
  "q-fin.PM", "q-fin.ST", "q-fin.RM", "q-fin.MF",
  "q-fin.CP", "q-fin.TR", "q-fin.GN", "q-fin.EC",
  "stat.ML", "cs.LG"
)

LOOKBACK_DAYS <- 14L

fetch_arxiv <- function() {
  query <- paste0("cat:", ARXIV_CATEGORIES, collapse = "+OR+")
  url   <- paste0(
    "http://export.arxiv.org/api/query",
    "?search_query=", query,
    "&sortBy=submittedDate&sortOrder=descending",
    "&max_results=200"
  )

  message("Fetching arXiv: ", length(ARXIV_CATEGORIES), " categories, max 200 results...")

  xml <- tryCatch(
    with_retry(function() {
      resp <- request(url) |>
        req_headers("User-Agent" = UA) |>
        req_timeout(60) |>
        req_perform()
      read_xml(resp_body_string(resp))
    }, max_attempts = 3L, base_wait = 3),
    error = function(e) {
      message("  arXiv fetch failed: ", e$message)
      NULL
    }
  )

  if (is.null(xml)) return(tibble())

  ns <- c(a = "http://www.w3.org/2005/Atom")
  entries <- xml_find_all(xml, ".//a:entry", ns)
  if (length(entries) == 0L) return(tibble())

  cutoff <- TODAY - LOOKBACK_DAYS

  rows <- map(entries, function(e) {
    id_full   <- xml_text(xml_find_first(e, "a:id", ns))
    bare_id   <- str_extract(id_full, "(?<=abs/)[^v]+")  # e.g., "2401.12345"
    title     <- xml_text(xml_find_first(e, "a:title", ns)) |> str_squish()
    summary   <- xml_text(xml_find_first(e, "a:summary", ns)) |> str_squish()
    published <- xml_text(xml_find_first(e, "a:published", ns))
    authors   <- xml_text(xml_find_all(e, "a:author/a:name", ns))
    cats      <- xml_attr(xml_find_all(e, "a:category", ns), "term")
    tibble(
      id        = paste0("arxiv:", bare_id),
      title     = title,
      abstract  = summary,
      authors   = list(authors),
      date      = as.Date(published),
      source    = "arxiv",
      category  = paste(intersect(cats, ARXIV_CATEGORIES), collapse = "; "),
      url       = paste0("https://arxiv.org/abs/", bare_id)
    )
  }) |> bind_rows()

  rows <- rows |> filter(!is.na(date), date >= cutoff)
  message("  arXiv: ", nrow(rows), " papers in last ", LOOKBACK_DAYS, " days")
  rows
}

# Best-effort SSRN fetch. SSRN's Cloudflare is aggressive — one shot, no retries.
fetch_ssrn <- function() {
  url <- "https://www.ssrn.com/index.cfm/en/janda/?networkID=203"
  message("Trying SSRN (best-effort): networkID=203 (Financial Economics Network)...")

  html <- tryCatch({
    resp <- request(url) |>
      req_headers("User-Agent" = UA, "Accept-Language" = "en-US,en;q=0.9") |>
      req_timeout(30) |>
      req_perform()
    if (resp_status(resp) >= 400) return(NULL)
    read_html(resp_body_string(resp))
  }, error = function(e) {
    message("  SSRN fetch failed: ", e$message)
    NULL
  })

  if (is.null(html)) return(tibble())

  links <- html_elements(html, "a[href*='papers.cfm?abstract_id=']")
  if (length(links) == 0L) {
    message("  SSRN: no recognisable paper links — schema likely changed")
    return(tibble())
  }

  rows <- map(links, function(a) {
    href  <- html_attr(a, "href")
    title <- html_text2(a) |> str_squish()
    abstract_id <- str_extract(href, "(?<=abstract_id=)\\d+")
    if (is.na(abstract_id) || nchar(title) < 5) return(NULL)
    tibble(
      id        = paste0("ssrn:", abstract_id),
      title     = title,
      abstract  = NA_character_,
      authors   = list(character(0)),
      date      = NA_Date_,
      source    = "ssrn",
      category  = "ssrn:network=203",
      url       = if (startsWith(href, "http")) href else paste0("https://papers.ssrn.com/sol3/", href)
    )
  }) |> compact() |> bind_rows()

  rows <- rows |> distinct(id, .keep_all = TRUE) |> head(30)

  message("  SSRN: ", nrow(rows), " candidate papers")
  rows
}

# =============================================================================
# Snapshot writers
# =============================================================================

write_snapshot <- function(payload) {
  write_json(
    payload,
    SNAPSHOT_FILE,
    pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null"
  )
  message("Snapshot written: ", SNAPSHOT_FILE)
}

write_stub_snapshot <- function(candidates_count, sources_used, reason) {
  write_snapshot(list(
    run_date         = format(TODAY, "%Y-%m-%d"),
    candidates_count = candidates_count,
    sources_used     = sources_used,
    note             = reason,
    top_papers       = list(),
    topics           = list()
  ))
}

# =============================================================================
# Phase A driver
# =============================================================================

arxiv_df <- fetch_arxiv()
Sys.sleep(3)  # arXiv politeness: 1 req per 3s
ssrn_df  <- fetch_ssrn()

candidates <- bind_rows(arxiv_df, ssrn_df)

# Cap candidates fed to the LLM. Prioritise q-fin (the user's primary domain),
# then most-recent. ~80 papers ≈ 25–30k tokens of input — comfortable for Haiku.
MAX_CANDIDATES <- 80L
if (nrow(candidates) > MAX_CANDIDATES) {
  candidates <- candidates |>
    mutate(qfin_priority = if_else(str_detect(category, "q-fin\\."), 0L, 1L)) |>
    arrange(qfin_priority, desc(date)) |>
    select(-qfin_priority) |>
    head(MAX_CANDIDATES)
  message("Capped candidates to top ", MAX_CANDIDATES, " (q-fin first, then most-recent).")
}

sources_used <- c(
  if (nrow(arxiv_df) > 0) paste0("arxiv:", unique(unlist(strsplit(arxiv_df$category, "; ")))) else character(),
  if (nrow(ssrn_df)  > 0) "ssrn:network=203" else character()
) |> unique()

message("Total candidates: ", nrow(candidates))

if (nrow(candidates) == 0L) {
  write_stub_snapshot(0L, sources_used, "Both arXiv and SSRN returned no papers")
  quit(save = "no", status = 0)
}

if (DRY_RUN) {
  message("DRY-RUN — skipping Phase B agent. Writing Phase-A-only snapshot.")
  write_snapshot(list(
    run_date         = format(TODAY, "%Y-%m-%d"),
    candidates_count = nrow(candidates),
    sources_used     = sources_used,
    note             = "Dry run: Phase A only, no LLM clustering.",
    top_papers       = list(),
    topics           = list(),
    candidates_preview = head(
      candidates |>
        mutate(authors = map_chr(authors, ~ paste(head(.x, 3), collapse = ", "))) |>
        select(id, title, source, date, url),
      20
    )
  ))
  quit(save = "no", status = 0)
}

if (nchar(ANTHROPIC_API_KEY) == 0L) {
  message("ANTHROPIC_API_KEY not set — running Phase A only.")
  write_stub_snapshot(nrow(candidates), sources_used,
                      "ANTHROPIC_API_KEY missing; topic clustering skipped.")
  quit(save = "no", status = 0)
}

# =============================================================================
# Phase B — Claude Haiku tool-use clustering
# =============================================================================

# Build the candidate list as a compact text block for the agent
candidate_block <- candidates |>
  mutate(
    authors_str = map_chr(authors, ~ paste(head(.x, 4), collapse = ", ")),
    abs_short   = ifelse(is.na(abstract) | nchar(abstract) == 0,
                         "(abstract not available)",
                         substr(abstract, 1, 600))
  ) |>
  mutate(
    block = sprintf(
      "[%s] %s\n  Source: %s | Category: %s | Date: %s\n  Authors: %s\n  Abstract: %s\n  URL: %s",
      id, title, source, category,
      ifelse(is.na(date), "?", as.character(date)),
      authors_str, abs_short, url
    )
  ) |>
  pull(block) |>
  paste(collapse = "\n\n")

AGENT_TOOLS <- list(
  list(
    name = "fetch_paper_abstract",
    description = paste(
      "Fetch the full abstract for a candidate paper by id. Use only if a",
      "paper's abstract was not provided or was truncated and you need more",
      "information to classify it. Pass the id exactly as listed (e.g. 'arxiv:2401.12345')."
    ),
    input_schema = list(
      type = "object",
      properties = list(id = list(type = "string", description = "Paper id from candidate list")),
      required  = list("id")
    )
  ),
  list(
    name = "finalize_research_summary",
    description = paste(
      "Submit the final clustered summary. Provide top_papers (3 most-trending),",
      "and topics (5-10 named clusters). DO NOT include edges — those are derived",
      "deterministically from topic paper_ids."
    ),
    input_schema = list(
      type = "object",
      properties = list(
        top_papers = list(
          type = "array",
          description = "Top 3 most-trending papers with rank, why_trending, and the topic names they belong to.",
          items = list(
            type = "object",
            properties = list(
              id            = list(type = "string"),
              rank          = list(type = "integer", description = "1, 2, or 3"),
              why_trending = list(type = "string",
                                   description = "1-2 sentences on why this is notable right now."),
              topics        = list(type = "array", items = list(type = "string"),
                                   description = "Names of the topic clusters this paper belongs to.")
            ),
            required = list("id", "rank", "why_trending", "topics")
          )
        ),
        topics = list(
          type  = "array",
          description = "5-10 named topic clusters covering the candidates.",
          items = list(
            type = "object",
            properties = list(
              name      = list(type = "string",
                               description = "Human-readable topic name, e.g. 'transformer-based volatility forecasting'."),
              paper_ids = list(type = "array", items = list(type = "string"),
                               description = "ids of papers in this cluster (must come from candidate list)."),
              summary   = list(type = "string",
                               description = "1-2 sentences summarising the topic.")
            ),
            required = list("name", "paper_ids", "summary")
          )
        )
      ),
      required = list("top_papers", "topics")
    )
  )
)

# Agent tool dispatch
candidate_lookup <- candidates |> select(id, title, abstract, url) |>
  mutate(abstract = ifelse(is.na(abstract), "", abstract))

dispatch_tool <- function(tool_name, input) {
  if (tool_name == "fetch_paper_abstract") {
    pid <- as.character(input$id)
    row <- candidate_lookup |> filter(id == pid)
    if (nrow(row) == 0L) return(paste0("No paper with id '", pid, "' in candidate list."))
    if (nchar(row$abstract[1]) > 0L) return(row$abstract[1])
    if (startsWith(pid, "arxiv:")) {
      arxiv_id <- sub("^arxiv:", "", pid)
      api <- paste0("http://export.arxiv.org/api/query?id_list=", arxiv_id)
      out <- tryCatch({
        resp <- request(api) |> req_headers("User-Agent" = UA) |> req_timeout(20) |> req_perform()
        x <- read_xml(resp_body_string(resp))
        ns <- c(a = "http://www.w3.org/2005/Atom")
        xml_text(xml_find_first(x, ".//a:entry/a:summary", ns)) |> str_squish()
      }, error = function(e) "")
      if (nchar(out) > 0) return(out)
    }
    "Abstract not available."
  } else {
    paste0("Unknown tool: ", tool_name)
  }
}

MAX_AGENT_TURNS <- 8L

system_prompt <- paste(
  "You are a quantitative-research analyst surfacing the most interesting recent",
  "papers in {data analytics, data science, finance, quantitative finance}.",
  "",
  "You'll be given a list of candidate papers from arXiv and (optionally) SSRN",
  "from the last two weeks. Your job:",
  "  1. Pick the TOP 3 most notable / trending papers.",
  "  2. Cluster the candidates into 5-10 named topic groups. Use human-readable",
  "     topic names (e.g. 'transformer-based volatility forecasting',",
  "     'reinforcement learning for portfolio allocation').",
  "  3. Each candidate paper should appear in at least one topic. Papers can",
  "     belong to multiple topics if they span clusters — this is what creates",
  "     the mind-graph edges (computed deterministically downstream).",
  "",
  "When you're ready, call finalize_research_summary with the result. Do NOT",
  "emit edges — they're derived from paper_ids intersections in post-processing.",
  "",
  "Be efficient. Use fetch_paper_abstract sparingly — only if a candidate's",
  "abstract is truly insufficient for classification. Aim to finalize in 1-3",
  "tool calls total.",
  sep = "\n"
)

user_prompt <- paste0(
  "Run date: ", format(TODAY, "%A, %d %B %Y"), "\n",
  "Lookback: ", LOOKBACK_DAYS, " days\n",
  "Candidate count: ", nrow(candidates), "\n",
  "Sources: ", paste(sources_used, collapse = ", "), "\n\n",
  "## Candidates\n\n",
  candidate_block, "\n\n",
  "Now cluster these into 5-10 topics, pick the top 3, and call finalize_research_summary."
)

run_agent <- function() {
  messages  <- list(list(role = "user", content = user_prompt))
  finalized <- NULL

  for (turn in seq_len(MAX_AGENT_TURNS)) {
    message("  Agent turn ", turn, "...")

    resp <- tryCatch(
      with_retry(function() {
        request("https://api.anthropic.com/v1/messages") |>
          req_headers(
            "x-api-key"         = ANTHROPIC_API_KEY,
            "anthropic-version" = "2023-06-01",
            "content-type"      = "application/json"
          ) |>
          req_body_json(list(
            model      = "claude-haiku-4-5-20251001",
            max_tokens = 4000L,
            system     = system_prompt,
            tools      = AGENT_TOOLS,
            messages   = messages
          )) |>
          req_timeout(180) |>
          req_perform()
      }, max_attempts = 3L, base_wait = 3),
      error = function(e) { message("  Agent API error: ", e$message); NULL }
    )

    if (is.null(resp)) break

    r        <- resp_body_json(resp)
    messages <- c(messages, list(list(role = "assistant", content = r$content)))

    if (!identical(r$stop_reason, "tool_use")) {
      message("  Agent stopped: ", r$stop_reason)
      break
    }

    tool_results <- list()
    for (block in r$content) {
      if (!identical(block$type, "tool_use")) next

      if (identical(block$name, "finalize_research_summary")) {
        finalized    <- block$input
        tool_results <- c(tool_results, list(list(
          type        = "tool_result",
          tool_use_id = block$id,
          content     = "Summary finalized."
        )))
        break
      }

      message("    Tool: ", block$name, " — ", paste(unlist(block$input), collapse = ", "))
      result <- dispatch_tool(block$name, block$input)
      tool_results <- c(tool_results, list(list(
        type        = "tool_result",
        tool_use_id = block$id,
        content     = substr(result, 1, 4000)
      )))
      Sys.sleep(0.3)
    }

    messages <- c(messages, list(list(role = "user", content = tool_results)))

    if (!is.null(finalized)) return(finalized)
  }

  NULL
}

agent_result <- tryCatch(run_agent(), error = function(e) {
  writeLines(conditionMessage(e), ERROR_LOG)
  message("Agent error: ", e$message)
  NULL
})

if (is.null(agent_result)) {
  message("Agent did not finalize — writing stub snapshot.")
  write_stub_snapshot(nrow(candidates), sources_used,
                      "Agent failed to finalize; candidates fetched but not clustered.")
  quit(save = "no", status = 0)
}

# =============================================================================
# Build & validate snapshot from agent result
# =============================================================================

valid_ids <- candidates$id

enrich_top <- function(tp) {
  pid <- as.character(tp$id)
  row <- candidates |> filter(id == pid)
  if (nrow(row) == 0L) return(NULL)
  list(
    id            = pid,
    rank          = as.integer(tp$rank),
    title         = row$title[1],
    authors       = row$authors[[1]],
    url           = row$url[1],
    source        = row$source[1],
    why_trending  = as.character(tp$why_trending),
    topics        = as.list(unlist(tp$topics))
  )
}

clean_topic <- function(t) {
  pids <- unique(as.character(unlist(t$paper_ids)))
  pids <- intersect(pids, valid_ids)
  if (length(pids) == 0L) return(NULL)
  list(
    name      = as.character(t$name),
    paper_ids = as.list(pids),
    summary   = as.character(t$summary %||% "")
  )
}

top_papers_clean <- map(agent_result$top_papers, enrich_top) |> compact()
topics_clean     <- map(agent_result$topics, clean_topic) |> compact()

if (length(topics_clean) == 0L) {
  message("Agent returned no usable topics — writing stub snapshot.")
  write_stub_snapshot(nrow(candidates), sources_used,
                      "Agent finalized but no valid topics survived id filtering.")
  quit(save = "no", status = 0)
}

snapshot <- list(
  run_date         = format(TODAY, "%Y-%m-%d"),
  candidates_count = nrow(candidates),
  sources_used     = sources_used,
  top_papers       = top_papers_clean,
  topics           = topics_clean
)

write_snapshot(snapshot)
message("Done. Topics: ", length(topics_clean),
        " | Top papers: ", length(top_papers_clean),
        " | Candidates: ", nrow(candidates))
