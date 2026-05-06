#!/usr/bin/env Rscript
# =============================================================================
# scan_ssrn.R
# Bi-weekly research-trends scanner.
#
# Phase A (deterministic): pull last 14 days of papers from arXiv across three
#   buckets:
#     - data_science    (stat.ML, cs.LG, stat.ME, stat.AP)
#     - finance         (q-fin.*)
#     - social_sciences (econ.*)
# Phase B (Claude Haiku tool-use): cluster candidates into ~5-10 named topics
#   and pick the TOP 5 most-trending papers per bucket.
#   Edges are derived deterministically in the Quarto page from paper-id
#   intersections — agent does NOT emit edges.
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
  library(jsonlite)
  library(lubridate)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tidyr)
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
# Phase A — deterministic fetch (arXiv only, three buckets)
# =============================================================================

ARXIV_BUCKETS <- list(
  data_science    = c("stat.ML", "cs.LG", "stat.ME", "stat.AP"),
  finance         = c("q-fin.PM", "q-fin.ST", "q-fin.RM", "q-fin.MF",
                      "q-fin.CP", "q-fin.TR", "q-fin.GN", "q-fin.EC"),
  social_sciences = c("econ.EM", "econ.GN", "econ.TH")
)

# Pretty labels for display
BUCKET_LABELS <- c(
  data_science    = "Data Science",
  finance         = "Finance",
  social_sciences = "Social Sciences"
)

LOOKBACK_DAYS  <- 14L
PER_BUCKET_MAX <- 80L  # arXiv max_results per bucket query

fetch_arxiv_bucket <- function(bucket_name, cats) {
  query <- paste0("cat:", cats, collapse = "+OR+")
  url   <- paste0(
    "http://export.arxiv.org/api/query",
    "?search_query=", query,
    "&sortBy=submittedDate&sortOrder=descending",
    "&max_results=", PER_BUCKET_MAX
  )

  message("Fetching arXiv [", bucket_name, "]: ", length(cats),
          " categories, max ", PER_BUCKET_MAX, " results...")

  xml <- tryCatch(
    with_retry(function() {
      resp <- request(url) |>
        req_headers("User-Agent" = UA) |>
        req_timeout(60) |>
        req_perform()
      read_xml(resp_body_string(resp))
    }, max_attempts = 3L, base_wait = 3),
    error = function(e) {
      message("  arXiv [", bucket_name, "] fetch failed: ", e$message)
      NULL
    }
  )

  if (is.null(xml)) return(tibble())

  ns <- c(a = "http://www.w3.org/2005/Atom")
  entries <- xml_find_all(xml, ".//a:entry", ns)
  if (length(entries) == 0L) return(tibble())

  cutoff   <- TODAY - LOOKBACK_DAYS
  all_cats <- unlist(ARXIV_BUCKETS)

  rows <- map(entries, function(e) {
    id_full   <- xml_text(xml_find_first(e, "a:id", ns))
    bare_id   <- str_extract(id_full, "(?<=abs/)[^v]+")
    title     <- xml_text(xml_find_first(e, "a:title", ns)) |> str_squish()
    summary   <- xml_text(xml_find_first(e, "a:summary", ns)) |> str_squish()
    published <- xml_text(xml_find_first(e, "a:published", ns))
    authors   <- xml_text(xml_find_all(e, "a:author/a:name", ns))
    cats_raw  <- xml_attr(xml_find_all(e, "a:category", ns), "term")
    tibble(
      id        = paste0("arxiv:", bare_id),
      title     = title,
      abstract  = summary,
      authors   = list(authors),
      date      = as.Date(published),
      source    = "arxiv",
      category  = paste(intersect(cats_raw, all_cats), collapse = "; "),
      url       = paste0("https://arxiv.org/abs/", bare_id),
      bucket    = bucket_name
    )
  }) |> bind_rows()

  rows <- rows |> filter(!is.na(date), date >= cutoff)
  message("  arXiv [", bucket_name, "]: ", nrow(rows),
          " papers in last ", LOOKBACK_DAYS, " days")
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

empty_top_papers <- function() {
  list(data_science = list(), finance = list(), social_sciences = list())
}

write_stub_snapshot <- function(candidates_count, sources_used, reason) {
  write_snapshot(list(
    run_date         = format(TODAY, "%Y-%m-%d"),
    candidates_count = candidates_count,
    sources_used     = sources_used,
    note             = reason,
    top_papers       = empty_top_papers(),
    topics           = list()
  ))
}

# =============================================================================
# Phase A driver
# =============================================================================

bucket_names <- names(ARXIV_BUCKETS)
bucket_dfs   <- list()
for (i in seq_along(bucket_names)) {
  bn <- bucket_names[[i]]
  bucket_dfs[[bn]] <- fetch_arxiv_bucket(bn, ARXIV_BUCKETS[[bn]])
  if (i < length(bucket_names)) Sys.sleep(3)  # arXiv politeness: 1 req per 3s
}

arxiv_df <- bind_rows(bucket_dfs)

# A paper can match multiple buckets — collect the union of buckets per id
candidates <- if (nrow(arxiv_df) == 0L) {
  tibble()
} else {
  arxiv_df |>
    group_by(id) |>
    summarise(
      title    = first(title),
      abstract = first(abstract),
      authors  = list(first(authors)[[1]]),
      date     = first(date),
      source   = first(source),
      category = first(category),
      url      = first(url),
      buckets  = list(unique(bucket)),
      .groups  = "drop"
    ) |>
    arrange(desc(date))
}

# Cap candidates fed to the LLM. Since data_science floods the result,
# stratify the cap to keep finance + social_sciences well represented.
PER_BUCKET_CAP <- 25L
if (nrow(candidates) > 0L) {
  capped <- map_dfr(bucket_names, function(bn) {
    candidates |>
      filter(map_lgl(buckets, ~ bn %in% .x)) |>
      arrange(desc(date)) |>
      head(PER_BUCKET_CAP)
  }) |> distinct(id, .keep_all = TRUE)
  candidates <- capped
}

# Per-bucket category list for sources_used
sources_used <- candidates |>
  mutate(cats_split = strsplit(category, "; ")) |>
  pull(cats_split) |>
  unlist() |>
  unique() |>
  (\(x) if (length(x) > 0L) paste0("arxiv:", x) else character())()

message("Total deduped candidates: ", nrow(candidates))

if (nrow(candidates) == 0L) {
  write_stub_snapshot(0L, sources_used, "arXiv returned no papers in any bucket")
  quit(save = "no", status = 0)
}

bucket_counts <- map_int(bucket_names, ~ sum(map_lgl(candidates$buckets, function(b) .x %in% b)))
names(bucket_counts) <- bucket_names
message("Per-bucket counts: ",
        paste(names(bucket_counts), bucket_counts, sep = "=", collapse = ", "))

if (DRY_RUN) {
  message("DRY-RUN — skipping Phase B agent. Writing Phase-A-only snapshot.")
  write_snapshot(list(
    run_date         = format(TODAY, "%Y-%m-%d"),
    candidates_count = nrow(candidates),
    sources_used     = sources_used,
    note             = "Dry run: Phase A only, no LLM clustering.",
    top_papers       = empty_top_papers(),
    topics           = list(),
    bucket_counts    = as.list(bucket_counts),
    candidates_preview = head(
      candidates |>
        mutate(authors_str = map_chr(authors, ~ paste(head(.x, 3), collapse = ", ")),
               buckets_str = map_chr(buckets, ~ paste(.x, collapse = ", "))) |>
        select(id, title, source, date, buckets_str, url),
      30
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

# Build the candidate list as a compact text block for the agent. Include
# bucket assignment so the agent knows which top-5 list each paper qualifies for.
candidate_block <- candidates |>
  mutate(
    authors_str = map_chr(authors, ~ paste(head(.x, 4), collapse = ", ")),
    buckets_str = map_chr(buckets, ~ paste(.x, collapse = ", ")),
    abs_short   = ifelse(is.na(abstract) | nchar(abstract) == 0,
                         "(abstract not available)",
                         substr(abstract, 1, 600))
  ) |>
  mutate(
    block = sprintf(
      "[%s] %s\n  Buckets: %s | Category: %s | Date: %s\n  Authors: %s\n  Abstract: %s\n  URL: %s",
      id, title, buckets_str, category,
      ifelse(is.na(date), "?", as.character(date)),
      authors_str, abs_short, url
    )
  ) |>
  pull(block) |>
  paste(collapse = "\n\n")

paper_item_schema <- list(
  type = "object",
  properties = list(
    id           = list(type = "string"),
    rank         = list(type = "integer", description = "1 to 5 within the bucket"),
    why_trending = list(type = "string",
                        description = "1-2 sentences on why this is notable right now."),
    topics       = list(type = "array", items = list(type = "string"),
                        description = "Names of the topic clusters this paper belongs to.")
  ),
  required = list("id", "rank", "why_trending", "topics")
)

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
      "Submit the final clustered summary. Provide top_papers as three lists",
      "(data_science, finance, social_sciences) with up to 5 papers each, plus",
      "topics (5-10 named clusters spanning ALL candidates).",
      "Each top-5 paper MUST have the corresponding bucket in its 'Buckets:' field.",
      "DO NOT include edges — those are derived deterministically from paper_ids."
    ),
    input_schema = list(
      type = "object",
      properties = list(
        top_papers = list(
          type = "object",
          description = "Three top-5 lists keyed by bucket.",
          properties = list(
            data_science    = list(type = "array", items = paper_item_schema,
                                   description = "Top 5 papers in data science (stat.ML, cs.LG, stat.ME, stat.AP)."),
            finance         = list(type = "array", items = paper_item_schema,
                                   description = "Top 5 papers in finance (q-fin.*)."),
            social_sciences = list(type = "array", items = paper_item_schema,
                                   description = "Top 5 papers in social sciences (econ.*).")
          ),
          required = list("data_science", "finance", "social_sciences")
        ),
        topics = list(
          type  = "array",
          description = "5-10 named topic clusters spanning ALL candidates (not per-bucket).",
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
  "You are a research analyst surfacing the most interesting recent papers across",
  "three buckets:",
  "  - data_science    (stat.ML, cs.LG, stat.ME, stat.AP)",
  "  - finance         (q-fin.*)",
  "  - social_sciences (econ.*)",
  "",
  "You'll be given candidate papers from arXiv (last two weeks). Each paper lists",
  "the bucket(s) it belongs to. Your job:",
  "  1. Pick the TOP 5 most notable / trending papers PER BUCKET — three lists.",
  "     A paper can appear in multiple lists if its 'Buckets:' field includes",
  "     multiple buckets. Each list is ranked 1-5 (1 = most notable).",
  "     If a bucket has fewer than 5 candidates, return as many as exist.",
  "  2. Cluster ALL candidates into 5-10 named topic groups (cross-bucket).",
  "     Use human-readable topic names (e.g. 'transformer-based volatility",
  "     forecasting', 'reinforcement learning for portfolio allocation').",
  "  3. Each candidate paper should appear in at least one topic. Papers can",
  "     belong to multiple topics if they span clusters — this drives the",
  "     mind-graph edges (computed deterministically downstream).",
  "",
  "When ready, call finalize_research_summary. Do NOT emit edges — they're",
  "derived from paper_ids intersections in post-processing.",
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
  "Per-bucket counts: ",
  paste(names(bucket_counts), bucket_counts, sep = "=", collapse = ", "), "\n\n",
  "## Candidates\n\n",
  candidate_block, "\n\n",
  "Now pick top-5 per bucket, cluster all candidates into 5-10 topics, and call finalize_research_summary."
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
  if (nrow(row) == 0L) return(NULL)  # agent hallucinated id — drop it
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

clean_bucket_list <- function(lst) {
  if (is.null(lst)) return(list())
  out <- map(lst, enrich_top) |> compact()
  # Re-rank 1..N in case the agent skipped a number or ranked beyond available
  if (length(out) > 0L) {
    out <- out[order(map_int(out, ~ as.integer(.x$rank)))]
    for (i in seq_along(out)) out[[i]]$rank <- i
  }
  out
}

top_papers_clean <- list(
  data_science    = clean_bucket_list(agent_result$top_papers$data_science),
  finance         = clean_bucket_list(agent_result$top_papers$finance),
  social_sciences = clean_bucket_list(agent_result$top_papers$social_sciences)
)

topics_clean <- map(agent_result$topics, clean_topic) |> compact()

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
  bucket_counts    = as.list(bucket_counts),
  top_papers       = top_papers_clean,
  topics           = topics_clean
)

write_snapshot(snapshot)
message("Done. Topics: ", length(topics_clean),
        " | Top papers: ds=", length(top_papers_clean$data_science),
        ", fin=", length(top_papers_clean$finance),
        ", soc=", length(top_papers_clean$social_sciences),
        " | Candidates: ", nrow(candidates))
