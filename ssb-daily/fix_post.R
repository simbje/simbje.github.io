#!/usr/bin/env Rscript
# =============================================================================
# fix_post.R
# Runs after `quarto render`. Reads the freeze JSON for today's SSB post,
# detects R error blocks AND missing figures, calls Claude to fix the QMD,
# re-renders the single post, and appends the error pattern to
# error_patterns.md for future learning.
# Always exits 0 — never blocks deployment.
# =============================================================================

library(httr2)
library(jsonlite)

ANTHROPIC_API_KEY <- Sys.getenv("ANTHROPIC_API_KEY")
TODAY             <- Sys.Date()
POST_SLUG         <- format(TODAY, "%Y-%m-%d")
POST_DIR          <- file.path("ssb-daily", "posts", POST_SLUG)
POST_FILE         <- file.path(POST_DIR, "index.qmd")
FREEZE_DIR        <- file.path("_freeze", "ssb-daily", "posts", POST_SLUG, "index")
FREEZE_JSON       <- file.path(FREEZE_DIR, "execute-results", "html.json")
FIGURE_DIR        <- file.path(FREEZE_DIR, "figure-html")
PATTERNS_FILE     <- file.path("ssb-daily", "error_patterns.md")
MAX_PATTERNS      <- 15L

message("fix_post.R — checking ", POST_SLUG)

bail <- function(msg) { message(msg); quit(save = "no", status = 0) }

# ── Detect ::: cell-output-error blocks (and legacy ## Error headings) ────────
# Returns a list of character snippets, one per error block.
detect_error_blocks <- function(lines) {
  open_re <- "^::: \\{\\.cell-output \\.cell-output-error\\}"
  head_re <- "^## Error"
  open_idx <- grep(open_re, lines)
  head_idx <- grep(head_re, lines)

  snippets <- character(0)

  # Walk each ::: opener to its matching close fence (lone ":::" line)
  for (i in open_idx) {
    end <- length(lines)
    for (j in (i + 1L):min(length(lines), i + 20L)) {
      if (identical(trimws(lines[j]), ":::")) { end <- j; break }
    }
    snippets <- c(snippets, paste(lines[i:end], collapse = "\n"))
  }

  # Legacy ## Error: ±4-line window
  for (i in head_idx) {
    start <- max(1L, i - 4L)
    end   <- min(length(lines), i + 4L)
    snippets <- c(snippets, paste(lines[start:end], collapse = "\n"))
  }

  snippets
}

# ── List all plot-* chunk labels declared in the QMD ─────────────────────────
# Posts keep message=FALSE so the published reports stay clean — that means an
# empty SSB fetch leaves NO visible trace in the rendered markdown. We therefore
# classify data-unavailability structurally: every declared plot chunk produced
# no figure AND nothing actually errored.
plot_chunk_labels <- function(qmd_path) {
  if (!file.exists(qmd_path)) return(character(0))
  qmd <- readLines(qmd_path, warn = FALSE)
  fence_idx <- grep("^```\\{r\\s+([^,}\\s]+)", qmd, perl = TRUE)
  labels <- sub("^```\\{r\\s+([^,}\\s]+).*$", "\\1", qmd[fence_idx], perl = TRUE)
  labels[grepl("^plot[-_]", labels)]
}

# ── Extract SSB table IDs referenced in the QMD ──────────────────────────────
# Handles both forms the posts use:
#   ApiData("https://data.ssb.no/api/v0/no/table/05196", ...)   (URL — the common one)
#   ApiData("05196", ...) / ApiData(05196, ...)                 (bare ID)
extract_ssb_table_ids <- function(qmd_path) {
  if (!file.exists(qmd_path)) return(character(0))
  txt <- paste(readLines(qmd_path, warn = FALSE), collapse = "\n")
  url_ids  <- unlist(regmatches(txt, gregexpr("table/(\\d{4,6})", txt, perl = TRUE)))
  bare_ids <- unlist(regmatches(txt, gregexpr('ApiData\\s*\\(\\s*["\']?(\\d{4,6})', txt, perl = TRUE)))
  ids <- gsub("\\D", "", c(url_ids, bare_ids))            # keep digits only
  unique(ids[nchar(ids) >= 4L & nchar(ids) <= 6L])
}

# ── Detect plot-* chunks in the QMD that produced no figure ──────────────────
# Returns a character vector of chunk labels with no corresponding PNG.
detect_empty_figures <- function(qmd_path, figure_dir) {
  if (!file.exists(qmd_path)) return(character(0))
  qmd <- readLines(qmd_path, warn = FALSE)
  fence_idx <- grep("^```\\{r\\s+([^,}\\s]+)", qmd, perl = TRUE)
  labels <- sub("^```\\{r\\s+([^,}\\s]+).*$", "\\1", qmd[fence_idx], perl = TRUE)
  plot_labels <- labels[grepl("^plot[-_]", labels)]
  if (length(plot_labels) == 0L) return(character(0))

  if (!dir.exists(figure_dir)) {
    # Whole figure dir is missing — every plot chunk is empty
    return(plot_labels)
  }

  pngs <- list.files(figure_dir, pattern = "\\.png$")
  # PNG names are "{label}-{n}.png"; strip "-N.png" to recover the chunk label
  png_labels <- unique(sub("-\\d+\\.png$", "", pngs))
  setdiff(plot_labels, png_labels)
}

# ── Log SSB table IDs that returned no data (blacklist source) ───────────────
# generate_post.R reads these "Data unavailable" entries and (a) strips the IDs
# from its seed list and (b) tells the discovery agent never to pick them again.
log_unavailable <- function(slug, table_ids, patterns_file, max_patterns) {
  table_str <- if (length(table_ids) > 0L) paste(table_ids, collapse = ", ") else "unknown"
  new_entry <- paste0(
    "## ", slug, "\n\n",
    "**Data unavailable:** SSB tables ", table_str,
      " returned no data or API error\n",
    "**Fixes applied:** none (post scrapped — data-level issue, not a code bug)\n"
  )
  existing <- if (file.exists(patterns_file) && file.size(patterns_file) > 0L)
    paste(readLines(patterns_file, warn = FALSE), collapse = "\n") else ""
  sections <- if (nzchar(existing))
    strsplit(existing, "\n(?=## )", perl = TRUE)[[1]] else character(0)
  sections <- tail(sections, max_patterns - 1L)
  writeLines(paste(c(sections, new_entry), collapse = "\n\n"), patterns_file)
}

# ── Drop a scrapped post's row from the topic index ──────────────────────────
# The post is being unpublished, so it should not linger in the "recent posts"
# list the discovery agent uses to avoid repetition.
remove_topic_index_row <- function(slug) {
  idx <- file.path("ssb-daily", "posts", "_topic_index.csv")
  if (!file.exists(idx)) return(invisible())
  df <- tryCatch(read.csv(idx, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(df) || !"date" %in% names(df)) return(invisible())
  keep <- df[df$date != slug, , drop = FALSE]
  if (nrow(keep) < nrow(df)) write.csv(keep, idx, row.names = FALSE)
}

tryCatch({

  if (!file.exists(POST_FILE))  bail("No post for today — nothing to check.")
  if (!file.exists(FREEZE_JSON)) bail("Freeze JSON not found — render may have failed upstream.")

  # ── Read freeze JSON ──────────────────────────────────────────────────────
  freeze      <- jsonlite::read_json(FREEZE_JSON)
  rendered_md <- freeze$result$markdown
  if (is.null(rendered_md) || !nzchar(rendered_md))
    bail("Freeze JSON has no markdown content.")

  lines <- strsplit(rendered_md, "\n")[[1]]

  # ── Detect errors ─────────────────────────────────────────────────────────
  error_snippets <- detect_error_blocks(lines)
  empty_figures  <- detect_empty_figures(POST_FILE, FIGURE_DIR)
  all_plots      <- plot_chunk_labels(POST_FILE)
  ssb_table_ids  <- extract_ssb_table_ids(POST_FILE)

  has_code_error <- length(error_snippets) > 0L

  # Data-unavailability vs code bug:
  #  - A genuine code bug surfaces as a ::: cell-output-error block (error=TRUE)
  #    and is fixable → hand it to Claude.
  #  - An empty SSB fetch/filter does NOT error: the plot template's else-branch
  #    emits the note "...returned no data for this series" (via cat, so it lands
  #    in the rendered markdown even though message=FALSE). That note — or, for
  #    older posts without it, every plot chunk coming back empty — means the
  #    DATA was unavailable. Such a post is not worth publishing: scrap it and
  #    blacklist its tables so the model stops downloading from them.
  note_present    <- any(grepl("returned no data for this series", lines, fixed = TRUE))
  all_plots_empty <- length(all_plots) > 0L &&
    length(empty_figures) == length(all_plots)
  scrap_post      <- !has_code_error && (note_present || all_plots_empty)

  if (!scrap_post && length(error_snippets) == 0L && length(empty_figures) == 0L) {
    bail("No errors or missing figures found — post looks clean.")
  }

  message("  Found ", length(error_snippets), " error block(s), ",
          length(empty_figures), "/", max(length(all_plots), 0L), " plot(s) missing",
          if (note_present) ", no-data note present" else "", ".")

  # ── Scrap path: data unavailable → unpublish the post + blacklist tables ──
  if (scrap_post) {
    table_str <- if (length(ssb_table_ids) > 0L)
      paste(ssb_table_ids, collapse = ", ") else "unknown"
    why <- if (note_present) "emitted the no-data note" else "produced no figures at all"
    message("  Data unavailable (", why, ") — scrapping post. SSB tables: ", table_str)

    # 1. Blacklist the tables so the discovery agent stops downloading them.
    log_unavailable(POST_SLUG, ssb_table_ids, PATTERNS_FILE, MAX_PATTERNS)
    message("  Blacklisted tables logged to ", PATTERNS_FILE)

    # 2. Forget the scrapped angle so it isn't counted as a published post.
    remove_topic_index_row(POST_SLUG)

    # 3. Delete source, freeze cache and any already-rendered output so the
    #    half-empty article never reaches the published site.
    unlink(POST_DIR, recursive = TRUE)
    unlink(file.path("_freeze", "ssb-daily", "posts", POST_SLUG), recursive = TRUE)
    unlink(file.path("_site",   "ssb-daily", "posts", POST_SLUG), recursive = TRUE)
    message("  Removed sources + rendered output for ", POST_SLUG)

    # 4. Re-render so the listing, RSS feed and category pages drop the post.
    rr <- tryCatch(
      system2("quarto", "render", stdout = TRUE, stderr = TRUE),
      error = function(e) paste("re-render failed:", conditionMessage(e))
    )
    message(paste(utils::tail(rr, 5L), collapse = "\n"))

    bail("Post scrapped (data unavailable) and tables blacklisted.")
  }

  # ── Build error_text for Claude (code-bug path only) ──────────────────────
  parts <- character(0)
  if (length(error_snippets) > 0L) {
    parts <- c(parts, paste0(
      "--- Error ", seq_along(error_snippets), " ---\n", error_snippets
    ))
  }
  if (length(empty_figures) > 0L) {
    parts <- c(parts, paste0(
      "--- Missing figure: chunk '", empty_figures, "' produced no PNG ---\n",
      "The plot chunk ran but emitted no figure. Likely causes: ggplot built ",
      "but never print()-ed; chunk's null-guard skipped the body silently; ",
      "data frame referenced is undefined or empty."
    ))
  }
  error_text <- paste(parts, collapse = "\n\n")

  # ── Read raw QMD ──────────────────────────────────────────────────────────
  qmd_content <- paste(readLines(POST_FILE, warn = FALSE), collapse = "\n")

  if (nchar(ANTHROPIC_API_KEY) == 0L)
    bail("ANTHROPIC_API_KEY not set — cannot fix errors automatically.")

  # ── Call Claude to fix the QMD ────────────────────────────────────────────
  message("  Calling Claude to fix errors...")

  fixer_system <- paste0(
    "You are fixing R runtime errors and missing-figure bugs in a Quarto blog post.\n\n",
    "Rules:\n",
    "- Return ONLY the corrected .qmd, starting with ---\n",
    "- Do NOT rewrite, restructure, or improve working code\n",
    "- Fix only the specific errors shown — minimal targeted changes\n",
    "- Common fixes:\n",
    "  * 'object X not found' → variable assigned only inside an if-branch that didn't run. ",
    "    Initialize `X <- NULL` at the top of the wrangle chunk BEFORE any conditional logic.\n",
    "  * Plot guards must use `if (exists(\"X\") && !is.null(X) && nrow(X) > 0)` to be fully safe.\n",
    "  * Missing figure but no error → add explicit `print(p)` if missing; ensure null-guard does not silently skip every branch.\n",
    "  * Empty SSB fetch → add `if (is.null(df) || nrow(df) == 0) df <- NULL` after ApiData().\n",
    "  * Wrong filter values, mismatched column names → fix to match the spec.\n",
    "- After fixing, add a one-line comment # fixed: <brief reason> near the change\n\n",
    "OUTPUT FORMAT:\n",
    "Line 1: FIXES: <comma-separated list of what was fixed>\n",
    "Line 2+: Corrected .qmd starting with ---"
  )

  fixer_user <- paste0(
    "The following issues appeared in the rendered output:\n\n",
    error_text, "\n\n",
    "Here is the full QMD source:\n\n",
    qmd_content
  )

  resp <- tryCatch(
    request("https://api.anthropic.com/v1/messages") |>
      req_headers(
        "x-api-key"         = ANTHROPIC_API_KEY,
        "anthropic-version" = "2023-06-01",
        "content-type"      = "application/json"
      ) |>
      req_body_json(list(
        model      = "claude-haiku-4-5-20251001",
        max_tokens = 16000L,
        system     = fixer_system,
        messages   = list(list(role = "user", content = fixer_user))
      )) |>
      req_timeout(300) |>
      req_perform(),
    error = function(e) { message("  Claude API error: ", e$message); NULL }
  )

  if (is.null(resp)) bail("Claude call failed — leaving original post in place.")

  raw_response <- tryCatch({
    r   <- resp_body_json(resp)
    blk <- Filter(function(b) identical(b$type, "text"), r$content)
    if (length(blk) == 0L) stop("no text blocks")
    paste(vapply(blk, function(b) b$text, character(1L)), collapse = "")
  }, error = function(e) { message("  Response parse failed: ", e$message); NULL })

  if (is.null(raw_response)) bail("Could not parse Claude response.")

  fixes_line <- tryCatch(
    regmatches(raw_response,
               regexpr("(?i)^FIXES:[ \t]*[^\n]*", raw_response, perl = TRUE)),
    error = function(e) character(0)
  )
  fixes_desc <- if (length(fixes_line) > 0L)
    trimws(sub("(?i)^FIXES:[ \t]*", "", fixes_line, perl = TRUE))
  else "unknown"
  message("  Fixes: ", fixes_desc)

  m <- regexpr("(?m)^---[ \t]*$", raw_response, perl = TRUE)
  if (m[[1]] < 0L) bail("Claude response contained no QMD front matter — skipping.")
  fixed_qmd <- substring(raw_response, m[[1]])

  writeLines(fixed_qmd, POST_FILE)
  message("  Fixed QMD written to ", POST_FILE)

  # ── Delete freeze cache and re-render ────────────────────────────────────
  freeze_post_dir <- file.path("_freeze", "ssb-daily", "posts", POST_SLUG)
  if (dir.exists(freeze_post_dir)) {
    unlink(freeze_post_dir, recursive = TRUE)
    message("  Cleared freeze cache: ", freeze_post_dir)
  }

  message("  Re-rendering post...")
  render_rc <- system2("quarto", c("render", POST_FILE), stdout = TRUE, stderr = TRUE)
  message(paste(render_rc, collapse = "\n"))

  # ── Append to error_patterns.md ──────────────────────────────────────────
  first_signal <- if (length(error_snippets) > 0L) {
    # First non-empty line inside the first error block
    err_lines <- strsplit(error_snippets[1], "\n")[[1]]
    err_msg   <- err_lines[grepl("^Error", err_lines, perl = TRUE)]
    if (length(err_msg) > 0L) trimws(err_msg[1]) else trimws(err_lines[1])
  } else {
    paste0("Missing figure: ", empty_figures[1])
  }

  new_entry <- paste0(
    "## ", POST_SLUG, "\n\n",
    "**Error:** `", substr(first_signal, 1L, 120L), "`\n",
    "**Fixes applied:** ", fixes_desc, "\n"
  )

  existing_content <- if (file.exists(PATTERNS_FILE) && file.size(PATTERNS_FILE) > 0L)
    paste(readLines(PATTERNS_FILE, warn = FALSE), collapse = "\n")
  else ""

  sections <- if (nzchar(existing_content))
    strsplit(existing_content, "\n(?=## )", perl = TRUE)[[1]]
  else character(0)
  sections <- tail(sections, MAX_PATTERNS - 1L)

  updated <- paste(c(sections, new_entry), collapse = "\n\n")
  writeLines(updated, PATTERNS_FILE)
  message("  Pattern appended to ", PATTERNS_FILE)

  message("fix_post.R complete — errors fixed and pattern recorded.")

}, error = function(e) {
  message("fix_post.R unexpected error: ", e$message, " — continuing deployment.")
})

quit(save = "no", status = 0)
