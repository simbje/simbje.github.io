#!/usr/bin/env Rscript
# =============================================================================
# fix_post.R
# Runs after `quarto render`. Reads the freeze JSON for today's SSB post,
# detects R error blocks, calls Claude to fix the QMD, re-renders the single
# post, and appends the error pattern to error_patterns.md for future learning.
# Always exits 0 — never blocks deployment.
# =============================================================================

library(httr2)
library(jsonlite)

ANTHROPIC_API_KEY <- Sys.getenv("ANTHROPIC_API_KEY")
TODAY             <- Sys.Date()
POST_SLUG         <- format(TODAY, "%Y-%m-%d")
POST_DIR          <- file.path("ssb-daily", "posts", POST_SLUG)
POST_FILE         <- file.path(POST_DIR, "index.qmd")
FREEZE_JSON       <- file.path("_freeze", "ssb-daily", "posts", POST_SLUG,
                               "index", "execute-results", "html.json")
PATTERNS_FILE     <- file.path("ssb-daily", "error_patterns.md")
MAX_PATTERNS      <- 15L

message("fix_post.R — checking ", POST_SLUG)

# ── Safe exit helper ──────────────────────────────────────────────────────────
bail <- function(msg) { message(msg); quit(save = "no", status = 0) }

tryCatch({

  # ── 1. Verify post exists ──────────────────────────────────────────────────
  if (!file.exists(POST_FILE))  bail("No post for today — nothing to check.")
  if (!file.exists(FREEZE_JSON)) bail("Freeze JSON not found — render may have failed upstream.")

  # ── 2. Read freeze JSON and extract rendered markdown ──────────────────────
  freeze      <- jsonlite::read_json(FREEZE_JSON)
  rendered_md <- freeze$result$markdown
  if (is.null(rendered_md) || !nzchar(rendered_md))
    bail("Freeze JSON has no markdown content.")

  # ── 3. Detect error blocks ─────────────────────────────────────────────────
  lines      <- strsplit(rendered_md, "\n")[[1]]
  error_idx  <- grep("^## Error", lines)

  if (length(error_idx) == 0L) {
    bail("No errors found in rendered output — post looks clean.")
  }

  message("  Found ", length(error_idx), " error block(s).")

  # Collect ±4 lines of context around each error hit
  extract_context <- function(idx, all_lines, window = 4L) {
    start <- max(1L, idx - window)
    end   <- min(length(all_lines), idx + window)
    paste(all_lines[start:end], collapse = "\n")
  }
  error_snippets <- vapply(error_idx, extract_context, character(1L),
                           all_lines = lines)
  error_text <- paste(
    paste0("--- Error ", seq_along(error_snippets), " ---\n", error_snippets),
    collapse = "\n\n"
  )

  # ── 4. Read the raw QMD ───────────────────────────────────────────────────
  qmd_content <- paste(readLines(POST_FILE, warn = FALSE), collapse = "\n")

  # ── 5. Bail if no API key ─────────────────────────────────────────────────
  if (nchar(ANTHROPIC_API_KEY) == 0L)
    bail("ANTHROPIC_API_KEY not set — cannot fix errors automatically.")

  # ── 6. Call Claude to fix the QMD ─────────────────────────────────────────
  message("  Calling Claude to fix errors...")

  fixer_system <- paste0(
    "You are fixing R runtime errors in a Quarto blog post.\n\n",
    "Rules:\n",
    "- Return ONLY the corrected .qmd, starting with ---\n",
    "- Do NOT rewrite, restructure, or improve working code\n",
    "- Fix only the specific errors shown — minimal targeted changes\n",
    "- Common fixes: add null guards (if (!is.null(df))), fix wrong filter values, ",
    "add missing print(), fix mismatched column names\n",
    "- After fixing, add a one-line comment # fixed: <brief reason> near the change\n\n",
    "OUTPUT FORMAT:\n",
    "Line 1: FIXES: <comma-separated list of what was fixed>\n",
    "Line 2+: Corrected .qmd starting with ---"
  )

  fixer_user <- paste0(
    "The following R errors appeared in the rendered output:\n\n",
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

  # Extract fixes line
  fixes_line <- tryCatch(
    regmatches(raw_response,
               regexpr("(?i)^FIXES:[ \t]*[^\n]*", raw_response, perl = TRUE)),
    error = function(e) character(0)
  )
  fixes_desc <- if (length(fixes_line) > 0L)
    trimws(sub("(?i)^FIXES:[ \t]*", "", fixes_line, perl = TRUE))
  else "unknown"
  message("  Fixes: ", fixes_desc)

  # Extract corrected QMD
  m <- regexpr("(?m)^---[ \t]*$", raw_response, perl = TRUE)
  if (m[[1]] < 0L) bail("Claude response contained no QMD front matter — skipping.")
  fixed_qmd <- substring(raw_response, m[[1]])

  # ── 7. Write fixed QMD ────────────────────────────────────────────────────
  writeLines(fixed_qmd, POST_FILE)
  message("  Fixed QMD written to ", POST_FILE)

  # ── 8. Delete freeze cache and re-render single post ─────────────────────
  freeze_post_dir <- file.path("_freeze", "ssb-daily", "posts", POST_SLUG)
  if (dir.exists(freeze_post_dir)) {
    unlink(freeze_post_dir, recursive = TRUE)
    message("  Cleared freeze cache: ", freeze_post_dir)
  }

  message("  Re-rendering post...")
  render_rc <- system2("quarto", c("render", POST_FILE), stdout = TRUE, stderr = TRUE)
  message(paste(render_rc, collapse = "\n"))

  # ── 9. Append to error_patterns.md ───────────────────────────────────────
  first_error <- trimws(lines[error_idx[1]])
  new_entry <- paste0(
    "## ", POST_SLUG, "\n\n",
    "**Error:** `", substr(first_error, 1L, 120L), "`\n",
    "**Fixes applied:** ", fixes_desc, "\n"
  )

  existing_content <- if (file.exists(PATTERNS_FILE) && file.size(PATTERNS_FILE) > 0L)
    paste(readLines(PATTERNS_FILE, warn = FALSE), collapse = "\n")
  else ""

  # Split into sections by ## heading and keep last MAX_PATTERNS - 1
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
