#!/usr/bin/env Rscript
# =============================================================================
# test_local.R
# Local debugging tool for SSB daily posts.
#
# Usage (from project root):
#   Rscript ssb-daily/test_local.R                        # test today's post
#   Rscript ssb-daily/test_local.R 2026-03-12             # test a specific post
#   Rscript ssb-daily/test_local.R --generate             # generate + test a new post
#   Rscript ssb-daily/test_local.R --diagnose 2026-03-12  # diagnose without rendering
#
# What it does:
#   1. Deletes the _freeze cache for the post so it always re-executes
#   2. Renders the post with quarto and captures all output/errors
#   3. Checks for the known "silent plot failure" pattern (no figure output)
#   4. Opens the result in your browser
# =============================================================================

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(lubridate)
  library(glue)
})

# ── Parse arguments ───────────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)
GENERATE_NEW  <- "--generate"  %in% args
DIAGNOSE_ONLY <- "--diagnose"  %in% args
date_args     <- args[!grepl("^--", args)]
TARGET_DATE   <- if (length(date_args) > 0) date_args[1] else format(Sys.Date(), "%Y-%m-%d")

POST_DIR      <- file.path("ssb-daily", "posts", TARGET_DATE)
POST_FILE     <- file.path(POST_DIR, "index.qmd")
FREEZE_DIR    <- file.path("_freeze", "ssb-daily", "posts", TARGET_DATE)

cat("=============================================================\n")
cat("SSB Daily Local Test Tool\n")
cat("=============================================================\n")
cat("Target date :", TARGET_DATE, "\n")
cat("Post file   :", POST_FILE, "\n\n")

# ── Step 0 (optional): generate a fresh post via Claude API ──────────────────
if (GENERATE_NEW) {
  ANTHROPIC_API_KEY <- Sys.getenv("ANTHROPIC_API_KEY")
  if (nchar(ANTHROPIC_API_KEY) == 0) {
    stop("ANTHROPIC_API_KEY not set. Add it to your .Renviron:\n",
         "  usethis::edit_r_environ()\n",
         "  ANTHROPIC_API_KEY=sk-ant-...")
  }
  cat("Generating new post via Claude API...\n")

  # Remove existing post so generate_post.R doesn't skip
  if (dir.exists(POST_DIR)) {
    message("Removing existing post for ", TARGET_DATE, " before re-generating.")
    unlink(POST_DIR, recursive = TRUE)
  }

  source("ssb-daily/generate_post.R")
}

# ── Step 1: Check post exists ────────────────────────────────────────────────
if (!file.exists(POST_FILE)) {
  cat("ERROR: Post not found:", POST_FILE, "\n")
  cat("Run with --generate to create it, or pass a valid date.\n\n")
  cat("Available posts:\n")
  existing <- list.dirs("ssb-daily/posts", full.names = FALSE, recursive = FALSE)
  cat(paste(" -", sort(existing), collapse = "\n"), "\n")
  quit(status = 1)
}

# ── Step 2: Diagnose the .qmd source ─────────────────────────────────────────
cat("--------------------------------------------------------------\n")
cat("DIAGNOSIS: Inspecting .qmd source\n")
cat("--------------------------------------------------------------\n")

qmd_text <- readLines(POST_FILE, warn = FALSE, encoding = "UTF-8")

# Check for known problematic patterns
issues <- character(0)

# Norwegian hardcoded column names in rename()
norw_renames <- grep("rename\\(.*[æøåÆØÅ]", qmd_text, value = TRUE)
if (length(norw_renames) > 0) {
  issues <- c(issues, paste0(
    "ISSUE: rename() uses hardcoded Norwegian column names.\n",
    "  These may fail on Linux (CI) due to UTF-8 encoding mismatches.\n",
    "  Affected lines:\n",
    paste("    ", norw_renames, collapse = "\n")
  ))
}

# filter() using Norwegian string literals
norw_filters <- grep('(filter|==).*"[^"]*[æøåÆØÅ][^"]*"', qmd_text, value = TRUE)
if (length(norw_filters) > 0) {
  issues <- c(issues, paste0(
    "ISSUE: filter() or == uses hardcoded Norwegian string values.\n",
    "  E.g. gender == \"Begge kjønn\" may not match actual API output on CI.\n",
    "  Affected lines (sample):\n",
    paste("    ", head(norw_filters, 5), collapse = "\n")
  ))
}

# Old stat(x) ggridges syntax (deprecated, errors in newer versions)
stat_x <- grep("stat\\(x\\)", qmd_text, value = TRUE)
if (length(stat_x) > 0) {
  issues <- c(issues, paste0(
    "ISSUE: stat(x) is deprecated in ggridges. Use after_stat(x) instead.\n",
    "  Affected lines:\n",
    paste("    ", stat_x, collapse = "\n")
  ))
}

# Plots without print()
# Find ggplot calls not followed by print()
gg_chunks <- which(grepl("ggplot\\(", qmd_text))
no_print  <- sapply(gg_chunks, function(i) {
  chunk_window <- qmd_text[i:min(i + 30, length(qmd_text))]
  !any(grepl("^\\s*print\\(", chunk_window))
})
if (any(no_print)) {
  issues <- c(issues,
    "ISSUE: Some ggplot() calls may not have explicit print(p). Implicit printing\n",
    "  is unreliable inside Quarto chunks. Always end plot chunks with print(p)."
  )
}

# error: false in global quarto config may silence plot errors
if (file.exists("_quarto.yml")) {
  qy <- readLines("_quarto.yml", warn = FALSE)
  if (any(grepl("error:\\s*false", qy))) {
    issues <- c(issues,
      "NOTE: _quarto.yml has 'execute: error: false' — R errors in chunks halt rendering.\n",
      "  Posts use knitr::opts_chunk$set(error=TRUE) which should override this, but\n",
      "  verify the setup chunk is the very first code chunk in the post."
    )
  }
}

if (length(issues) == 0) {
  cat("No obvious source issues detected.\n")
} else {
  for (iss in issues) cat("  *", iss, "\n")
}

# ── Step 3: Run SSB data fetches in-process to check for NULL dfs ──────────--
cat("\n--------------------------------------------------------------\n")
cat("DIAGNOSIS: Testing SSB API data fetches from this post\n")
cat("--------------------------------------------------------------\n")

# Extract all ApiData() calls from the post
api_calls <- grep("ApiData\\(", qmd_text, value = TRUE)
table_ids  <- unique(regmatches(
  api_calls,
  regexpr("table/([0-9]+)", api_calls)
))
table_ids  <- gsub("table/", "", table_ids)

if (length(table_ids) == 0) {
  cat("No SSB table IDs found in post.\n")
} else {
  cat("SSB tables referenced:", paste(table_ids, collapse = ", "), "\n\n")
  if (requireNamespace("PxWebApiData", quietly = TRUE)) {
    for (tid in table_ids) {
      url <- paste0("https://data.ssb.no/api/v0/no/table/", tid)
      cat("Checking table", tid, "... ")
      result <- tryCatch({
        meta <- PxWebApiData::ApiData(url, returnMetaFrames = TRUE)
        cat("OK (", length(meta), "dimensions )\n")
        cat("  Dimensions and first values:\n")
        for (d in names(meta)) {
          vals <- head(meta[[d]][[1]], 3)
          cat("  ", d, ":", paste(vals, collapse = ", "), "\n")
        }
      }, error = function(e) {
        cat("FAILED:", conditionMessage(e), "\n")
      })
      Sys.sleep(0.3)
    }
  } else {
    cat("PxWebApiData not installed — skipping live API check.\n")
  }
}

# ── Step 4: Check existing freeze cache for missing figure output ─────────---
cat("\n--------------------------------------------------------------\n")
cat("DIAGNOSIS: Checking freeze cache for missing plot output\n")
cat("--------------------------------------------------------------\n")

freeze_json <- file.path(FREEZE_DIR, "index", "execute-results", "html.json")
if (!file.exists(freeze_json)) {
  cat("No freeze cache found — post will be freshly executed on render.\n")
} else {
  cat("Freeze cache found:", freeze_json, "\n")
  freeze <- jsonlite::fromJSON(freeze_json)
  md     <- freeze$result$markdown

  # Count plot chunks vs figure outputs
  n_plot_chunks  <- length(gregexpr("print\\(p", md)[[1]])
  n_fig_outputs  <- length(gregexpr("cell-output-display", md)[[1]])

  cat(sprintf("  print(p*) calls in frozen markdown : %d\n", max(0, n_plot_chunks)))
  cat(sprintf("  cell-output-display (figures) found: %d\n", max(0, n_fig_outputs)))

  if (n_plot_chunks > 0 && n_fig_outputs <= 0) {
    cat("\n  *** CONFIRMED BUG: Plots ran but no figure output was captured! ***\n")
    cat("  This freeze is broken. It will be deleted so the post re-executes.\n")
  } else if (n_fig_outputs > 0) {
    cat("  Freeze looks OK — figures are present in cache.\n")
  }
}

if (DIAGNOSE_ONLY) {
  cat("\nDiagnosis complete (--diagnose mode, skipping render).\n")
  quit(status = 0)
}

# ── Step 5: Delete the freeze cache so the post re-executes fully ─────────---
cat("\n--------------------------------------------------------------\n")
cat("ACTION: Clearing freeze cache\n")
cat("--------------------------------------------------------------\n")

if (dir.exists(FREEZE_DIR)) {
  unlink(FREEZE_DIR, recursive = TRUE)
  cat("Deleted:", FREEZE_DIR, "\n")
} else {
  cat("No freeze cache to delete.\n")
}

# ── Step 6: Render the post with quarto ──────────────────────────────────────
cat("\n--------------------------------------------------------------\n")
cat("RENDER: Running quarto render on post\n")
cat("--------------------------------------------------------------\n")
cat("This re-executes all chunks — watch for errors below.\n\n")

quarto_cmd <- paste0('quarto render "', POST_FILE, '" --execute --no-cache 2>&1')
cat("Command:", quarto_cmd, "\n\n")

render_output <- system(quarto_cmd, intern = TRUE)
cat(paste(render_output, collapse = "\n"), "\n")

# Detect errors in render output
error_lines <- grep("Error|error|failed|NULL|warning: no figure", render_output,
                    value = TRUE, ignore.case = FALSE)
if (length(error_lines) > 0) {
  cat("\n*** ERRORS / WARNINGS detected in render output: ***\n")
  cat(paste(error_lines, collapse = "\n"), "\n")
}

# ── Step 7: Check the rendered HTML for figures ───────────────────────────---
cat("\n--------------------------------------------------------------\n")
cat("CHECKING rendered HTML for <img> tags\n")
cat("--------------------------------------------------------------\n")

# quarto renders individual files to _site/ssb-daily/posts/<date>/index.html
html_path <- file.path("_site", "ssb-daily", "posts", TARGET_DATE, "index.html")
if (!file.exists(html_path)) {
  cat("Rendered HTML not found at:", html_path, "\n")
  cat("(Render may have failed, or output path differs)\n")
} else {
  html <- paste(readLines(html_path, warn = FALSE), collapse = "\n")
  n_imgs   <- length(gregexpr("<img", html)[[1]])
  n_canvas <- length(gregexpr("cell-output-display", html)[[1]])
  cat("Images (<img> tags) in rendered HTML     :", max(0, n_imgs), "\n")
  cat("Plot output divs (cell-output-display)   :", max(0, n_canvas), "\n")
  if (n_imgs <= 1) {  # 1 because there may be navbar logo etc.
    cat("\n*** WARNING: Very few images found — plots likely missing from output. ***\n")
  } else {
    cat("Looks good — images are present in the rendered output.\n")
  }
}

# ── Step 8: Open in browser ──────────────────────────────────────────────────
if (file.exists(html_path)) {
  cat("\nOpening rendered post in browser...\n")
  if (.Platform$OS.type == "windows") {
    shell(paste0('start "" "', normalizePath(html_path, winslash = "\\"), '"'))
  } else {
    system(paste0("open '", html_path, "'"))
  }
}

cat("\n=============================================================\n")
cat("Done. Check the output above for errors.\n")
cat("=============================================================\n")
