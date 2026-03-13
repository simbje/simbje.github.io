#!/usr/bin/env Rscript
# =============================================================================
# debug_plots.R
# Run this interactively in RStudio to trace exactly why plots fail.
# Each section is meant to be run step by step (Ctrl+Enter).
# =============================================================================

library(tidyverse)
library(PxWebApiData)
library(scales)

# ── 1. Check what column names the SSB API actually returns ──────────────────
# This is the most common cause of silent failures.
# On Linux/CI the Unicode encoding of Norwegian chars can differ from Windows.

cat("=== SSB column name encoding check ===\n\n")

check_table <- function(table_id, params = list()) {
  url <- paste0("https://data.ssb.no/api/v0/no/table/", table_id)
  cat("Table:", table_id, "\n")

  # Get metadata
  meta <- tryCatch(
    PxWebApiData::ApiData(url, returnMetaFrames = TRUE),
    error = function(e) { cat("  META FAILED:", e$message, "\n"); NULL }
  )
  if (!is.null(meta)) {
    cat("  Dimensions:", paste(names(meta), collapse = ", "), "\n")
    for (d in names(meta)) {
      cat("  ", d, " — first value:", meta[[d]][[1]][1], "\n")
      # Show raw bytes of dimension name to detect encoding issues
      cat("    raw bytes of dim name:", paste(chartr("", "", chartr("", "", d)), "\n"))
    }
  }

  # Fetch actual data
  fetch_args <- c(list(url), params)
  raw <- tryCatch(
    do.call(PxWebApiData::ApiData, fetch_args),
    error = function(e) { cat("  FETCH FAILED:", e$message, "\n"); NULL }
  )
  if (!is.null(raw)) {
    df <- raw[[1]]
    cat("  Data columns:", paste(names(df), collapse = ", "), "\n")
    cat("  Rows:", nrow(df), "\n")
    cat("  First few values column by column:\n")
    for (col in names(df)) {
      cat("    ", col, ":", paste(head(unique(df[[col]]), 4), collapse = " | "), "\n")
    }
  }
  cat("\n")
  invisible(raw)
}

# Test population table (05810) — most commonly used
raw_pop <- check_table("05810", list(
  Kjonn = TRUE,
  Alder = TRUE,
  Tid = list(filter = "top", values = 5)
))

# Test fertility table (05196)
raw_fert <- check_table("05196", list(
  Kjonn = TRUE,
  Alder = TRUE,
  Tid = list(filter = "top", values = 5)
))

# Test bankruptcies (10582)
raw_bank <- check_table("10582", list(
  NACE2007 = TRUE,
  ContentsCode = TRUE,
  Tid = list(filter = "top", values = 5)
))

# ── 2. Check whether rename() works with the actual column names ──────────---
cat("=== Testing rename() with Norwegian column names ===\n\n")

if (!is.null(raw_pop)) {
  df <- raw_pop[[1]]
  cat("Actual column names from API:\n")
  print(names(df))

  # Test hardcoded rename (this is what the generated posts do)
  test_rename <- tryCatch({
    df |> rename(gender = kjønn, age_group = alder)
    "rename(kjønn, alder) SUCCEEDED"
  }, error = function(e) {
    paste("rename(kjønn, alder) FAILED:", e$message)
  })
  cat(test_rename, "\n")

  # Test safe approach: find columns by position instead
  # (This is what the posts SHOULD do)
  kjonn_col <- grep("[Kk]j.*nn|gender|sex", names(df), value = TRUE)[1]
  alder_col <- grep("[Aa]lder|age", names(df), value = TRUE)[1]
  cat("Safe column detection: kjønn-like =", kjonn_col, ", alder-like =", alder_col, "\n")
}

# ── 3. Reproduce a plot from a real post and check if it works ───────────────
cat("\n=== Reproducing population age plot from 2026-03-12 post ===\n\n")

if (!is.null(raw_pop)) {
  df_pop <- raw_pop[[1]] |>
    mutate(
      value    = as.numeric(value),
      time_col = names(raw_pop[[1]])[grepl("tid|aar|year|år", names(raw_pop[[1]]), ignore.case = TRUE)][1]
    )

  # Safe: use actual column name discovered above
  time_col <- names(raw_pop[[1]])[grepl("tid|aar|year|år", names(raw_pop[[1]]), ignore.case = TRUE)][1]
  cat("Time column detected:", time_col, "\n")

  df_pop <- raw_pop[[1]] |>
    mutate(
      value = as.numeric(value),
      year  = as.integer(.data[[time_col]])
    ) |>
    filter(!is.na(value), !is.na(year))

  cat("df_pop rows:", nrow(df_pop), "\n")
  cat("df_pop columns:", paste(names(df_pop), collapse = ", "), "\n")

  # Detect gender / age columns safely
  gender_col <- grep("[Kk]j.*nn|gender|sex|kjonn", names(df_pop), value = TRUE)[1]
  age_col    <- grep("[Aa]lder|age", names(df_pop), value = TRUE)[1]
  cat("Gender column:", gender_col, "\n")
  cat("Age column:", age_col, "\n")

  if (!is.na(gender_col) && !is.na(age_col)) {
    cat("Unique gender values:", paste(unique(df_pop[[gender_col]]), collapse = " | "), "\n")
    cat("Unique age values:", paste(head(unique(df_pop[[age_col]]), 10), collapse = " | "), "\n")
  }

  # Now try the actual filter that would be used in the post
  gender_vals <- unique(df_pop[[gender_col]])
  both_sexes  <- gender_vals[grepl("begge|total|alle|both", gender_vals, ignore.case = TRUE)][1]
  cat("'Both sexes' value found:", both_sexes, "\n")

  if (!is.na(both_sexes)) {
    df_age_share <- df_pop |>
      filter(.data[[gender_col]] == both_sexes) |>
      group_by(year) |>
      mutate(
        total = sum(value, na.rm = TRUE),
        share = value / total * 100
      ) |>
      ungroup()
    cat("Age share data rows:", nrow(df_age_share), "\n")

    # Attempt the plot
    p <- ggplot(df_age_share, aes(x = year, y = share, fill = .data[[age_col]])) +
      geom_area(alpha = 0.8) +
      labs(
        title    = "Population age structure (debug plot)",
        subtitle = "This plot tests whether ggplot can render from SSB data",
        x        = NULL, y = "Share (%)"
      ) +
      theme_minimal()

    cat("Attempting print(p)...\n")
    print(p)
    cat("print(p) completed.\n")
  } else {
    cat("Could not identify 'both sexes' row — plot skipped.\n")
  }
}

# ── 4. Check installed packages ──────────────────────────────────────────────
cat("\n=== Package availability (as installed on this machine) ===\n\n")

pkgs <- c(
  "tidyverse", "PxWebApiData", "ggridges", "ggbeeswarm", "ggalluvial",
  "waffle", "gganimate", "gifski", "MetBrewer", "nord", "wesanderson",
  "viridis", "sf", "rnaturalearth", "patchwork", "ggtext", "ggrepel",
  "cowplot", "gt", "gtExtras"
)

status <- sapply(pkgs, function(p) {
  if (requireNamespace(p, quietly = TRUE)) "OK" else "MISSING"
})
df_pkgs <- data.frame(package = pkgs, status = status)
print(df_pkgs[df_pkgs$status == "MISSING", ], row.names = FALSE)
if (all(status == "OK")) cat("All packages OK.\n")

# ── 5. Encoding sanity check ──────────────────────────────────────────────---
cat("\n=== Encoding sanity check ===\n\n")
cat("Current locale            :", Sys.getlocale(), "\n")
cat("getOption('encoding')     :", getOption("encoding"), "\n")
test_str  <- "kjønn"
test_bytes <- chartr("", "", test_str)
cat("'kjønn' nchar             :", nchar(test_str), "\n")
cat("'kjønn' nchar(type='bytes'):", nchar(test_str, type = "bytes"), "\n")
cat("If bytes > chars, UTF-8 multi-byte encoding is active (expected on Linux).\n")
cat("On Windows with Latin-1 locale, ø may be 1 byte — this can cause\n")
cat("mismatches when the API returns UTF-8 column names.\n\n")

cat("=============================================================\n")
cat("Debug complete. Check output above to identify the failure.\n")
cat("Common fixes:\n")
cat("  1. Replace rename(kjønn=...) with rename(.data[[col_name]]=...)\n")
cat("  2. Replace filter(gender=='Begge kjønn') with dynamic value detection\n")
cat("  3. Replace stat(x) with after_stat(x) in ggridges calls\n")
cat("=============================================================\n")
