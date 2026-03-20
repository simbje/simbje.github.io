#!/usr/bin/env Rscript
# =============================================================================
# generate_post.R
# Calls the Anthropic API to generate a new daily SSB analysis post.
# Claude picks the topic, writes the R code, and creates the .qmd file.
# Run by GitHub Actions every morning at 07:00 CET.
# =============================================================================

library(httr2)
library(jsonlite)
library(lubridate)
library(glue)
library(PxWebApiData)

# ── Config ────────────────────────────────────────────────────────────────────
ANTHROPIC_API_KEY <- Sys.getenv("ANTHROPIC_API_KEY")
TODAY             <- Sys.Date()
POST_SLUG         <- format(TODAY, "%Y-%m-%d")
POST_DIR          <- file.path("ssb-daily", "posts", POST_SLUG)
POST_FILE         <- file.path(POST_DIR, "index.qmd")
SEED              <- as.integer(format(TODAY, "%Y%m%d"))  # stable, explicit

if (nchar(ANTHROPIC_API_KEY) == 0) stop("ANTHROPIC_API_KEY not set")
if (dir.exists(POST_DIR)) {
  message("Post already exists for ", POST_SLUG, " — skipping.")
  quit(save = "no", status = 0)
}

# ── Retry helper ──────────────────────────────────────────────────────────────
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

# ── SSB Table list ─────────────────────────────────────────────────────────────
SSB_TABLE_LIST <- list(
  # Economy
  c("14700", "Consumer Price Index (new series from 2026, replaces 03013)"),
  c("03013", "Consumer Price Index by consumption group, monthly (2015=100)"),
  c("03014", "Consumer Price Index 12-month rate, by consumption group, monthly"),
  c("09170", "GDP and related measures, quarterly"),
  c("09174", "Wages, employment and productivity by industry, annual"),
  c("09189", "National accounts, quarterly main figures"),
  c("09190", "GDP by industry, quarterly"),
  c("11174", "Household final consumption expenditure"),
  c("10948", "General government revenue and expenditure, annual"),
  c("12880", "National accounts, annual main figures"),
  # Labour
  c("05111", "Population by labour force status, age and sex, annual (AKU)"),
  c("05110", "Population by labour force status, age and sex, quarterly (AKU)"),
  c("13760", "Labour force, employment, unemployment, seasonally adjusted, monthly"),
  c("11350", "Average monthly earnings by industry and sector"),
  c("08536", "Registered unemployed by age and county, monthly"),
  c("07984", "R&D personnel in business enterprise sector"),
  c("08771", "Job vacancies by major industry division"),
  c("03629", "Work stoppages and working days lost"),
  c("12549", "Sickness absence by industry"),
  # Housing
  c("07221", "House price index, existing dwellings"),
  c("06265", "Building activity, dwellings started and completed"),
  c("05889", "New dwellings completed by county"),
  # Demographics
  c("07459", "Population by region, sex, age and year"),
  c("05810", "Population by age, sex and year"),
  c("04861", "Area and population of urban settlements"),
  c("05803", "Births by mothers age"),
  c("09817", "Immigration and emigration by citizenship"),
  c("05196", "Fertility rates"),
  c("10634", "Marriages and divorces"),
  c("09481", "Population changes, quarterly"),
  # Energy & Environment
  c("08307", "Electricity balance, monthly"),
  c("13931", "First-time registered vehicles by fuel type and county"),
  c("09288", "Energy balance for Norway by energy product"),
  c("03321", "Greenhouse gas emissions by source"),
  # Education
  c("09429", "Students in higher education by field of study"),
  c("08956", "Upper secondary completion rates"),
  c("07184", "Kindergartens, children and staff"),
  # Health
  c("06913", "Activities in somatic hospitals"),
  # Business & Industry
  c("07196", "Enterprises by industry and organisational form"),
  c("10582", "Bankruptcies opened by industry, quarterly"),
  c("08419", "Retail trade index, monthly"),
  c("08800", "Overnight stays by nationality and accommodation type"),
  # Crime
  c("08406", "Offences reported to police by type"),
  # Trade
  c("08807", "External trade in goods, imports and exports"),
  # KOSTRA (municipality-level, annual)
  c("12362", "KOSTRA: Kindergarten key figures by municipality"),
  c("12215", "KOSTRA: Primary school key figures by municipality"),
  c("12006", "KOSTRA: Elderly care key figures by municipality"),
  c("12163", "KOSTRA: Municipal financial key figures"),
  c("12559", "KOSTRA: Water and sewage key figures by municipality")
)

# ── Pre-fetch real metadata from SSB ──────────────────────────────────────────
MAX_TABLES <- 15

message("Pre-fetching SSB metadata to validate tables and discover real parameters...")

fetch_table_meta <- function(table_id, description) {
  url <- paste0("https://data.ssb.no/api/v0/no/table/", table_id)
  for (attempt in 1:3) {
    result <- tryCatch({
      meta <- PxWebApiData::ApiData(url, returnMetaFrames = TRUE)
      # Validate: must be a named list of data frames
      if (!is.list(meta) || is.null(names(meta)) ||
          !all(vapply(meta, is.data.frame, logical(1L)))) {
        stop("metadata is not a named list of data frames")
      }
      list(id = table_id, description = description, meta = meta, ok = TRUE)
    }, error = function(e) {
      list(id = table_id, description = description, meta = NULL, ok = FALSE,
           err = conditionMessage(e))
    })
    if (result$ok) return(result)
    if (attempt < 3) {
      message("  Retry ", attempt, " for ", table_id, ": ", result$err)
      Sys.sleep(2^attempt)
    }
  }
  message("  SKIP ", table_id, ": ", result$err)
  result
}

set.seed(SEED)
shuffled <- SSB_TABLE_LIST[sample(length(SSB_TABLE_LIST))]

table_results <- list()
valid_count   <- 0L
for (tbl in shuffled) {
  res <- fetch_table_meta(tbl[[1]], tbl[[2]])
  table_results[[length(table_results) + 1L]] <- res
  if (res$ok) {
    valid_count <- valid_count + 1L
    message("  OK  ", tbl[[1]], " — ", tbl[[2]])
    if (valid_count >= MAX_TABLES) break
  }
  Sys.sleep(0.2)
}

valid_tables <- Filter(function(r) r$ok, table_results)
message("Validated ", length(valid_tables), " tables.")

if (length(valid_tables) == 0L) stop("No SSB tables could be reached. Check network/API.")

# ── Build dynamic catalogue string from real metadata ─────────────────────────
format_param <- function(frame, max_values = 8L) {
  if (is.null(frame) || !is.data.frame(frame) || nrow(frame) == 0L) return(NA_character_)
  code_col  <- names(frame)[1]
  label_col <- if (ncol(frame) >= 2L) names(frame)[2L] else code_col
  top  <- head(frame, max_values)
  vals <- if (code_col == label_col) {
    paste0('"', top[[code_col]], '"')
  } else {
    paste0('"', top[[code_col]], '" (', top[[label_col]], ')')
  }
  suffix <- if (nrow(frame) > max_values) paste0(" ... ", nrow(frame), " total") else ""
  paste0(paste(vals, collapse = ", "), suffix)
}

build_catalogue <- function(valid_tables) {
  sections <- lapply(valid_tables, function(res) {
    param_lines <- lapply(names(res$meta), function(p) {
      formatted <- format_param(res$meta[[p]])
      if (is.na(formatted) || nchar(formatted) == 0L) return(NULL)
      paste0("  - **", p, "**: ", formatted)
    })
    param_lines <- Filter(Negate(is.null), param_lines)
    paste0(
      "### ", res$id, " — ", res$description, "\n",
      paste(unlist(param_lines), collapse = "\n")
    )
  })
  paste0(
    "## Verified SSB datasets with EXACT parameter names\n\n",
    "CRITICAL: The parameter names and values below come directly from the SSB API.\n",
    "Use them EXACTLY as written. Do NOT invent or modify parameter names.\n",
    "For dimension parameters, pass TRUE to get all values, or a character vector of specific codes.\n",
    "For Tid use list(filter='top', values=N) to get the last N periods.\n\n",
    paste(unlist(sections), collapse = "\n\n")
  )
}

SSB_CATALOGUE <- build_catalogue(valid_tables)

# ── Previous posts (topic diversity) ─────────────────────────────────────────
TOPIC_INDEX_FILE <- file.path("ssb-daily", "posts", "_topic_index.csv")

read_topic_index <- function(path) {
  if (!file.exists(path)) return(data.frame())
  tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) data.frame())
}

append_topic_index <- function(path, date, title, datasets, chart_types) {
  existing     <- read_topic_index(path)
  expected_cols <- c("date", "title", "datasets", "chart_types")
  new_row <- data.frame(
    date        = as.character(date),
    title       = substr(title, 1, 80),
    datasets    = substr(datasets, 1, 60),
    chart_types = substr(chart_types, 1, 60),
    stringsAsFactors = FALSE
  )
  # Normalize existing schema to avoid rbind() errors on column drift
  if (nrow(existing) > 0) {
    for (col in expected_cols) {
      if (!col %in% names(existing)) existing[[col]] <- NA_character_
    }
    existing <- existing[, expected_cols, drop = FALSE]
  }
  updated <- rbind(existing, new_row)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(updated, path, row.names = FALSE)
}

topic_index <- read_topic_index(TOPIC_INDEX_FILE)
recent_topics_note <- if (nrow(topic_index) > 0) {
  recent <- tail(topic_index, 60)
  rows <- paste0(
    "- ", recent$date, ': "', recent$title, '" | datasets: ',
    recent$datasets, " | charts: ", recent$chart_types,
    collapse = "\n"
  )
  paste0(
    "\n## Recent posts (last ", nrow(recent), " — avoid repeating same angle or dataset combo):\n",
    rows
  )
} else {
  "\n## No previous posts yet — this is the first one!"
}

# ── System prompt ─────────────────────────────────────────────────────────────
SYSTEM_PROMPT <- glue('
You are a world-class data journalist and R programmer specializing in Norwegian economics and statistics.
Your job is to write a complete, self-contained Quarto (.qmd) blog post that performs a fresh, insightful
analysis of Statistics Norway (SSB) data using R.

## Tone & Style
- Curious, journalistic, data-driven
- Accessible to an educated general audience (not just economists)
- Find the story in the numbers — surprises, trends, Norway-specific angles
- Write like a TidyTuesday post: show your work, explain your choices
- NO emojis, emoticons or decorative symbols anywhere in the post
- Professional and clean throughout

## Visualization requirements
- Create 3-5 ggplot2 charts per post
- Rotate through styles across posts: lollipop, slope, ridgeline (ggridges),
  dumbbell, area with annotations, small multiples/facets, bump, heatmap,
  alluvial (ggalluvial), waffle, beeswarm (ggbeeswarm), waterfall, polar,
  and classic line/bar/scatter combinations
- Available color packages: MetBrewer, nord, wesanderson, viridis — pick one cohesive palette
- Every chart must have: clear title, subtitle with insight, caption citing SSB, clean theme
- Use theme_minimal() or theme_void() as base, then customize
- Add annotations, reference lines, and direct labels where helpful
- Always assign every ggplot to a variable then call print() explicitly

## Parameter usage (CRITICAL)
The catalogue in the user prompt contains verified parameter names from the SSB API.
Use them EXACTLY as written — never invent or modify parameter names.
For dimension parameters, pass TRUE for all values or a character vector of specific codes.
Use Tid = list(filter="top", values=N) to get the last N time periods.

## Data fetching pattern
```{{r fetch-data}}
df <- NULL
tryCatch({{
  raw <- ApiData(
    "https://data.ssb.no/api/v0/no/table/TABLE_ID",
    EXACT_PARAM_FROM_CATALOGUE = TRUE,
    Tid = list(filter = "top", values = 40)
  )
  tmp <- raw[[1]]
  print(names(tmp))

  # Detect time column (SSB uses Norwegian names: "år", "kvartal", "måned")
  time_col <- names(tmp)[grepl(
    "tid|\u00e5r|kvartal|m\u00e5ned|aar|maaned|year|month|quarter",
    names(tmp), ignore.case = TRUE, perl = TRUE
  )][1]
  if (is.na(time_col)) time_col <- names(tmp)[length(names(tmp)) - 1L]

  # Detect numeric value column (SSB columns vary — never assume "value")
  value_col <- names(tmp)[vapply(tmp, is.numeric, logical(1L))][1]
  if (is.na(value_col)) value_col <- names(tmp)[length(names(tmp))]

  df <- tmp |>
    mutate(
      value    = as.numeric(.data[[value_col]]),
      time_str = .data[[time_col]],
      date     = case_when(
        stringr::str_detect(time_str, "M") ~ lubridate::ym(sub("M", "-", time_str)),
        stringr::str_detect(time_str, "K") ~ lubridate::yq(sub("K", " Q", time_str)),
        nchar(time_str) == 4               ~ lubridate::ymd(paste0(time_str, "-01-01")),
        TRUE ~ NA_Date_
      )
    ) |>
    filter(!is.na(value), !is.na(date))
}}, error = function(e) message("Fetch failed: ", e$message))
```

## Column handling (CRITICAL)
- NEVER use hardcoded Norwegian column names in rename() or filter()
- Detect categorical columns by partial match:
  ```r
  gender_col <- names(tmp)[grepl("kj.nn|gender|sex|kjonn", names(tmp), ignore.case=TRUE)][1]
  both_val   <- unique(df[[gender_col]])[grepl("begge|total|alle|both",
                  unique(df[[gender_col]]), ignore.case=TRUE)][1]
  df_f <- df |> filter(.data[[gender_col]] == both_val)
  ```

## Plotting rules (CRITICAL)
- ALWAYS: assign plot to variable, then call print() explicitly
- ALWAYS: guard with if (!is.null(df)) {{ ... }}
- NEVER rely on implicit printing
- Every plot chunk must use chunk options:
  ```{{r plot-name}}
  #| fig-height: 5
  #| fig-width: 9
  #| fig-show: asis
  #| dev: "png"
  ```

Correct:
  p <- ggplot(df, aes(x, y)) + geom_line()
  print(p)

Wrong:
  ggplot(df, aes(x, y)) + geom_line()   # no print — may not show

## R code requirements
- Set error=TRUE in knitr opts so one chunk failure does not kill the whole post
- Use tidyverse throughout; include all library() calls at the top
- Code must be fully reproducible
- Use echo: true

## Post structure
1. Brief intro (2-3 sentences) — the hook, why this matters
2. Data section — fetch + wrangle with exact parameter names, print column names
3. Analysis sections — 2-3 distinct angles with charts
4. Key findings — 3-5 bullet points with numbers
5. Closing reflection — broader context or what to watch next

## YAML front matter
```
---
title: "COMPELLING TITLE"
description: "ONE SENTENCE SUMMARY"
date: "DATE_TODAY"
categories: [SSB, category1, category2]
---
```

## Output format (REQUIRED — exactly this structure, nothing else)
Your entire response must consist of exactly two parts with no preamble:

PART 1 — one line of metadata:
METADATA: title="<post title>" datasets="<table IDs, comma separated>" chart_types="<styles used, comma separated>"

PART 2 — the raw .qmd file starting immediately with --- (YAML front matter).
No markdown fences, no explanation, no text before or after these two parts.

Example:
METADATA: title="Norwegian wage growth" datasets="11350,05111" chart_types="lollipop,ridgeline,area"
---
title: "Norwegian wage growth"
...
')

# ── User prompt ───────────────────────────────────────────────────────────────
USER_PROMPT <- glue('
Today is {format(TODAY, "%A, %d %B %Y")}.
Random seed for topic selection: {SEED}

{SSB_CATALOGUE}

{recent_topics_note}

Please write a complete Quarto blog post for today.

Guidelines:
- Pick 1-3 SSB datasets that tell an interesting story together, or go deep on one
- The topic should feel timely, relevant, or surprising
- Include at least one angle that connects to broader Norwegian society or economy
- Use at least 3 different chart types
- Make the most striking chart the first or most prominent one

Every post MUST follow this data chunk skeleton exactly:

STEP 1 — Setup chunk:
```r
knitr::opts_chunk$set(echo=TRUE, warning=FALSE, message=FALSE, error=TRUE)
```

STEP 2 — Data chunk using EXACT parameter names from the catalogue:
```r
df <- NULL

tryCatch({{
  raw <- ApiData(
    "https://data.ssb.no/api/v0/no/table/XXXXX",
    EXACT_PARAM_NAME = TRUE,
    Tid = list(filter="top", values=40)
  )
  tmp <- raw[[1]]
  print(names(tmp))

  time_col <- names(tmp)[grepl(
    "tid|\u00e5r|kvartal|m\u00e5ned|aar|maaned|year|month|quarter",
    names(tmp), ignore.case = TRUE, perl = TRUE
  )][1]
  if (is.na(time_col)) time_col <- names(tmp)[length(names(tmp)) - 1L]

  value_col <- names(tmp)[vapply(tmp, is.numeric, logical(1L))][1]
  if (is.na(value_col)) value_col <- names(tmp)[length(names(tmp))]

  df <- tmp |>
    mutate(
      value    = as.numeric(.data[[value_col]]),
      time_str = .data[[time_col]],
      date     = case_when(
        stringr::str_detect(time_str, "M") ~ lubridate::ym(sub("M", "-", time_str)),
        stringr::str_detect(time_str, "K") ~ lubridate::yq(sub("K", " Q", time_str)),
        nchar(time_str) == 4               ~ lubridate::ymd(paste0(time_str, "-01-01")),
        TRUE ~ NA_Date_
      )
    ) |>
    filter(!is.na(value), !is.na(date))
}}, error = function(e) message("Data fetch failed: ", e$message))
```

STEP 3 — Plot chunk (guard + explicit print):
```r
if (!is.null(df)) {{
  p <- ggplot(df, aes(x = date, y = value)) +
    geom_line(color = pal[1], linewidth = 1.2) +
    labs(title = "Your title", subtitle = "Your insight",
         caption = "Source: SSB", x = NULL, y = "Value") +
    theme_minimal()
  print(p)
}}
```
'
)

# ── Call Anthropic API ────────────────────────────────────────────────────────
message("Calling Anthropic API to generate post for ", POST_SLUG, "...")

response <- with_retry(function() {
  request("https://api.anthropic.com/v1/messages") |>
    req_headers(
      "x-api-key"         = ANTHROPIC_API_KEY,
      "anthropic-version" = "2023-06-01",
      "content-type"      = "application/json"
    ) |>
    req_body_json(list(
      model      = "claude-sonnet-4-5-20250929",
      max_tokens = 16000,
      system     = SYSTEM_PROMPT,
      messages   = list(
        list(role = "user", content = USER_PROMPT)
      )
    )) |>
    req_timeout(300) |>
    req_perform()
}, max_attempts = 3L, base_wait = 5)

result <- resp_body_json(response)

stop_reason <- if (is.null(result$stop_reason)) "unknown" else result$stop_reason
if (stop_reason == "max_tokens") {
  warning("API response was truncated (hit max_tokens). Post may have incomplete code chunks.")
}

# ── Concatenate all text blocks safely ────────────────────────────────────────
text_blocks <- Filter(function(b) identical(b$type, "text"), result$content)
if (length(text_blocks) == 0L) stop("No text blocks in API response.")
raw_text <- paste(vapply(text_blocks, function(b) b$text, character(1L)), collapse = "")

# ── Parse metadata (tolerates missing, lowercase, slightly different format) ──
extract_meta_field <- function(text, field) {
  pat <- paste0("(?i)", field, "\\s*=\\s*\"([^\"]+)\"")
  m   <- regmatches(text, regexpr(pat, text, perl = TRUE))
  if (length(m) == 0L || nchar(m) == 0L) return("")
  sub(paste0("(?i)", field, "\\s*=\\s*\"([^\"]+)\""), "\\1", m, perl = TRUE)
}

meta_line    <- tryCatch(
  regmatches(raw_text, regexpr("(?i)metadata:[ \t]*[^\n]*", raw_text, perl = TRUE)),
  error = function(e) character(0)
)
meta_title    <- extract_meta_field(meta_line, "title")
meta_datasets <- extract_meta_field(meta_line, "datasets")
meta_charts   <- extract_meta_field(meta_line, "chart_types")
if (nchar(meta_title) == 0L) meta_title <- "SSB Analysis"

# ── Extract .qmd — find first YAML front matter (--- at line start) ───────────
m <- regexpr("(?m)^---[ \t]*$", raw_text, perl = TRUE)
if (m[[1]] < 0L) {
  stop("Could not find YAML front matter in API response. Raw output:\n",
       substr(raw_text, 1, 500))
}
qmd_raw <- substring(raw_text, m[[1]])

# ── Enforce today's date — only within YAML front matter ─────────────────────
local({
  fm_m <- regexpr("(?s)^---[ \t]*\n.*?\n---", qmd_raw, perl = TRUE)
  if (fm_m[[1]] > 0L && attr(fm_m, "match.length") > 0L) {
    fm_len <- attr(fm_m, "match.length")
    fm      <- substring(qmd_raw, 1L, fm_len)
    rest    <- substring(qmd_raw, fm_len + 1L)
    fm      <- gsub('date:\\s*"[^"]*"',
                    paste0('date: "', format(TODAY, "%Y-%m-%d"), '"'), fm)
    qmd_raw <<- paste0(fm, rest)
  }
})

# ── Remove image: thumbnail.png — it never exists and breaks rendering ────────
qmd_raw <- gsub('\nimage:\\s*["\']thumbnail\\.png["\']', "", qmd_raw)

# ── Patch: harden time_col detection in generated code ───────────────────────
# Strategy: scan line-by-line for any `*_col <- names(VAR)[...]` assignment
# that uses a grepl pattern, and ensure the canonical Unicode-safe pattern and
# NA fallback are both present. Handles arbitrary variable names including
# expressions like raw[[1]].
local({
  lines     <- strsplit(qmd_raw, "\n", fixed = TRUE)[[1]]
  canonical <- paste0('grepl("tid|\u00e5r|kvartal|m\u00e5ned|aar|maaned|year|month|quarter"')

  i <- 1L
  while (i <= length(lines)) {
    if (grepl("time_col\\s*<-\\s*names\\(", lines[i], perl = TRUE) &&
        grepl("grepl\\(", lines[i], perl = TRUE)) {

      # 1. Replace the grepl pattern with the canonical one
      lines[i] <- sub('grepl\\("[^"]*"', canonical, lines[i], perl = TRUE)

      # 2. Ensure perl = TRUE is present
      if (!grepl("perl\\s*=\\s*TRUE", lines[i], perl = TRUE)) {
        lines[i] <- sub(
          "ignore\\.case\\s*=\\s*TRUE\\)",
          "ignore.case = TRUE, perl = TRUE)",
          lines[i], perl = TRUE
        )
      }

      # 3. Extract variable name — handles tmp, df, raw[[1]], etc.
      var_match <- regmatches(lines[i], regexpr("names\\(([^)]+)\\)", lines[i], perl = TRUE))
      var_name  <- if (length(var_match) > 0L) {
        sub("names\\(([^)]+)\\)", "\\1", var_match, perl = TRUE)
      } else {
        "tmp"
      }

      # 4. Ensure NA fallback is on a nearby line
      next_i <- i + 1L
      while (next_i <= length(lines) && trimws(lines[next_i]) == "") next_i <- next_i + 1L
      has_fallback <- next_i <= length(lines) &&
        grepl("is\\.na\\(time_col\\)", lines[next_i], perl = TRUE)
      if (!has_fallback) {
        indent   <- sub("^([ \t]*).*", "\\1", lines[i], perl = TRUE)
        fallback <- paste0(indent, "if (is.na(time_col)) time_col <- names(",
                           var_name, ")[length(names(", var_name, ")) - 1L]")
        lines <- c(lines[seq_len(i)], fallback, lines[seq.int(i + 1L, length(lines))])
        i <- i + 1L
      }
    }
    i <- i + 1L
  }
  qmd_raw <<- paste(lines, collapse = "\n")
})

# ── Validate generated QMD before writing ─────────────────────────────────────
validate_qmd <- function(content) {
  issues <- character(0)
  if (!grepl("^---", trimws(content))) {
    issues <- c(issues, "Missing YAML front matter")
  }
  fence_positions <- gregexpr("(?m)^```", content, perl = TRUE)[[1]]
  if (!identical(fence_positions, -1L) && length(fence_positions) %% 2L != 0L) {
    issues <- c(issues, paste0("Unbalanced code fences (", length(fence_positions), " occurrences)"))
  }
  if (!grepl("ApiData\\(", content)) {
    issues <- c(issues, "No ApiData() call found")
  }
  if (!grepl("print\\(", content)) {
    issues <- c(issues, "No print() call found — plots may not render")
  }
  issues
}

validation_issues <- validate_qmd(qmd_raw)
if (length(validation_issues) > 0L) {
  warning("QMD validation warnings:\n", paste("-", validation_issues, collapse = "\n"))
}

# ── Write post file ───────────────────────────────────────────────────────────
dir.create(POST_DIR, recursive = TRUE, showWarnings = FALSE)
writeLines(qmd_raw, POST_FILE)
message("Post written to: ", POST_FILE)

# ── Update compact topic index ────────────────────────────────────────────────
append_topic_index(TOPIC_INDEX_FILE, POST_SLUG, meta_title, meta_datasets, meta_charts)
message("Topic index updated: ", meta_title, " | ", meta_datasets, " | ", meta_charts)
