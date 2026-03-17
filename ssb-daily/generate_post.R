#!/usr/bin/env Rscript
# =============================================================================
# generate_post.R
# Calls the Anthropic API to generate a new daily SSB analysis post.
# Claude picks the topic, writes the R code, and creates the .qmd file.
# Run by GitHub Actions every morning at 07:00 CET.
# =============================================================================

# Ensure correct library path
# library paths managed by r-lib/actions/setup-r

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
SEED              <- as.integer(TODAY)  # deterministic topic variation

if (nchar(ANTHROPIC_API_KEY) == 0) stop("ANTHROPIC_API_KEY not set")
if (dir.exists(POST_DIR)) {
  message("Post already exists for ", POST_SLUG, " — skipping.")
  quit(status = 0)
}

# ── SSB Table list ─────────────────────────────────────────────────────────────
# All candidate tables. We pre-fetch their real metadata before calling Claude,
# so Claude receives exact parameter names/values and never has to guess.
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
# Shuffled so a different subset is sampled each day (seed = today's date).
# We fetch up to MAX_TABLES to keep the prompt a reasonable size.
MAX_TABLES <- 15

message("Pre-fetching SSB metadata to validate tables and discover real parameters...")

fetch_table_meta <- function(table_id, description) {
  url <- paste0("https://data.ssb.no/api/v0/no/table/", table_id)
  tryCatch({
    meta <- PxWebApiData::ApiData(url, returnMetaFrames = TRUE)
    list(id = table_id, description = description, meta = meta, ok = TRUE)
  }, error = function(e) {
    message("  SKIP ", table_id, ": ", conditionMessage(e))
    list(id = table_id, description = description, meta = NULL, ok = FALSE)
  })
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
  Sys.sleep(0.2)  # be polite to SSB API
}

valid_tables <- Filter(function(r) r$ok, table_results)
message("Validated ", length(valid_tables), " tables.")

if (length(valid_tables) == 0L) stop("No SSB tables could be reached. Check network/API.")

# ── Build dynamic catalogue string from real metadata ─────────────────────────
format_param <- function(frame, max_values = 8L) {
  if (is.null(frame) || nrow(frame) == 0L) return("")
  code_col  <- names(frame)[1]
  label_col <- if (ncol(frame) >= 2L) names(frame)[2L] else code_col
  top       <- head(frame, max_values)
  vals      <- if (code_col == label_col) {
    paste0('"', top[[code_col]], '"')
  } else {
    paste0('"', top[[code_col]], '" (', top[[label_col]], ')')
  }
  suffix <- if (nrow(frame) > max_values) paste0(" ... ", nrow(frame), " total") else ""
  paste0(paste(vals, collapse = ", "), suffix)
}

build_catalogue <- function(valid_tables) {
  sections <- sapply(valid_tables, function(res) {
    param_lines <- sapply(names(res$meta), function(p) {
      formatted <- format_param(res$meta[[p]])
      if (nchar(formatted) == 0L) return(NULL)
      paste0("  - **", p, "**: ", formatted)
    })
    param_lines <- Filter(Negate(is.null), param_lines)
    paste0(
      "### ", res$id, " — ", res$description, "\n",
      paste(param_lines, collapse = "\n")
    )
  })
  paste0(
    "## Verified SSB datasets with EXACT parameter names\n\n",
    "CRITICAL: The parameter names and values below come directly from the SSB API.\n",
    "Use them EXACTLY as written. Do NOT invent or modify parameter names.\n",
    "For dimension parameters, pass TRUE to get all values, or a character vector of specific codes.\n",
    "For Tid use list(filter='top', values=N) to get the last N periods.\n\n",
    paste(sections, collapse = "\n\n")
  )
}

SSB_CATALOGUE <- build_catalogue(valid_tables)

# ── Previous posts (for topic diversity — compact, token-safe) ───────────────
# We never pass full post content. Instead we maintain a tiny index file:
# ssb-daily/posts/_topic_index.csv  with columns: date, title, datasets, chart_types
# Each row is ~80 chars. 365 rows ≈ 30k chars ≈ ~8k tokens — totally fine.
# We pass only the last 60 entries to the prompt (covers 2 months).

TOPIC_INDEX_FILE <- file.path("ssb-daily", "posts", "_topic_index.csv")

read_topic_index <- function(path) {
  if (!file.exists(path)) return(data.frame())
  tryCatch(
    read.csv(path, stringsAsFactors = FALSE),
    error = function(e) data.frame()
  )
}

append_topic_index <- function(path, date, title, datasets, chart_types) {
  existing <- read_topic_index(path)
  new_row  <- data.frame(
    date        = as.character(date),
    title       = substr(title, 1, 80),        # cap length
    datasets    = substr(datasets, 1, 60),
    chart_types = substr(chart_types, 1, 60),
    stringsAsFactors = FALSE
  )
  updated <- rbind(existing, new_row)
  write.csv(updated, path, row.names = FALSE)
}

topic_index    <- read_topic_index(TOPIC_INDEX_FILE)
recent_topics_note <- if (nrow(topic_index) > 0) {
  recent <- tail(topic_index, 60)
  rows   <- paste0(
    "- ", recent$date, ": \"", recent$title, "\" | datasets: ",
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
- NO emojis, emoticons or decorative symbols anywhere in the post — not in headings, text, bullet points or captions
- Professional and clean throughout

## Visualization requirements (CRITICAL)
- Create 3-5 ggplot2 charts minimum per post
- Rotate through these styles across posts: 
  waffle charts, lollipop charts, slope charts, ridgeline plots (ggridges),
  dumbbell charts, area charts with annotations, small multiples/facets,
  animated plots (gganimate), bump charts, heatmaps, alluvial/sankey (ggalluvial),
  geographic maps (sf + Norway shapefiles from "geodata" or "rnaturalearth"),
  beeswarm plots (ggbeeswarm), waterfall charts, circular/polar plots,
  and classic but beautifully styled line/bar/scatter combos
- Use a cohesive, beautiful color palette (e.g., from MetBrewer, nord, wesanderson, or hand-picked)
- Every chart must have: clear title, subtitle with insight, caption citing SSB, clean theme
- Use theme_minimal() or theme_void() as base, then customize heavily
- Add annotations, reference lines, and labels directly on the chart where helpful
- Aim for publication-ready quality

## Parameter usage (CRITICAL — use ONLY the names provided in the catalogue)
The catalogue in the user prompt contains verified parameter names and valid values fetched
directly from the SSB API moments before this call. Use them EXACTLY as written.

Rules:
- NEVER invent or modify parameter names — use only what is listed in the catalogue
- For dimension parameters, pass TRUE to get all values, or a character vector of specific codes shown in the catalogue
- Use Tid = list(filter="top", values=N) to get the last N time periods
- Only use ContentsCode codes that appear in the catalogue for that table

When calling ApiData():
```{{r fetch-data}}
df <- NULL
tryCatch({{
  raw <- ApiData(
    "https://data.ssb.no/api/v0/no/table/TABLE_ID",
    EXACT_PARAM_FROM_CATALOGUE = TRUE,   # use the name from the catalogue
    Tid = list(filter = "top", values = 40)
  )
  tmp <- raw[[1]]
  print(names(tmp))  # always print column names so the output is visible
  df <- tmp |> mutate(value = as.numeric(value), ...)
}}, error = function(e) message("Fetch failed: ", e$message))
```

## Column handling rules (CRITICAL — silent failures hide all plots)
- NEVER use hardcoded Norwegian column names in rename() or filter()
  - BAD:  `rename(gender = kjønn)`  or  `filter(gender == "Begge kjønn")`
  - GOOD: detect the column name first, then use .data[[col_name]]
- Find the time column with the exact regex below — "aar"/"maaned" do NOT match "år"/"måned":
  ```r
  time_col <- names(tmp)[grepl(
    "tid|\u00e5r|kvartal|m\u00e5ned|aar|maaned|year|month|quarter",
    names(tmp), ignore.case = TRUE, perl = TRUE
  )][1]
  if (is.na(time_col)) time_col <- names(tmp)[length(names(tmp)) - 1L]
  ```
- Find categorical columns by position or partial match, never by exact Norwegian name:
  ```r
  # Example: detect the gender/sex column safely
  gender_col <- names(tmp)[grepl("kj.nn|gender|sex|kjonn", names(tmp), ignore.case=TRUE)][1]
  # Then filter using the detected value:
  both <- unique(df[[gender_col]])[grepl("begge|total|alle|both", unique(df[[gender_col]]), ignore.case=TRUE)][1]
  df_filtered <- df |> filter(.data[[gender_col]] == both)
  ```

## Plotting rules (CRITICAL — plots not showing is the most common failure)
- ALWAYS assign every ggplot to a named variable first, then call print() on it explicitly
- NEVER rely on implicit printing — it does not work reliably inside Quarto code chunks
- Every plot chunk must end with print(p) or print(p1), print(p2) etc.
- ALWAYS guard with if (!is.null(df)) {{ p <- ggplot(...); print(p) }}
- Use chunk options for every plot chunk like this:
  ```{{r plot-name}}
  #| fig-height: 5
  #| fig-width: 9
  #| fig-show: asis
  #| dev: "png"
- Never use the old knitr syntax fig.height=5 inside the backtick header — use #| options instead

Correct pattern — always do this:

  p <- ggplot(df, aes(x, y)) +
    geom_line() +
    labs(title = \"Title\")
  print(p)

Wrong pattern — never do this:

  ggplot(df, aes(x, y)) +   # no assignment, no print — plot may not show
    geom_line()


## R code requirements
- Always wrap data fetching in tryCatch with informative error messages
- Use tidyverse throughout (dplyr, tidyr, ggplot2, lubridate, scales)
- Include all library() calls at the top
- Code must be fully reproducible and runnable in a clean R session
- Use echo: true so readers can see the code
- Add brief prose comments explaining WHY you make analytical choices

## Post structure
Every post must have this exact YAML front matter format:
```
---
title: "COMPELLING TITLE"
description: "ONE SENTENCE SUMMARY"
date: "DATE_TODAY"
categories: [SSB, category1, category2]
---
```

Then structure:
1. Brief intro (2-3 sentences) — the hook, why this matters today
2. Data section — fetch + wrangle using exact parameter names from the catalogue, print column names
3. Analysis section(s) — 2-3 distinct analytical angles with charts
4. Key findings — 3-5 bullet points with the numbers
5. Closing reflection — broader context or what to watch next

## Output format
Return your response in TWO clearly separated sections:

SECTION 1 — a single line of metadata (used for deduplication tracking):
METADATA: title="<post title>" datasets="<table IDs used, comma separated>" chart_types="<chart styles used, comma separated>"

SECTION 2 — the raw .qmd file content, starting immediately with --- (YAML front matter).
No markdown fences, no preamble, no explanation around the .qmd content.

Example output structure:
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
- Make the thumbnail-worthy chart the first or most prominent one
Remember: return ONLY the raw .qmd content, starting with ---

Every post MUST follow this skeleton exactly:

STEP 1 — Setup chunk:
```r
knitr::opts_chunk$set(echo=TRUE, warning=FALSE, message=FALSE, error=TRUE)
```

STEP 2 — Data chunk using EXACT parameter names from the catalogue:
```r
df <- NULL  # ALWAYS initialise to NULL outside tryCatch

tryCatch({{
  # Parameter names below come from the verified catalogue — do NOT change them
  raw <- ApiData(
    "https://data.ssb.no/api/v0/no/table/XXXXX",
    EXACT_PARAM_NAME = TRUE,          # use the name from the catalogue
    Tid = list(filter="top", values=40)
  )

  tmp <- raw[[1]]
  print(names(tmp))  # always print column names

  # Find time column defensively (SSB uses varying names including Norwegian)
  # IMPORTANT: SSB returns "\u00e5r" (år), "kvartal", "m\u00e5ned" — NOT "aar"/"maaned"
  time_col <- names(tmp)[grepl(
    "tid|\u00e5r|kvartal|m\u00e5ned|aar|maaned|year|month|quarter",
    names(tmp), ignore.case = TRUE, perl = TRUE
  )][1]
  # Fallback: SSB column order is always dims..., statistikkvariabel, time, value
  # so second-to-last column is time if the regex above missed it
  if (is.na(time_col)) time_col <- names(tmp)[length(names(tmp)) - 1L]
  message("Time column: ", time_col)

  df <- tmp |>
    mutate(
      value    = as.numeric(value),
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

STEP 3 — Plot chunk (guard + print):
```r
if (!is.null(df)) {{
  p <- ggplot(df, aes(x = date, y = value)) +
    geom_line(color = pal[1], linewidth = 1.2) +
    labs(title = "Your title", subtitle = "Your insight",
         caption = "Source: SSB", x = NULL, y = "Value") +
    theme_minimal()
  print(p)  # ALWAYS call print() explicitly
}}
```
'
)

# ── Call Anthropic API ────────────────────────────────────────────────────────
message("Calling Anthropic API to generate post for ", POST_SLUG, "...")

response <- request("https://api.anthropic.com/v1/messages") |>
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

result  <- resp_body_json(response)

# ── Check for truncated response ─────────────────────────────────────────────
stop_reason <- if (is.null(result$stop_reason)) "unknown" else result$stop_reason
if (stop_reason == "max_tokens") {
  warning("API response was truncated (hit max_tokens). Post may have incomplete code chunks.")
}

raw_text <- result$content[[1]]$text

# ── Parse response — split metadata from .qmd content ────────────────────────

# Extract METADATA line
meta_match   <- regmatches(raw_text, regexpr("METADATA:.*", raw_text))
meta_title   <- if (length(meta_match) > 0) gsub('.*title="([^"]+)".*',   "\\1", meta_match) else "SSB Analysis"
meta_datasets<- if (length(meta_match) > 0) gsub('.*datasets="([^"]+)".*',"\\1", meta_match) else ""
meta_charts  <- if (length(meta_match) > 0) gsub('.*chart_types="([^"]+)".*',"\\1",meta_match) else ""

# Extract .qmd content — everything from first --- onwards
qmd_raw <- sub("(?s).*?(---\\s*\ntitle:)", "---\ntitle:", raw_text, perl = TRUE)

if (!startsWith(trimws(qmd_raw), "---")) {
  stop("Could not parse .qmd content from API response. Raw output:\n", substr(raw_text, 1, 500))
}

# Enforce today's date
qmd_raw <- gsub(
  'date: "[^"]*"',
  paste0('date: "', format(TODAY, "%Y-%m-%d"), '"'),
  qmd_raw
)

# Remove image: field — thumbnail.png never exists and breaks rendering
qmd_raw <- gsub('\nimage: "thumbnail.png"', "", qmd_raw, fixed = TRUE)
qmd_raw <- gsub("\nimage: 'thumbnail.png'", "", qmd_raw, fixed = TRUE)

# ── Patch: fix time-column detection in generated code ────────────────────────
# The LLM sometimes emits an incomplete regex that misses "år" (U+00E5+r),
# causing time_col to be NA, which throws inside tryCatch and leaves df NULL.
# Strategy: scan line-by-line; on any line with `time_col <- names(`,
# replace the grepl string with the canonical pattern and ensure the NA fallback
# appears on the very next non-blank line.
local({
  lines     <- strsplit(qmd_raw, "\n", fixed = TRUE)[[1]]
  canonical <- 'grepl("tid|\u00e5r|kvartal|m\u00e5ned|aar|maaned|year|month|quarter"'
  i <- 1L
  while (i <= length(lines)) {
    if (grepl("time_col\\s*<-\\s*names\\(", lines[i], perl = TRUE)) {
      # 1. Replace whatever grepl string is present with the canonical pattern
      lines[i] <- sub('grepl\\("[^"]*"', canonical, lines[i], perl = TRUE)
      # 2. Ensure perl = TRUE is present (add it if only ignore.case = TRUE)
      if (!grepl("perl\\s*=\\s*TRUE", lines[i], perl = TRUE)) {
        lines[i] <- sub(
          "ignore\\.case\\s*=\\s*TRUE\\)",
          "ignore.case = TRUE, perl = TRUE)",
          lines[i], perl = TRUE
        )
      }
      # 3. Ensure the NA fallback is on the next line
      next_i <- i + 1L
      # skip blank lines when checking
      while (next_i <= length(lines) && trimws(lines[next_i]) == "") next_i <- next_i + 1L
      has_fallback <- next_i <= length(lines) &&
        grepl("is\\.na\\(time_col\\)", lines[next_i], perl = TRUE)
      if (!has_fallback) {
        var_name <- sub(".*names\\((\\w+)\\).*", "\\1", lines[i], perl = TRUE)
        indent   <- sub("^([ \t]*).*", "\\1", lines[i], perl = TRUE)
        fallback <- paste0(indent, "if (is.na(time_col)) time_col <- names(",
                           var_name, ")[length(names(", var_name, ")) - 1L]")
        lines <- c(lines[seq_len(i)], fallback, lines[seq.int(i + 1L, length(lines))])
        i <- i + 1L  # skip the newly inserted line
      }
    }
    i <- i + 1L
  }
  qmd_raw <<- paste(lines, collapse = "\n")
})

# ── Write post file ───────────────────────────────────────────────────────────
dir.create(POST_DIR, recursive = TRUE, showWarnings = FALSE)
writeLines(qmd_raw, POST_FILE)
message("Post written to: ", POST_FILE)

# ── Update compact topic index ────────────────────────────────────────────────
append_topic_index(TOPIC_INDEX_FILE, POST_SLUG, meta_title, meta_datasets, meta_charts)
message("Topic index updated: ", meta_title, " | ", meta_datasets, " | ", meta_charts)
