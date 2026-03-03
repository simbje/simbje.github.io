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

# ── SSB Dataset catalogue ─────────────────────────────────────────────────────
# A curated set of interesting SSB tables with known-working API parameters.
# Claude will pick from these (or combine several) each day.
SSB_CATALOGUE <- '
## Available SSB datasets (PxWebApiData)

### Economy & Prices
- **03013** — Consumer Price Index (KPI). ContentsCode="KpiAar", monthly
- **10235** — Producer Price Index. ContentsCode="PrisIndeks"
- **09189** — GDP quarterly (Nasjonalregnskap). ContentsCode="BNPB"
- **11174** — Household consumption. ContentsCode="Forbruk"
- **10948** — Government revenue & expenditure

### Labour Market
- **05111** — AKU unemployment rate (%). Kjonn="0", ArbStyrkStatus="04", ContentsCode="AKULedigProsent"
- **07459** — Employed persons by industry
- **11350** — Average monthly earnings by sector. ContentsCode="Lonn"
- **08536** — Registered unemployment by county (NAV)

### Housing & Construction
- **07221** — House prices per sqm by dwelling type. ContentsCode="KvPris"
- **06265** — Building permits granted
- **05889** — New dwellings completed

### Demographics & Population
- **09481** — Population by municipality
- **05803** — Births and deaths
- **09817** — Immigration by country of origin. ContentsCode="Innvandrere"
- **10211** — Population projections

### Energy & Environment
- **10315** — Electricity production and consumption. ContentsCode="KWhProd"
- **03321** — Greenhouse gas emissions by source
- **13931** — Electric vehicle registrations

### Education
- **09429** — Students in higher education by field
- **12886** — Completed education levels by age group

### Health
- **04861** — Causes of death
- **06913** — Hospital admissions by diagnosis group

### Business & Industry
- **07196** — Number of enterprises by industry
- **10582** — Bankruptcies by industry (very relevant given master thesis!)
- **08419** — Retail trade volume index

### International Trade
- **08800** — Exports by product group (oil & gas, fish, etc.)
- **08807** — Imports by product group

---

## PxWebApiData usage pattern (follow this EXACTLY)
```r
library(PxWebApiData)

# Step 1: Fetch
result <- ApiData(
  "https://data.ssb.no/api/v0/en/table/TABLE_ID",
  ContentsCode = "CODE",
  Tid = list(filter = "top", values = 40)
)

# Step 2: ALWAYS extract and inspect first — column names vary by table!
df <- result[[1]]
print(names(df))   # do this in a chunk so you can see what columns exist
print(head(df))

# Step 3: Find the time column defensively — it is NOT always called "Tid"
time_col <- names(df)[grepl("tid|aar|kvartal|maaned|year|quarter", 
                             names(df), ignore.case = TRUE)][1]
message("Time column is: ", time_col)

# Step 4: Parse time using the discovered column name
df <- df %>%
  mutate(
    value = as.numeric(value),
    time_str = .data[[time_col]],
    date = case_when(
      str_detect(time_str, "M")  ~ ym(str_replace(time_str, "M", "-")),
      str_detect(time_str, "K")  ~ yq(str_replace(time_str, "K", " Q")),
      str_length(time_str) == 4  ~ ymd(paste0(time_str, "-01-01")),
      TRUE ~ NA_Date_
    )
  ) %>%
  filter(!is.na(value), !is.na(date))
```
'

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

## Visualization requirements (CRITICAL)
- Create 3–5 ggplot2 charts minimum per post
- Rotate through these styles across posts: 
  waffle charts, lollipop charts, slope charts, ridgeline plots (ggridges),
  dumbell charts, area charts with annotations, small multiples/facets,
  animated plots (gganimate), bump charts, heatmaps, alluvial/sankey (ggalluvial),
  geographic maps (sf + Norway shapefiles from "geodata" or "rnaturalearth"),
  beeswarm plots (ggbeeswarm), waterfall charts, circular/polar plots,
  and classic but beautifully styled line/bar/scatter combos
- Use a cohesive, beautiful color palette (e.g., from MetBrewer, nord, wesanderson, or hand-picked)
- Every chart must have: clear title, subtitle with insight, caption citing SSB, clean theme
- Use theme_minimal() or theme_void() as base, then customize heavily
- Add annotations, reference lines, and labels directly on the chart where helpful
- Aim for "publication ready" quality

## R code requirements
- Always wrap data fetching in tryCatch with informative error messages
- Use tidyverse throughout (dplyr, tidyr, ggplot2, lubridate, scales)
- Include all library() calls at the top
- Code must be fully reproducible and runnable in a clean R session
- Use echo: true so readers can see the code
- Add brief prose comments explaining WHY you make analytical choices

## R code defensive requirements (CRITICAL — follow exactly)
- After every ApiData() call, ALWAYS do: `df <- result[[1]]` then immediately print `names(df)` and `head(df)` in a chunk
- NEVER assume column names — SSB returns different names per table. Always use `names(df)` to discover them first
- The time column is sometimes called "Tid", sometimes the full dimension name like "Kvartal" or "Maaned" — check with names()
- Parse time AFTER confirming the column exists: `time_col <- names(df)[grepl("id|aar|kvartal|maaned", names(df), ignore.case=TRUE)][1]`
- Every chunk that could fail MUST be wrapped in tryCatch
- Add `knitr::opts_chunk$set(error = TRUE)` in the setup chunk so one error never kills the whole post

## Post structure
Every post must have this exact YAML front matter format (NO image field):
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
2. Data section — fetch + wrangle, ALWAYS show names(df) and head(df) first
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
- Reference the bankruptcy dataset (10582) occasionally since this site owner is writing 
  a master thesis on bankruptcy prediction — but not every day

Remember: return ONLY the raw .qmd content, starting with ---
')

# ── Call Anthropic API ────────────────────────────────────────────────────────
message("Calling Anthropic API to generate post for ", POST_SLUG, "...")

response <- request("https://api.anthropic.com/v1/messages") |>
  req_headers(
    "x-api-key"         = ANTHROPIC_API_KEY,
    "anthropic-version" = "2023-06-01",
    "content-type"      = "application/json"
  ) |>
  req_body_json(list(
    model      = "claude-opus-4-5",
    max_tokens = 8000,
    system     = SYSTEM_PROMPT,
    messages   = list(
      list(role = "user", content = USER_PROMPT)
    )
  )) |>
  req_timeout(120) |>
  req_perform()

result  <- resp_body_json(response)
qmd_raw <- result$content[[1]]$text

# ── Parse response — split metadata from .qmd content ────────────────────────
raw_text <- result$content[[1]]$text

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

# ── Write post file ───────────────────────────────────────────────────────────
dir.create(POST_DIR, recursive = TRUE, showWarnings = FALSE)
writeLines(qmd_raw, POST_FILE)
message("✅ Post written to: ", POST_FILE)

# ── Update compact topic index ────────────────────────────────────────────────
append_topic_index(TOPIC_INDEX_FILE, POST_SLUG, meta_title, meta_datasets, meta_charts)
message("📋 Topic index updated: ", meta_title, " | ", meta_datasets, " | ", meta_charts)
