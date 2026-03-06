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

IMPORTANT: Since the script always discovers parameters with returnMetaFrames = TRUE 
before fetching data, Claude should feel free to explore ANY 5-digit SSB table — 
the discovery step will reveal if a table exists and what parameters it has. 
The tables below are verified starting points, but Claude can also try nearby 
table IDs or search SSB Statbank (https://www.ssb.no/en/statbank) for more.

### Economy & Prices
- **03013** — Consumer Price Index (KPI) by consumption group, monthly (2015=100). Note: being replaced by **14700** in 2026.
- **03014** — Consumer Price Index (KPI) 12-month rate, by consumption group, monthly
- **09170** — GDP and related measures, quarterly (Nasjonalregnskap)
- **09174** — Wages, employment and productivity by industry, annual (Nasjonalregnskap)
- **09189** — National accounts, quarterly main figures
- **09190** — GDP by industry, quarterly
- **11174** — Household final consumption expenditure
- **10948** — General government revenue and expenditure, annual
- **12880** — National accounts, annual main figures

### Labour Market
- **05111** — Population by labour force status, age and sex, annual (AKU)
- **05110** — Population by labour force status, age and sex, quarterly (AKU)
- **13760** — Labour force, employment, unemployment — seasonally adjusted, monthly
- **11350** — Average monthly earnings by industry (SIC2007) and sector
- **08536** — Registered unemployed by age and county (NAV monthly)
- **07984** — R&D personnel and FTE in business enterprise sector
- **08771** — Job vacancies by major industry division (SIC2007)
- **03629** — Work stoppages, wage earners involved and working days lost
- **12549** — Sickness absence, by industry and type of absence

### Housing & Construction
- **07221** — House price index, existing dwellings
- **06265** — Building activity — dwellings started, under construction, completed
- **05889** — New dwellings completed by county

### Demographics & Population
- **07459** — Population by region (municipality), sex, age and year
- **05810** — Population by age, sex and year (national)
- **04861** — Area and population of urban settlements
- **05803** — Births, by mothers age
- **09817** — Immigration and emigration by citizenship and country
- **05196** — Fertility rates
- **10634** — Marriages and divorces
- **09481** — Population changes, quarterly

### Energy & Environment
- **08307** — Electricity balance, monthly (production, consumption, exports, imports)
- **13931** — First-time registered vehicles by fuel type and county
- **09288** — Energy balance for Norway, by energy product
- **03321** — Greenhouse gas emissions by source

### Education
- **09429** — Students in higher education by field of study
- **08956** — Upper secondary completion rates
- **07184** — Kindergartens — children, staff, operating expenses

### Health
- **06913** — Activities in somatic hospitals — patient statistics

### Business & Industry
- **07196** — Enterprises by industry (NACE) and organisational form
- **10582** — Bankruptcies opened, by industry, quarterly
- **08419** — Retail trade index, monthly (volume and value)
- **08800** — Overnight stays by nationality and accommodation type

### Crime & Justice
- **08406** — Offences reported to the police by type of offence

### KOSTRA — Municipality Data (kommune-level, annual)
KOSTRA covers Norwegian municipalities. Published 15 March (preliminary) and 15 June (final).
Use Region = "EAK" for national average, or municipality codes like "3001" (Halden), "0301"/"3005" (Oslo), etc.
Use Region = TRUE for all municipalities. Tid = annual 4-digit year.
KOSTRA tables are numbered 12xxx–13xxx. The discovery step will reveal exact parameters.
Example KOSTRA tables to try:
- **12362** — Barnehage (kindergarten) key figures
- **12215** — Grunnskole (primary school) key figures  
- **12006** — Pleie og omsorg (elderly care) key figures
- **12163** — Municipal financial key figures
- **12559** — Water and sewage key figures

### International Trade
- **08800** — (also used under Business — tourism nights)
- **08807** — External trade in goods — imports and exports

---

## PxWebApiData usage pattern
```r
library(PxWebApiData)

# Fetch data
result <- ApiData(
  "https://data.ssb.no/api/v0/no/table/TABLE_ID",
  ContentsCode = "CODE",          # the measure you want
  Tid = list(filter="top", values=40)  # last 40 periods
)

# result is a list: result[[1]] is a data.frame with columns:
# value, Tid (time), and dimension columns
df <- result[[1]] %>%
  mutate(
    value = as.numeric(value),
    # parse Tid depending on frequency:
    # Monthly  "2024M01" -> ym("2024-01")
    # Quarterly "2024K1" -> yq("2024 Q1")  
    # Annual   "2024"   -> ymd(paste0(Tid, "-01-01"))
  )
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

## Parameter discovery (CRITICAL — wrong parameter names is the most common failure)
You do NOT know the exact parameter names for SSB tables. They vary between tables and your
guesses will be wrong. You MUST always discover parameters first using returnMetaFrames = TRUE.

MANDATORY first chunk for EVERY table you use — do this BEFORE any data fetching:

```{{r discover-params}}
# Discover the actual parameter names and valid values for this table
meta <- PxWebApiData::ApiData(
  "https://data.ssb.no/api/v0/no/table/TABLE_ID",
  returnMetaFrames = TRUE
)

# Print parameter names — use THESE in your ApiData() call, not guesses
cat("Valid parameters:\\n")
print(names(meta))

# Print first few valid values for each parameter
for (param in names(meta)) {{
  cat("\\n---", param, "---\\n")
  print(head(meta[[param]], 10))
}}
```

Then use the EXACT parameter names from the discovery output in your ApiData() call.
For example, if discovery shows the parameters are "UtslpTilLuft", "UtslpEnergivare",
"ContentsCode", and "Tid" — those are what you use. NEVER guess names like "Kjoretoytype"
or "Drivstofftype" — they will fail.

When calling ApiData() for the actual data:
- Use TRUE for a dimension parameter to get all values for that dimension
- Use Tid = list(filter="top", values=N) to get the last N time periods
- Only specify ContentsCode if you know the exact code from the discovery step

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
2. Parameter discovery chunk — returnMetaFrames = TRUE for each table
3. Data section — fetch + wrangle using discovered parameter names, show the raw shape
4. Analysis section(s) — 2-3 distinct analytical angles with charts
5. Key findings — 3-5 bullet points with the numbers
6. Closing reflection — broader context or what to watch next

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

STEP 2 — Discovery chunk (MANDATORY for every table):
```r
# Discover valid parameter names — NEVER guess them
meta <- PxWebApiData::ApiData(
  "https://data.ssb.no/api/v0/no/table/XXXXX",
  returnMetaFrames = TRUE
)
cat("Valid parameters:\\n")
print(names(meta))
for (param in names(meta)) {{
  cat("\\n---", param, "---\\n")
  print(head(meta[[param]], 10))
}}
```

STEP 3 — Data chunk using discovered names:
```r
df <- NULL  # ALWAYS initialise to NULL outside

tryCatch({{
  # Use the EXACT parameter names from the discovery step above
  raw <- ApiData("https://data.ssb.no/api/v0/no/table/XXXXX",
                 # e.g. if discovery showed "UtslpEnergivare" and "ContentsCode":
                 UtslpEnergivare = TRUE,
                 ContentsCode = TRUE,
                 Tid = list(filter="top", values=40))
  
  tmp <- raw[[1]]
  print(names(tmp))  # always print column names
  
  # discover time column defensively
  time_col <- names(tmp)[grepl("tid|aar|kvartal|maaned|year|month|quarter", names(tmp), ignore.case=TRUE)][1]
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

STEP 4 — Plot chunk (guard + print):
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
stop_reason <- result$stop_reason %||% "unknown"
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

# ── Write post file ───────────────────────────────────────────────────────────
dir.create(POST_DIR, recursive = TRUE, showWarnings = FALSE)
writeLines(qmd_raw, POST_FILE)
message("Post written to: ", POST_FILE)

# ── Update compact topic index ────────────────────────────────────────────────
append_topic_index(TOPIC_INDEX_FILE, POST_SLUG, meta_title, meta_datasets, meta_charts)
message("Topic index updated: ", meta_title, " | ", meta_datasets, " | ", meta_charts)
