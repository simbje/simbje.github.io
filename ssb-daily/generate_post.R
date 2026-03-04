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
- **03013** — Consumer Price Index (KPI), monthly. ContentsCode="KpiAar"
- **03014** — CPI by expenditure group (food, housing, transport etc.)
- **10235** — Producer Price Index. ContentsCode="PrisIndeks"
- **09189** — GDP quarterly (Nasjonalregnskap). ContentsCode="BNPB"
- **09190** — GDP by industry, quarterly
- **11174** — Household consumption by category
- **10948** — Government revenue and expenditure
- **12880** — National accounts, annual main figures
- **10714** — Household saving rate
- **07197** — Wage share of GDP
- **10269** — Interest rates, Norges Bank key policy rate

### Labour Market
- **05111** — AKU unemployment rate (%). Kjonn="0", ArbStyrkStatus="04", ContentsCode="AKULedigProsent"
- **07459** — Employed persons by industry (NACE)
- **11350** — Average monthly earnings by sector. ContentsCode="Lonn"
- **08536** — Registered unemployment by county (NAV)
- **11615** — Part-time employment by gender and sector
- **07984** — Labour productivity by industry
- **12549** — Sick leave rates by industry and gender
- **05940** — Working hours per week by occupation
- **13425** — Job vacancies by industry
- **11441** — Employees on temporary contracts
- **09174** — Strike days lost per year

### Housing & Construction
- **07221** — House prices per sqm by dwelling type. ContentsCode="KvPris"
- **06265** — Building permits granted by type
- **05889** — New dwellings completed by county
- **08655** — Housing starts vs completions
- **07276** — Average housing size by household type
- **09313** — Rental prices by city
- **12805** — Households by tenure (own vs rent)
- **10748** — Mortgage debt by age group

### Demographics & Population
- **09481** — Population by municipality, quarterly
- **05803** — Births, deaths and natural change
- **09817** — Immigration and emigration by country of origin
- **10211** — Population projections to 2100
- **05196** — Fertility rate by age of mother
- **08771** — Life expectancy at birth by gender
- **09599** — Population density by municipality
- **10634** — Marriages and divorces
- **12255** — Household size distribution
- **07459** — Population by age group and gender
- **10618** — Internal migration between counties

### Energy & Environment
- **10315** — Electricity production and consumption. ContentsCode="KWhProd"
- **03321** — Greenhouse gas emissions by source (CO2 equiv)
- **13931** — Electric vehicle registrations by county
- **09383** — Energy consumption by industry
- **11813** — Renewable energy share of total production
- **10774** — Electricity prices for households
- **07580** — Oil and gas production on the Norwegian shelf
- **09287** — District heating production
- **12880** — Energy intensity of the economy
- **13165** — Solar panel installations
- **08940** — Waste generation and recycling rates
- **10478** — Air pollution emissions by sector

### Education
- **09429** — Students in higher education by field of study
- **12886** — Completed education levels by age group
- **08956** — Upper secondary school completion rates
- **09429** — University graduates by field, annual
- **12535** — Student loans and grants (Lånekassen)
- **07184** — Kindergarten coverage rate by municipality
- **09429** — International students in Norway
- **10840** — School results (national tests) by county
- **12890** — Adult education participation

### Health & Social
- **04861** — Causes of death by diagnosis group
- **06913** — Hospital admissions by diagnosis (DRG)
- **11843** — Mental health outpatient visits
- **09550** — Disability benefit recipients by age
- **12140** — Social assistance recipients
- **08655** — Alcohol consumption per capita
- **08756** — Smoking rates by age and gender
- **09937** — Obesity rates by county
- **11721** — GP consultations per capita
- **10440** — Waiting times for specialist treatment
- **07902** — Long-term sick leave by diagnosis

### Business & Industry
- **07196** — Number of enterprises by industry (NACE)
- **10582** — Bankruptcies by industry, quarterly
- **08419** — Retail trade volume index, monthly
- **11721** — Industrial production index
- **10714** — Business investment by industry
- **09174** — Company profit margins by sector
- **12430** — New business registrations (Brønnøysund)
- **07459** — Turnover in service industries
- **08800** — Tourism nights by nationality
- **10259** — Restaurant and hotel revenue index
- **13050** — E-commerce turnover
- **11440** — R&D expenditure by sector
- **09023** — Patent applications by industry

### KOSTRA — Municipality Data (kommune-level, annual)
KOSTRA covers all 356 Norwegian municipalities. Published 15 March (preliminary) and 15 June (final).
Use Region = "EAK" for national average, or codes: "0301" (Oslo), "3024" (Baerum), "1103" (Stavanger), "5001" (Trondheim), "4601" (Bergen).
Use Region = TRUE for all municipalities. Time = annual 4-digit year.

#### Barnehage (Kindergarten)
- **12370** — Barnehage key figures per kommune (coverage rate, staff per child, cost per child)
- **12568** — Share of children in barnehage by age group and kommune
- **12369** — Expenditure per child in barnehage per kommune

#### Grunnskole (Primary School)
- **12215** — Grunnskole key figures: expenditure per pupil, teacher density, special education
- **12216** — Teacher density by grade and kommune
- **12220** — Share of pupils receiving special education per kommune

#### Pleie og omsorg (Elderly Care)
- **12006** — Elderly care key figures: expenditure per resident 80+, nursing home coverage
- **12007** — Nursing home places per 1000 residents 80+ per kommune
- **12008** — Expenditure on elderly care per resident per kommune

#### Sosialtjeneste (Social Services)
- **11133** — Number of social assistance recipients per kommune
- **11134** — Average duration of social assistance per kommune
- **11135** — Social assistance expenditure per resident per kommune

#### Barnevern (Child Welfare)
- **12359** — Child welfare key figures per kommune
- **09051** — Children with investigations or measures per 1000 residents aged 0-17
- **09052** — Child welfare expenditure per resident per kommune

#### Kommuneokonomi (Municipal Finance)
- **12163** — Municipal financial key figures: net operating result, debt, reserves
- **12164** — Net operating result as pct of gross operating income per kommune
- **12165** — Long-term debt per resident per kommune
- **12166** — Discretionary fund as pct of gross operating income per kommune
- **12167** — Free income per resident (tax + block grant) per kommune

#### Tekniske tjenester (Water, Waste, Roads)
- **12559** — Water and sewage key figures: fees, pipe network per kommune
- **12560** — Waste collection: source sorting rate, fees per kommune
- **12561** — Municipal roads: maintenance cost, length per resident

#### Kultur og idrett (Culture and Sport)
- **12557** — Culture key figures: expenditure per resident, library, music school
- **11940** — Library: loans per resident, opening hours per kommune
- **11941** — Music school: pupils per 1000 residents aged 6-15 per kommune
- **12558** — Sport: municipal sports facilities, expenditure per resident

#### Plan og bygg (Planning and Building)
- **12572** — Planning and building: processing time, building permits per kommune
- **12388** — Municipal roads and public transport key figures
- **12389** — Walking and cycling paths per resident per kommune

### International Trade & Shipping
- **08800** — Exports by product group (oil, gas, fish, metals)
- **08807** — Imports by product group
- **09174** — Trade balance monthly
- **10890** — Fish exports by species and destination country
- **07580** — Oil and gas export volumes and values
- **11350** — Shipping earnings (Norwegian merchant fleet)
- **12109** — Foreign direct investment stocks
- **08940** — Container throughput at Norwegian ports

### Transport & Infrastructure
- **13931** — Vehicle registrations by fuel type
- **08567** — Road traffic volume index
- **10384** — Aviation passengers by airport
- **09023** — Rail passenger journeys
- **07984** — Cycling infrastructure length by municipality
- **12799** — Traffic accidents by severity and county
- **11440** — Electric scooter registrations
- **09383** — Public transport usage by mode

### Agriculture & Fisheries
- **10890** — Fish landings by species and port
- **08655** — Agricultural land use by crop type
- **09174** — Farm income and subsidies
- **07580** — Aquaculture production (salmon, trout, etc.)
- **12140** — Food price index at farm gate
- **08771** — Number of farms and farm sizes
- **10315** — Forestry harvest volumes

### Crime & Justice
- **08406** — Reported crimes by type (theft, violence, fraud)
- **09594** — Prosecutions and convictions
- **10258** — Prison population by offence type
- **12655** — Domestic violence reports
- **09023** — Cybercrime incidents
- **10890** — Drug offences by county
- **08567** — Traffic offences (speeding, DUI)

### Culture & Leisure
- **08419** — Cinema admissions and revenue
- **10714** — Museum visits by institution type
- **09550** — Library loans and visits
- **11843** — Sports club membership by sport
- **12535** — Lottery and gambling expenditure
- **07902** — Newspaper readership trends
- **13050** — Streaming service usage
- **09937** — Holiday and travel patterns

### Public Finances & Welfare
- **10948** — Central government budget by ministry
- **09189** — Municipal finances and debt
- **12880** — Pension fund (Oljefondet / GPFG) flows
- **10269** — Tax revenue by type
- **08940** — Social security expenditure by benefit type
- **11721** — Child benefit recipients
- **10440** — Parental leave take-up by gender
- **07276** — Income distribution and Gini coefficient
- **09313** — Poverty rates by household type
- **12805** — Wealth distribution by percentile

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
