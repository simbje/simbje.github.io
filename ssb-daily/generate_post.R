#!/usr/bin/env Rscript
# =============================================================================
# generate_post.R
# Phase 1: Discovery agent browses SSB via tool_use to find + verify datasets.
# Phase 2: Generator writes the QMD using the agent's confirmed spec.
# Run by GitHub Actions every morning at 07:00 CET.
# =============================================================================

library(httr2)
library(jsonlite)
library(lubridate)
library(PxWebApiData)

# ── Config ────────────────────────────────────────────────────────────────────
ANTHROPIC_API_KEY <- Sys.getenv("ANTHROPIC_API_KEY")
TODAY             <- Sys.Date()
POST_SLUG         <- format(TODAY, "%Y-%m-%d")
POST_DIR          <- file.path("ssb-daily", "posts", POST_SLUG)
POST_FILE         <- file.path(POST_DIR, "index.qmd")

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

# ── Topic index helpers ────────────────────────────────────────────────────────
TOPIC_INDEX_FILE <- file.path("ssb-daily", "posts", "_topic_index.csv")

read_topic_index <- function(path) {
  if (!file.exists(path)) return(data.frame())
  tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) data.frame())
}

append_topic_index <- function(path, date, title, datasets, chart_types) {
  existing      <- read_topic_index(path)
  expected_cols <- c("date", "title", "datasets", "chart_types")
  new_row <- data.frame(
    date        = as.character(date),
    title       = substr(title, 1, 80),
    datasets    = substr(datasets, 1, 60),
    chart_types = substr(chart_types, 1, 60),
    stringsAsFactors = FALSE
  )
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

topic_index        <- read_topic_index(TOPIC_INDEX_FILE)
recent_topics_note <- if (nrow(topic_index) > 0) {
  recent <- tail(topic_index, 60)
  rows <- paste0(
    "- ", recent$date, ': "', recent$title, '" | datasets: ',
    recent$datasets, " | charts: ", recent$chart_types,
    collapse = "\n"
  )
  paste0("Recent posts (avoid repeating same angle or dataset):\n", rows)
} else {
  "No previous posts yet."
}

# ── SSB seed table list ────────────────────────────────────────────────────────
SSB_SEED_TABLES <- paste(c(
  "14700 - Consumer Price Index (new series 2026)",
  "03013 - Consumer Price Index by consumption group, monthly",
  "09170 - GDP and related measures, quarterly",
  "09189 - National accounts, quarterly main figures",
  "09190 - GDP by industry, quarterly",
  "11174 - Household final consumption expenditure",
  "05111 - Labour force status by age and sex, annual",
  "05110 - Labour force status by age and sex, quarterly",
  "13760 - Labour force and unemployment, seasonally adjusted, monthly",
  "11350 - Average monthly earnings by industry and sector",
  "08536 - Registered unemployed by age and county, monthly",
  "08771 - Job vacancies by industry",
  "12549 - Sickness absence by industry",
  "07221 - House price index, existing dwellings",
  "06265 - Building activity, dwellings started and completed",
  "07459 - Population by region, sex, age and year",
  "05803 - Births by mothers age",
  "09817 - Immigration and emigration by citizenship",
  "05196 - Fertility rates",
  "10634 - Marriages and divorces",
  "09481 - Population changes, quarterly",
  "08307 - Electricity balance, monthly",
  "13931 - First-time registered vehicles by fuel type and county",
  "03321 - Greenhouse gas emissions by source",
  "09429 - Students in higher education by field of study",
  "07184 - Kindergartens, children and staff",
  "07196 - Enterprises by industry and organisational form",
  "10582 - Bankruptcies by industry, quarterly",
  "08419 - Retail trade index, monthly",
  "08800 - Overnight stays by nationality and accommodation type",
  "08484 - Offences reported to police by type",
  "08807 - External trade in goods"
), collapse = "\n")

# ── Agent tool definitions ─────────────────────────────────────────────────────
AGENT_TOOLS <- list(
  list(
    name        = "get_ssb_metadata",
    description = paste0(
      "Fetch parameter names and allowed values for an SSB table. ",
      "Returns the exact parameter names to pass to ApiData(). ",
      "Call this first to understand what dimensions a table has."
    ),
    input_schema = list(
      type       = "object",
      properties = list(
        table_id = list(type = "string", description = "SSB table ID, e.g. '07221'")
      ),
      required = list("table_id")
    )
  ),
  list(
    name        = "fetch_ssb_sample",
    description = paste0(
      "Fetch a small sample of real data from an SSB table using all dimension parameters set to TRUE. ",
      "Returns exact column names and sample category values. ",
      "Use this to confirm a table works and to record the actual column names the post code must use."
    ),
    input_schema = list(
      type       = "object",
      properties = list(
        table_id  = list(type = "string"),
        n_periods = list(
          type        = "integer",
          description = "Number of most recent periods to fetch (default 5)"
        )
      ),
      required = list("table_id")
    )
  ),
  list(
    name        = "finalize_topic",
    description = paste0(
      "Call this once you have verified 1-3 working datasets. ",
      "Provide the complete spec — it will be passed directly to the post generator."
    ),
    input_schema = list(
      type       = "object",
      properties = list(
        datasets = list(
          type  = "array",
          items = list(
            type       = "object",
            properties = list(
              table_id     = list(type = "string"),
              description  = list(type = "string"),
              param_names  = list(
                type        = "array",
                items       = list(type = "string"),
                description = "Exact ApiData() parameter names (all except Tid)"
              ),
              column_names = list(
                type        = "array",
                items       = list(type = "string"),
                description = "Actual column names in raw[[1]] from a confirmed fetch"
              ),
              time_column = list(
                type        = "string",
                description = "Exact column name for the time dimension (contains year/quarter/month codes)"
              ),
              value_column = list(
                type        = "string",
                description = "Exact column name for the numeric value (usually 'value')"
              ),
              series_column = list(
                type        = "string",
                description = "Column identifying WHAT entity is measured (e.g. Makrost for national accounts, næring for industry, kjønn for gender)"
              ),
              frequency = list(
                type        = "string",
                enum        = list("year", "quarter", "month"),
                description = "Time frequency inferred from Tid sample values"
              ),
              measure_column = list(
                type        = "string",
                description = "Optional: column for HOW the value is expressed (e.g. ContentsCode = current prices / volume change). Omit if table has no unit/measure split."
              ),
              series_examples = list(
                type        = "array",
                items       = list(type = "string"),
                description = "Actual values from series_column (e.g. GDP, household consumption)"
              ),
              measure_examples = list(
                type        = "array",
                items       = list(type = "string"),
                description = "Actual values from measure_column (e.g. current prices, volume change). Omit if no measure_column."
              )
            ),
            required = list("table_id", "param_names", "column_names",
                            "time_column", "value_column", "series_column", "frequency")
          )
        ),
        story_angle = list(
          type        = "string",
          description = "The journalistic angle for today's post"
        )
      ),
      required = list("datasets", "story_angle")
    )
  )
)

# ── Tool dispatch ─────────────────────────────────────────────────────────────
dispatch_ssb_tool <- function(tool_name, input) {
  if (tool_name == "get_ssb_metadata") {
    table_id <- as.character(input$table_id)
    url      <- paste0("https://data.ssb.no/api/v0/no/table/", table_id)
    tryCatch({
      meta <- PxWebApiData::ApiData(url, returnMetaFrames = TRUE)
      if (!is.list(meta) || length(meta) == 0L)
        return(paste0("Table ", table_id, ": no metadata returned"))
      lines <- vapply(names(meta), function(p) {
        frame  <- meta[[p]]
        if (!is.data.frame(frame) || nrow(frame) == 0L) return(paste0(p, ": (empty)"))
        vals   <- head(as.character(frame[[1]]), 10L)
        suffix <- if (nrow(frame) > 10L) paste0(" ... (", nrow(frame), " total)") else ""
        paste0(p, ": [", paste(vals, collapse = ", "), suffix, "]")
      }, character(1L))
      paste0("Table ", table_id, " parameters:\n", paste(lines, collapse = "\n"))
    }, error = function(e) paste0("ERROR for ", table_id, ": ", e$message))

  } else if (tool_name == "fetch_ssb_sample") {
    table_id <- as.character(input$table_id)
    n        <- if (!is.null(input$n_periods)) as.integer(input$n_periods) else 5L
    url      <- paste0("https://data.ssb.no/api/v0/no/table/", table_id)
    tryCatch({
      meta        <- PxWebApiData::ApiData(url, returnMetaFrames = TRUE)
      param_names <- names(meta)
      call_args   <- c(
        list(url),
        setNames(
          lapply(param_names, function(p) {
            if (p == "Tid") list(filter = "top", values = n) else TRUE
          }),
          param_names
        )
      )
      raw <- do.call(PxWebApiData::ApiData, call_args)
      tmp <- raw[[1]]

      cat_cols <- names(tmp)[!vapply(tmp, is.numeric, logical(1L))]
      num_cols <- names(tmp)[vapply(tmp, is.numeric, logical(1L))]

      cat_summary <- vapply(cat_cols, function(col) {
        vals <- head(unique(as.character(tmp[[col]])), 10L)
        paste0("  ", col, ": [", paste(vals, collapse = ", "), "]")
      }, character(1L))

      paste0(
        "Table ", table_id, " — ", nrow(tmp), " rows fetched\n",
        "ALL columns: ", paste(names(tmp), collapse = ", "), "\n",
        "Categorical:\n", paste(cat_summary, collapse = "\n"), "\n",
        "Numeric: ", paste(num_cols, collapse = ", ")
      )
    }, error = function(e) paste0("ERROR fetching ", table_id, ": ", e$message))

  } else {
    paste0("Unknown tool: ", tool_name)
  }
}

# ── Discovery agent ────────────────────────────────────────────────────────────
MAX_AGENT_TURNS <- 14L

run_discovery_agent <- function() {
  agent_system <- paste0(
    "You are a data journalist discovering SSB (Statistics Norway) datasets for today's blog post.\n\n",
    "Workflow:\n",
    "1. Pick 2-4 promising tables from the seed list\n",
    "2. Call get_ssb_metadata() to learn exact parameter names for each\n",
    "3. Call fetch_ssb_sample() on the best 1-2 tables to confirm data quality\n",
    "4. After each fetch_ssb_sample(), classify the dimension roles (see below)\n",
    "5. Call finalize_topic() with the complete verified spec\n\n",
    "## Dimension role classification (CRITICAL — do this for every fetched table)\n",
    "After fetch_ssb_sample() returns, assign each column to exactly one role:\n\n",
    "time_column:   the column whose values look like '2024', '2024K1', '2024M01'\n",
    "value_column:  the first numeric column (almost always named 'value')\n",
    "series_column: the main categorical column identifying WHAT entity is measured\n",
    "               National accounts → Makrost\n",
    "               Labour/industry   → næring (or similar)\n",
    "               Demographics      → alder, kjønn, or region\n",
    "               Housing           → boligtype or region\n",
    "frequency:     inspect Tid sample values — 'K' suffix → 'quarter',\n",
    "               'M' suffix → 'month', 4-digit only → 'year'\n\n",
    "measure_column (optional — only for tables with a unit/measure split):\n",
    "               A SECOND categorical column describing HOW the value is expressed\n",
    "               e.g. ContentsCode = 'current prices' / 'volume change' / 'constant prices'\n",
    "               National accounts tables almost always have this.\n",
    "               Simple indicator tables (unemployment rate, house price index) usually do NOT.\n\n",
    "CRITICAL DISTINCTION:\n",
    "  series_column = WHAT entity  (GDP, household consumption, public investment, Aust-Agder)\n",
    "  measure_column = HOW measured (current NOK, fixed 2015 prices, % change from prev year)\n",
    "  These are NEVER the same column. Never confuse them.\n\n",
    "- Be efficient: aim to finalize in 6-8 tool calls total\n",
    "- Avoid tables that return errors"
  )

  agent_user <- paste0(
    "Today: ", format(TODAY, "%A, %d %B %Y"), "\n\n",
    "Seed tables:\n", SSB_SEED_TABLES, "\n\n",
    recent_topics_note, "\n\n",
    "Discover and verify 1-3 interesting datasets, then call finalize_topic()."
  )

  messages  <- list(list(role = "user", content = agent_user))
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
            system     = agent_system,
            tools      = AGENT_TOOLS,
            messages   = messages
          )) |>
          req_timeout(120) |>
          req_perform()
      }, max_attempts = 2L, base_wait = 3),
      error = function(e) { message("  Agent API error: ", e$message); NULL }
    )

    if (is.null(resp)) break

    r        <- resp_body_json(resp)
    messages <- c(messages, list(list(role = "assistant", content = r$content)))

    if (!identical(r$stop_reason, "tool_use")) {
      message("  Agent stopped (", r$stop_reason, ")")
      break
    }

    tool_results <- list()
    for (block in r$content) {
      if (!identical(block$type, "tool_use")) next

      if (identical(block$name, "finalize_topic")) {
        finalized    <- block$input
        tool_results <- c(tool_results, list(list(
          type        = "tool_result",
          tool_use_id = block$id,
          content     = "Topic finalized."
        )))
        break
      }

      message("    Tool: ", block$name, " — table ", block$input$table_id)
      result       <- dispatch_ssb_tool(block$name, block$input)
      tool_results <- c(tool_results, list(list(
        type        = "tool_result",
        tool_use_id = block$id,
        content     = result
      )))
      Sys.sleep(0.3)
    }

    messages <- c(messages, list(list(role = "user", content = tool_results)))

    if (!is.null(finalized)) {
      message("  Agent finalized: ", finalized$story_angle)
      return(finalized)
    }
  }

  message("Discovery agent did not finalize a topic.")
  NULL
}

# ── Build verified spec string for the generation prompt ──────────────────────
build_verified_spec <- function(agent_result) {
  sections <- lapply(agent_result$datasets, function(d) {
    param_names  <- as.character(unlist(d$param_names))
    column_names <- as.character(unlist(d$column_names))
    description  <- if (!is.null(d$description)) as.character(d$description) else as.character(d$table_id)
    frequency    <- if (!is.null(d$frequency)) as.character(d$frequency) else "unknown"

    param_str <- paste(
      paste0("    ", param_names, " = TRUE"),
      collapse = ",\n"
    )

    # Emit explicit R variable assignments — no guessing in the generator
    dim_lines <- paste0(
      'time_col   <- "', as.character(d$time_column),   '"\n',
      'value_col  <- "', as.character(d$value_column),  '"\n',
      'series_col <- "', as.character(d$series_column), '"'
    )
    has_measure <- !is.null(d$measure_column) && nchar(as.character(d$measure_column)) > 0
    if (has_measure) {
      dim_lines <- paste0(dim_lines, '\nmeasure_col <- "', as.character(d$measure_column), '"')
    }

    # Series / measure example values for building accurate filters
    ex_str <- ""
    if (!is.null(d$series_examples) && length(d$series_examples) > 0L) {
      ex_str <- paste0(ex_str,
        "series_col values (filter WHAT):  [",
        paste(as.character(unlist(d$series_examples)), collapse = ", "), "]\n")
    }
    if (has_measure && !is.null(d$measure_examples) && length(d$measure_examples) > 0L) {
      ex_str <- paste0(ex_str,
        "measure_col values (filter HOW):  [",
        paste(as.character(unlist(d$measure_examples)), collapse = ", "), "]\n")
    }

    paste0(
      "### ", d$table_id, " — ", description, "  (frequency: ", frequency, ")\n",
      "ApiData() call:\n",
      '  ApiData("https://data.ssb.no/api/v0/no/table/', d$table_id, '",\n',
      param_str, ',\n',
      '    Tid = list(filter = "top", values = 40)\n',
      "  )\n",
      "All columns in raw[[1]]: ", paste(column_names, collapse = ", "), "\n",
      "Dimension assignments (copy verbatim into fetch chunk):\n",
      dim_lines, "\n",
      ex_str
    )
  })

  paste0(
    "## VERIFIED DATASETS — confirmed by discovery agent\n\n",
    "Use ONLY these table IDs. All parameters and column names are from real API calls.\n",
    "Do NOT invent table IDs or parameter names.\n\n",
    paste(unlist(sections), collapse = "\n\n"),
    "\n\nStory angle: ", as.character(agent_result$story_angle)
  )
}

# ── Run discovery agent ────────────────────────────────────────────────────────
message("Phase 1: Running discovery agent...")
agent_result <- run_discovery_agent()
if (is.null(agent_result)) stop("Discovery agent failed to finalize a topic. Check SSB API connectivity.")

VERIFIED_SPEC <- build_verified_spec(agent_result)
valid_ids     <- vapply(agent_result$datasets, function(d) as.character(d$table_id), character(1L))
message("Verified tables: ", paste(valid_ids, collapse = ", "))

# ── System prompt (plain string — no glue, no brace escaping needed) ──────────
SYSTEM_PROMPT <- 'You are a world-class data journalist and R programmer specializing in Norwegian economics and statistics.
Your job is to write a complete, self-contained Quarto (.qmd) blog post analyzing Statistics Norway (SSB) data.

## Tone & Style
- Curious, journalistic, data-driven
- Accessible to an educated general audience
- Find the story in the numbers — surprises, trends, Norway-specific angles
- NO emojis, emoticons or decorative symbols
- Professional and clean throughout

## Visualization requirements
- Create 3-5 ggplot2 charts
- Rotate styles: lollipop, slope, ridgeline (ggridges), dumbbell, area with annotations,
  small multiples/facets, heatmap, alluvial (ggalluvial), beeswarm (ggbeeswarm), waterfall,
  polar, and classic line/bar/scatter combinations
- Color packages: MetBrewer, nord, wesanderson, or viridis — one cohesive palette per post
- Every chart: clear title, subtitle with insight, SSB caption, clean theme
- Use theme_minimal() or theme_void() as base, then customize
- Add annotations, reference lines, and direct labels where helpful
- Always assign ggplot to a variable then call print() explicitly

## Using the verified dataset spec (CRITICAL)
The user prompt contains a VERIFIED SPEC produced by a discovery agent that made real API calls.
- Use ONLY the table IDs listed in the spec — never invent table IDs
- Copy the ApiData() call exactly as shown — do not change parameter names
- "Dimension assignments" shows exact R variable names to use — copy them verbatim
- Do NOT use ApiData(returnMetaFrames = TRUE) in the post code

## Data fetching pattern
```{r fetch-data}
df <- NULL
tryCatch({
  raw <- ApiData(
    "https://data.ssb.no/api/v0/no/table/TABLE_ID",
    PARAM1 = TRUE,
    PARAM2 = TRUE,
    Tid = list(filter = "top", values = 40)
  )
  tmp <- raw[[1]]

  # Copy EXACT assignments from the spec — do NOT re-detect these columns
  time_col   <- "EXACT_FROM_SPEC"
  value_col  <- "EXACT_FROM_SPEC"
  series_col <- "EXACT_FROM_SPEC"
  # measure_col <- "EXACT_FROM_SPEC"  # only if spec includes measure_col

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

  if (nrow(df) == 0) stop("Empty after cleaning")
}, error = function(e) message("Fetch failed: ", e$message))
```

## Filtering rules (CRITICAL — series vs measure distinction)
- series_col  = WHAT entity is measured (GDP, household consumption, unemployment, Aust-Agder)
- measure_col = HOW the value is expressed (current prices, fixed 2015 prices, volume change %)

To filter for a specific entity:
  df_sub <- df |> filter(.data[[series_col]] == "exact value from series_col values in spec")

To filter for a unit of measurement:
  df_sub <- df |> filter(.data[[measure_col]] == "exact value from measure_col values in spec")

NEVER filter series_col for unit concepts (prices, volume, %).
NEVER filter measure_col for entity concepts (GDP, household, sector names).
Use only the exact string values listed under "series_col values" and "measure_col values" in the spec.

## Post-filter guard (CRITICAL — every filter selecting a category subset)
After ANY filter() that narrows to specific category labels:
  df_sub <- df |> filter(.data[[series_col]] == "Exact Value")
  if (nrow(df_sub) == 0) {
    message("Filter empty. Values: ", paste(head(unique(df[[series_col]]), 15), collapse = ", "))
    df_sub <- NULL
  }
  if (!is.null(df_sub)) {
    p <- ggplot(df_sub, ...) + ...
    print(p)
  }

## Frequency guard (CRITICAL)
The spec specifies frequency for each table. Respect it strictly:
- frequency = "year"    → NO quarterly or monthly decomposition; use year-over-year trends
- frequency = "quarter" → NO monthly seasonal charts; K1/K2/K3/K4 faceting is fine
- frequency = "month"   → full seasonal / monthly analysis allowed

## Seasonal analysis guard
Before any monthly/seasonal chart:
  has_monthly <- any(stringr::str_detect(df$time_str, "M\\d{2}"), na.rm = TRUE)
Only do seasonal charts if has_monthly is TRUE.

## Plotting rules (CRITICAL)
- ALWAYS: p <- ggplot(...) + ...; print(p)
- ALWAYS: guard with if (!is.null(df)) { ... }
- NEVER rely on implicit printing
- Every plot chunk must have:
```{r plot-name}
#| fig-height: 5
#| fig-width: 9
#| fig-show: asis
#| dev: "png"
```

## R code requirements
- First chunk: knitr::opts_chunk$set(echo=TRUE, warning=FALSE, message=FALSE, error=TRUE)
- Include all library() calls at the top
- Use tidyverse throughout
- echo: true

## Post structure
1. Brief intro (2-3 sentences) — the hook, why this matters
2. Data section — fetch + wrangle
3. 2-3 analysis sections with charts
4. Key findings — 3-5 bullet points with numbers
5. Closing reflection — broader context

## YAML front matter
---
title: "COMPELLING TITLE"
description: "ONE SENTENCE SUMMARY"
date: "DATE_TODAY"
categories: [SSB, category1, category2]
---

## Output format (REQUIRED — exactly this, nothing else)
PART 1: METADATA: title="..." datasets="..." chart_types="..."
PART 2: Raw .qmd starting with ---
No preamble, no markdown fences around the output, no explanation.'

# ── User prompt ────────────────────────────────────────────────────────────────
USER_PROMPT <- paste0(
  "Today is ", format(TODAY, "%A, %d %B %Y"), ".\n\n",
  VERIFIED_SPEC, "\n\n",
  recent_topics_note, "\n\n",
  "Write a complete Quarto blog post following the story angle above.\n",
  "Copy the ApiData() parameters exactly as shown in the spec.\n",
  "Include 3-5 charts with different visualization styles.\n\n",
  "STEP 1 — Setup chunk (first chunk in the document):\n",
  "```r\n",
  "knitr::opts_chunk$set(echo=TRUE, warning=FALSE, message=FALSE, error=TRUE)\n",
  "```\n\n",
  "STEP 2 — Data chunk: use EXACT ApiData() params from the spec above.\n\n",
  "STEP 3 — Plot chunks: guard with if (!is.null(df)), always print() explicitly."
)

# ── Generation call ────────────────────────────────────────────────────────────
message("Phase 2: Generating post...")

response <- with_retry(function() {
  request("https://api.anthropic.com/v1/messages") |>
    req_headers(
      "x-api-key"         = ANTHROPIC_API_KEY,
      "anthropic-version" = "2023-06-01",
      "content-type"      = "application/json"
    ) |>
    req_body_json(list(
      model      = "claude-sonnet-4-6",
      max_tokens = 16000L,
      system     = SYSTEM_PROMPT,
      messages   = list(list(role = "user", content = USER_PROMPT))
    )) |>
    req_timeout(300) |>
    req_perform()
}, max_attempts = 3L, base_wait = 5)

result <- resp_body_json(response)

if (identical(result$stop_reason, "max_tokens")) {
  warning("Response truncated (max_tokens). Post may be incomplete.")
}

text_blocks <- Filter(function(b) identical(b$type, "text"), result$content)
if (length(text_blocks) == 0L) stop("No text in API response.")
raw_text <- paste(vapply(text_blocks, function(b) b$text, character(1L)), collapse = "")

# ── Parse metadata ─────────────────────────────────────────────────────────────
extract_meta_field <- function(text, field) {
  pat <- paste0("(?i)", field, "\\s*=\\s*\"([^\"]+)\"")
  m   <- regmatches(text, regexpr(pat, text, perl = TRUE))
  if (length(m) == 0L || nchar(m) == 0L) return("")
  sub(paste0("(?i)", field, "\\s*=\\s*\"([^\"]+)\""), "\\1", m, perl = TRUE)
}

meta_line     <- tryCatch(
  regmatches(raw_text, regexpr("(?i)metadata:[ \t]*[^\n]*", raw_text, perl = TRUE)),
  error = function(e) character(0)
)
meta_title    <- extract_meta_field(meta_line, "title")
meta_datasets <- extract_meta_field(meta_line, "datasets")
meta_charts   <- extract_meta_field(meta_line, "chart_types")
if (nchar(meta_title) == 0L) meta_title <- "SSB Analysis"

# ── Extract QMD ───────────────────────────────────────────────────────────────
m <- regexpr("(?m)^---[ \t]*$", raw_text, perl = TRUE)
if (m[[1]] < 0L) stop("No YAML front matter in response:\n", substr(raw_text, 1, 500))
qmd_raw <- substring(raw_text, m[[1]])

# ── Enforce today's date ───────────────────────────────────────────────────────
local({
  fm_m <- regexpr("(?s)^---[ \t]*\n.*?\n---", qmd_raw, perl = TRUE)
  if (fm_m[[1]] > 0L && attr(fm_m, "match.length") > 0L) {
    fm_len <- attr(fm_m, "match.length")
    fm     <- substring(qmd_raw, 1L, fm_len)
    rest   <- substring(qmd_raw, fm_len + 1L)
    fm     <- gsub('date:\\s*"[^"]*"', paste0('date: "', format(TODAY, "%Y-%m-%d"), '"'), fm)
    qmd_raw <<- paste0(fm, rest)
  }
})

# ── Remove thumbnail ──────────────────────────────────────────────────────────
qmd_raw <- gsub('\nimage:\\s*["\']thumbnail\\.png["\']', "", qmd_raw)

# ── Reviewer pass ─────────────────────────────────────────────────────────────
REVIEWER_SYSTEM_PROMPT <- 'You are a code reviewer for Quarto blog posts that use SSB data.
Fix ONLY the specific bugs listed. Do not refactor or rewrite anything else.

BUG 1 — HARDCODED SSB PARAMETER CODES
  Wrong: ContentsCode = "Lonn" or Kjonn = "1" or NACE = c("nr23_6")
  Fix: Replace with TRUE for every dimension parameter except Tid.
  Tid = list(filter="top", values=N) is always correct — never change it.
  Only fix inside ApiData() calls — leave filter() and mutate() alone.

BUG 2 — MISSING NULL GUARD ON PLOT CHUNK
  Wrong: p <- ggplot(df, ...) without if (!is.null(df)) { }
  Fix: Wrap the entire plot in if (!is.null(df)) { ... }

BUG 3 — MISSING print() ON GGPLOT OBJECT
  Wrong: ggplot(df, aes(x, y)) + geom_line()
  Fix: p <- ggplot(df, aes(x, y)) + geom_line(); print(p)

BUG 4 — SEASONAL CHART WITHOUT has_monthly GUARD
  Fix: has_monthly <- any(stringr::str_detect(df$time_str, "M\\d{2}"), na.rm=TRUE)
       Wrap chart in: if (has_monthly) { ... }

BUG 5 — CROSS-CHUNK VARIABLE WITHOUT exists() CHECK
  Fix: Add exists("df_combined") to the guard condition.

BUG 6 — NORWEGIAN CHARACTERS IN grepl PATTERNS / MISSING NA GUARD
  Use . for ae/oe/aa in grepl patterns (never raw Norwegian chars).
  After every *_col <- names(tmp)[grepl(...)][1], add:
    if (is.na(col)) stop("Cannot detect column: ", paste(names(tmp), collapse=", "))

BUG 7 — FILTER WITHOUT nrow GUARD
  After every filter() selecting categories:
    if (nrow(df_sub) == 0) { message("Filter empty: ", paste(head(unique(df[[col]]), 15), collapse=", ")); df_sub <- NULL }
  Then: if (!is.null(df_sub)) { p <- ggplot(df_sub, ...); print(p) }

OUTPUT FORMAT:
Line 1: ISSUES: <comma-separated bugs fixed, or "none">
Line 2+: Corrected .qmd starting with ---'

review_and_fix_qmd <- function(qmd_content, valid_table_ids) {
  message("Reviewer pass...")
  reviewer_user <- paste0(
    "Valid table IDs: ", paste(valid_table_ids, collapse = ", "), "\n\n",
    "Review and fix:\n\n", qmd_content
  )

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
          max_tokens = 16000L,
          system     = REVIEWER_SYSTEM_PROMPT,
          messages   = list(list(role = "user", content = reviewer_user))
        )) |>
        req_timeout(300) |>
        req_perform()
    }, max_attempts = 2L, base_wait = 5),
    error = function(e) { message("  Reviewer failed: ", e$message); NULL }
  )

  if (is.null(resp)) return(list(qmd = qmd_content, issues = "reviewer_failed"))

  raw <- tryCatch({
    r   <- resp_body_json(resp)
    blk <- Filter(function(b) identical(b$type, "text"), r$content)
    if (length(blk) == 0L) stop("no text blocks")
    paste(vapply(blk, function(b) b$text, character(1L)), collapse = "")
  }, error = function(e) { message("  Reviewer parse failed: ", e$message); NULL })

  if (is.null(raw)) return(list(qmd = qmd_content, issues = "parse_failed"))

  issues_m <- regmatches(raw, regexpr("(?i)^ISSUES:[ \t]*[^\n]*", raw, perl = TRUE))
  issues   <- if (length(issues_m) > 0L) {
    trimws(sub("(?i)^ISSUES:[ \t]*", "", issues_m, perl = TRUE))
  } else "unknown"
  message("  Issues: ", issues)

  m <- regexpr("(?m)^---[ \t]*$", raw, perl = TRUE)
  if (m[[1]] < 0L) {
    message("  Reviewer returned no QMD — using original")
    return(list(qmd = qmd_content, issues = issues))
  }
  list(qmd = substring(raw, m[[1]]), issues = issues)
}

reviewer_result <- review_and_fix_qmd(qmd_raw, valid_ids)
qmd_raw         <- reviewer_result$qmd

# ── Validate ──────────────────────────────────────────────────────────────────
validate_qmd <- function(content, agent_result = NULL) {
  issues <- character(0)
  if (!grepl("^---", trimws(content)))
    issues <- c(issues, "Missing YAML front matter")
  fences <- gregexpr("(?m)^```", content, perl = TRUE)[[1]]
  if (!identical(fences, -1L) && length(fences) %% 2L != 0L)
    issues <- c(issues, paste0("Unbalanced code fences (", length(fences), ")"))
  if (!grepl("ApiData\\(", content))
    issues <- c(issues, "No ApiData() call found")
  if (!grepl("print\\(", content))
    issues <- c(issues, "No print() call — plots may not render")

  # Semantic checks against agent spec
  if (!is.null(agent_result)) {
    for (d in agent_result$datasets) {
      # Frequency mismatch: annual table described or filtered as quarterly
      if (identical(as.character(d$frequency), "year") &&
          grepl("K[1-4]|kvartal|quarter", content, ignore.case = TRUE, perl = TRUE)) {
        issues <- c(issues, paste0("Table ", d$table_id,
          " is annual (frequency=year) but code references quarters"))
      }
      # Series/measure confusion: measure column filtered for entity concepts
      mc <- if (!is.null(d$measure_column)) as.character(d$measure_column) else ""
      if (nchar(mc) > 0 &&
          grepl(mc, content, fixed = TRUE) &&
          grepl("household|GDP|BNP|syssels|investment|konsum",
                content, ignore.case = TRUE, perl = TRUE)) {
        issues <- c(issues, paste0("Possible series/measure confusion: entity concept ",
          "may be filtered from measure column '", mc, "'"))
      }
    }
  }
  issues
}

validation_issues <- validate_qmd(qmd_raw, agent_result)
if (length(validation_issues) > 0L)
  warning("Validation: ", paste(validation_issues, collapse = "; "))

# ── Write post ────────────────────────────────────────────────────────────────
dir.create(POST_DIR, recursive = TRUE, showWarnings = FALSE)
writeLines(qmd_raw, POST_FILE)
message("Post written: ", POST_FILE)

append_topic_index(TOPIC_INDEX_FILE, POST_SLUG, meta_title, meta_datasets, meta_charts)
message("Index updated: ", meta_title, " | ", meta_datasets, " | ", meta_charts)
