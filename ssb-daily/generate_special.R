#!/usr/bin/env Rscript
# =============================================================================
# generate_special.R  —  FRIDAY SPECIAL edition
# A weekly flagship deep-dive: instead of a descriptive post, this one FORECASTS
# and extracts structure from an SSB series using modern statistical machine
# learning (tidyverts: tsibble + fable + feasts — ETS, ARIMA, an ensemble
# combination, STL decomposition, and out-of-sample backtesting) and explains
# the method in plain language.
#
# The narrative is authored by Claude Opus 4.8 (the most capable model), but the
# forecasts and every reported number are COMPUTED IN R — never invented.
#
# Structure mirrors generate_post.R (same discovery agent + verified-spec flow)
# so it plugs into the same render / fix_post.R / commit pipeline. It writes to
# the same posts/<date> slug, so on Fridays it REPLACES the regular daily post.
# =============================================================================

library(httr2)
library(jsonlite)
library(lubridate)
library(PxWebApiData)

# ── Config ────────────────────────────────────────────────────────────────────
ANTHROPIC_API_KEY <- Sys.getenv("ANTHROPIC_API_KEY")
GENERATOR_MODEL   <- "claude-opus-4-8"          # most capable model for the writeup
DISCOVERY_MODEL   <- "claude-haiku-4-5-20251001" # cheap tool-use loop for discovery
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

# ── Blacklist: tables that recently returned no data (set by fix_post.R) ────────
blacklisted_table_ids <- local({
  path <- file.path("ssb-daily", "error_patterns.md")
  if (!file.exists(path) || file.size(path) == 0L) return(character(0))
  content  <- paste(readLines(path, warn = FALSE), collapse = "\n")
  sections <- strsplit(content, "\n(?=## )", perl = TRUE)[[1]]
  cutoff   <- format(Sys.Date() - 30L, "%Y-%m-%d")
  recent   <- Filter(function(s) {
    grepl("Data unavailable", s, fixed = TRUE) &&
    grepl("## (\\d{4}-\\d{2}-\\d{2})", s, perl = TRUE) &&
    regmatches(s, regexpr("## (\\d{4}-\\d{2}-\\d{2})", s, perl = TRUE)) >= paste0("## ", cutoff)
  }, sections)
  if (length(recent) == 0L) return(character(0))
  table_lines <- regmatches(recent, regexpr("SSB tables[^\n]*", recent, perl = TRUE))
  ids <- regmatches(table_lines, gregexpr("\\b\\d{4,6}\\b", table_lines))
  unique(unlist(ids))
})

unavailable_tables_note <- if (length(blacklisted_table_ids) > 0L) {
  paste0("NEVER use these SSB table IDs — they recently returned no data or an API error: ",
         paste(blacklisted_table_ids, collapse = ", "))
} else ""

# ── SSB seed table list — biased toward LONG, forecastable time series ──────────
# Friday specials forecast, so we favour tables with monthly/quarterly history and
# a clear temporal structure (prices, labour market, trade, energy, housing).
SSB_SEED_TABLES_VEC <- c(
  "14700 - Consumer Price Index (new series 2026), monthly",
  "03013 - Consumer Price Index by consumption group, monthly",
  "09170 - GDP and related measures, quarterly",
  "09189 - National accounts, quarterly main figures",
  "11174 - Household final consumption expenditure, quarterly",
  "05110 - Labour force status by age and sex, quarterly",
  "13760 - Labour force and unemployment, seasonally adjusted, monthly",
  "11350 - Average monthly earnings by industry and sector, quarterly",
  "08536 - Registered unemployed by age and county, monthly",
  "08771 - Job vacancies by industry, quarterly",
  "07221 - House price index, existing dwellings, quarterly",
  "06265 - Building activity, dwellings started and completed, monthly",
  "09481 - Population changes, quarterly",
  "08307 - Electricity balance, monthly",
  "13931 - First-time registered vehicles by fuel type and county, monthly",
  "08419 - Retail trade index, monthly",
  "08800 - Overnight stays by nationality and accommodation type, monthly",
  "08484 - Offences reported to police by type, quarterly",
  "08807 - External trade in goods, monthly",
  "10582 - Bankruptcies by industry, quarterly",
  "07459 - Population by region, sex, age and year, annual"
)

pool_tables_vec <- local({
  path <- file.path("ssb-daily", "table_pool.csv")
  if (!file.exists(path)) return(character(0))
  pool <- tryCatch(read.csv(path, stringsAsFactors = FALSE, colClasses = "character"),
                   error = function(e) NULL)
  if (is.null(pool) || nrow(pool) == 0L || !all(c("id", "label") %in% names(pool)))
    return(character(0))
  paste0(pool$id, " - ", pool$label)
})

all_tables_vec <- c(SSB_SEED_TABLES_VEC, pool_tables_vec)
all_tables_vec <- all_tables_vec[!duplicated(sub(" .*", "", all_tables_vec))]

if (length(blacklisted_table_ids) > 0L) {
  drop_re <- paste0("^(", paste(blacklisted_table_ids, collapse = "|"), ") ")
  all_tables_vec <- all_tables_vec[!grepl(drop_re, all_tables_vec)]
}

TABLE_CAP <- 50L
if (length(all_tables_vec) > TABLE_CAP) {
  is_seed   <- sub(" .*", "", all_tables_vec) %in% sub(" .*", "", SSB_SEED_TABLES_VEC)
  seed_keep <- all_tables_vec[is_seed]
  pool_keep <- all_tables_vec[!is_seed]
  n_extra   <- max(0L, TABLE_CAP - length(seed_keep))
  all_tables_vec <- c(seed_keep, sample(pool_keep, min(length(pool_keep), n_extra)))
}
message("Table pool: ", length(all_tables_vec), " tables offered to discovery agent.")
SSB_SEED_TABLES <- paste(all_tables_vec, collapse = "\n")

# ── Agent tool definitions (identical contract to the daily generator) ─────────
AGENT_TOOLS <- list(
  list(
    name        = "get_ssb_metadata",
    description = paste0(
      "Fetch parameter names and allowed values for an SSB table. ",
      "Returns the exact parameter names to pass to ApiData(). Call this first."
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
      "Fetch a small sample of real data from an SSB table with all dimension ",
      "parameters set to TRUE. Returns exact column names and sample values. ",
      "Also reports how many time periods exist — prefer tables with a long history."
    ),
    input_schema = list(
      type       = "object",
      properties = list(
        table_id  = list(type = "string"),
        n_periods = list(type = "integer",
                         description = "Number of most recent periods to fetch (default 8)")
      ),
      required = list("table_id")
    )
  ),
  list(
    name        = "finalize_topic",
    description = paste0(
      "Call this once you have verified ONE (ideally) or two forecastable datasets ",
      "with a long, regular time series. Provide the complete spec."
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
              param_names  = list(type = "array", items = list(type = "string"),
                                  description = "Exact ApiData() parameter names (all except Tid)"),
              column_names = list(type = "array", items = list(type = "string"),
                                  description = "Actual column names in raw[[1]]"),
              time_column  = list(type = "string",
                                  description = "Exact time-dimension column name"),
              value_column = list(type = "string",
                                  description = "Exact numeric value column (usually 'value')"),
              series_column = list(type = "string",
                                   description = "Column identifying WHAT entity is measured"),
              frequency    = list(type = "string", enum = list("year", "quarter", "month"),
                                  description = "Time frequency inferred from Tid sample values"),
              measure_column = list(type = "string",
                                    description = "Optional: column for HOW the value is expressed. Omit if none."),
              series_examples = list(type = "array", items = list(type = "string"),
                                     description = "Actual values from series_column"),
              measure_examples = list(type = "array", items = list(type = "string"),
                                      description = "Actual values from measure_column. Omit if none.")
            ),
            required = list("table_id", "param_names", "column_names",
                            "time_column", "value_column", "series_column", "frequency")
          )
        ),
        story_angle = list(type = "string",
                           description = "The forecasting question / angle for this week")
      ),
      required = list("datasets", "story_angle")
    )
  )
)

# ── Tool dispatch (adds a period-count hint to help pick long series) ──────────
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
    n        <- if (!is.null(input$n_periods)) as.integer(input$n_periods) else 8L
    url      <- paste0("https://data.ssb.no/api/v0/no/table/", table_id)
    tryCatch({
      meta        <- PxWebApiData::ApiData(url, returnMetaFrames = TRUE)
      param_names <- names(meta)
      tid_frame   <- meta[["Tid"]]
      n_periods_total <- if (is.data.frame(tid_frame)) nrow(tid_frame) else NA_integer_
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
        "Total time periods available (history length): ",
          ifelse(is.na(n_periods_total), "unknown", n_periods_total),
          " — prefer >= 24 for forecasting\n",
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
    "You are a data scientist choosing an SSB (Statistics Norway) time series to FORECAST ",
    "in this week's Friday special. You need ONE (ideally) or two series with a long, ",
    "regular history so statistical forecasting models can be fitted.\n\n",
    "Workflow:\n",
    "1. Pick 2-4 promising tables from the seed list — favour monthly or quarterly ones\n",
    "2. Call get_ssb_metadata() to learn exact parameter names\n",
    "3. Call fetch_ssb_sample() on the best 1-2 — check the reported history length\n",
    "4. Classify dimension roles (see below) for each fetched table\n",
    "5. Call finalize_topic() with the verified spec\n\n",
    "SELECTION PRIORITY: a long, continuous, numeric series (>= 24 observations, ",
    "monthly or quarterly preferred) that a general reader would find interesting to see ",
    "projected forward — prices, unemployment, wages, trade, energy, housing, transport.\n\n",
    "## Dimension role classification (do this for every fetched table)\n",
    "time_column:   values look like '2024', '2024K1', '2024M01'\n",
    "value_column:  the first numeric column (almost always 'value')\n",
    "series_column: the main categorical column identifying WHAT entity is measured\n",
    "frequency:     'K' suffix -> 'quarter', 'M' suffix -> 'month', 4-digit only -> 'year'\n",
    "measure_column (optional): a SECOND categorical column describing HOW the value is ",
    "expressed (e.g. ContentsCode = current prices / volume change). Omit if none.\n\n",
    "series_column = WHAT entity; measure_column = HOW measured. Never the same column.\n",
    "- Be efficient: finalize in 6-8 tool calls. Avoid tables that return errors."
  )

  agent_user <- paste0(
    "Today: ", format(TODAY, "%A, %d %B %Y"), " (Friday special).\n\n",
    "Seed tables:\n", SSB_SEED_TABLES, "\n\n",
    recent_topics_note, "\n\n",
    if (nzchar(unavailable_tables_note)) paste0(unavailable_tables_note, "\n\n") else "",
    "Find and verify ONE strong forecastable series (two at most), then call finalize_topic()."
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
            model      = DISCOVERY_MODEL,
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
          type = "tool_result", tool_use_id = block$id, content = "Topic finalized."
        )))
        break
      }
      message("    Tool: ", block$name, " — table ", block$input$table_id)
      result       <- dispatch_ssb_tool(block$name, block$input)
      tool_results <- c(tool_results, list(list(
        type = "tool_result", tool_use_id = block$id, content = result
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

# ── Build verified spec: pre-written fetch chunks (long history for forecasting) ─
# The special forecasts ONE primary series, so we always use the first dataset and
# name it `df` — that keeps the baked pipeline (which references df / series_col /
# measure_col) unambiguous.
build_verified_spec <- function(agent_result) {
  datasets <- agent_result$datasets[1]
  df_names <- "df"

  sections <- mapply(function(d, df_name) {
    param_names  <- as.character(unlist(d$param_names))
    description  <- if (!is.null(d$description)) as.character(d$description) else as.character(d$table_id)
    frequency    <- if (!is.null(d$frequency)) as.character(d$frequency) else "unknown"
    has_measure  <- !is.null(d$measure_column) && nchar(as.character(d$measure_column)) > 0L

    param_str    <- paste(paste0("    ", param_names, " = TRUE"), collapse = ",\n")
    measure_line <- if (has_measure) {
      paste0('  measure_col  <- "', as.character(d$measure_column), '"\n')
    } else ""

    # Long history (up to 400 periods) so the forecasting models have signal.
    fetch_chunk <- paste0(
      "```r\n",
      df_name, " <- NULL\n",
      "tryCatch({\n",
      "  raw <- ApiData(\n",
      '    "https://data.ssb.no/api/v0/no/table/', d$table_id, '",\n',
      param_str, ',\n',
      '    Tid = list(filter = "top", values = 400)\n',
      "  )\n",
      "  tmp          <- raw[[1]]\n",
      '  time_col     <- "', as.character(d$time_column),  '"\n',
      '  value_col    <- "', as.character(d$value_column), '"\n',
      '  series_col   <- "', as.character(d$series_column),'"\n',
      measure_line,
      "  ", df_name, " <- tmp |>\n",
      "    mutate(\n",
      "      value    = as.numeric(.data[[value_col]]),\n",
      "      time_str = .data[[time_col]],\n",
      "      date     = case_when(\n",
      '        stringr::str_detect(time_str, "M") ~ lubridate::ym(sub("M", "-", time_str)),\n',
      '        stringr::str_detect(time_str, "K") ~ lubridate::yq(sub("K", " Q", time_str)),\n',
      '        nchar(time_str) == 4               ~ lubridate::ymd(paste0(time_str, "-01-01")),\n',
      "        TRUE ~ NA_Date_\n",
      "      )\n",
      "    ) |>\n",
      "    filter(!is.na(value), !is.na(date))\n",
      '}, error = function(e) message("Fetch failed: ", e$message))\n',
      "if (is.null(", df_name, ") || nrow(", df_name, ") == 0) ", df_name, " <- NULL\n",
      "```"
    )

    ex_str <- ""
    if (!is.null(d$series_examples) && length(d$series_examples) > 0L)
      ex_str <- paste0(ex_str,
        'series_col ("', as.character(d$series_column), '") values — filter WHAT: [',
        paste(as.character(unlist(d$series_examples)), collapse = ", "), "]\n")
    if (has_measure && !is.null(d$measure_examples) && length(d$measure_examples) > 0L)
      ex_str <- paste0(ex_str,
        'measure_col ("', as.character(d$measure_column), '") values — filter HOW: [',
        paste(as.character(unlist(d$measure_examples)), collapse = ", "), "]\n")

    paste0(
      "### ", df_name, " — table ", d$table_id, " — ", description,
      "  (frequency: ", frequency, ")\n\n",
      "Pre-written fetch chunk — include verbatim, do NOT rewrite:\n",
      fetch_chunk, "\n\n", ex_str
    )
  }, datasets, df_names, SIMPLIFY = FALSE)

  primary_freq <- as.character(datasets[[1]]$frequency)

  paste0(
    "## VERIFIED DATASET — pre-written fetch chunk\n\n",
    "CRITICAL RULES:\n",
    "1. Include the fetch chunk VERBATIM — do NOT rewrite it or add grepl detection\n",
    "2. Use ONLY the table ID listed here\n",
    "3. The single data frame to forecast is `df`.\n",
    "4. Primary series frequency: ", primary_freq, "\n\n",
    paste(unlist(sections), collapse = "\n\n"),
    "\n\nForecasting question / angle: ", as.character(agent_result$story_angle)
  )
}

# ── Pre-written forecasting pipeline (included verbatim, like the fetch chunks) ─
# Produces (all NULL if data insufficient): ts_tsibble, fc_fit, fc_future,
# fc_backtest_acc, stl_components, fc_freq, fc_h. Heavily guarded so a short or
# irregular series degrades to "figure omitted" rather than crashing the render.
FORECAST_PIPELINE_CHUNK <- paste0(
  "```r\n",
  "# ---- Modern probabilistic forecasting pipeline (tidyverts: fable/feasts) ----\n",
  "ts_tsibble      <- NULL\n",
  "fc_fit          <- NULL\n",
  "fc_future       <- NULL\n",
  "fc_backtest_acc <- NULL\n",
  "stl_components  <- NULL\n",
  "fc_freq         <- NA_character_\n",
  "fc_h            <- NA_integer_\n",
  "fc_series_label <- NA_character_\n",
  "\n",
  "if (!is.null(df) && nrow(df) > 0) {\n",
  "  # Reduce to ONE clean univariate series before modelling.\n",
  "  ts_base <- df\n",
  "  # (a) if a measure/unit dimension exists, keep its single most common level\n",
  "  if (exists(\"measure_col\") && !is.null(measure_col) && measure_col %in% names(ts_base)) {\n",
  "    keep_measure <- ts_base |> dplyr::count(.data[[measure_col]], sort = TRUE) |>\n",
  "      dplyr::slice(1) |> dplyr::pull(1)\n",
  "    ts_base <- ts_base |> dplyr::filter(.data[[measure_col]] == keep_measure)\n",
  "  }\n",
  "  # (b) keep the single series_col entity with the LONGEST history\n",
  "  if (exists(\"series_col\") && !is.null(series_col) && series_col %in% names(ts_base) &&\n",
  "      dplyr::n_distinct(ts_base[[series_col]]) > 1) {\n",
  "    keep_series <- ts_base |> dplyr::group_by(.data[[series_col]]) |>\n",
  "      dplyr::summarise(np = dplyr::n_distinct(time_str), .groups = \"drop\") |>\n",
  "      dplyr::slice_max(np, n = 1, with_ties = FALSE) |> dplyr::pull(1)\n",
  "    ts_base <- ts_base |> dplyr::filter(.data[[series_col]] == keep_series)\n",
  "  }\n",
  "  if (exists(\"series_col\") && !is.null(series_col) && series_col %in% names(ts_base))\n",
  "    fc_series_label <- as.character(ts_base[[series_col]][1])\n",
  "  ts_total <- ts_base |>\n",
  "    dplyr::group_by(date, time_str) |>\n",
  "    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = \"drop\") |>\n",
  "    dplyr::arrange(date) |>\n",
  "    dplyr::filter(!is.na(value))\n",
  "\n",
  "  fc_freq <- dplyr::case_when(\n",
  "    any(stringr::str_detect(ts_total$time_str, \"M[0-9]{2}\"), na.rm = TRUE) ~ \"month\",\n",
  "    any(stringr::str_detect(ts_total$time_str, \"K[1-4]\"),   na.rm = TRUE) ~ \"quarter\",\n",
  "    TRUE ~ \"year\"\n",
  "  )\n",
  "  fc_h <- switch(fc_freq, month = 12L, quarter = 8L, 5L)\n",
  "\n",
  "  ts_tsibble <- tryCatch({\n",
  "    tt <- if (fc_freq == \"month\") {\n",
  "      ts_total |> dplyr::mutate(idx = tsibble::yearmonth(date))\n",
  "    } else if (fc_freq == \"quarter\") {\n",
  "      ts_total |> dplyr::mutate(idx = tsibble::yearquarter(date))\n",
  "    } else {\n",
  "      ts_total |> dplyr::mutate(idx = as.integer(lubridate::year(date)))\n",
  "    }\n",
  "    tt |>\n",
  "      dplyr::distinct(idx, .keep_all = TRUE) |>\n",
  "      dplyr::arrange(idx) |>\n",
  "      tsibble::as_tsibble(index = idx) |>\n",
  "      tsibble::fill_gaps()\n",
  "  }, error = function(e) { message(\"tsibble build failed: \", e$message); NULL })\n",
  "\n",
  "  if (!is.null(ts_tsibble) && sum(!is.na(ts_tsibble$value)) >= 16L) {\n",
  "    # Impute any gaps created by fill_gaps() so ETS/ARIMA get a complete series\n",
  "    ts_tsibble$value <- zoo::na.approx(ts_tsibble$value, na.rm = FALSE)\n",
  "    ts_tsibble$value <- zoo::na.locf(ts_tsibble$value, na.rm = FALSE)\n",
  "    ts_tsibble$value <- zoo::na.locf(ts_tsibble$value, fromLast = TRUE, na.rm = FALSE)\n",
  "\n",
  "    n_obs  <- nrow(ts_tsibble)\n",
  "    n_test <- max(2L, round(n_obs * 0.2))\n",
  "    train  <- ts_tsibble |> dplyr::slice_head(n = n_obs - n_test)\n",
  "\n",
  "    # Out-of-sample backtest: fit on train, score on the held-out tail\n",
  "    fc_backtest_acc <- tryCatch({\n",
  "      bt_fit <- train |>\n",
  "        fabletools::model(ETS = fable::ETS(value), ARIMA = fable::ARIMA(value)) |>\n",
  "        dplyr::mutate(Ensemble = (ETS + ARIMA) / 2)\n",
  "      bt_fc <- bt_fit |> fabletools::forecast(h = n_test)\n",
  "      fabletools::accuracy(bt_fc, ts_tsibble) |>\n",
  "        dplyr::select(.model, RMSE, MAE, MAPE) |>\n",
  "        dplyr::arrange(RMSE)\n",
  "    }, error = function(e) { message(\"backtest failed: \", e$message); NULL })\n",
  "\n",
  "    # Refit on the full series, forecast the future with prediction intervals\n",
  "    fc_res <- tryCatch({\n",
  "      fit <- ts_tsibble |>\n",
  "        fabletools::model(ETS = fable::ETS(value), ARIMA = fable::ARIMA(value)) |>\n",
  "        dplyr::mutate(Ensemble = (ETS + ARIMA) / 2)\n",
  "      list(fit = fit, fc = fit |> fabletools::forecast(h = fc_h))\n",
  "    }, error = function(e) { message(\"forecast failed: \", e$message); NULL })\n",
  "    if (!is.null(fc_res)) { fc_fit <- fc_res$fit; fc_future <- fc_res$fc }\n",
  "\n",
  "    # STL decomposition (trend / seasonal / remainder) — needs seasonality\n",
  "    stl_components <- tryCatch({\n",
  "      if (fc_freq == \"year\") NULL else\n",
  "        ts_tsibble |> fabletools::model(feasts::STL(value)) |> fabletools::components()\n",
  "    }, error = function(e) { message(\"STL failed: \", e$message); NULL })\n",
  "  }\n",
  "}\n",
  "```"
)

# ── Run discovery agent ────────────────────────────────────────────────────────
message("Phase 1: Running discovery agent (forecast-oriented)...")
agent_result <- run_discovery_agent()
if (is.null(agent_result)) {
  message("SSB API unavailable or discovery agent could not finalize — skipping.")
  quit(save = "no", status = 0)
}

VERIFIED_SPEC <- build_verified_spec(agent_result)
valid_ids     <- vapply(agent_result$datasets, function(d) as.character(d$table_id), character(1L))
message("Verified tables: ", paste(valid_ids, collapse = ", "))

# ── System prompt ──────────────────────────────────────────────────────────────
SYSTEM_PROMPT <- 'You are a world-class data scientist and R programmer writing the FRIDAY SPECIAL
edition of a blog that analyses Statistics Norway (SSB) data. This weekly flagship goes beyond
description: you FORECAST an SSB time series with modern statistical machine learning and you
EXPLAIN the method in plain language for a curious general audience.

You are Claude Opus 4.8, the most capable model Anthropic offers. The post should read as rigorous,
transparent, and honest applied forecasting — never hand-wavy, never overclaiming.

## What makes this edition special (REQUIRED)
- Fit REAL models in R and generate genuine forecasts with uncertainty (prediction intervals).
- Use the tidyverts stack already prepared for you: tsibble + fable + feasts.
  * ETS (exponential smoothing state-space model)
  * ARIMA (auto-selected)
  * an Ensemble that averages the two
  * STL decomposition (trend / seasonal / remainder)
  * an out-of-sample BACKTEST with accuracy metrics (RMSE, MAE, MAPE)
- Explain, in a dedicated methodology section, what these techniques do and why, and be candid
  about what the models can and cannot capture (shocks, policy changes, structural breaks).

## Use the pre-written chunks VERBATIM (CRITICAL)
The user prompt gives you (a) a verified fetch chunk and (b) a complete forecasting pipeline
chunk. Copy BOTH verbatim, each into its own ```{r} block. Do NOT rewrite them, do NOT add
grepl-based column detection, do NOT change the model formulas. After the pipeline runs, these
objects exist (any may be NULL if the series was too short/irregular):
  ts_tsibble      — the cleaned tsibble that was modelled
  fc_fit          — the fitted models (mable)
  fc_future       — the future forecast with intervals (fable)
  fc_backtest_acc — a tibble: .model, RMSE, MAE, MAPE (best first)
  stl_components  — STL components (NULL for annual series)
  fc_freq         — "month" / "quarter" / "year"
  fc_h            — forecast horizon used
  fc_series_label — the exact SSB label of the single series being forecast (name it in the text)

## Commentary must follow the data (CRITICAL — same discipline as the daily edition)
You have NOT seen the numbers. Therefore NEVER state a forecast value, an interval, an accuracy
score, a trend direction, or a named winner as static prose — that would be a guess, and if the
model did not run it becomes a comment about an analysis that does not exist, telling the reader
the post is not based on the data.
- Every such claim MUST be computed in R from the objects above and printed with cat()/sprintf()
  from INSIDE the guard of the relevant chunk, AFTER print(p). It then self-omits when data is missing.
- NEVER place a numeric result or ranking outside a guard.
- EXCEPTION — the methodology section: you MAY describe the TECHNIQUES in general terms as static
  prose (that is the educational point of the post). But any RESULT stays in code.

## Chunk labelling contract (CRITICAL — the CI pipeline keys off labels)
A post is automatically discarded if a chunk labelled "plot-*" produces no image, and it is
SCRAPPED entirely if any chunk emits the exact phrase "returned no data for this series". So:
- Label a chunk "plot-*" ONLY if it ALWAYS prints a real ggplot/autoplot IMAGE when its guard
  passes. The REQUIRED, always-expected figures are the history chart and the forecast fan
  chart — label these plot-* and give their else-branch the EXACT scrap phrase below. If those
  are empty the series is unusable and the post SHOULD be scrapped and retried.
- A chunk that prints a TABLE (gt) or only cat() TEXT produces no image — do NOT label it
  plot-*. Label tables "tbl-*" and text-only notes "note-*".
- An OPTIONAL figure that may be legitimately absent (e.g. STL, which is NULL for annual
  series) must NOT be labelled plot-*. Label it "fig-*" and give it a NEUTRAL else branch that
  does NOT contain the phrase "returned no data for this series" (otherwise the whole post is
  wrongly scrapped). Example neutral note:
    else { cat("\n*Seasonal decomposition is shown only for monthly or quarterly series.*\n\n") }

## Plotting rules (CRITICAL)
- ALWAYS: p <- ggplot(...)/autoplot(...) + ...; print(p). Never rely on implicit printing.
- ALWAYS guard the FULL body: if (exists("X") && !is.null(X) && nrow(X) > 0) { ...; print(p) }
- After print(p), cat()/sprintf() the interpretation computed from the data (never static prose).
- Forecast fan chart pattern (REQUIRED figure, label plot-forecast):
    if (exists("fc_future") && !is.null(fc_future) && exists("ts_tsibble") && !is.null(ts_tsibble)) {
      p <- fc_future |> dplyr::filter(.model == "Ensemble") |>
        autoplot(ts_tsibble, level = c(80, 95)) + labs(...) + theme_minimal()
      print(p)
      # then cat() the computed point forecast + 95% interval for the final horizon, e.g.:
      ens  <- fc_future |> dplyr::filter(.model == "Ensemble") |> dplyr::slice_tail(n = 1)
      pit  <- ens |> fabletools::hilo(95)
      cat(sprintf("\nThe ensemble projects %s by %s (95%% interval %s to %s).\n\n",
                  scales::comma(round(ens$.mean)), as.character(ens$idx),
                  scales::comma(round(pit[["95%"]]$lower)), scales::comma(round(pit[["95%"]]$upper))))
    } else { cat("\n*Figure omitted — Statistics Norway returned no data for this series.*\n\n") }
- REQUIRED plot-* figures use this else-branch EXACTLY:
    else { cat("\n*Figure omitted — Statistics Norway returned no data for this series.*\n\n") }
- Every plot-* / fig-* chunk carries:
```{r plot-name}
#| fig-height: 5
#| fig-width: 9
#| fig-show: asis
#| dev: "png"
```

## Variable-scope discipline (CRITICAL — prevents "object X not found")
Initialise every derived variable to NULL at the top of a wrangle chunk BEFORE any conditional
logic. Plot/commentary guards must check exists() && !is.null() && nrow()>0.

## R code requirements
- FIRST CHUNK (label: setup): ONE block with knitr::opts_chunk$set(echo=TRUE, warning=FALSE,
  message=FALSE, error=TRUE), ALL library() calls (tidyverse, lubridate, PxWebApiData, scales,
  tsibble, fable, feasts, fabletools, zoo, and a palette package such as MetBrewer), and ALL
  fetch chunks copied verbatim.
- SECOND CHUNK: the forecasting pipeline chunk, copied verbatim.
- Do NOT split setup/libraries/fetches into separate chunks.
- Use tidyverse throughout. echo: true.

## Post structure
1. Intro (2-3 sentences) — the forecasting question and why it matters. General only; NO specific numbers.
2. "The data" — fetch + a quick historical context chart (guarded, data-driven commentary).
3. "The approach" — methodology section: explain STL, ETS, ARIMA, the ensemble, prediction
   intervals, and the backtest, in plain language. This section MAY be static prose describing
   the methods (but not results). Note that the analysis is authored by Claude Opus 4.8.
4. "Validation" — show the backtest accuracy (from fc_backtest_acc) as a gt table in a "tbl-*"
   chunk (NOT plot-*), plus a code-emitted sentence naming which model won and by how much.
5. "The forecast" — the fan chart with intervals (label plot-forecast), plus a code-emitted
   sentence stating the point forecast and 95% interval for the final horizon.
6. STL decomposition chart in a "fig-stl" chunk (NOT plot-*), guarded on stl_components, with a
   neutral else branch (no scrap phrase) and a short data-driven note when present.
7. Key findings — ONE guarded R chunk (label "note-findings", NOT plot-*) emitting bullets
   computed from the data (no hand-written numbers).
8. Limitations & closing — honest, general caveats about forecasting (no invented numbers).

## Tone & Style
- Curious, rigorous, transparent. Accessible to an educated general audience.
- NO emojis or decorative symbols. Professional and clean.
- Chart titles/subtitles: neutral and descriptive of what is plotted, not an unverified conclusion.

## YAML front matter
---
title: "Friday Special: COMPELLING TITLE"
description: "ONE SENTENCE SUMMARY"
date: "DATE_TODAY"
categories: [SSB, Friday Special, forecasting, machine learning]
---

## Output format (REQUIRED — exactly this, nothing else)
PART 1: METADATA: title="..." datasets="..." chart_types="..."
PART 2: Raw .qmd starting with ---
No preamble, no markdown fences around the output, no explanation.'

# ── User prompt ────────────────────────────────────────────────────────────────
USER_PROMPT <- paste0(
  "Today is ", format(TODAY, "%A, %d %B %Y"), " — the Friday special.\n\n",
  VERIFIED_SPEC, "\n\n",
  "## PRE-WRITTEN FORECASTING PIPELINE — include VERBATIM as the second chunk\n",
  "Copy this into its own ```{r forecast-pipeline} block, unchanged:\n\n",
  FORECAST_PIPELINE_CHUNK, "\n\n",
  recent_topics_note, "\n\n",
  "Write the complete Friday-special Quarto post following the story angle and the 8-part structure.\n\n",
  "Chunk order:\n",
  "CHUNK 1 (label: setup): opts_chunk$set(...) + ALL library() calls + ALL fetch chunks, verbatim, in ONE block.\n",
  "CHUNK 2 (label: forecast-pipeline): the forecasting pipeline above, verbatim.\n",
  "CHUNK 3+: wrangle + output chunks, each guarded with ",
  "if (exists(\"X\") && !is.null(X) && nrow(X) > 0) { ...; print(p) }.\n",
  "Labelling contract (the CI keys off it):\n",
  "  - REQUIRED image figures (history chart, forecast fan chart) -> label plot-*, else-branch EXACTLY: ",
  "cat(\"\\n*Figure omitted — Statistics Norway returned no data for this series.*\\n\\n\").\n",
  "  - Backtest accuracy TABLE -> label tbl-* (NOT plot-*).\n",
  "  - OPTIONAL STL figure (NULL for annual) -> label fig-stl (NOT plot-*), NEUTRAL else note WITHOUT the scrap phrase.\n",
  "  - Key findings / text-only -> label note-* (NOT plot-*).\n",
  "After each print(p), cat() the interpretation computed from the data — never as static prose.\n",
  "The methodology section (\"The approach\") is prose describing the techniques only; results stay in code.\n",
  "FINAL CHUNK (label note-findings): Key findings — ONE guarded chunk emitting bullets computed from the data."
)

# ── Generation call (Opus) ─────────────────────────────────────────────────────
message("Phase 2: Generating Friday special with ", GENERATOR_MODEL, "...")

response <- with_retry(function() {
  request("https://api.anthropic.com/v1/messages") |>
    req_headers(
      "x-api-key"         = ANTHROPIC_API_KEY,
      "anthropic-version" = "2023-06-01",
      "content-type"      = "application/json"
    ) |>
    req_body_json(list(
      model      = GENERATOR_MODEL,
      max_tokens = 20000L,
      system     = SYSTEM_PROMPT,
      messages   = list(list(role = "user", content = USER_PROMPT))
    )) |>
    req_timeout(600) |>
    req_perform()
}, max_attempts = 3L, base_wait = 5)

result <- resp_body_json(response)

if (identical(result$stop_reason, "max_tokens"))
  warning("Response truncated (max_tokens). Post may be incomplete.")

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
if (nchar(meta_title) == 0L) meta_title <- "Friday Special: SSB Forecast"

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

# ── Validate ──────────────────────────────────────────────────────────────────
validate_qmd <- function(content) {
  issues <- character(0)
  if (!grepl("^---", trimws(content)))       issues <- c(issues, "Missing YAML front matter")
  fences <- gregexpr("(?m)^```", content, perl = TRUE)[[1]]
  if (!identical(fences, -1L) && length(fences) %% 2L != 0L)
    issues <- c(issues, paste0("Unbalanced code fences (", length(fences), ")"))
  if (!grepl("ApiData\\(", content))         issues <- c(issues, "No ApiData() call found")
  if (!grepl("fabletools::forecast|forecast\\(", content))
    issues <- c(issues, "No forecast() call — the special must forecast")
  if (!grepl("print\\(", content))           issues <- c(issues, "No print() call")
  issues
}

validation_issues <- validate_qmd(qmd_raw)
if (length(validation_issues) > 0L)
  warning("Validation: ", paste(validation_issues, collapse = "; "))

# ── Write post ────────────────────────────────────────────────────────────────
dir.create(POST_DIR, recursive = TRUE, showWarnings = FALSE)
writeLines(qmd_raw, POST_FILE)
message("Friday special written: ", POST_FILE)

append_topic_index(TOPIC_INDEX_FILE, POST_SLUG, meta_title, meta_datasets, meta_charts)
message("Index updated: ", meta_title, " | ", meta_datasets, " | ", meta_charts)
