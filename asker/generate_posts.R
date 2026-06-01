#!/usr/bin/env Rscript
# =============================================================================
# asker/generate_posts.R
# Genererer én omfattende bloggpost om Asker kommune basert på SSB/KOSTRA-data.
# Kjør én gang lokalt: source("asker/generate_posts.R")
# Output: blog/posts/YYYY-MM-DD-asker-kommunedata/index.qmd
# =============================================================================

library(httr2)
library(jsonlite)
library(lubridate)
library(PxWebApiData)

# ── Config ────────────────────────────────────────────────────────────────────
ANTHROPIC_API_KEY <- Sys.getenv("ANTHROPIC_API_KEY")
if (nchar(ANTHROPIC_API_KEY) == 0) stop("ANTHROPIC_API_KEY ikke satt")

TODAY     <- Sys.Date()
ASKER_CODE <- "3024"
POST_DIR  <- file.path("blog", "posts",
                       paste0(format(TODAY, "%Y-%m-%d"), "-asker-kommunedata"))
POST_FILE <- file.path(POST_DIR, "index.qmd")

if (dir.exists(POST_DIR)) {
  message("Post eksisterer allerede: ", POST_DIR)
  message("Slett mappen og kjør på nytt for å regenerere.")
  quit(save = "no", status = 0)
}

# ── Retry-hjelper ─────────────────────────────────────────────────────────────
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
      message("  Forsøk ", attempt, " feilet: ", conditionMessage(last_error),
              " — prøver igjen om ", wait, "s...")
      Sys.sleep(wait)
    }
  }
  stop(last_error)
}

# ── SSB-tabeller med kommunedata ───────────────────────────────────────────────
SSB_SEED_TABLES <- paste(c(
  "07459 - Folkemengde etter region (kommunenivå), kjønn, alder og år",
  "04469 - Befolkningsendringer etter region (fødsel, inn- og utvandring) kommunenivå",
  "05803 - Levendefødte etter mors bostedskommune",
  "06265 - Byggeaktivitet — igangsatte og fullførte boliger etter kommune",
  "13931 - Førstegangsregistrerte kjøretøy etter drivstofftype og region/kommune",
  "12575 - Barnehager — barn og ansatte etter kommune",
  "11616 - Sysselsatte etter bostedskommune",
  "13470 - Pendlere etter bosted og arbeidssted (kommune)",
  "09429 - Folkemengde per 1. januar etter kommune",
  "12949 - Barnehagedekning (KOSTRA) etter kommune",
  "12209 - Netto driftsutgifter grunnskole per innbygger (KOSTRA) etter kommune",
  "12370 - Barnehage nøkkeltall (KOSTRA) etter kommune",
  "06913 - Boliger etter bygningstype og kommune",
  "05196 - Fruktbarhetstall etter bostedskommune",
  "10614 - Innvandrere og norskfødte med innvandrerforeldre etter bostedskommune",
  "08801 - Grunnskoleelever etter bostedskommune",
  "09625 - Kommunale boliger etter kommune",
  "11342 - Byggesak saksbehandlingstid (KOSTRA) etter kommune",
  "13954 - Husholdninger etter antall personer og kommuner"
), collapse = "\n")

# ── Verktøydefinisjoner ───────────────────────────────────────────────────────
AGENT_TOOLS <- list(
  list(
    name        = "get_ssb_metadata",
    description = paste0(
      "Hent parameternavn og gyldige verdier for en SSB-tabell. ",
      "Kall denne først for å forstå tabellens dimensjoner."
    ),
    input_schema = list(
      type       = "object",
      properties = list(
        table_id = list(type = "string", description = "SSB tabell-ID, f.eks. '07459'")
      ),
      required = list("table_id")
    )
  ),
  list(
    name        = "fetch_ssb_sample",
    description = paste0(
      "Hent et lite utvalg data fra en SSB-tabell. ",
      "Søker automatisk etter Asker (3024) og rapporterer funn."
    ),
    input_schema = list(
      type       = "object",
      properties = list(
        table_id  = list(type = "string"),
        n_periods = list(type = "integer",
                         description = "Antall siste perioder å hente (standard 5)")
      ),
      required = list("table_id")
    )
  ),
  list(
    name        = "finalize_topic",
    description = "Kall når du har bekreftet 3-5 fungerende datasett for Asker (3024).",
    input_schema = list(
      type       = "object",
      properties = list(
        datasets = list(
          type  = "array",
          items = list(
            type       = "object",
            properties = list(
              table_id            = list(type = "string"),
              description         = list(type = "string"),
              param_names         = list(type = "array", items = list(type = "string"),
                description = "Eksakte ApiData()-parameternavn (alle unntatt Tid)"),
              column_names        = list(type = "array", items = list(type = "string"),
                description = "Faktiske kolonnenavn i raw[[1]]"),
              time_column         = list(type = "string"),
              value_column        = list(type = "string"),
              series_column       = list(type = "string",
                description = "HVA som måles — IKKE kommunekolonnen"),
              municipality_param  = list(type = "string",
                description = "SSB API-parameternavn for kommunedimensjonen (f.eks. 'Region')"),
              municipality_column = list(type = "string",
                description = "Kolonnenavn i raw[[1]] for kommunedimensjonen"),
              municipality_value  = list(type = "string",
                description = "Eksakt SSB-verdi for Asker (f.eks. '3024', 'Asker (2020-)')"),
              frequency           = list(type = "string",
                enum = list("year", "quarter", "month")),
              measure_column      = list(type = "string",
                description = "Valgfritt: kolonne for HVORDAN verdien uttrykkes"),
              series_examples     = list(type = "array", items = list(type = "string")),
              measure_examples    = list(type = "array", items = list(type = "string")),
              ssb_url             = list(type = "string",
                description = "SSB-URL til tabellen, f.eks. https://www.ssb.no/statbank/table/07459")
            ),
            required = list("table_id", "param_names", "column_names", "time_column",
                            "value_column", "series_column", "municipality_param",
                            "municipality_column", "municipality_value", "frequency")
          )
        ),
        post_title = list(type = "string",
          description = "Forslag til norsk tittel på bloggposten"),
        post_angle = list(type = "string",
          description = "Overordnet journalistisk vinkel og struktur for posten")
      ),
      required = list("datasets", "post_title", "post_angle")
    )
  )
)

# ── Verktøydispatch ───────────────────────────────────────────────────────────
dispatch_ssb_tool <- function(tool_name, input) {
  if (tool_name == "get_ssb_metadata") {
    table_id <- as.character(input$table_id)
    url      <- paste0("https://data.ssb.no/api/v0/no/table/", table_id)
    tryCatch({
      meta  <- PxWebApiData::ApiData(url, returnMetaFrames = TRUE)
      if (!is.list(meta) || length(meta) == 0L)
        return(paste0("Tabell ", table_id, ": ingen metadata"))
      lines <- vapply(names(meta), function(p) {
        frame  <- meta[[p]]
        if (!is.data.frame(frame) || nrow(frame) == 0L) return(paste0(p, ": (tom)"))
        vals   <- head(as.character(frame[[1]]), 15L)
        suffix <- if (nrow(frame) > 15L) paste0(" ... (", nrow(frame), " totalt)") else ""
        paste0(p, ": [", paste(vals, collapse = ", "), suffix, "]")
      }, character(1L))
      paste0("Tabell ", table_id, ":\n", paste(lines, collapse = "\n"))
    }, error = function(e) paste0("FEIL for ", table_id, ": ", e$message))

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

      asker_found <- character(0)
      for (col in cat_cols) {
        vals <- as.character(tmp[[col]])
        if (any(grepl("^3024$|Asker", vals, ignore.case = TRUE))) {
          asker_vals <- unique(vals[grepl("^3024$|Asker", vals, ignore.case = TRUE)])
          asker_found <- c(asker_found,
            paste0('  Kolonne "', col, '": [', paste(head(asker_vals, 5L), collapse = ", "), "]"))
        }
      }

      cat_summary <- vapply(cat_cols, function(col) {
        uvals  <- unique(as.character(tmp[[col]]))
        shown  <- head(uvals, 12L)
        suffix <- if (length(uvals) > 12L) paste0(" ... (", length(uvals), " unike)") else ""
        paste0("  ", col, ": [", paste(shown, collapse = ", "), suffix, "]")
      }, character(1L))

      asker_note <- if (length(asker_found) > 0L) {
        paste0("\nASKER FUNNET:\n", paste(asker_found, collapse = "\n"),
               "\n→ Egnet for Asker-analyse.")
      } else {
        "\nASKER IKKE FUNNET — prøv en annen tabell."
      }

      paste0("Tabell ", table_id, " — ", nrow(tmp), " rader\n",
             "Kolonner: ", paste(names(tmp), collapse = ", "), "\n",
             "Kategoriske:\n", paste(cat_summary, collapse = "\n"), "\n",
             "Numeriske: ", paste(num_cols, collapse = ", "),
             asker_note)
    }, error = function(e) paste0("FEIL: ", table_id, ": ", e$message))

  } else {
    paste0("Ukjent verktøy: ", tool_name)
  }
}

# ── Discovery-agent (finner 3-5 datasett) ─────────────────────────────────────
MAX_TURNS <- 20L

run_discovery_agent <- function() {
  agent_system <- paste0(
    "Du er en datajournalist som finner SSB/KOSTRA-statistikk om Asker kommune (kode 3024).\n",
    "Asker ble dannet i 2020 (sammenslåing av Asker 0220, Røyken 0227, Hurum 0628).\n",
    "Den er en stor Akershus-kommune — velstående, mange pendlere, grenser til Oslo.\n\n",
    "MÅL: Finn 3-5 varierte og interessante datasett om Asker for én samlet bloggpost.\n",
    "Dekk gjerne: befolkning, bolig, pendling, barnehage/utdanning, næring/sysselsetting, miljø/elbiler.\n\n",
    "ARBEIDSFLYT:\n",
    "1. Velg 4-6 tabeller fra frøtabellisten\n",
    "2. Kall get_ssb_metadata() for å se dimensjoner\n",
    "3. Kall fetch_ssb_sample() — bekreft at 'ASKER FUNNET' rapporteres\n",
    "4. Identifiser dimensjonsroller (se nedenfor)\n",
    "5. Kall finalize_topic() med 3-5 bekreftede datasett\n\n",
    "DIMENSJONSROLLER:\n",
    "- municipality_param:  API-parameternavn (fra metadata, f.eks. 'Region')\n",
    "- municipality_column: kolonnenavn i raw[[1]] (f.eks. 'region')\n",
    "- municipality_value:  eksakt Asker-verdi (hva ASKER FUNNET viste, f.eks. '3024')\n",
    "- time_column:   kolonne med '2024', '2024K1' eller '2024M01'\n",
    "- value_column:  første numeriske kolonne (nesten alltid 'value')\n",
    "- series_column: hoved-kategorisk kolonne for HVA som måles (IKKE kommunekolonnen)\n",
    "- frequency:     'year' (4 siffer), 'quarter' (K-suffiks), 'month' (M-suffiks)\n",
    "- ssb_url:       https://www.ssb.no/statbank/table/{table_id}\n\n",
    "Mål: 10-14 verktøykall totalt. Velg datasett som gir spennende Asker-historier."
  )

  agent_user <- paste0(
    "Dato: ", format(TODAY, "%d. %B %Y"), "\n\n",
    "Frøtabeller:\n", SSB_SEED_TABLES, "\n\n",
    "Finn 3-5 gode, varierte Asker-datasett og kall finalize_topic()."
  )

  messages  <- list(list(role = "user", content = agent_user))
  finalized <- NULL

  for (turn in seq_len(MAX_TURNS)) {
    message("  Agent-tur ", turn, "...")

    resp <- tryCatch(
      with_retry(function() {
        request("https://api.anthropic.com/v1/messages") |>
          req_headers("x-api-key" = ANTHROPIC_API_KEY,
                      "anthropic-version" = "2023-06-01",
                      "content-type" = "application/json") |>
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
      error = function(e) { message("  API-feil: ", e$message); NULL }
    )

    if (is.null(resp)) break

    r        <- resp_body_json(resp)
    messages <- c(messages, list(list(role = "assistant", content = r$content)))

    if (!identical(r$stop_reason, "tool_use")) break

    tool_results <- list()
    for (block in r$content) {
      if (!identical(block$type, "tool_use")) next

      if (identical(block$name, "finalize_topic")) {
        finalized    <- block$input
        tool_results <- c(tool_results, list(list(
          type = "tool_result", tool_use_id = block$id, content = "Finalisert."
        )))
        break
      }

      message("    ", block$name,
              if (!is.null(block$input$table_id)) paste0(" — ", block$input$table_id) else "")
      result       <- dispatch_ssb_tool(block$name, block$input)
      tool_results <- c(tool_results, list(list(
        type = "tool_result", tool_use_id = block$id, content = result
      )))
      Sys.sleep(0.3)
    }

    messages <- c(messages, list(list(role = "user", content = tool_results)))

    if (!is.null(finalized)) {
      message("  Finaliserte ", length(finalized$datasets), " datasett")
      return(finalized)
    }
  }

  NULL
}

# ── Bygg verifisert spesifikasjon ─────────────────────────────────────────────
build_verified_spec <- function(agent_result) {
  n        <- length(agent_result$datasets)
  df_names <- if (n == 1L) "df" else paste0("df", seq_len(n))

  sections <- mapply(function(d, df_name) {
    param_names        <- as.character(unlist(d$param_names))
    frequency          <- if (!is.null(d$frequency)) as.character(d$frequency) else "unknown"
    has_measure        <- !is.null(d$measure_column) && nchar(as.character(d$measure_column)) > 0L
    municipality_param <- as.character(d$municipality_param)
    municipality_value <- as.character(d$municipality_value)
    ssb_url            <- if (!is.null(d$ssb_url)) as.character(d$ssb_url) else
                            paste0("https://www.ssb.no/statbank/table/", d$table_id)

    param_lines <- vapply(param_names, function(p) {
      if (p == municipality_param)
        paste0('    ', p, ' = list(filter = "item", values = "', municipality_value, '")')
      else
        paste0("    ", p, " = TRUE")
    }, character(1L))

    measure_line <- if (has_measure)
      paste0('  measure_col  <- "', as.character(d$measure_column), '"\n') else ""

    fetch_chunk <- paste0(
      "```r\n",
      df_name, " <- NULL\n",
      "tryCatch({\n",
      "  raw <- ApiData(\n",
      '    "https://data.ssb.no/api/v0/no/table/', d$table_id, '",\n',
      paste(param_lines, collapse = ",\n"), ",\n",
      '    Tid = list(filter = "top", values = 40)\n',
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
      '}, error = function(e) message("Henting feilet: ", e$message))\n',
      "```"
    )

    ex_str <- paste0(
      "Kilde: ", ssb_url, "\n",
      if (!is.null(d$series_examples) && length(d$series_examples) > 0L)
        paste0('series_col ("', as.character(d$series_column), '"): [',
               paste(as.character(unlist(d$series_examples)), collapse = ", "), "]\n")
      else "",
      if (has_measure && !is.null(d$measure_examples) && length(d$measure_examples) > 0L)
        paste0('measure_col ("', as.character(d$measure_column), '"): [',
               paste(as.character(unlist(d$measure_examples)), collapse = ", "), "]\n")
      else ""
    )

    paste0(
      "### ", df_name, " — tabell ", d$table_id,
      " — ", if (!is.null(d$description)) as.character(d$description) else "",
      "  (frekvens: ", frequency, ", Asker-filter: ",
      municipality_param, "='", municipality_value, "')\n\n",
      "Forhåndsskrevet chunk — inkluder ORDRETT:\n", fetch_chunk, "\n\n", ex_str
    )
  }, agent_result$datasets, df_names, SIMPLIFY = FALSE)

  any_measure <- any(vapply(agent_result$datasets, function(d)
    !is.null(d$measure_column) && nchar(as.character(d$measure_column)) > 0L, logical(1L)))

  paste0(
    "## VERIFISERTE DATASETT\n\n",
    "Regler:\n",
    "1. Inkluder hentechunkene ORDRETT\n",
    "2. Data-frame variabler: ", paste(df_names, collapse = ", "), "\n",
    "3. Kolonnevariabler: time_col, value_col, series_col",
    if (any_measure) ", measure_col" else "", "\n",
    "4. Data er filtrert til Asker (3024) på API-nivå\n\n",
    paste(unlist(sections), collapse = "\n\n"),
    "\n\nForeslått tittel: ", as.character(agent_result$post_title),
    "\nOverordnet vinkel: ", as.character(agent_result$post_angle)
  )
}

# ── System-prompt for postgenerering ──────────────────────────────────────────
SYSTEM_PROMPT <- 'Du er en datajournalist og R-programmerer. Skriv én komplett Quarto-bloggpost
på norsk bokmål om Asker kommune, basert på SSB- og KOSTRA-data.

## Poststruktur (fast rekkefølge)
1. **Ingress** (2-3 setninger) — kroken: hva forteller data om Asker?
2. **Om datagrunnlaget** — ett avsnitt om SSB kommunestatistikk og KOSTRA
   (hva KOSTRA er, hva det dekker, hvorfor det er nyttig for kommuneanalyse)
3. **Seksjon per datasett** (3-5 seksjoner, én per tema):
   - Norsk seksjonsoverskrift
   - 1-2 avsnitt med kontekst og funn
   - Én ggplot2-figur (se krav nedenfor)
   - SSB-kildekoblinger (bruk ssb_url fra spec)
4. **Nøkkelfunn** — 4-6 punktpunkter med konkrete tall fra analysen
5. **Avslutning** — bredere refleksjon: hva sier dataene om Asker som kommune?

## Tone og stil
- Journalistisk, nysgjerrig, tilgjengelig
- Skriv som om leseren er en Asker-innbygger som vil forstå kommunens utvikling
- Ingen emojier. Profesjonell og ryddig.
- ALT på norsk bokmål — titler, akser, etiketter, kildeangivelser

## Visualiseringskrav
- Én figur per seksjon (3-5 figurer totalt)
- Roter stiler: lollipop, slope, area med annotasjoner, small multiples, heatmap,
  dumbbell, beeswarm, waterfall og klassiske linje/søyle/punkt-kombinasjoner
- Fargepakker: MetBrewer, nord, wesanderson eller viridis — én palett per post
- Tittel på norsk, undertittel med innsikt, kildeangivelse "Kilde: SSB"
- Bruk theme_minimal() eller theme_void() med norsk akseformattering
- ALLTID: p <- ggplot(...) + ...; print(p)

## Datahenting (KRITISK)
Spec inneholder FORHÅNDSSKREVNE hentechunker. Kopier dem ORDRETT inn i {r setup}-blokken.
IKKE skriv grepl-basert kolonnedeteksjon. IKKE omskriv hentechunkene.

## R-kodestruktur (KRITISK)
CHUNK 1 — etikett: setup — ÉN blokk med ALT:
  - knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE, error=TRUE)
  - ALLE library()-kall
  - ALLE hentechunker fra spec, ordrett

CHUNK 2 — etikett: wrangle — datatransformasjon:
  NULL-initialiser ALLE avledede variabler ØVERST i chunken FØR betinget logikk:
    df1_trend <- NULL
    df1_top   <- NULL
    if (!is.null(df1)) {
      df1_trend <- df1 |> ...
      df1_top   <- df1_trend |> ...
    }

CHUNK 3+ — plot-chunks (én per figur):
  Trippelsjekk-vakt ALLTID:
    if (exists("df1_trend") && !is.null(df1_trend) && nrow(df1_trend) > 0) {
      p <- ggplot(df1_trend, ...) + ...
      print(p)
    }
  Chunk-alternativer:
    #| fig-height: 5
    #| fig-width: 9
    #| fig-show: asis
    #| dev: "png"

## Filtreringsregler
- series_col = HVA som måles
- measure_col = HVORDAN verdien uttrykkes (kun hvis listet)
- Filtrer: df_sub <- df |> filter(.data[[series_col]] == "eksakt verdi")
- Alltid nrow-sjekk etter filter:
    if (nrow(df_sub) == 0) { message("Tomt filter"); df_sub <- NULL }

## YAML (norsk, freeze: true)
---
title: "NORSK TITTEL"
description: "ÉN SETNING PÅ NORSK"
date: "DATO"
categories: [SSB, KOSTRA, Asker]
lang: nb
freeze: true
---

## Utdataformat (PÅKREVD)
Bare rå .qmd som starter med ---
Ingen innledende tekst, ingen forklaring.'

# ── Generer post ──────────────────────────────────────────────────────────────
generate_post <- function(agent_result) {
  verified_spec <- build_verified_spec(agent_result)

  user_prompt <- paste0(
    "Dato: ", format(TODAY, "%d. %B %Y"), "\n\n",
    verified_spec, "\n\n",
    "Skriv den komplette Quarto-bloggposten om Asker kommune. ",
    "Følg poststrukturen nøye: ingress → om datagrunnlaget (SSB + KOSTRA) → ",
    "3-5 analyseseksjoner med figurer → nøkkelfunn → avslutning.\n\n",
    "Chunk-rekkefølge:\n",
    "1. {r setup} — ALL infrastruktur + ALLE hentechunker ordrett\n",
    "2. {r wrangle} — datatransformasjon med NULL-initialisering\n",
    "3. {r plot-navn} × antall figurer — én figur per chunk med trippelsjekk\n\n",
    "Tekst mellom chunks: norsk prosa som forklarer funn og kontekst."
  )

  message("Genererer post med Sonnet...")
  response <- with_retry(function() {
    request("https://api.anthropic.com/v1/messages") |>
      req_headers("x-api-key" = ANTHROPIC_API_KEY,
                  "anthropic-version" = "2023-06-01",
                  "content-type" = "application/json") |>
      req_body_json(list(
        model      = "claude-sonnet-4-6",
        max_tokens = 16000L,
        system     = SYSTEM_PROMPT,
        messages   = list(list(role = "user", content = user_prompt))
      )) |>
      req_timeout(300) |>
      req_perform()
  }, max_attempts = 3L, base_wait = 5)

  result      <- resp_body_json(response)
  text_blocks <- Filter(function(b) identical(b$type, "text"), result$content)
  if (length(text_blocks) == 0L) stop("Ingen tekst i API-respons.")
  raw_text <- paste(vapply(text_blocks, function(b) b$text, character(1L)), collapse = "")

  # Ekstraher QMD
  m <- regexpr("(?m)^---[ \t]*$", raw_text, perl = TRUE)
  if (m[[1]] < 0L) stop("Ingen YAML front matter i respons")
  qmd_raw <- substring(raw_text, m[[1]])

  # Påtving dato, freeze: true og lang: nb
  local({
    fm_m <- regexpr("(?s)^---[ \t]*\n.*?\n---", qmd_raw, perl = TRUE)
    if (fm_m[[1]] > 0L && attr(fm_m, "match.length") > 0L) {
      fm_len <- attr(fm_m, "match.length")
      fm     <- substring(qmd_raw, 1L, fm_len)
      rest   <- substring(qmd_raw, fm_len + 1L)
      fm <- gsub('date:\\s*"[^"]*"', paste0('date: "', format(TODAY, "%Y-%m-%d"), '"'), fm)
      if (!grepl("freeze:", fm)) fm <- sub("(\n---\\s*$)", "\nfreeze: true\\1", fm)
      if (!grepl("lang:",   fm)) fm <- sub("(\n---\\s*$)", "\nlang: nb\\1",     fm)
      qmd_raw <<- paste0(fm, rest)
    }
  })
  qmd_raw <- gsub('\nimage:\\s*["\']thumbnail\\.png["\']', "", qmd_raw)

  qmd_raw
}

# ── Revisor ───────────────────────────────────────────────────────────────────
REVIEWER_PROMPT <- 'Koderevisor for Quarto-poster med SSB-data. Fiks KUN disse feilene:

F1 — Hardkodede SSB-koder i ApiData(): erstatt med TRUE (unntatt municipality-param og Tid)
F2 — Manglende null-vakt på plot: wrap i if (!is.null(df) && nrow(df) > 0) { ... }
F3 — Manglende print(): legg til p <- ggplot(...); print(p)
F4 — Variabel ikke NULL-initialisert øverst i wrangle-chunk: legg til X <- NULL
F5 — Filter uten nrow-sjekk: legg til if (nrow(df_sub) == 0) { ... df_sub <- NULL }
F6 — Manglende exists() i plot-vakt: oppgrader til trippelsjekk

Linje 1: ISSUES: <fikste feil eller "none">
Linje 2+: Korrigert .qmd fra ---'

review_qmd <- function(qmd_content, valid_ids) {
  message("Revisjonsgjennomgang...")
  resp <- tryCatch(
    with_retry(function() {
      request("https://api.anthropic.com/v1/messages") |>
        req_headers("x-api-key" = ANTHROPIC_API_KEY,
                    "anthropic-version" = "2023-06-01",
                    "content-type" = "application/json") |>
        req_body_json(list(
          model    = "claude-haiku-4-5-20251001",
          max_tokens = 16000L,
          system   = REVIEWER_PROMPT,
          messages = list(list(role = "user", content = paste0(
            "Gyldige tabell-IDer: ", paste(valid_ids, collapse = ", "), "\n\n",
            qmd_content
          )))
        )) |>
        req_timeout(300) |>
        req_perform()
    }, max_attempts = 2L, base_wait = 5),
    error = function(e) { message("  Revisjon feilet: ", e$message); NULL }
  )

  if (is.null(resp)) return(qmd_content)

  raw <- tryCatch({
    r   <- resp_body_json(resp)
    blk <- Filter(function(b) identical(b$type, "text"), r$content)
    paste(vapply(blk, function(b) b$text, character(1L)), collapse = "")
  }, error = function(e) NULL)

  if (is.null(raw)) return(qmd_content)

  issues_m <- regmatches(raw, regexpr("(?i)^ISSUES:[ \t]*[^\n]*", raw, perl = TRUE))
  if (length(issues_m) > 0L)
    message("  Funn: ", trimws(sub("(?i)^ISSUES:[ \t]*", "", issues_m, perl = TRUE)))

  m <- regexpr("(?m)^---[ \t]*$", raw, perl = TRUE)
  if (m[[1]] < 0L) return(qmd_content)
  substring(raw, m[[1]])
}

# ══════════════════════════════════════════════════════════════════════════════
# KJØR
# ══════════════════════════════════════════════════════════════════════════════
message("Phase 1: Discovery-agent finner Asker-datasett...")
agent_result <- run_discovery_agent()

if (is.null(agent_result)) {
  stop("Discovery-agent feilet — ingen datasett funnet.")
}

valid_ids <- vapply(agent_result$datasets, function(d) as.character(d$table_id), character(1L))
message("Verifiserte tabeller: ", paste(valid_ids, collapse = ", "))

message("\nPhase 2: Genererer bloggpost...")
qmd_raw <- generate_post(agent_result)

message("\nPhase 3: Revisjonsgjennomgang...")
qmd_raw <- review_qmd(qmd_raw, valid_ids)

message("\nSkriver post til: ", POST_FILE)
dir.create(POST_DIR, recursive = TRUE, showWarnings = FALSE)
writeLines(qmd_raw, POST_FILE)

message("\nFerdig!")
message("Sjekk posten: quarto render \"", POST_FILE, "\"")
