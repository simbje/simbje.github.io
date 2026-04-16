#!/usr/bin/env Rscript
# =============================================================================
# classify.R
# Reads unclassified listings from SQLite, calls the Claude API to assign
# one of 10 housing categories, updates the DB, then exports a CSV snapshot
# for the Quarto dashboard to render.
# Run by GitHub Actions after scraper.R, or manually:
#   Rscript finn-housing/classify.R
# =============================================================================

library(httr2)
library(jsonlite)
library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)

# ── Config ────────────────────────────────────────────────────────────────────
# Resolve paths relative to THIS script's location so the script works
# regardless of working directory (RStudio, terminal, GitHub Actions).
.script_dir <- tryCatch({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[grep("--file=", args)]
  if (length(file_arg) > 0)
    dirname(normalizePath(sub("--file=", "", file_arg[1])))
  else
    normalizePath(getwd())  # interactive / sourced fallback
}, error = function(e) normalizePath(getwd()))

DB_PATH    <- file.path(.script_dir, "data", "finn_housing.db")
CSV_PATH   <- file.path(.script_dir, "data", "listings_export.csv")
BATCH_SIZE <- 999999  # classify everything unclassified
API_BATCH  <- 10L    # listings per Claude API call (batching = fewer, faster calls)
MODEL      <- "claude-haiku-4-5-20251001"  # cheap + fast; good enough for classification

ANTHROPIC_API_KEY <- Sys.getenv("ANTHROPIC_API_KEY")
if (nchar(ANTHROPIC_API_KEY) == 0) stop("ANTHROPIC_API_KEY not set")

# ── Retry helper (same pattern as ssb-daily/generate_post.R) ─────────────────
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

# ── Batch Claude helper ───────────────────────────────────────────────────────
# Sends up to API_BATCH listing prompts in one API call and returns a list of
# parsed result objects (one per prompt, same order).
# Returns NULL on failure — caller falls back gracefully.
call_claude_batch <- function(system_prompt, user_prompts, max_tokens_each = 160L) {
  n <- length(user_prompts)

  numbered <- paste(
    vapply(seq_len(n), function(i)
      paste0("### Listing ", i, "\n", user_prompts[[i]]),
      character(1L)),
    collapse = "\n\n"
  )

  batch_header <- paste0(
    "Classify each of the following ", n, " listings.\n",
    "Return ONLY a valid JSON array with exactly ", n, " objects ",
    "in the same order as the input. Each object must use the same keys ",
    "described in your instructions.\n\n"
  )

  response <- with_retry(function() {
    request("https://api.anthropic.com/v1/messages") |>
      req_headers(
        "x-api-key"         = ANTHROPIC_API_KEY,
        "anthropic-version" = "2023-06-01",
        "content-type"      = "application/json"
      ) |>
      req_body_json(list(
        model      = MODEL,
        max_tokens = max_tokens_each * n + 100L,
        system     = trimws(system_prompt),
        messages   = list(list(role = "user",
                               content = paste0(batch_header, numbered)))
      )) |>
      req_timeout(120) |>
      req_perform()
  }, max_attempts = 3L, base_wait = 3)

  raw_json   <- resp_body_json(response)
  txt_blocks <- Filter(function(b) identical(b$type, "text"), raw_json$content)
  raw_text   <- paste(vapply(txt_blocks, function(b) b$text, character(1L)), collapse = "")
  raw_text   <- str_remove_all(raw_text, "```json|```")
  parsed     <- fromJSON(trimws(raw_text), simplifyDataFrame = FALSE)

  # fromJSON may return a data.frame when all objects share keys — normalise to list
  if (is.data.frame(parsed))
    lapply(seq_len(nrow(parsed)), function(i) as.list(parsed[i, ]))
  else
    parsed
}


# ── Classification system prompt ──────────────────────────────────────────────
SYSTEM_PROMPT <- '
You are a Norwegian real estate expert classifying Oslo housing listings into exactly one of these 10 categories:

1. Luxury           — High-end property, premium finishes, price > 8M NOK, or prestigious address (Frogner, Aker Brygge, Tjuvholmen, Oslofjord).
2. Family home      — 3+ rooms, suitable for families, often suburban with outdoor space or garage.
3. Starter / budget — Small or affordable first-time buyer property, price < 3.5M NOK, 1–2 rooms.
4. Investment       — Strong rental yield potential, near university campuses, student areas (Blindern, Grünerløkka, Sagene, Torshov), or multi-unit setup.
5. Student / young professional — Small (1–2 rooms), central location, good public transport, price 2–5M NOK.
6. New development  — Built 2018 or later, or "nybygg", "prosjekt", "nøkkelferdig" in title/description.
7. Renovation       — Explicitly needs work: "oppussingsobjekt", "selges som den er", "as-is", low price per sqm (< 40 000 NOK/m²).
8. Central urban    — Mid-range city apartment in central Oslo districts (St. Hanshaugen, Grünerløkka, Majorstuen, Bislett, Sentrum), not luxury.
9. Suburban         — Located in outer Oslo districts (Nordstrand, Østensjø, Røa, Holmlia, Bøler, Groruddalen, Ullern, Vestre Aker).
10. Waterfront / premium location — On or near water (fjord, river, lake), scenic view, or premium natural setting.

Oslo price context:
- Budget: under 3.5M NOK
- Mid-range: 3.5M–7M NOK
- Premium: 7M–10M NOK
- Luxury: over 10M NOK
- Typical price/m²: central ~85 000 NOK, suburban ~55 000 NOK

Rules:
- Pick exactly ONE category that best fits the listing.
- If multiple categories apply, pick the most distinctive one.
- "New development" and "Renovation" take precedence over location categories when evidence is clear.
- "Luxury" and "Waterfront" take precedence over "Central urban" or "Suburban".

Respond with ONLY valid JSON (no markdown, no explanation outside JSON):
{"category": "<category name>", "confidence": "high|medium|low", "reasoning": "<1–2 sentence explanation>"}
'

# ── Connect to DB ─────────────────────────────────────────────────────────────
if (!file.exists(DB_PATH)) stop("Database not found: ", DB_PATH, ". Run scraper.R first.")
con <- dbConnect(SQLite(), DB_PATH)

unclassified <- dbGetQuery(con, paste0(
  "SELECT finn_id, title, price, size_sqm, rooms, address, neighborhood,
          property_type, year_built, description
   FROM listings
   WHERE category IS NULL
   LIMIT ", BATCH_SIZE
))

message("Unclassified listings to process: ", nrow(unclassified))

valid_cats <- c("Luxury", "Family home", "Starter / budget", "Investment",
                "Student / young professional", "New development",
                "Renovation", "Central urban", "Suburban",
                "Waterfront / premium location")

if (nrow(unclassified) == 0) {
  message("Nothing to classify.")
} else {
  for (batch_start in seq(1, nrow(unclassified), by = API_BATCH)) {
    idx   <- batch_start:min(batch_start + API_BATCH - 1L, nrow(unclassified))
    batch <- unclassified[idx, ]
    message("  Category batch [", batch_start, "–", max(idx),
            " / ", nrow(unclassified), "]")

    prompts <- lapply(seq_len(nrow(batch)), function(i) {
      row       <- batch[i, ]
      desc_short <- if (!is.na(row$description) && nchar(row$description) > 300)
        paste0(substr(row$description, 1, 297), "...") else row$description
      price_fmt  <- if (!is.na(row$price))
        paste0(formatC(row$price, format = "d", big.mark = " "), " NOK") else "ukjent pris"
      paste0(
        "Title: ",        if (!is.na(row$title))         row$title         else "N/A", "\n",
        "Price: ",        price_fmt, "\n",
        "Size: ",         if (!is.na(row$size_sqm))  paste0(row$size_sqm, " m²")  else "N/A", "\n",
        "Rooms: ",        if (!is.na(row$rooms))      paste0(row$rooms, " rom")    else "N/A", "\n",
        "Address: ",      if (!is.na(row$address))       row$address       else "N/A", "\n",
        "Neighborhood: ", if (!is.na(row$neighborhood))  row$neighborhood  else "N/A", "\n",
        "Type: ",         if (!is.na(row$property_type)) row$property_type else "N/A", "\n",
        "Year built: ",   if (!is.na(row$year_built))    row$year_built    else "N/A", "\n",
        "Description: ",  if (!is.na(desc_short))        desc_short        else "N/A"
      )
    })

    results <- tryCatch(
      call_claude_batch(SYSTEM_PROMPT, prompts),
      error = function(e) { message("    Batch failed: ", e$message); NULL }
    )
    if (is.null(results)) { Sys.sleep(2); next }

    classified_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

    for (j in seq_len(nrow(batch))) {
      item <- results[[j]]
      if (is.null(item)) next

      cat_val  <- item$category
      conf_val <- item$confidence
      reas_val <- item$reasoning
      if (is.null(cat_val) || is.na(cat_val)) next

      if (!cat_val %in% valid_cats) {
        scores  <- vapply(valid_cats, function(v)
          adist(tolower(cat_val), tolower(v)), numeric(1))
        cat_val <- valid_cats[which.min(scores)]
        message("    Fuzzy-matched '", item$category, "' → '", cat_val, "'")
      }

      dbExecute(con, "
        UPDATE listings
        SET category = ?, category_confidence = ?, category_reasoning = ?, classified_at = ?
        WHERE finn_id = ?",
        params = list(cat_val, conf_val, reas_val, classified_at, batch$finn_id[j])
      )
    }
    Sys.sleep(0.5)
  }
}

# ── Standard classification ───────────────────────────────────────────────────
# Separate pass: classify the interior standard / condition of each listing.
# Uses description (Om boligen), year_built, price/m², and title as signals.

STANDARD_PROMPT <- '
You are a Norwegian real estate expert assessing the interior standard and physical condition of Oslo housing listings into exactly one of these 5 categories:

1. Luxury finish       — Premium materials throughout: marble or stone surfaces, designer kitchen brands (Gaggenau, Miele, Bulthaup, Kvik high-end), underfloor heating in all rooms, bespoke joinery, high-specification bathrooms. Often explicitly described as "eksklusiv", "luksus", or very high price/m².
2. High standard       — Fully renovated within approximately the last 10 years: new kitchen with quality appliances, updated bathroom(s), modern flooring throughout. Described as "nyrenovert", "totalrenovert", "gjennomgående oppgradert", or similar.
3. Good standard       — Well-maintained and functional. Kitchen and bathroom in reasonable modern condition. Normal wear for the age of the building. No immediate renovation needed.
4. Average / dated     — Functional but clearly dated finishes. Older kitchen or bathroom, worn flooring, or cosmetic issues. Liveable as-is but would benefit from updating. Often older buildings with no mention of renovation.
5. Renovation project  — Requires significant work before comfortable habitation. Keywords: "oppussingsobjekt", "selges som den er", "as-is", "stor oppgraderingspotensial", very low price/m² (< 40 000 NOK/m²) for the Oslo area, or description explicitly states the property needs renovation.

Rules:
- Base the classification primarily on the description text.
- Year built is a secondary signal: built after 2015 suggests modern standard; built before 1975 with no mention of renovation suggests Average / dated or Renovation project.
- Explicit renovation keywords in title or description override age-based assumptions.
- "Renovation project" requires clear evidence — do not assign it based on age alone.
- If the description is very short or absent, use title and year_built to make your best assessment.

Respond with ONLY valid JSON (no markdown, no explanation outside JSON):
{"standard": "<category name>", "confidence": "high|medium|low", "reasoning": "<1-2 sentence explanation>"}
'

VALID_STANDARDS <- c("Luxury finish", "High standard", "Good standard",
                     "Average / dated", "Renovation project")

needs_standard <- dbGetQuery(con, paste0(
  "SELECT finn_id, title, price, size_sqm, year_built, property_type, description
   FROM listings
   WHERE standard IS NULL
     AND (description IS NOT NULL OR title IS NOT NULL)
   LIMIT ", BATCH_SIZE
))

message("\nListings needing standard classification: ", nrow(needs_standard))

if (nrow(needs_standard) > 0) {
  for (batch_start in seq(1, nrow(needs_standard), by = API_BATCH)) {
    idx   <- batch_start:min(batch_start + API_BATCH - 1L, nrow(needs_standard))
    batch <- needs_standard[idx, ]
    message("  Standard batch [", batch_start, "–", max(idx),
            " / ", nrow(needs_standard), "]")

    prompts <- lapply(seq_len(nrow(batch)), function(i) {
      row           <- batch[i, ]
      price_sqm_val <- if (!is.na(row$price) && !is.na(row$size_sqm) && row$size_sqm > 0)
        round(row$price / row$size_sqm) else NA_integer_
      # Use more of the description for standard — condition is often in the detail
      desc_std <- if (!is.na(row$description) && nchar(row$description) > 600)
        paste0(substr(row$description, 1, 597), "...") else row$description
      paste0(
        "Assess the interior standard of this Oslo housing listing:\n\n",
        "Title: ",       if (!is.na(row$title))         row$title         else "N/A", "\n",
        "Year built: ",  if (!is.na(row$year_built))     row$year_built     else "N/A", "\n",
        "Type: ",        if (!is.na(row$property_type))  row$property_type  else "N/A", "\n",
        "Price/m²: ",    if (!is.na(price_sqm_val))
                           paste0(format(price_sqm_val, big.mark = " "), " NOK/m²") else "N/A", "\n",
        "Description: ", if (!is.na(desc_std))            desc_std           else "N/A"
      )
    })

    results <- tryCatch(
      call_claude_batch(STANDARD_PROMPT, prompts, max_tokens_each = 160L),
      error = function(e) { message("    Batch failed: ", e$message); NULL }
    )
    if (is.null(results)) { Sys.sleep(2); next }

    standard_classified_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

    for (j in seq_len(nrow(batch))) {
      item <- results[[j]]
      if (is.null(item)) next

      std_val  <- item$standard
      conf_val <- item$confidence
      reas_val <- item$reasoning
      if (is.null(std_val) || is.na(std_val)) next

      if (!std_val %in% VALID_STANDARDS) {
        scores  <- vapply(VALID_STANDARDS, function(v)
          adist(tolower(std_val), tolower(v)), numeric(1))
        std_val <- VALID_STANDARDS[which.min(scores)]
        message("    Fuzzy-matched '", item$standard, "' → '", std_val, "'")
      }

      dbExecute(con, "
        UPDATE listings
        SET standard = ?, standard_confidence = ?, standard_reasoning = ?,
            standard_classified_at = ?
        WHERE finn_id = ?",
        params = list(std_val, conf_val, reas_val, standard_classified_at, batch$finn_id[j])
      )
    }
    Sys.sleep(0.5)
  }
}

# ── Geocode listings that lack coordinates ────────────────────────────────────
# Uses OpenStreetMap Nominatim (free, no API key). Max 1 req/sec per ToS.
GEOCODE_BATCH <- 30  # geocode at most this many per run

geocode_osm <- function(address) {
  if (is.na(address) || nchar(trimws(address)) == 0)
    return(c(NA_real_, NA_real_))
  query <- paste0(trimws(address), ", Oslo, Norway")
  resp <- tryCatch(
    request("https://nominatim.openstreetmap.org/search") |>
      req_url_query(q = query, format = "json", limit = "1", countrycodes = "no") |>
      req_headers("User-Agent" = "finn-housing-personal-quarto/1.0 (educational)") |>
      req_timeout(10) |>
      req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp)) return(c(NA_real_, NA_real_))
  data <- tryCatch(resp_body_json(resp), error = function(e) list())
  if (length(data) == 0) return(c(NA_real_, NA_real_))
  c(as.numeric(data[[1]]$lat), as.numeric(data[[1]]$lon))
}

ungeocoded <- dbGetQuery(con, paste0(
  "SELECT finn_id, address FROM listings WHERE lat IS NULL LIMIT ", GEOCODE_BATCH
))

if (nrow(ungeocoded) > 0) {
  message("\nGeocoding ", nrow(ungeocoded), " listings via Nominatim...")
  geocoded_n <- 0L
  for (i in seq_len(nrow(ungeocoded))) {
    row <- ungeocoded[i, ]
    coords <- geocode_osm(row$address)
    if (!is.na(coords[1])) {
      dbExecute(con,
        "UPDATE listings SET lat = ?, lon = ? WHERE finn_id = ?",
        params = list(coords[1], coords[2], row$finn_id)
      )
      geocoded_n <- geocoded_n + 1L
    }
    Sys.sleep(1.1)  # Nominatim rate limit: max 1 req/sec
  }
  message("  Geocoded ", geocoded_n, "/", nrow(ungeocoded), " listings.")
}

# ── Export CSV snapshot for Quarto rendering ──────────────────────────────────
message("\nExporting CSV snapshot to: ", CSV_PATH)

all_listings <- dbGetQuery(con, "
  SELECT finn_id, title, price, size_sqm, rooms, address, neighborhood,
         property_type, year_built, broker, url, scraped_at, lat, lon,
         category, category_confidence, category_reasoning, classified_at,
         standard, standard_confidence, standard_reasoning
  FROM listings
  ORDER BY scraped_at DESC
")

dir.create(dirname(CSV_PATH), recursive = TRUE, showWarnings = FALSE)
write.csv(all_listings, CSV_PATH, row.names = FALSE, fileEncoding = "UTF-8")

total       <- nrow(all_listings)
classified  <- sum(!is.na(all_listings$category))
message("CSV written: ", total, " total listings, ", classified, " classified.")

dbDisconnect(con)
