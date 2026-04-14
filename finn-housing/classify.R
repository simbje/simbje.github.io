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
DB_PATH    <- file.path("finn-housing", "data", "finn_housing.db")
CSV_PATH   <- file.path("finn-housing", "data", "listings_export.csv")
BATCH_SIZE <- 999999  # classify everything unclassified

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

if (nrow(unclassified) == 0) {
  message("Nothing to classify. Exporting CSV and exiting.")
} else {

  # ── Classify each listing ─────────────────────────────────────────────────
  for (i in seq_len(nrow(unclassified))) {
    row <- unclassified[i, ]
    message("  [", i, "/", nrow(unclassified), "] Classifying finn_id=", row$finn_id)

    # Build compact prompt
    desc_short <- if (!is.na(row$description) && nchar(row$description) > 300) {
      paste0(substr(row$description, 1, 297), "...")
    } else {
      row$description
    }

    price_fmt <- if (!is.na(row$price)) {
      paste0(formatC(row$price, format = "d", big.mark = " "), " NOK")
    } else "ukjent pris"

    size_fmt <- if (!is.na(row$size_sqm)) paste0(row$size_sqm, " m²") else "ukjent størrelse"
    rooms_fmt <- if (!is.na(row$rooms)) paste0(row$rooms, " rom") else "ukjent antall rom"

    user_prompt <- paste0(
      "Classify this Oslo housing listing:\n\n",
      "Title: ", if (!is.na(row$title)) row$title else "N/A", "\n",
      "Price: ", price_fmt, "\n",
      "Size: ", size_fmt, "\n",
      "Rooms: ", rooms_fmt, "\n",
      "Address: ", if (!is.na(row$address)) row$address else "N/A", "\n",
      "Neighborhood: ", if (!is.na(row$neighborhood)) row$neighborhood else "N/A", "\n",
      "Type: ", if (!is.na(row$property_type)) row$property_type else "N/A", "\n",
      "Year built: ", if (!is.na(row$year_built)) row$year_built else "N/A", "\n",
      "Description: ", if (!is.na(desc_short)) desc_short else "N/A"
    )

    # Call Claude API
    result <- tryCatch({
      response <- with_retry(function() {
        request("https://api.anthropic.com/v1/messages") |>
          req_headers(
            "x-api-key"         = ANTHROPIC_API_KEY,
            "anthropic-version" = "2023-06-01",
            "content-type"      = "application/json"
          ) |>
          req_body_json(list(
            model      = "claude-sonnet-4-6",
            max_tokens = 256,
            system     = trimws(SYSTEM_PROMPT),
            messages   = list(list(role = "user", content = user_prompt))
          )) |>
          req_timeout(60) |>
          req_perform()
      }, max_attempts = 3L, base_wait = 3)

      raw_json <- resp_body_json(response)
      text_blocks <- Filter(function(b) identical(b$type, "text"), raw_json$content)
      raw_text <- paste(vapply(text_blocks, function(b) b$text, character(1L)), collapse = "")

      # Strip any accidental markdown fences
      raw_text <- str_remove_all(raw_text, "```json|```")
      fromJSON(trimws(raw_text))

    }, error = function(e) {
      message("    Classification failed: ", e$message)
      NULL
    })

    if (is.null(result)) next

    # Validate category name against our list
    valid_cats <- c("Luxury", "Family home", "Starter / budget", "Investment",
                    "Student / young professional", "New development",
                    "Renovation", "Central urban", "Suburban",
                    "Waterfront / premium location")

    cat_val  <- result$category
    conf_val <- result$confidence
    reas_val <- result$reasoning

    if (!cat_val %in% valid_cats) {
      # Fuzzy match to nearest valid category
      scores <- vapply(valid_cats, function(v)
        adist(tolower(cat_val), tolower(v)), numeric(1))
      cat_val <- valid_cats[which.min(scores)]
      message("    Fuzzy-matched '", result$category, "' → '", cat_val, "'")
    }

    classified_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

    dbExecute(con, "
      UPDATE listings
      SET category = ?, category_confidence = ?, category_reasoning = ?, classified_at = ?
      WHERE finn_id = ?",
      params = list(cat_val, conf_val, reas_val, classified_at, row$finn_id)
    )

    Sys.sleep(0.3)  # small pause between API calls
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
  for (i in seq_len(nrow(needs_standard))) {
    row <- needs_standard[i, ]
    message("  [", i, "/", nrow(needs_standard), "] Standard for finn_id=", row$finn_id)

    price_sqm_val <- if (!is.na(row$price) && !is.na(row$size_sqm) && row$size_sqm > 0)
      round(row$price / row$size_sqm) else NA_integer_

    # Use more of the description for standard — condition is often in the detail
    desc_std <- if (!is.na(row$description) && nchar(row$description) > 600) {
      paste0(substr(row$description, 1, 597), "...")
    } else {
      row$description
    }

    std_prompt <- paste0(
      "Assess the interior standard of this Oslo housing listing:\n\n",
      "Title: ",       if (!is.na(row$title))      row$title      else "N/A", "\n",
      "Year built: ",  if (!is.na(row$year_built))  row$year_built  else "N/A", "\n",
      "Type: ",        if (!is.na(row$property_type)) row$property_type else "N/A", "\n",
      "Price/m²: ",    if (!is.na(price_sqm_val))  paste0(format(price_sqm_val, big.mark = " "), " NOK/m²") else "N/A", "\n",
      "Description: ", if (!is.na(desc_std))        desc_std        else "N/A"
    )

    result <- tryCatch({
      response <- with_retry(function() {
        request("https://api.anthropic.com/v1/messages") |>
          req_headers(
            "x-api-key"         = ANTHROPIC_API_KEY,
            "anthropic-version" = "2023-06-01",
            "content-type"      = "application/json"
          ) |>
          req_body_json(list(
            model      = "claude-sonnet-4-6",
            max_tokens = 256,
            system     = trimws(STANDARD_PROMPT),
            messages   = list(list(role = "user", content = std_prompt))
          )) |>
          req_timeout(60) |>
          req_perform()
      }, max_attempts = 3L, base_wait = 3)

      raw_json  <- resp_body_json(response)
      txt_blocks <- Filter(function(b) identical(b$type, "text"), raw_json$content)
      raw_text  <- paste(vapply(txt_blocks, function(b) b$text, character(1L)), collapse = "")
      raw_text  <- str_remove_all(raw_text, "```json|```")
      fromJSON(trimws(raw_text))

    }, error = function(e) {
      message("    Standard classification failed: ", e$message)
      NULL
    })

    if (is.null(result)) next

    std_val  <- result$standard
    conf_val <- result$confidence
    reas_val <- result$reasoning

    if (!std_val %in% VALID_STANDARDS) {
      scores  <- vapply(VALID_STANDARDS, function(v)
        adist(tolower(std_val), tolower(v)), numeric(1))
      std_val <- VALID_STANDARDS[which.min(scores)]
      message("    Fuzzy-matched '", result$standard, "' → '", std_val, "'")
    }

    dbExecute(con, "
      UPDATE listings
      SET standard = ?, standard_confidence = ?, standard_reasoning = ?
      WHERE finn_id = ?",
      params = list(std_val, conf_val, reas_val, row$finn_id)
    )

    Sys.sleep(0.3)
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
