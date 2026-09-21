#!/usr/bin/env Rscript
# =============================================================================
# scraper.R
# Scrapes finn.no for-sale (bolig til salgs) listings in Oslo.
# New listings are inserted into a local SQLite database.
# Already-seen finn IDs are skipped (idempotent).
# Run by GitHub Actions daily, or manually: Rscript finn-housing/scraper.R
# =============================================================================

library(httr2)
library(rvest)
library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
library(lubridate)

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

DB_PATH   <- file.path(.script_dir, "data", "finn_housing.db")
MAX_PAGES <- 10      # max search-result pages to scrape per run
PAGE_SLEEP <- 1.5    # seconds between search-result page fetches
DETAIL_SLEEP <- 1.2  # seconds between individual listing fetches

# How many useless fetches in a row before we conclude finn.no is blocking us
# (or has changed its HTML) and stop scraping. Keeps a blocked run from burning
# 10+ CI minutes on requests that will all fail, while keeping what we have.
MAX_CONSECUTIVE_FAILURES <- 10L

BASE_SEARCH_URL <- paste0(
  "https://www.finn.no/realestate/homes/search.html",
  "?location=0.20061",   # Oslo / Akershus region
  "&sort=PUBLISHED_DESC"
)

UA <- paste0(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
  "AppleWebKit/537.36 (KHTML, like Gecko) ",
  "Chrome/124.0.0.0 Safari/537.36"
)

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
      message("  Forsøk ", attempt, " mislyktes: ", conditionMessage(last_error),
              " — prøver igjen om ", wait, "s...")
      Sys.sleep(wait)
    }
  }
  stop(last_error)
}

# ── Database setup ────────────────────────────────────────────────────────────
dir.create(dirname(DB_PATH), recursive = TRUE, showWarnings = FALSE)
con <- dbConnect(SQLite(), DB_PATH)

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS listings (
    finn_id             TEXT PRIMARY KEY,
    title               TEXT,
    price               INTEGER,
    size_sqm            REAL,
    rooms               INTEGER,
    address             TEXT,
    neighborhood        TEXT,
    property_type       TEXT,
    year_built          INTEGER,
    description         TEXT,
    url                 TEXT,
    scraped_at          TEXT,
    lat                 REAL,
    lon                 REAL,
    category            TEXT,
    category_confidence TEXT,
    category_reasoning  TEXT,
    classified_at           TEXT,
    standard_classified_at  TEXT
  );
")

# Migrate existing DBs: add new columns if they don't exist yet
tryCatch(dbExecute(con, "ALTER TABLE listings ADD COLUMN lat                     REAL"), error = function(e) NULL)
tryCatch(dbExecute(con, "ALTER TABLE listings ADD COLUMN lon                     REAL"), error = function(e) NULL)
tryCatch(dbExecute(con, "ALTER TABLE listings ADD COLUMN broker                  TEXT"), error = function(e) NULL)
tryCatch(dbExecute(con, "ALTER TABLE listings ADD COLUMN standard                TEXT"), error = function(e) NULL)
tryCatch(dbExecute(con, "ALTER TABLE listings ADD COLUMN standard_confidence     TEXT"), error = function(e) NULL)
tryCatch(dbExecute(con, "ALTER TABLE listings ADD COLUMN standard_reasoning      TEXT"), error = function(e) NULL)
tryCatch(dbExecute(con, "ALTER TABLE listings ADD COLUMN standard_classified_at  TEXT"), error = function(e) NULL)
tryCatch(dbExecute(con, "ALTER TABLE listings ADD COLUMN floor                  INTEGER"), error = function(e) NULL)
tryCatch(dbExecute(con, "ALTER TABLE listings ADD COLUMN has_balcony            INTEGER"), error = function(e) NULL)

existing_ids <- dbGetQuery(con, "SELECT finn_id FROM listings")$finn_id
message("Eksisterende annonser i DB: ", length(existing_ids))

# ── CSV snapshot export ───────────────────────────────────────────────────────
# Defined up front because this is also the crash-recovery path: whatever the
# DB already holds must reach the dashboard even if a scrape dies half-way.
# Category columns stay NA until classify.R runs — the dashboard handles that.
CSV_PATH <- file.path(.script_dir, "data", "listings_export.csv")

# Keep this list identical to classify.R's — it is the contract the dashboard
# reads. Columns absent from an older DB are padded with NA rather than failing
# the export, so a schema that predates a column can never cost us the run.
EXPORT_COLS <- c(
  "finn_id", "title", "price", "size_sqm", "rooms", "address", "neighborhood",
  "property_type", "year_built", "broker", "url", "scraped_at", "lat", "lon",
  "category", "category_confidence", "category_reasoning", "classified_at",
  "standard", "standard_confidence", "standard_reasoning", "standard_classified_at",
  "floor", "has_balcony"
)

export_snapshot <- function() {
  have <- dbListFields(con, "listings")
  sel  <- intersect(EXPORT_COLS, have)
  snap <- dbGetQuery(con, paste0(
    "SELECT ", paste(sel, collapse = ", "),
    " FROM listings ORDER BY scraped_at DESC"
  ))
  for (nm in setdiff(EXPORT_COLS, sel)) snap[[nm]] <- rep(NA, nrow(snap))
  snap <- snap[, EXPORT_COLS, drop = FALSE]

  write.csv(snap, CSV_PATH, row.names = FALSE, fileEncoding = "UTF-8")
  message("CSV-øyeblikksbilde skrevet: ", nrow(snap), " annonser → ", CSV_PATH)
  invisible(nrow(snap))
}

# ── Crash safety net ──────────────────────────────────────────────────────────
# finn.no breaks in a new way every few months. An abort here used to take the
# whole pipeline with it: the CSV was never written, and because the step failed,
# classify.R and the commit step never ran — so an entire scrape was discarded.
# Instead: keep what the DB has, flag the run as degraded, and exit 0 so the
# analysis downstream still runs on the data that did survive. The phases below
# are individually guarded too; this is the net for the bug we have not hit yet.
scrape_degraded <- FALSE

options(error = function() {
  cat("::warning::finn.no-skrapingen stoppet uventet - lagrer delvise resultater og fortsetter.\n")
  message("\n!! Uventet feil — eksporterer det databasen har og avslutter pent.")
  try(export_snapshot(), silent = TRUE)
  try(dbDisconnect(con), silent = TRUE)
  quit(save = "no", status = 0)
})

# ── HTML fetch helper ─────────────────────────────────────────────────────────
fetch_html <- function(url) {
  with_retry(function() {
    resp <- request(url) |>
      req_headers("User-Agent" = UA, "Accept-Language" = "nb-NO,nb;q=0.9") |>
      req_timeout(30) |>
      req_perform()
    read_html(resp_body_string(resp))
  }, max_attempts = 3L, base_wait = 2)
}

# ── Parse a number from a messy string (e.g. "5 990 000 kr" → 5990000) ───────
parse_number_no <- function(x) {
  if (is.na(x) || nchar(trimws(x)) == 0) return(NA_integer_)
  n <- as.integer(gsub("[^0-9]", "", x))
  if (length(n) == 0 || is.na(n)) NA_integer_ else n
}

# ── Extract finn_id from a finn.no listing URL ────────────────────────────────
extract_finn_id <- function(href) {
  m <- str_match(href, "finnkode=(\\d+)")
  if (!is.na(m[1, 2])) return(m[1, 2])
  # Fallback: last numeric segment in URL path
  m2 <- str_match(href, "/(\\d{6,})/?$")
  if (!is.na(m2[1, 2])) return(m2[1, 2])
  NA_character_
}

# ── Scrape one search-results page → data.frame of basic listing info ─────────
# NOTE: finn.no HTML structure can change. If selectors break, inspect the page
#       and update the css() calls below. Data attributes (data-*) are more
#       stable than class names.
scrape_search_page <- function(page_num) {
  url <- paste0(BASE_SEARCH_URL, "&page=", page_num)
  message("  Henter søkeside ", page_num, ": ", url)

  doc <- tryCatch(fetch_html(url), error = function(e) {
    message("  Klarte ikke hente side ", page_num, ": ", e$message)
    NULL
  })
  if (is.null(doc)) return(NULL)

  # ── Try to find listing cards ─────────────────────────────────────────────
  # Finn.no renders listing cards as <article> elements.
  # The link to the full listing is in an <a> tag inside each card.
  cards <- html_elements(doc, "article")

  if (length(cards) == 0) {
    # Fallback: any link pointing to a realestate ad
    warning("Ingen <article>-kort funnet på side ", page_num,
            " — finn.no HTML kan ha endret seg. Faller tilbake til ren lenke-uthenting.")
    links <- html_elements(doc, "a[href*='finnkode']")
    if (length(links) == 0) {
      message("  Ingen annonsekort funnet på side ", page_num, " — stopper paginering.")
      return(NULL)
    }
    hrefs <- html_attr(links, "href")
    ids   <- vapply(hrefs, extract_finn_id, character(1))
    ids   <- unique(ids[!is.na(ids)])
    return(data.frame(finn_id = ids, title = NA_character_, price = NA_integer_,
                      size_sqm = NA_real_, rooms = NA_integer_,
                      address = NA_character_, url = paste0("https://www.finn.no/realestate/homes/ad.html?finnkode=", ids),
                      stringsAsFactors = FALSE))
  }

  # ── Extract fields from each card ────────────────────────────────────────
  rows <- lapply(cards, function(card) {
    # Link & finn_id
    link_node <- html_element(card, "a[href*='finnkode'], a[href*='/realestate/']")
    href      <- if (!is.null(link_node)) html_attr(link_node, "href") else NA_character_
    if (is.na(href)) return(NULL)
    finn_id   <- extract_finn_id(href)
    if (is.na(finn_id)) return(NULL)
    full_url  <- if (startsWith(href, "http")) href else paste0("https://www.finn.no", href)

    # Title — usually the first heading in the card
    title_node <- html_element(card, "h2, h3, [class*='heading'], [class*='title']")
    title      <- if (!is.null(title_node)) trimws(html_text(title_node)) else NA_character_

    # Price — look for element with "kr" or price-related class/text
    price_node <- html_element(card, "[class*='price'], [class*='Price']")
    price_raw  <- if (!is.null(price_node)) html_text(price_node) else NA_character_
    price      <- parse_number_no(price_raw)

    # Size / rooms — look for small feature text blocks
    detail_nodes <- html_elements(card, "dl dt, dl dd, [class*='detail'], [class*='attribute']")
    detail_text  <- paste(trimws(html_text(detail_nodes)), collapse = " | ")

    size_sqm <- NA_real_
    rooms    <- NA_integer_

    m_size <- str_match(detail_text, "(\\d+[,.]?\\d*)\\s*m²")
    if (!is.na(m_size[1, 2])) size_sqm <- as.numeric(gsub(",", ".", m_size[1, 2]))

    m_rooms <- str_match(detail_text, "(\\d+)\\s*rom")
    if (!is.na(m_rooms[1, 2])) rooms <- as.integer(m_rooms[1, 2])

    # Address — text below the title, often in a <p> or <span>
    addr_node <- html_element(card, "p, [class*='address'], [class*='location']")
    address   <- if (!is.null(addr_node)) trimws(html_text(addr_node)) else NA_character_
    # Clean up: remove title duplication
    if (!is.na(title) && !is.na(address) && startsWith(address, title)) {
      address <- trimws(substring(address, nchar(title) + 1))
    }

    data.frame(finn_id = finn_id, title = title, price = price,
               size_sqm = size_sqm, rooms = rooms, address = address,
               url = full_url, stringsAsFactors = FALSE)
  })

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(NULL)
  bind_rows(rows)
}

# ── Broker domain → display name lookup ──────────────────────────────────────
# Keyed on the first subdomain segment of the broker's external link URL.
BROKER_DOMAINS <- c(
  "dnbeiendom"     = "DNB Eiendom",
  "privatmegleren" = "PrivatMegleren",
  "eie"            = "EIE Eiendomsmegling",
  "krogsveen"      = "Krogsveen",
  "aktiv"          = "Aktiv Eiendomsmegling",
  "nordvik"        = "Nordvik",
  "obos"           = "OBOS Eiendomsmeglere",
  "garanti"        = "Garanti Eiendomsmegling",
  "sem-johnsen"    = "Sem & Johnsen",
  "usbl"           = "USBL",
  "meglerhuset"    = "Meglerhuset",
  "foss"           = "Foss & Co",
  "home"           = "Home Eiendomsmegling",
  "remax"          = "RE/MAX",
  "buysellrent"    = "Buy Sell Rent",
  "ambita"         = "Ambita",
  "estate"         = "Estate Eiendomsmegling"
)

# ── The shape of a detail record ──────────────────────────────────────────────
# Every field scrape_listing_detail() can return must be listed here. A field
# missing from the returned list arrives at dbExecute() as NULL, and SQLite
# rejects that with "Parameter N does not have length 1" — which is exactly how
# a blocked fetch used to abort the whole run: the old failure path omitted
# floor and has_balcony, so the first insert after a block killed the script.
empty_detail <- function() {
  list(title = NA_character_, price = NA_integer_,
       size_sqm = NA_real_, rooms = NA_integer_,
       address = NA_character_, neighborhood = NA_character_,
       property_type = NA_character_, year_built = NA_integer_,
       description = NA_character_, broker = NA_character_,
       floor = NA_integer_, has_balcony = NA_integer_)
}

# Force a value into something SQLite can bind: exactly length 1, never NULL.
db_val <- function(x) if (length(x) != 1L) NA else x

# Fill in anything the parser did not produce, so every record stays bindable.
complete_detail <- function(d) {
  base <- empty_detail()
  for (nm in names(base)) base[[nm]] <- db_val(d[[nm]])
  base
}

# TRUE when a detail page yielded nothing usable — i.e. the fetch was blocked or
# the HTML changed, as opposed to a listing that is genuinely sparse.
detail_is_empty <- function(d) {
  all(is.na(c(d$title, d$price, d$size_sqm, d$address)))
}

# ── Scrape individual listing page for extra details ──────────────────────────
scrape_listing_detail <- function(finn_id) {
  url <- paste0("https://www.finn.no/realestate/homes/ad.html?finnkode=", finn_id)

  doc <- tryCatch(fetch_html(url), error = function(e) {
    message("    Detaljhenting mislyktes for ", finn_id, ": ", e$message)
    NULL
  })
  if (is.null(doc)) return(empty_detail())

  # ── Key facts table (dl/dt/dd pattern on finn.no detail pages) ───────────
  dts <- trimws(html_text(html_elements(doc, "dt")))
  dds <- trimws(html_text(html_elements(doc, "dd")))
  facts <- setNames(as.list(dds), dts)

  # Diagnostic: log what keys are available (helps catch future HTML changes)
  if (length(facts) == 0) {
    message("    ADVARSEL: ingen dt/dd-fakta funnet for ", finn_id,
            " — finn.no HTML kan ha endret seg")
  } else {
    message("    Faktanøkler: ", paste(head(names(facts), 15), collapse = ", "))
  }

  # Helper: get a fact value — tries exact match first, then partial/substring
  get_fact <- function(keys) {
    for (k in keys) {
      # Exact match (case-insensitive)
      hit <- facts[tolower(names(facts)) == tolower(k)]
      if (length(hit) > 0 && !is.na(hit[[1]]) && nchar(hit[[1]]) > 0)
        return(hit[[1]])
    }
    # Partial match: fact key *contains* one of the search terms
    for (k in keys) {
      hit <- facts[grepl(tolower(k), tolower(names(facts)), fixed = TRUE)]
      if (length(hit) > 0 && !is.na(hit[[1]]) && nchar(hit[[1]]) > 0)
        return(hit[[1]])
    }
    NA_character_
  }

  # ── Title (h1 on the detail page) ────────────────────────────────────────
  title_node <- html_element(doc, "h1")
  title <- if (!is.null(title_node)) trimws(html_text(title_node)) else NA_character_

  # ── Price ─────────────────────────────────────────────────────────────────
  # Prefer "Totalpris" (includes shared debt / fellesgjeld for co-ops).
  # For properties with no shared debt, "Totalpris" is absent and
  # "Prisantydning" / "Pris" IS the total price — so fall back to those.
  price <- parse_number_no(get_fact(c("Totalpris", "Prisantydning", "Pris")))

  # Fallback 1: dedicated price element by data-testid or class
  if (is.na(price)) {
    price_node <- html_element(
      doc,
      "[data-testid*='totalpris'], [data-testid*='Totalpris'], [data-testid*='total-price']"
    )
    if (!is.null(price_node)) price <- parse_number_no(html_text(price_node))
  }

  # Fallback 2: scan inline elements for text containing "kr" + a large number
  if (is.na(price)) {
    cands <- html_elements(doc, "span, p, div, strong, b, td, li")
    for (node in cands) {
      txt <- trimws(html_text(node))
      if (nchar(txt) > 50) next   # skip big blocks — price labels are short
      if (grepl("kr", txt, ignore.case = TRUE) &&
          grepl("[0-9][\\s\\.][0-9]{3}", txt, perl = TRUE)) {
        candidate <- parse_number_no(txt)
        if (!is.na(candidate) && candidate > 500000 && candidate < 200000000) {
          price <- candidate
          break
        }
      }
    }
  }

  # Fallback 3: regex on the raw page body near the word "totalpris"
  if (is.na(price)) {
    raw_txt <- tryCatch(html_text(html_element(doc, "body")), error = function(e) NA_character_)
    if (!is.na(raw_txt)) {
      m <- str_match(raw_txt,
                     "(?i)totalpris[^0-9]{0,30}([1-9][0-9 \\.\\u00a0]{4,14})")
      if (!is.na(m[1, 2])) {
        candidate <- parse_number_no(m[1, 2])
        if (!is.na(candidate) && candidate > 500000) price <- candidate
      }
    }
  }

  if (is.na(price)) message("    MERK: pris fortsatt NA etter alle fallbacks for ", finn_id)

  # ── Size ──────────────────────────────────────────────────────────────────
  # Prefer interior usable area (BRA-i), fall back to total usable area (BRA)
  size_raw <- get_fact(c(
    "Internt bruksareal (BRA-i)", "BRA-i", "Prim\u00e6rrom", "P-ROM",
    "Bruksareal", "BRA"
  ))
  size_sqm <- if (!is.na(size_raw)) {
    m <- str_match(size_raw, "(\\d+[,.]?\\d*)")
    if (!is.na(m[1, 2])) as.numeric(gsub(",", ".", m[1, 2])) else NA_real_
  } else NA_real_

  # ── Rooms ─────────────────────────────────────────────────────────────────
  rooms <- suppressWarnings(as.integer(get_fact(c("Rom", "Antall rom"))))

  # ── Address ───────────────────────────────────────────────────────────────
  # Try common selectors first, then fall back to parsing the page <title>
  addr_node <- html_element(doc, "[data-testid*='address'], h1 + p, h1 ~ p")
  address <- if (!is.null(addr_node)) trimws(html_text(addr_node)) else NA_character_
  if (is.na(address) || nchar(address) == 0) {
    page_title_text <- tryCatch(
      html_text(html_element(doc, "title")), error = function(e) NA_character_
    )
    # Page title: "3-roms leilighet - Grefsenveien 35, 0485 Oslo - FINN Eiendom"
    m_addr <- str_match(page_title_text,
                        "-\\s*([^-|]+\\d{4}\\s+[A-Za-z\u00c6\u00d8\u00c5\u00e6\u00f8\u00e5]+[^-|]*)\\s*(?:-|\\|)")
    if (!is.na(m_addr[1, 2])) address <- trimws(m_addr[1, 2])
  }

  # ── Neighborhood / municipality ───────────────────────────────────────────
  neighborhood <- get_fact(c("Bydel", "Beliggenhet", "Nabolag", "Sted"))
  if (is.na(neighborhood)) {
    bc_node <- html_element(doc, "nav[aria-label*='breadcrumb'] a:last-child,
                                  [class*='breadcrumb'] a:last-child")
    if (!is.null(bc_node)) neighborhood <- trimws(html_text(bc_node))
  }
  # Fallback: match known Oslo district names against the address string
  if (is.na(neighborhood) && !is.na(address) && nchar(address) > 0) {
    known_oslo <- c(
      "Frogner", "Grünerløkka", "Majorstuen", "St. Hanshaugen",
      "Sagene", "Gamle Oslo", "Nordstrand", "Østensjø",
      "Alna", "Bjerke", "Grorud", "Stovner", "Søndre Nordstrand",
      "Ullern", "Vestre Aker", "Nordre Aker", "Sentrum", "Marka",
      "Vålerenga", "Tøyen", "Grønland", "Torshov", "Sinsen",
      "Grefsen", "Storo", "Nydalen", "Sandaker", "Bislett",
      "Holmlia", "Lambertseter", "Romsås", "Furuset", "Helsfyr"
    )
    for (d in known_oslo) {
      if (grepl(d, address, ignore.case = TRUE)) {
        neighborhood <- d
        break
      }
    }
  }

  # ── Property type ─────────────────────────────────────────────────────────
  property_type <- get_fact(c("Boligtype", "Type", "Eiendomstype"))

  # ── Year built ────────────────────────────────────────────────────────────
  year_raw   <- get_fact(c("Bygg\u00e5r", "Bygge\u00e5r", "\u00c5r bygget"))
  year_built <- suppressWarnings(as.integer(year_raw))

  # ── Description (Om boligen) ──────────────────────────────────────────────
  # Strategy 1: find the "Om boligen" heading and collect its following paragraphs
  description <- tryCatch({
    headings <- html_elements(doc, "h2, h3, h4")
    om_idx   <- which(grepl("om\\s+boligen", tolower(trimws(html_text(headings)))))
    if (length(om_idx) > 0) {
      h     <- headings[[om_idx[1]]]
      paras <- html_elements(h, xpath = "following-sibling::p")
      if (length(paras) == 0)
        paras <- html_elements(h, xpath = "../following-sibling::*/p | ../following-sibling::p")
      paras <- paras[nchar(trimws(html_text(paras))) > 10]
      if (length(paras) > 0)
        paste(trimws(html_text(paras)), collapse = "\n\n")
      else
        NA_character_
    } else NA_character_
  }, error = function(e) NA_character_)

  # Strategy 2: class/tag fallback when heading not found
  if (is.na(description) || nchar(description) < 20) {
    desc_node <- html_element(
      doc, "[class*='description'], [class*='Description'], section p, article p"
    )
    if (!is.null(desc_node)) description <- trimws(html_text(desc_node))
  }

  # ── Broker (eiendomsmegler firma) ─────────────────────────────────────────
  # Broker name is not in the facts table; identify it from external link domains.
  broker <- tryCatch({
    hrefs <- html_attr(html_elements(doc, "a[href^='http']"), "href")
    hrefs <- hrefs[!is.na(hrefs) &
                   !grepl("finn\\.no|facebook|instagram|google|maps\\.app|vitecnext", hrefs,
                           ignore.case = TRUE)]
    found <- NA_character_
    for (h in hrefs) {
      # Extract first domain segment, e.g. "dnbeiendom" from "dnbeiendom.no"
      seg <- tolower(str_match(h, "https?://(?:www\\.)?([^\\.]+)")[, 2])
      if (!is.na(seg) && seg %in% names(BROKER_DOMAINS)) {
        found <- unname(BROKER_DOMAINS[seg])
        break
      }
    }
    # Fallback: clean up first unknown external domain as display name
    if (is.na(found) && length(hrefs) > 0) {
      seg <- str_match(hrefs[1], "https?://(?:www\\.)?([^\\.]+)")[, 2]
      if (!is.na(seg))
        found <- tools::toTitleCase(gsub("[-_]", " ", seg))
    }
    found
  }, error = function(e) NA_character_)

  # ── Floor number (etasje) ─────────────────────────────────────────────────────
  floor_raw <- get_fact(c("Etasje", "Etasjenummer", "Etg"))
  floor <- if (!is.na(floor_raw)) {
    m <- str_match(floor_raw, "(\\d+)")
    if (!is.na(m[1, 2])) suppressWarnings(as.integer(m[1, 2])) else NA_integer_
  } else NA_integer_

  # ── Balcony (balkong) ─────────────────────────────────────────────────────────
  balcony_raw <- get_fact(c("Balkong", "Terrasse/balkong", "Terrasse"))
  has_balcony <- if (!is.na(balcony_raw)) {
    as.integer(grepl("^ja", tolower(trimws(balcony_raw))))
  } else NA_integer_

  complete_detail(
    list(title = title, price = price, size_sqm = size_sqm, rooms = rooms,
         address = address, neighborhood = neighborhood,
         property_type = property_type, year_built = year_built,
         description = description, broker = broker,
         floor = floor, has_balcony = has_balcony)
  )
}

# ── Main scrape loop ───────────────────────────────────────────────────────────
message("Starter finn.no-henting — ", Sys.time())

new_rows   <- list()
stop_early <- FALSE

for (pg in seq_len(MAX_PAGES)) {
  if (stop_early) break

  page_df <- scrape_search_page(pg)
  if (is.null(page_df) || nrow(page_df) == 0) {
    message("  Ingen resultater på side ", pg, " — stopper.")
    # Nothing at all on the very first page means finn.no blocked us or changed
    # its HTML — not that Oslo ran out of flats for sale. Flag it, but carry on:
    # the backfill and the export below still have work to do on existing rows.
    if (pg == 1L) {
      scrape_degraded <- TRUE
      cat("::warning::Ingen annonser pa forste sokeside - finn.no kan ha endret HTML eller blokkert oss.\n")
    }
    break
  }

  novel <- page_df[!page_df$finn_id %in% existing_ids, ]
  message("  Side ", pg, ": ", nrow(page_df), " annonser, ",
          nrow(novel), " nye (ikke i DB ennå)")

  if (nrow(novel) == 0) {
    message("  Alle annonser allerede sett — stopper paginering.")
    stop_early <- TRUE
  } else {
    new_rows <- c(new_rows, list(novel))
  }

  Sys.sleep(PAGE_SLEEP)
}

if (length(new_rows) > 0) {
  search_results <- bind_rows(new_rows)
  message("\nHenter detaljer for ", nrow(search_results), " nye annonser...")

  scraped_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

  # Each listing is isolated: one unparseable page, one dropped connection or
  # one rejected insert must not cost us the listings already stored. Too many
  # useless fetches in a row means we are blocked, so we stop early and let the
  # export below save what we got instead of grinding through the rest.
  consecutive_failures <- 0L
  n_saved <- 0L
  n_empty <- 0L
  n_error <- 0L

  for (i in seq_len(nrow(search_results))) {
    row <- search_results[i, ]
    message("  [", i, "/", nrow(search_results), "] finn_id=", row$finn_id)

    status <- tryCatch({
      detail <- scrape_listing_detail(row$finn_id)

      # Prefer search-result values when available; fall back to detail-page values.
      # (Search results page is often JS-rendered, so most fields arrive as NA.)
      final_title   <- if (!is.na(row$title))    row$title    else detail$title
      final_price   <- if (!is.na(row$price))    row$price    else detail$price
      final_size    <- if (!is.na(row$size_sqm)) row$size_sqm else detail$size_sqm
      final_rooms   <- if (!is.na(row$rooms))    row$rooms    else detail$rooms
      final_address <- if (!is.na(row$address) && nchar(row$address) > 0)
                         row$address else detail$address

      dbExecute(con, "
        INSERT OR IGNORE INTO listings
          (finn_id, title, price, size_sqm, rooms, address,
           neighborhood, property_type, year_built, description,
           broker, url, scraped_at, floor, has_balcony)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
          db_val(row$finn_id), db_val(final_title), db_val(final_price),
          db_val(final_size), db_val(final_rooms), db_val(final_address),
          db_val(detail$neighborhood), db_val(detail$property_type),
          db_val(detail$year_built), db_val(detail$description),
          db_val(detail$broker), db_val(row$url), scraped_at,
          db_val(detail$floor), db_val(detail$has_balcony)
        )
      )

      # The row is stored either way — a blank one can be completed by the
      # backfill pass later — but a blank detail page still counts as a failed
      # fetch for the purpose of noticing that we are being blocked.
      if (detail_is_empty(detail)) "empty" else "ok"
    }, error = function(e) {
      message("    Hopper over finn_id=", row$finn_id, " — ", conditionMessage(e))
      "error"
    })

    if (identical(status, "ok")) {
      n_saved <- n_saved + 1L
      consecutive_failures <- 0L
    } else {
      if (identical(status, "empty")) n_empty <- n_empty + 1L else n_error <- n_error + 1L
      consecutive_failures <- consecutive_failures + 1L
    }

    Sys.sleep(DETAIL_SLEEP)

    if (consecutive_failures >= MAX_CONSECUTIVE_FAILURES) {
      message("  ", consecutive_failures, " mislykkede henting på rad — finn.no blokkerer oss",
              " trolig. Stopper detaljhentingen og beholder det vi har.")
      cat("::warning::finn.no sluttet a svare etter ", i, " av ", nrow(search_results),
          " annonser - beholder de innsamlede dataene og fortsetter.\n", sep = "")
      scrape_degraded <- TRUE
      break
    }
  }  # end new-listings loop

  message("  Detaljhenting ferdig: ", n_saved, " med data, ", n_empty,
          " tomme, ", n_error, " feilet.")

  # Individual failures are routine — ads get pulled while we are paginating.
  # Only call the run degraded when a real share of it failed.
  n_attempted <- n_saved + n_empty + n_error
  if (n_attempted > 0 && (n_empty + n_error) > n_attempted / 3) scrape_degraded <- TRUE
} else {
  message("Ingen nye annonser funnet.")
}

total <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM listings")$n
message("\nTotalt annonser i DB: ", total)

# ── Backfill existing listings that are missing core data ─────────────────────
# The search-results page stopped returning metadata (JS rendering), so old
# listings in the DB have NULL price / size_sqm. Re-fetch their detail pages
# to fill in the gaps. Cap at 60 per run to stay within CI time limits.
BACKFILL_BATCH <- 300L

needs_backfill <- dbGetQuery(con, paste0(
  "SELECT finn_id FROM listings
   WHERE (price IS NULL OR size_sqm IS NULL)
     AND scraped_at IS NOT NULL
   ORDER BY scraped_at DESC
   LIMIT ", BACKFILL_BATCH
))

if (nrow(needs_backfill) > 0) {
  message("\nEtterlyser ", nrow(needs_backfill),
          " eksisterende annonser som mangler pris/størrelse...")
  consecutive_failures <- 0L
  for (i in seq_len(nrow(needs_backfill))) {
    fid <- needs_backfill$finn_id[i]
    message("  [", i, "/", nrow(needs_backfill), "] etterlys finn_id=", fid)
    d <- tryCatch(scrape_listing_detail(fid), error = function(e) NULL)

    if (is.null(d) || detail_is_empty(d)) {
      consecutive_failures <- consecutive_failures + 1L
      if (consecutive_failures >= MAX_CONSECUTIVE_FAILURES) {
        message("  ", consecutive_failures, " tomme svar på rad — avbryter etterlysingen",
                " etter ", i, " av ", nrow(needs_backfill), ".")
        scrape_degraded <- TRUE
        break
      }
      Sys.sleep(DETAIL_SLEEP)
      next
    }
    consecutive_failures <- 0L

    tryCatch(dbExecute(con, "
      UPDATE listings SET
        title         = COALESCE(title,         ?),
        price         = COALESCE(price,         ?),
        size_sqm      = COALESCE(size_sqm,      ?),
        rooms         = COALESCE(rooms,         ?),
        address       = COALESCE(address,       ?),
        neighborhood  = COALESCE(neighborhood,  ?),
        property_type = COALESCE(property_type, ?),
        year_built    = COALESCE(year_built,    ?),
        description   = COALESCE(description,   ?),
        broker        = COALESCE(broker,        ?),
        floor         = COALESCE(floor,         ?),
        has_balcony   = COALESCE(has_balcony,   ?)
      WHERE finn_id = ?",
      params = list(
        db_val(d$title), db_val(d$price), db_val(d$size_sqm), db_val(d$rooms),
        db_val(d$address), db_val(d$neighborhood), db_val(d$property_type),
        db_val(d$year_built), db_val(d$description), db_val(d$broker),
        db_val(d$floor), db_val(d$has_balcony),
        fid
      )
    ), error = function(e) {
      message("    Kunne ikke oppdatere ", fid, " — ", conditionMessage(e))
    })
    Sys.sleep(DETAIL_SLEEP)
  }
  message("  Etterlysing ferdig.")
}

# ── Backfill floor + balcony for existing listings ────────────────────────────
FLOOR_BACKFILL_BATCH <- 150L

needs_floor <- dbGetQuery(con, paste0(
  "SELECT finn_id FROM listings
   WHERE floor IS NULL AND has_balcony IS NULL
     AND scraped_at IS NOT NULL
   ORDER BY scraped_at DESC
   LIMIT ", FLOOR_BACKFILL_BATCH
))

if (nrow(needs_floor) > 0) {
  message("\nEtterlyser etasje/balkong for ", nrow(needs_floor), " annonser...")
  consecutive_failures <- 0L
  for (i in seq_len(nrow(needs_floor))) {
    fid <- needs_floor$finn_id[i]
    message("  [", i, "/", nrow(needs_floor), "] etterlys finn_id=", fid)
    d <- tryCatch(scrape_listing_detail(fid), error = function(e) NULL)

    if (is.null(d) || detail_is_empty(d)) {
      consecutive_failures <- consecutive_failures + 1L
      if (consecutive_failures >= MAX_CONSECUTIVE_FAILURES) {
        message("  ", consecutive_failures, " tomme svar på rad — avbryter etterlysingen",
                " etter ", i, " av ", nrow(needs_floor), ".")
        scrape_degraded <- TRUE
        break
      }
      Sys.sleep(DETAIL_SLEEP)
      next
    }
    consecutive_failures <- 0L

    if (!is.na(d$floor) || !is.na(d$has_balcony)) {
      tryCatch(dbExecute(con,
        "UPDATE listings SET floor = ?, has_balcony = ? WHERE finn_id = ?",
        params = list(db_val(d$floor), db_val(d$has_balcony), fid)
      ), error = function(e) {
        message("    Kunne ikke oppdatere etasje/balkong for ", fid, " — ", conditionMessage(e))
      })
    }
    Sys.sleep(DETAIL_SLEEP)
  }
  message("  Etasje/balkong-etterlysing ferdig.")
}

# ── Export CSV snapshot so the Quarto page can render even before classify.R ──
n_exported <- export_snapshot()

if (scrape_degraded) {
  message("\nStatus: DELVIS — noe av skrapingen mislyktes, men alt vi fikk er lagret.")
  cat("::warning::Skrapingen var delvis - ", n_exported,
      " annonser lagret. Analysen fortsetter med det vi har.\n", sep = "")
} else {
  message("\nStatus: OK — ", n_exported, " annonser lagret.")
}

dbDisconnect(con)
