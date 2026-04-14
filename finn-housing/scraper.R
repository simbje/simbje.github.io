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
DB_PATH   <- file.path("finn-housing", "data", "finn_housing.db")
MAX_PAGES <- 10      # max search-result pages to scrape per run
PAGE_SLEEP <- 1.5    # seconds between search-result page fetches
DETAIL_SLEEP <- 1.2  # seconds between individual listing fetches

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
      message("  Attempt ", attempt, " failed: ", conditionMessage(last_error),
              " — retrying in ", wait, "s...")
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
    classified_at       TEXT
  );
")

# Migrate existing DBs: add lat/lon columns if they don't exist yet
tryCatch(dbExecute(con, "ALTER TABLE listings ADD COLUMN lat REAL"), error = function(e) NULL)
tryCatch(dbExecute(con, "ALTER TABLE listings ADD COLUMN lon REAL"), error = function(e) NULL)

existing_ids <- dbGetQuery(con, "SELECT finn_id FROM listings")$finn_id
message("Existing listings in DB: ", length(existing_ids))

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
  message("  Fetching search page ", page_num, ": ", url)

  doc <- tryCatch(fetch_html(url), error = function(e) {
    message("  Failed to fetch page ", page_num, ": ", e$message)
    NULL
  })
  if (is.null(doc)) return(NULL)

  # ── Try to find listing cards ─────────────────────────────────────────────
  # Finn.no renders listing cards as <article> elements.
  # The link to the full listing is in an <a> tag inside each card.
  cards <- html_elements(doc, "article")

  if (length(cards) == 0) {
    # Fallback: any link pointing to a realestate ad
    warning("No <article> cards found on page ", page_num,
            " — finn.no HTML may have changed. Falling back to link-only extraction.")
    links <- html_elements(doc, "a[href*='finnkode']")
    if (length(links) == 0) {
      message("  No listing cards found on page ", page_num, " — stopping pagination.")
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

# ── Scrape individual listing page for extra details ──────────────────────────
scrape_listing_detail <- function(finn_id) {
  url <- paste0("https://www.finn.no/realestate/homes/ad.html?finnkode=", finn_id)

  doc <- tryCatch(fetch_html(url), error = function(e) {
    message("    Detail fetch failed for ", finn_id, ": ", e$message)
    NULL
  })
  if (is.null(doc)) {
    return(list(title = NA_character_, price = NA_integer_,
                size_sqm = NA_real_, rooms = NA_integer_,
                address = NA_character_, neighborhood = NA_character_,
                property_type = NA_character_, year_built = NA_integer_,
                description = NA_character_))
  }

  # ── Key facts table (dl/dt/dd pattern on finn.no detail pages) ───────────
  dts <- trimws(html_text(html_elements(doc, "dt")))
  dds <- trimws(html_text(html_elements(doc, "dd")))
  facts <- setNames(as.list(dds), dts)

  # Helper: get a fact value case-insensitively
  get_fact <- function(keys) {
    for (k in keys) {
      hit <- facts[tolower(names(facts)) == tolower(k)]
      if (length(hit) > 0 && !is.na(hit[[1]]) && nchar(hit[[1]]) > 0)
        return(hit[[1]])
    }
    NA_character_
  }

  # ── Title (h1 on the detail page) ────────────────────────────────────────
  title_node <- html_element(doc, "h1")
  title <- if (!is.null(title_node)) trimws(html_text(title_node)) else NA_character_

  # ── Price ─────────────────────────────────────────────────────────────────
  # "Prisantydning" = asking price; avoid "Totalpris" which includes debt
  price <- parse_number_no(get_fact(c("Prisantydning", "Pris")))

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

  list(title = title, price = price, size_sqm = size_sqm, rooms = rooms,
       address = address, neighborhood = neighborhood,
       property_type = property_type, year_built = year_built,
       description = description)
}

# ── Main scrape loop ───────────────────────────────────────────────────────────
message("Starting finn.no scrape — ", Sys.time())

new_rows   <- list()
stop_early <- FALSE

for (pg in seq_len(MAX_PAGES)) {
  if (stop_early) break

  page_df <- scrape_search_page(pg)
  if (is.null(page_df) || nrow(page_df) == 0) {
    message("  No results on page ", pg, " — stopping.")
    break
  }

  novel <- page_df[!page_df$finn_id %in% existing_ids, ]
  message("  Page ", pg, ": ", nrow(page_df), " listings, ",
          nrow(novel), " new (not yet in DB)")

  if (nrow(novel) == 0) {
    message("  All listings already seen — stopping pagination.")
    stop_early <- TRUE
  } else {
    new_rows <- c(new_rows, list(novel))
  }

  Sys.sleep(PAGE_SLEEP)
}

if (length(new_rows) > 0) {
  search_results <- bind_rows(new_rows)
  message("\nFetching details for ", nrow(search_results), " new listings...")

  scraped_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

  for (i in seq_len(nrow(search_results))) {
  row <- search_results[i, ]
  message("  [", i, "/", nrow(search_results), "] finn_id=", row$finn_id)

  detail <- scrape_listing_detail(row$finn_id)
  Sys.sleep(DETAIL_SLEEP)

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
       url, scraped_at)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      row$finn_id, final_title, final_price, final_size, final_rooms, final_address,
      detail$neighborhood, detail$property_type, detail$year_built, detail$description,
      row$url, scraped_at
    )
  )
  }  # end new-listings loop
} else {
  message("No new listings found.")
}

total <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM listings")$n
message("\nTotal listings in DB: ", total)

# ── Backfill existing listings that are missing core data ─────────────────────
# The search-results page stopped returning metadata (JS rendering), so old
# listings in the DB have NULL price / size_sqm. Re-fetch their detail pages
# to fill in the gaps. Cap at 60 per run to stay within CI time limits.
BACKFILL_BATCH <- 60L

needs_backfill <- dbGetQuery(con, paste0(
  "SELECT finn_id FROM listings
   WHERE (price IS NULL OR size_sqm IS NULL)
     AND scraped_at IS NOT NULL
   ORDER BY scraped_at DESC
   LIMIT ", BACKFILL_BATCH
))

if (nrow(needs_backfill) > 0) {
  message("\nBackfilling ", nrow(needs_backfill),
          " existing listings missing price/size...")
  for (i in seq_len(nrow(needs_backfill))) {
    fid <- needs_backfill$finn_id[i]
    message("  [", i, "/", nrow(needs_backfill), "] backfill finn_id=", fid)
    d <- tryCatch(scrape_listing_detail(fid), error = function(e) NULL)
    if (is.null(d)) { Sys.sleep(DETAIL_SLEEP); next }

    dbExecute(con, "
      UPDATE listings SET
        title         = COALESCE(title,         ?),
        price         = COALESCE(price,         ?),
        size_sqm      = COALESCE(size_sqm,      ?),
        rooms         = COALESCE(rooms,         ?),
        address       = COALESCE(address,       ?),
        neighborhood  = COALESCE(neighborhood,  ?),
        property_type = COALESCE(property_type, ?),
        year_built    = COALESCE(year_built,    ?)
      WHERE finn_id = ?",
      params = list(
        d$title, d$price, d$size_sqm, d$rooms, d$address,
        d$neighborhood, d$property_type, d$year_built,
        fid
      )
    )
    Sys.sleep(DETAIL_SLEEP)
  }
  message("  Backfill done.")
}

# ── Export CSV snapshot so the Quarto page can render even before classify.R ──
# Category columns will be NA until classify.R runs — the dashboard handles this.
CSV_PATH <- file.path("finn-housing", "data", "listings_export.csv")
snap <- dbGetQuery(con, "
  SELECT finn_id, title, price, size_sqm, rooms, address, neighborhood,
         property_type, year_built, url, scraped_at, lat, lon,
         category, category_confidence, category_reasoning, classified_at
  FROM listings ORDER BY scraped_at DESC
")
write.csv(snap, CSV_PATH, row.names = FALSE, fileEncoding = "UTF-8")
message("CSV snapshot written: ", CSV_PATH)

dbDisconnect(con)
