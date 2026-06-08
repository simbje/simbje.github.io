#!/usr/bin/env Rscript
# =============================================================================
# discover_tables.R
# Runs on a schedule (1st & 15th of each month — roughly every two weeks).
# Pulls the live SSB table catalogue, keeps actively-updated, plottable tables,
# picks a diverse batch across subjects, and APPENDS the new ones to
# ssb-daily/table_pool.csv. generate_post.R reads that pool alongside the
# hand-curated seed list, so the daily discovery agent has a growing supply of
# real, verified-by-SSB table IDs to draw from.
#
# Only real catalogue IDs are ever added — nothing is invented — so a table can
# never be a 404. Tables already in the seed list, already in the pool, or on
# the no-data blacklist are skipped. Always exits 0.
# =============================================================================

suppressMessages({
  library(httr2)
  library(jsonlite)
})

CATALOG_URL  <- "https://data.ssb.no/api/pxwebapi/v2-beta/tables"
POOL_FILE    <- file.path("ssb-daily", "table_pool.csv")
GEN_FILE     <- file.path("ssb-daily", "generate_post.R")
PATTERNS_FILE<- file.path("ssb-daily", "error_patterns.md")
TARGET_NEW   <- 15L          # new tables to add per run
MAX_PER_SUBJ <- 2L           # diversity cap per subject within one run
PAGE_SIZE    <- 1000L

bail <- function(msg) { message(msg); quit(save = "no", status = 0) }

# ── Known IDs to skip: seed list + existing pool + blacklist ─────────────────
seed_ids <- local({
  if (!file.exists(GEN_FILE)) return(character(0))
  txt <- readLines(GEN_FILE, warn = FALSE)
  m   <- regmatches(txt, gregexpr('"(\\d{4,6}) - ', txt, perl = TRUE))
  ids <- sub('"(\\d{4,6}) - ', "\\1", unlist(m), perl = TRUE)
  unique(ids)
})

pool_existing <- if (file.exists(POOL_FILE)) {
  tryCatch(read.csv(POOL_FILE, stringsAsFactors = FALSE, colClasses = "character"),
           error = function(e) NULL)
} else NULL
pool_ids <- if (!is.null(pool_existing) && "id" %in% names(pool_existing))
  pool_existing$id else character(0)

blacklist_ids <- local({
  if (!file.exists(PATTERNS_FILE) || file.size(PATTERNS_FILE) == 0L) return(character(0))
  content  <- paste(readLines(PATTERNS_FILE, warn = FALSE), collapse = "\n")
  sections <- strsplit(content, "\n(?=## )", perl = TRUE)[[1]]
  recent   <- Filter(function(s) grepl("Data unavailable", s, fixed = TRUE), sections)
  if (length(recent) == 0L) return(character(0))
  lines <- regmatches(recent, regexpr("SSB tables[^\n]*", recent, perl = TRUE))
  unique(unlist(regmatches(lines, gregexpr("\\b\\d{4,6}\\b", lines))))
})

known_ids <- unique(c(seed_ids, pool_ids, blacklist_ids))
message("Known IDs to skip: ", length(known_ids),
        " (seed ", length(seed_ids), " / pool ", length(pool_ids),
        " / blacklist ", length(blacklist_ids), ")")

# ── Fetch the full catalogue (paged) ─────────────────────────────────────────
fetch_page <- function(page) {
  request(CATALOG_URL) |>
    req_url_query(lang = "en", pageSize = PAGE_SIZE, pageNumber = page) |>
    req_timeout(60) |>
    req_retry(max_tries = 3L, backoff = function(i) 5) |>
    req_perform() |>
    resp_body_json(simplifyVector = FALSE)
}

all_tables <- tryCatch({
  first <- fetch_page(1L)
  total_pages <- first$page$totalPages %||% 1L
  tabs <- first$tables
  if (total_pages > 1L) {
    for (p in 2:total_pages) tabs <- c(tabs, fetch_page(p)$tables)
  }
  tabs
}, error = function(e) bail(paste0("Catalogue fetch failed: ", conditionMessage(e),
                                   " — leaving pool unchanged.")))

message("Fetched ", length(all_tables), " tables from SSB catalogue.")

# ── Filter to good candidates ────────────────────────────────────────────────
this_year <- as.integer(format(Sys.Date(), "%Y"))
recent_years <- as.character((this_year - 1L):(this_year + 1L))

clean_label <- function(lbl) {
  lbl <- sub("^\\d+:\\s*", "", lbl)          # drop leading "13760: "
  lbl <- gsub("[\r\n]+", " ", lbl)
  lbl <- trimws(gsub("\\s+", " ", lbl))
  substr(lbl, 1L, 90L)
}

candidates <- Filter(function(x) {
  id    <- x$id %||% ""
  nvars <- length(x$variableNames %||% list())
  tu    <- x$timeUnit %||% ""
  lp    <- x$lastPeriod %||% ""
  (x$category %||% "") == "public" &&
    nchar(id) >= 4L && !(id %in% known_ids) &&
    nvars >= 2L && nvars <= 5L &&
    tu %in% c("Annual", "Monthly", "Quarterly") &&
    substr(lp, 1L, 4L) %in% recent_years
}, all_tables)

message("Candidates after filtering: ", length(candidates))
if (length(candidates) == 0L) bail("No new candidate tables this run — pool unchanged.")

# ── Diverse pick: spread across subjects, capped per subject ─────────────────
set.seed(as.integer(Sys.time()))
candidates <- sample(candidates)                       # shuffle
count0     <- function(v, k) { val <- v[k]; if (length(val) == 0L || is.na(val)) 0L else as.integer(val) }
subj_count <- integer(0)
picked     <- list()
for (x in candidates) {
  if (length(picked) >= TARGET_NEW) break
  s <- x$subjectCode %||% "??"
  if (count0(subj_count, s) >= MAX_PER_SUBJ) next
  subj_count[s] <- count0(subj_count, s) + 1L
  picked[[length(picked) + 1L]] <- x
}
# Top up if subject diversity left us short
if (length(picked) < TARGET_NEW) {
  picked_ids <- vapply(picked, function(x) x$id, character(1))
  for (x in candidates) {
    if (length(picked) >= TARGET_NEW) break
    if (!(x$id %in% picked_ids)) picked[[length(picked) + 1L]] <- x
  }
}

new_rows <- do.call(rbind, lapply(picked, function(x) {
  data.frame(
    id         = x$id,
    label      = clean_label(x$label %||% x$id),
    subject    = x$subjectCode %||% "",
    timeUnit   = x$timeUnit %||% "",
    lastPeriod = x$lastPeriod %||% "",
    updated    = substr(x$updated %||% "", 1L, 10L),
    added      = as.character(Sys.Date()),
    stringsAsFactors = FALSE
  )
}))

# ── Append to pool ───────────────────────────────────────────────────────────
pool <- if (!is.null(pool_existing) && nrow(pool_existing) > 0L) {
  for (col in names(new_rows)) if (!col %in% names(pool_existing)) pool_existing[[col]] <- ""
  rbind(pool_existing[, names(new_rows), drop = FALSE], new_rows)
} else new_rows

pool <- pool[!duplicated(pool$id), , drop = FALSE]
dir.create(dirname(POOL_FILE), recursive = TRUE, showWarnings = FALSE)
write.csv(pool, POOL_FILE, row.names = FALSE)

message("Added ", nrow(new_rows), " new table(s); pool now holds ", nrow(pool), ".")
message("New: ", paste(new_rows$id, collapse = ", "))
quit(save = "no", status = 0)
