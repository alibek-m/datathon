suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(purrr)
  library(dplyr)
})

ALGOLIA_APP_ID  <- "45BWZJ1SGC"
ALGOLIA_API_KEY <- "NzllNTY5MzJiZGM2OTY2ZTQwMDEzOTNhYWZiZGRjODlhYzVkNjBmOGRjNzJiMWM4ZTU0ZDlhYTZjOTJiMjlhMWFuYWx5dGljc1RhZ3M9eWNkYyZyZXN0cmljdEluZGljZXM9WUNDb21wYW55X3Byb2R1Y3Rpb24lMkNZQ0NvbXBhbnlfQnlfTGF1bmNoX0RhdGVfcHJvZHVjdGlvbiZ0YWdGaWx0ZXJzPSU1QiUyMnljZGNfcHVibGljJTIyJTVE"
INDEX_NAME      <- "YCCompany_production"
HITS_PER_PAGE   <- 1000
USER_AGENT      <- "Lawrence-University-Datathon-Project (a.mamyrbay@gmail.com)"

algolia_url <- sprintf("https://%s-dsn.algolia.net/1/indexes/%s/query",
                       ALGOLIA_APP_ID, INDEX_NAME)

algolia_post <- function(params) {
  request(algolia_url) |>
    req_method("POST") |>
    req_headers(
      "X-Algolia-Application-Id" = ALGOLIA_APP_ID,
      "X-Algolia-API-Key"        = ALGOLIA_API_KEY,
      "User-Agent"               = USER_AGENT,
      "Content-Type"             = "application/json"
    ) |>
    req_body_json(list(params = params)) |>
    req_throttle(rate = 1) |>
    req_retry(max_tries = 3) |>
    req_perform() |>
    resp_body_json(simplifyVector = FALSE)
}

message("Discovering batches via facets ...")
facet_resp <- algolia_post("hitsPerPage=0&facets=%5B%22batch%22%5D&maxValuesPerFacet=500")
batches    <- names(facet_resp$facets$batch)
total_expected <- facet_resp$nbHits
message(sprintf("Found %d batches, %d total companies", length(batches), total_expected))

fetch_batch <- function(batch_name) {
  enc <- utils::URLencode(sprintf("[\"batch:%s\"]", batch_name), reserved = TRUE)
  params <- sprintf("hitsPerPage=%d&facetFilters=%s", HITS_PER_PAGE, enc)
  resp <- algolia_post(params)
  if (length(resp$hits) >= HITS_PER_PAGE) {
    warning(sprintf("Batch %s hit pagination cap (>= %d) -- partial data!", batch_name, HITS_PER_PAGE))
  }
  resp$hits
}

all_hits <- list()
for (i in seq_along(batches)) {
  b <- batches[i]
  message(sprintf("[%d/%d] Fetching batch '%s' ...", i, length(batches), b))
  hits <- fetch_batch(b)
  message(sprintf("    -> %d hits", length(hits)))
  all_hits <- c(all_hits, hits)
}

ids <- map_int(all_hits, "id")
dup <- duplicated(ids)
if (any(dup)) {
  message(sprintf("Removing %d duplicate ids", sum(dup)))
  all_hits <- all_hits[!dup]
}

message(sprintf("Total unique hits collected: %d (expected %d)", length(all_hits), total_expected))

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
out_path <- file.path("data/raw", sprintf("yc_raw_%s.json", format(Sys.Date(), "%Y%m%d")))
write_json(all_hits, out_path, pretty = FALSE, auto_unbox = TRUE)
message("Saved: ", out_path)

scrape_meta <- list(
  scrape_date    = format(Sys.Date()),
  app_id         = ALGOLIA_APP_ID,
  index          = INDEX_NAME,
  total_expected = total_expected,
  hits_in_file   = length(all_hits),
  batches_seen   = length(batches),
  source_url     = "https://www.ycombinator.com/companies"
)
write_json(scrape_meta, "data/raw/scrape_meta.json", pretty = TRUE, auto_unbox = TRUE)
message("Saved: data/raw/scrape_meta.json")
