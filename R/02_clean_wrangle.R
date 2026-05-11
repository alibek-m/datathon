suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(lubridate)
  library(janitor)
  library(readr)
})

source("R/utils.R")

raw_files <- list.files("data/raw", pattern = "^yc_raw_\\d{8}\\.json$", full.names = TRUE)
stopifnot(length(raw_files) >= 1)
raw_path  <- tail(sort(raw_files), 1)
message("Reading: ", raw_path)
raw <- fromJSON(raw_path, simplifyDataFrame = FALSE)

flatten_chr <- function(x, sep = ";") {
  if (is.null(x) || length(x) == 0) NA_character_ else paste(unlist(x), collapse = sep)
}

records <- map(raw, function(r) {
  list(
    id              = r$id %||% NA_integer_,
    name            = r$name %||% NA_character_,
    slug            = r$slug %||% NA_character_,
    website         = r$website %||% NA_character_,
    batch           = r$batch %||% NA_character_,
    status          = r$status %||% NA_character_,
    stage           = r$stage %||% NA_character_,
    team_size       = r$team_size %||% NA_real_,
    industry        = r$industry %||% NA_character_,
    subindustry     = r$subindustry %||% NA_character_,
    industries      = flatten_chr(r$industries),
    tags            = flatten_chr(r$tags),
    regions         = flatten_chr(r$regions),
    all_locations   = r$all_locations %||% NA_character_,
    one_liner       = r$one_liner %||% NA_character_,
    long_description = r$long_description %||% NA_character_,
    top_company     = isTRUE(r$top_company),
    is_hiring       = isTRUE(r$isHiring),
    nonprofit       = isTRUE(r$nonprofit),
    launched_at     = r$launched_at %||% NA_real_
  )
})

df <- bind_rows(records) |> as_tibble()
message("Rows read: ", nrow(df))

batch_parts <- parse_batch(df$batch)
loc_parts   <- split_location(df$all_locations)

clean <- df |>
  bind_cols(batch_parts) |>
  bind_cols(loc_parts) |>
  mutate(
    status         = str_to_title(status),
    outcome        = derive_outcome(status),
    team_bucket    = bucket_team_size(team_size),
    in_sf_bay      = flag_sf_bay(all_locations),
    in_us          = is_us(country),
    company_age    = as.integer(format(Sys.Date(), "%Y")) - batch_year,
    era            = batch_era(batch_year),
    launched_date  = if_else(is.na(launched_at), as.Date(NA), as.Date(as.POSIXct(launched_at, origin = "1970-01-01", tz = "UTC")))
  ) |>
  select(
    id, name, slug, batch, batch_year, batch_season, era,
    status, outcome, stage, top_company,
    team_size, team_bucket,
    industry, subindustry, industries, tags,
    city, region, country, regions, in_sf_bay, in_us, is_remote,
    company_age, is_hiring, nonprofit,
    launched_date, website, one_liner, long_description
  )

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
write_csv(clean, "data/processed/yc_clean.csv")
message("Saved: data/processed/yc_clean.csv (", nrow(clean), " rows, ", ncol(clean), " cols)")

industry_long <- clean |>
  filter(!is.na(industries)) |>
  separate_longer_delim(industries, delim = ";") |>
  rename(industry_tag = industries) |>
  select(id, name, batch_year, era, status, outcome, country, in_us, team_size, team_bucket, top_company, industry_tag)

write_csv(industry_long, "data/processed/yc_industries_long.csv")
message("Saved: data/processed/yc_industries_long.csv (", nrow(industry_long), " rows)")
