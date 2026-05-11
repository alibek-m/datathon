suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

parse_batch <- function(batch_str) {
  m <- str_match(batch_str, "^(Winter|Summer|Spring|Fall|IK\\d*)\\s*(\\d{4})$")
  tibble(batch_season = m[, 2], batch_year = as.integer(m[, 3]))
}

bucket_team_size <- function(n) {
  factor(
    case_when(
      is.na(n) | n == 0 ~ NA_character_,
      n <= 5            ~ "1-5",
      n <= 10           ~ "6-10",
      n <= 25           ~ "11-25",
      n <= 100          ~ "26-100",
      n <= 500          ~ "101-500",
      TRUE              ~ "500+"
    ),
    levels = c("1-5", "6-10", "11-25", "26-100", "101-500", "500+"),
    ordered = TRUE
  )
}

derive_outcome <- function(status) {
  factor(
    case_when(
      status %in% c("Acquired", "Public") ~ "Successful",
      status == "Active"                  ~ "Surviving",
      status == "Inactive"                ~ "Failed",
      TRUE                                ~ NA_character_
    ),
    levels = c("Successful", "Surviving", "Failed")
  )
}

split_location <- function(loc) {
  loc <- str_squish(loc)
  primary <- str_split_i(loc, ";", 1) |> str_squish()
  is_remote_only <- !is.na(primary) & str_to_lower(primary) == "remote"
  is_remote <- str_detect(coalesce(loc, ""), "(?i)remote")
  primary[is_remote_only] <- NA_character_

  pieces  <- str_split(primary, ",\\s*")
  n_parts <- lengths(pieces)
  city    <- map2_chr(pieces, n_parts, ~ if (.y >= 1 && !is.na(.x[[1]])) .x[[1]] else NA_character_)
  region  <- map2_chr(pieces, n_parts, ~ if (.y >= 3) .x[[2]] else NA_character_)
  country <- map2_chr(pieces, n_parts, ~ if (.y >= 1 && !is.na(.x[[1]])) tail(.x, 1) else NA_character_)
  city[is_remote_only]    <- NA_character_
  region[is_remote_only]  <- NA_character_
  country[is_remote_only] <- NA_character_
  tibble(city, region, country, is_remote)
}

flag_sf_bay <- function(loc) {
  bay <- "(?i)san francisco|palo alto|mountain view|menlo park|sunnyvale|oakland|berkeley|san jose|redwood city|cupertino|santa clara|emeryville|south san francisco|brisbane|daly city|burlingame"
  str_detect(coalesce(loc, ""), bay)
}

is_us <- function(country) {
  country %in% c("USA", "US", "United States", "United States of America")
}

wilson_ci <- function(successes, total, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2)
  p <- ifelse(total > 0, successes / total, NA_real_)
  denom  <- 1 + z^2 / total
  centre <- (p + z^2 / (2 * total)) / denom
  margin <- z * sqrt((p * (1 - p) + z^2 / (4 * total)) / total) / denom
  tibble(p = p, lo = pmax(0, centre - margin), hi = pmin(1, centre + margin))
}

batch_era <- function(year) {
  factor(
    case_when(
      year <= 2014           ~ "2005-2014",
      year >= 2015 & year <= 2024 ~ "2015-2024",
      TRUE                    ~ NA_character_
    ),
    levels = c("2005-2014", "2015-2024")
  )
}
