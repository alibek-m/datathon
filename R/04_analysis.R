suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(broom)
})

source("R/utils.R")

clean       <- read_csv("data/processed/yc_clean.csv", show_col_types = FALSE)
ind_long    <- read_csv("data/processed/yc_industries_long.csv", show_col_types = FALSE)

CURRENT_YEAR <- as.integer(format(Sys.Date(), "%Y"))
MATURITY_YR  <- 5
mature_cutoff <- CURRENT_YEAR - MATURITY_YR

mature <- clean |> filter(batch_year <= mature_cutoff, !is.na(outcome))
message(sprintf("Mature subset (batches <= %d): %d companies", mature_cutoff, nrow(mature)))

success_rate <- function(df, group_col) {
  out <- df |>
    filter(outcome %in% c("Successful", "Failed")) |>
    group_by(.data[[group_col]]) |>
    summarise(
      n_total = n(),
      n_success = sum(outcome == "Successful"),
      .groups = "drop"
    ) |>
    rename(group = !!group_col)
  bind_cols(out, wilson_ci(out$n_success, out$n_total)) |> arrange(desc(p))
}

q1_industry <- success_rate(mature, "industry") |> filter(n_total >= 20)
q1_country  <- mature |>
  mutate(country_bucket = case_when(
    in_us  ~ "USA",
    is.na(country) ~ "Unknown",
    TRUE ~ "Non-US"
  )) |>
  success_rate("country_bucket") |>
  filter(n_total >= 20)
q1_team     <- success_rate(mature, "team_bucket") |> filter(!is.na(group), n_total >= 20)
q1_sf       <- success_rate(mature, "in_sf_bay")
q1_top_country <- mature |>
  count(country, sort = TRUE) |>
  filter(n >= 30, !is.na(country)) |>
  pull(country) |>
  setdiff("Remote") |>
  (\(top_countries) mature |>
     filter(country %in% top_countries) |>
     success_rate("country") |>
     filter(n_total >= 30))()

q1_industry_long_raw <- ind_long |>
  filter(batch_year <= mature_cutoff,
         outcome %in% c("Successful", "Failed"),
         !is.na(industry_tag)) |>
  group_by(industry_tag) |>
  summarise(n_total = n(),
            n_success = sum(outcome == "Successful"),
            .groups = "drop") |>
  filter(n_total >= 30)
q1_industry_long <- bind_cols(q1_industry_long_raw,
                              wilson_ci(q1_industry_long_raw$n_success,
                                        q1_industry_long_raw$n_total)) |>
  arrange(desc(p))

write_csv(q1_industry,      "data/processed/q1_industry.csv")
write_csv(q1_country,       "data/processed/q1_country.csv")
write_csv(q1_team,          "data/processed/q1_team.csv")
write_csv(q1_sf,            "data/processed/q1_sf_bay.csv")
write_csv(q1_top_country,   "data/processed/q1_top_country.csv")
write_csv(q1_industry_long, "data/processed/q1_industry_tags.csv")

logit_df <- mature |>
  filter(outcome %in% c("Successful", "Failed"),
         !is.na(industry), !is.na(team_bucket)) |>
  mutate(
    success      = as.integer(outcome == "Successful"),
    industry     = factor(industry),
    region_bkt   = factor(case_when(in_us ~ "USA", is.na(country) ~ "Unknown", TRUE ~ "Non-US")),
    team_bucket  = factor(team_bucket, ordered = FALSE),
    batch_decade = factor(case_when(
      batch_year <= 2014 ~ "2005-2014",
      batch_year <= 2021 ~ "2015-2021",
      TRUE              ~ NA_character_
    ))
  ) |>
  filter(!is.na(batch_decade))

logit_fit <- glm(success ~ industry + region_bkt + team_bucket + batch_decade,
                 family = binomial, data = logit_df)
logit_tidy <- tidy(logit_fit, exponentiate = TRUE, conf.int = TRUE)
write_csv(logit_tidy, "data/processed/q1_logit.csv")
message("Logit AIC: ", round(AIC(logit_fit), 1), " | n = ", nobs(logit_fit))

q2_industry_year <- clean |>
  filter(!is.na(batch_year), !is.na(industry)) |>
  count(batch_year, industry, name = "n") |>
  group_by(batch_year) |>
  mutate(share = n / sum(n)) |>
  ungroup()

q2_geo_year <- clean |>
  filter(!is.na(batch_year)) |>
  mutate(geo = case_when(in_us ~ "USA", is.na(country) ~ "Unknown", TRUE ~ "Non-US")) |>
  count(batch_year, geo) |>
  group_by(batch_year) |>
  mutate(share = n / sum(n)) |>
  ungroup()

q2_batch_size <- clean |>
  filter(!is.na(batch_year)) |>
  count(batch_year, name = "batch_size")

q2_country_year <- clean |>
  filter(!is.na(country), !is.na(batch_year)) |>
  count(batch_year, country, name = "n")

q2_top_country_share <- clean |>
  filter(!is.na(batch_year), top_company) |>
  count(batch_year, name = "n_top") |>
  full_join(q2_batch_size, by = "batch_year") |>
  mutate(top_rate = coalesce(n_top, 0L) / batch_size) |>
  arrange(batch_year)

write_csv(q2_industry_year,     "data/processed/q2_industry_year.csv")
write_csv(q2_geo_year,          "data/processed/q2_geo_year.csv")
write_csv(q2_batch_size,        "data/processed/q2_batch_size.csv")
write_csv(q2_country_year,      "data/processed/q2_country_year.csv")
write_csv(q2_top_country_share, "data/processed/q2_top_company_rate.csv")

q3_era_raw <- mature |>
  filter(outcome %in% c("Successful", "Failed"),
         !is.na(industry)) |>
  mutate(era_q3 = case_when(
    batch_year <= 2014 ~ "2005-2014",
    batch_year <= 2021 ~ "2015-2021",
    TRUE              ~ NA_character_
  )) |>
  filter(!is.na(era_q3)) |>
  group_by(industry, era_q3) |>
  summarise(n_total = n(),
            n_success = sum(outcome == "Successful"),
            .groups = "drop") |>
  filter(n_total >= 15)
q3_era <- bind_cols(q3_era_raw, wilson_ci(q3_era_raw$n_success, q3_era_raw$n_total))

q3_wide <- q3_era |>
  select(industry, era_q3, p, n_total) |>
  pivot_wider(names_from = era_q3, values_from = c(p, n_total)) |>
  filter(!is.na(`p_2005-2014`), !is.na(`p_2015-2021`)) |>
  mutate(delta = `p_2015-2021` - `p_2005-2014`) |>
  arrange(desc(delta))

write_csv(q3_era,  "data/processed/q3_era_long.csv")
write_csv(q3_wide, "data/processed/q3_era_wide.csv")

cat("\n=========================================\n")
cat("KEY FINDINGS\n")
cat("=========================================\n")
cat("\n[Q1] Success rate by industry (mature batches):\n")
print(q1_industry |> select(group, n_total, n_success, p, lo, hi))

cat("\n[Q1] Success rate USA vs Non-US:\n")
print(q1_country)

cat("\n[Q1] Success rate by team size:\n")
print(q1_team |> select(group, n_total, n_success, p, lo, hi))

cat("\n[Q1] Top logit odds ratios (excluding intercept):\n")
print(logit_tidy |> filter(term != "(Intercept)") |> arrange(desc(estimate)) |> head(10))

cat("\n[Q3] Era shift in success rate by industry:\n")
print(q3_wide)

cat("\nDone. All tables saved to data/processed/\n")
