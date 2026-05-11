suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(janitor)
  library(stringr)
})

clean <- read_csv("data/processed/yc_clean.csv", show_col_types = FALSE)
cat("=========================================\n")
cat("YC dataset summary\n")
cat("=========================================\n")
cat(sprintf("Rows: %d  |  Cols: %d\n", nrow(clean), ncol(clean)))

cat("\n-- Status distribution --\n")
print(tabyl(clean, status) |> adorn_pct_formatting(digits = 1))

cat("\n-- Outcome distribution --\n")
print(tabyl(clean, outcome) |> adorn_pct_formatting(digits = 1))

cat("\n-- Batch year range --\n")
cat(sprintf("Min: %d, Max: %d\n",
            min(clean$batch_year, na.rm = TRUE),
            max(clean$batch_year, na.rm = TRUE)))

cat("\n-- Top 10 industries --\n")
print(clean |> count(industry, sort = TRUE) |> head(10))

cat("\n-- Top 10 countries --\n")
print(clean |> count(country, sort = TRUE) |> head(10))

cat("\n-- Team size buckets --\n")
print(tabyl(clean, team_bucket) |> adorn_pct_formatting(digits = 1))

cat("\n-- Top company flag --\n")
print(tabyl(clean, top_company))

cat("\n-- Missing data summary --\n")
miss <- clean |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  tidyr::pivot_longer(everything(), names_to = "col", values_to = "n_missing") |>
  filter(n_missing > 0) |>
  arrange(desc(n_missing))
print(miss, n = nrow(miss))

cat("\n-- Sample rows --\n")
print(clean |> select(name, batch, status, outcome, industry, country, team_size) |> sample_n(8))
