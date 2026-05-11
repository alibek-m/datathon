suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(stringr)
  library(scales)
  library(forcats)
  library(broom)
  library(patchwork)
  library(ggrepel)
})

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

source("R/palette.R")

q1_industry  <- read_csv("data/processed/q1_industry.csv", show_col_types = FALSE)
q1_team      <- read_csv("data/processed/q1_team.csv", show_col_types = FALSE)
logit_tidy   <- read_csv("data/processed/q1_logit.csv", show_col_types = FALSE)

p_industry <- q1_industry |>
  filter(group != "Unspecified") |>
  mutate(group = fct_reorder(group, p)) |>
  ggplot(aes(x = group, y = p)) +
  geom_segment(aes(xend = group, y = 0, yend = p), color = GREY_MED, linewidth = 0.5) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.25, color = YC_DARK, linewidth = 0.5) +
  geom_point(aes(size = n_total), color = YC_ORANGE) +
  geom_text(aes(y = hi, label = sprintf("%.0f%%  (n=%d)", p * 100, n_total)),
            hjust = -0.15, vjust = 0.5, size = 3.7, color = YC_DARK, fontface = "bold") +
  scale_y_continuous(labels = label_percent(), limits = c(0, 0.85), expand = expansion(mult = c(0, 0.05))) +
  scale_size_continuous(range = c(3, 9), guide = "none") +
  coord_flip() +
  labs(
    title    = "Fintech and B2B lead YC's exit hit rate",
    subtitle = "Share of mature-batch companies (≤5 yr post-batch) reaching Acquired or Public,\nwith 95% Wilson confidence intervals",
    x = NULL, y = "Exit hit rate",
    caption = sprintf("Source: ycombinator.com/companies (scraped %s). Excludes still-active companies. Bubble size ∝ cohort size.",
                      format(Sys.Date(), "%b %Y"))
  ) +
  theme_yc()

ggsave("outputs/figures/q1_industry_success.png", p_industry, width = 10, height = 6, dpi = 200, bg = CREAM)

p_team <- q1_team |>
  mutate(group = factor(group, levels = c("1-5","6-10","11-25","26-100","101-500","500+"))) |>
  ggplot(aes(x = group, y = p)) +
  geom_col(fill = YC_ORANGE, alpha = 0.85, width = 0.6) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2, color = YC_DARK, linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.0f%%", p * 100)),
            vjust = -0.5, color = YC_DARK, fontface = "bold", size = 3.8,
            position = position_nudge(y = 0.04)) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1.05), expand = expansion(mult = c(0, 0.02))) +
  labs(
    title    = "Survival is a function of size",
    subtitle = "Exit hit rate climbs from 28% (tiny teams) to 100% (500+ employees) —\nbut size is partly an outcome, not just a cause",
    x = "Team size (current)", y = "Exit hit rate",
    caption = "Mature batches only. Surviving (Active) companies excluded from denominator."
  ) +
  theme_yc()

ggsave("outputs/figures/q1_team_size.png", p_team, width = 9, height = 5.5, dpi = 200, bg = CREAM)

p_forest <- logit_tidy |>
  filter(term != "(Intercept)",
         !str_detect(term, "team_bucket500\\+"),
         !is.na(conf.low), !is.na(conf.high),
         conf.high < 100) |>
  mutate(
    var_group = case_when(
      str_starts(term, "industry")     ~ "Industry",
      str_starts(term, "team_bucket")  ~ "Team size",
      str_starts(term, "region_bkt")   ~ "Region",
      str_starts(term, "batch_decade") ~ "Era",
      TRUE                              ~ "Other"
    ),
    label = term |>
      str_remove("^industry|^team_bucket|^region_bkt|^batch_decade") |>
      str_replace_all("_", " "),
    label = fct_reorder(label, estimate),
    sig = ifelse(p.value < 0.05, "p < 0.05", "n.s.")
  ) |>
  ggplot(aes(x = estimate, y = label, color = sig)) +
  geom_vline(xintercept = 1, color = GREY_MED, linetype = "dashed") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.25, linewidth = 0.6) +
  geom_point(size = 3) +
  scale_x_log10(breaks = c(0.1, 0.25, 0.5, 1, 2, 4, 10, 30), labels = c("0.1","0.25","0.5","1","2","4","10","30")) +
  scale_color_manual(values = c("n.s." = WARM_GREY, "p < 0.05" = INK_BLUE)) +
  facet_grid(var_group ~ ., scales = "free_y", space = "free_y") +
  labs(
    title    = "What predicts a YC exit?",
    subtitle = "Odds ratios from logistic regression (Successful vs Failed). 1.0 = no effect.",
    x = "Odds ratio (log scale)", y = NULL, color = NULL,
    caption = "Reference levels: Industry = B2B; Team = 1–5; Region = Non-US; Era = 2005–2014. Excludes 500+ team bucket (perfect separation)."
  ) +
  theme_yc() +
  theme(legend.position = "top")

ggsave("outputs/figures/q1_logit_forest.png", p_forest, width = 10, height = 7, dpi = 200, bg = CREAM)

q2_industry_year <- read_csv("data/processed/q2_industry_year.csv", show_col_types = FALSE)
q2_geo_year      <- read_csv("data/processed/q2_geo_year.csv", show_col_types = FALSE)
q2_batch_size    <- read_csv("data/processed/q2_batch_size.csv", show_col_types = FALSE)

industry_levels <- q2_industry_year |>
  group_by(industry) |>
  summarise(total = sum(n)) |>
  arrange(desc(total)) |>
  pull(industry)

p_industry_evo <- q2_industry_year |>
  filter(industry != "Unspecified", batch_year >= 2007, batch_year <= 2024) |>
  mutate(industry = factor(industry, levels = industry_levels)) |>
  ggplot(aes(x = batch_year, y = share, fill = industry)) +
  geom_area(color = CREAM, linewidth = 0.2) +
  scale_y_continuous(labels = label_percent(), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(2008, 2024, 4), expand = c(0, 0)) +
  scale_fill_manual(values = industry_palette) +
  labs(
    title    = "B2B has eaten YC — and Consumer has lost the throne",
    subtitle = "Share of each YC batch by primary industry, 2007–2024",
    x = "Batch year", y = "Share of batch", fill = "Industry",
    caption = sprintf("Source: ycombinator.com/companies (scraped %s).", format(Sys.Date(), "%b %Y"))
  ) +
  theme_yc() +
  theme(panel.grid.major.x = element_blank())

ggsave("outputs/figures/q2_industry_evolution.png", p_industry_evo, width = 11, height = 6, dpi = 200, bg = CREAM)

p_batch_size <- q2_batch_size |>
  filter(batch_year >= 2005, batch_year <= 2024) |>
  ggplot(aes(x = batch_year, y = batch_size)) +
  geom_col(fill = YC_ORANGE, alpha = 0.85) +
  geom_text(aes(label = batch_size), vjust = -0.5, size = 3, color = YC_DARK) +
  scale_x_continuous(breaks = seq(2005, 2024, 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "YC kept getting bigger — then ChatGPT happened",
    subtitle = "Companies funded per batch year, 2005–2024",
    x = "Batch year", y = "Companies funded",
    caption = "Mid-2022 onward reflects post-ZIRP recalibration and AI-cohort surge."
  ) +
  theme_yc()

ggsave("outputs/figures/q2_batch_size.png", p_batch_size, width = 11, height = 5.5, dpi = 200, bg = CREAM)

q3_wide <- read_csv("data/processed/q3_era_wide.csv", show_col_types = FALSE)

p_era_shift <- q3_wide |>
  tidyr::pivot_longer(c(`p_2005-2014`, `p_2015-2021`),
                      names_to = "era", values_to = "rate") |>
  mutate(era = str_remove(era, "^p_")) |>
  ggplot(aes(x = era, y = rate, group = industry, color = industry)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = sprintf("%s  %.0f%%", industry, rate * 100)),
            data = ~ filter(.x, era == "2015-2021"),
            hjust = 0, nudge_x = 0.08, direction = "y",
            min.segment.length = 0, segment.color = GREY_MED, segment.size = 0.3,
            size = 4, fontface = "bold", seed = 42) +
  scale_y_continuous(labels = label_percent(), limits = c(0.2, 0.65)) +
  scale_color_manual(values = industry_palette) +
  expand_limits(x = 2.7) +
  labs(
    title    = "The 2010s playbook stopped paying off",
    subtitle = "Exit hit rate for mature YC companies, by industry and era",
    x = "Batch era", y = "Exit hit rate", color = NULL,
    caption = "Both eras restricted to batches ≥5 years old to limit censoring bias.\nIndustries with <15 mature companies in either era excluded."
  ) +
  theme_yc() +
  theme(legend.position = "none")

ggsave("outputs/figures/q3_era_shift.png", p_era_shift, width = 9, height = 6, dpi = 200, bg = CREAM)

message("Static figures rendered:")
message(paste(" -", list.files("outputs/figures", full.names = TRUE), collapse = "\n"))
