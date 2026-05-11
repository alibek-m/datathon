suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
  library(ggplot2); library(scales); library(forcats); library(broom)
  library(ggrepel); library(patchwork)
  library(ggbeeswarm); library(ggridges); library(ggalluvial)
  library(treemapify); library(ggforce); library(ggdist)
})

source("R/utils.R")
source("R/palette.R")

NOT_STARTED <- c("Summer 2026", "Fall 2026")
clean       <- read_csv("data/processed/yc_clean.csv", show_col_types = FALSE) |>
  filter(!batch %in% NOT_STARTED)
ind_long    <- read_csv("data/processed/yc_industries_long.csv", show_col_types = FALSE)
q1_industry <- read_csv("data/processed/q1_industry.csv", show_col_types = FALSE)
q1_team     <- read_csv("data/processed/q1_team.csv", show_col_types = FALSE)
q1_logit    <- read_csv("data/processed/q1_logit.csv", show_col_types = FALSE)
q2_iy       <- read_csv("data/processed/q2_industry_year.csv", show_col_types = FALSE)
q3_era_long <- read_csv("data/processed/q3_era_long.csv", show_col_types = FALSE)
q3_wide     <- read_csv("data/processed/q3_era_wide.csv", show_col_types = FALSE)

OUT  <- "outputs/figures/gallery"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

save_png <- function(p, name, w = 11, h = 6) {
  path <- file.path(OUT, paste0(name, ".png"))
  ggsave(path, p, width = w, height = h, dpi = 200, bg = CREAM)
  message("  saved: ", path)
  invisible(path)
}

mature <- clean |> filter(batch_year <= 2021, !is.na(outcome), !is.na(industry), industry != "Unspecified")

message("=== Q1: industry hit rate ===")

# 1A — Lollipop (refined baseline)
q1_industry_clean <- q1_industry |> filter(group != "Unspecified") |> arrange(desc(p))
overall_rate <- with(mature, mean(outcome[outcome %in% c("Successful","Failed")] == "Successful"))

p_q1a <- q1_industry_clean |>
  mutate(group = fct_reorder(group, p)) |>
  ggplot(aes(x = group, y = p)) +
  geom_segment(aes(xend = group, y = 0, yend = p), color = GREY_MED, linewidth = 0.5) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.25, color = YC_DARK, linewidth = 0.5) +
  geom_point(aes(size = n_total), color = YC_ORANGE) +
  geom_text(aes(y = hi, label = sprintf("%.0f%% (n=%d)", p * 100, n_total)),
            hjust = -0.15, size = 4.2, color = YC_DARK, fontface = "bold") +
  scale_y_continuous(labels = label_percent(), limits = c(0, 0.85), expand = expansion(mult = c(0, 0.05))) +
  scale_size_continuous(range = c(3, 10), guide = "none") +
  coord_flip() +
  labs(title = "Industry exit hit rate — lollipop with CIs",
       subtitle = "Mature batches only; bubble = cohort size; bars = 95% Wilson CI",
       x = NULL, y = "Exit hit rate") +
  theme_yc()
save_png(p_q1a, "q1a_industry_lollipop")

# 1B — Diverging from grand mean
p_q1b <- q1_industry_clean |>
  mutate(group = fct_reorder(group, p),
         delta = p - overall_rate,
         sign  = if_else(delta >= 0, "above", "below")) |>
  ggplot(aes(x = delta, y = group, fill = sign)) +
  geom_col(width = 0.65) +
  geom_vline(xintercept = 0, color = YC_DARK, linewidth = 0.5) +
  geom_text(aes(label = sprintf("%+.0f pp", delta * 100),
                hjust = if_else(delta >= 0, -0.15, 1.15)),
            color = YC_DARK, fontface = "bold", size = 4.2) +
  scale_x_continuous(labels = label_percent(), expand = expansion(mult = 0.18)) +
  scale_fill_manual(values = c(above = INK_BLUE, below = TERRACOTTA), guide = "none") +
  labs(title = "Industries above & below the YC average",
       subtitle = sprintf("Each bar = percentage points above (blue) or below (terracotta) the overall %.0f%% exit rate", overall_rate * 100),
       x = "Difference from overall rate", y = NULL) +
  theme_yc()
save_png(p_q1b, "q1b_industry_diverging")

# 1C — Heatmap industry × era
era_industry <- mature |>
  mutate(era3 = case_when(batch_year <= 2010 ~ "2005-2010",
                          batch_year <= 2017 ~ "2011-2017",
                          batch_year <= 2021 ~ "2018-2021",
                          TRUE ~ NA_character_),
         era3 = factor(era3, levels = c("2005-2010","2011-2017","2018-2021"))) |>
  filter(!is.na(era3), outcome %in% c("Successful","Failed")) |>
  group_by(industry, era3) |>
  summarise(rate = mean(outcome == "Successful"), n = n(), .groups = "drop") |>
  filter(n >= 10)

p_q1c <- era_industry |>
  ggplot(aes(x = era3, y = fct_reorder(industry, rate, .fun = mean), fill = rate)) +
  geom_tile(color = CREAM, linewidth = 1.5) +
  geom_text(aes(label = sprintf("%.0f%%\nn=%d", rate * 100, n)),
            color = INK, size = 3.6, fontface = "bold", lineheight = 0.85) +
  scale_fill_gradient2(low = TERRACOTTA, mid = CREAM_GRID, high = INK_BLUE,
                       midpoint = overall_rate, labels = label_percent(),
                       name = "Hit rate") +
  labs(title = "Hit rate by industry & era — heatmap",
       subtitle = "Three eras × seven industries; cell color = exit rate, n = mature companies",
       x = NULL, y = NULL) +
  theme_yc() +
  theme(panel.grid = element_blank(),
        legend.position = "right",
        axis.text.x = element_text(face = "bold"))
save_png(p_q1c, "q1c_industry_era_heatmap", w = 10, h = 6)

# 1D — Stacked bar of outcome composition by industry
ind_outcome <- mature |>
  count(industry, outcome) |>
  group_by(industry) |>
  mutate(share = n / sum(n), total = sum(n)) |>
  ungroup() |>
  mutate(industry = fct_reorder(industry, total))

p_q1d <- ind_outcome |>
  ggplot(aes(x = share, y = industry, fill = outcome)) +
  geom_col() +
  geom_text(aes(label = if_else(share >= 0.06, sprintf("%.0f%%", share * 100), "")),
            position = position_stack(vjust = 0.5), color = CREAM, fontface = "bold", size = 3.8) +
  scale_x_continuous(labels = label_percent(), expand = c(0, 0)) +
  scale_fill_manual(values = outcome_palette) +
  labs(title = "Outcome mix per industry — 100% stacked bar",
       subtitle = "Mature batches; ordered by total cohort size",
       x = NULL, y = NULL, fill = NULL) +
  theme_yc() +
  theme(legend.position = "top")
save_png(p_q1d, "q1d_industry_stacked_outcome", w = 11, h = 5.5)

# 1E — Treemap of all mature companies, grouped by industry & colored by outcome
tree_data <- mature |>
  count(industry, outcome) |>
  group_by(industry) |>
  mutate(industry_n = sum(n)) |>
  ungroup() |>
  mutate(industry = fct_reorder(industry, -industry_n))

p_q1e <- tree_data |>
  ggplot(aes(area = n, fill = outcome, subgroup = industry)) +
  geom_treemap(color = CREAM, size = 4) +
  geom_treemap_subgroup_border(color = CREAM, size = 4) +
  geom_treemap_subgroup_text(place = "topleft", grow = FALSE, alpha = 0.9,
                             color = CREAM, fontface = "bold", padding.x = grid::unit(4, "mm"),
                             padding.y = grid::unit(3, "mm"), min.size = 14) +
  geom_treemap_text(aes(label = if_else(n >= 30, paste0(outcome, "\n", n), "")),
                    color = CREAM, place = "centre", size = 11, fontface = "bold", lineheight = 0.85) +
  scale_fill_manual(values = outcome_palette) +
  labs(title = "Every mature YC company — treemap",
       subtitle = "Block area = number of companies; sub-blocks colored by outcome",
       fill = NULL) +
  theme_yc() +
  theme(legend.position = "top")
save_png(p_q1e, "q1e_industry_treemap", w = 11, h = 6.5)

message("=== Q1: team size ===")

# 2A — Bar with CIs (refined baseline)
p_q2a <- q1_team |>
  filter(!is.na(group)) |>
  mutate(group = factor(group, levels = c("1-5","6-10","11-25","26-100","101-500","500+"))) |>
  ggplot(aes(x = group, y = p)) +
  geom_col(fill = YC_ORANGE, alpha = 0.85, width = 0.6) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2, color = YC_DARK, linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.0f%%", p * 100)),
            vjust = -0.5, color = YC_DARK, fontface = "bold", size = 4.6,
            position = position_nudge(y = 0.05)) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1.1), expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Survival climbs steeply with team size",
       subtitle = "Exit hit rate by current team size",
       x = "Team size (current)", y = "Exit hit rate") +
  theme_yc()
save_png(p_q2a, "q2a_team_bars")

# 2B — Stacked bar of full outcome composition by team size
team_outcome <- clean |>
  filter(batch_year <= 2021, !is.na(outcome), !is.na(team_bucket)) |>
  count(team_bucket, outcome) |>
  group_by(team_bucket) |>
  mutate(share = n / sum(n), total = sum(n)) |>
  ungroup()

p_q2b <- team_outcome |>
  ggplot(aes(x = team_bucket, y = share, fill = outcome)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = if_else(share >= 0.05, sprintf("%.0f%%", share * 100), "")),
            position = position_stack(vjust = 0.5), color = CREAM, fontface = "bold", size = 4) +
  geom_text(aes(x = team_bucket, y = 1.03, label = sprintf("n=%d", total)),
            data = ~distinct(., team_bucket, total), inherit.aes = FALSE,
            color = INK, size = 3.4, vjust = 0) +
  scale_y_continuous(labels = label_percent(), expand = expansion(mult = c(0, 0.06))) +
  scale_fill_manual(values = outcome_palette) +
  labs(title = "Outcome mix per team-size bucket",
       subtitle = "Mature batches; bigger teams have more Successful exits and fewer Failures",
       x = "Team size", y = NULL, fill = NULL) +
  theme_yc() +
  theme(legend.position = "top")
save_png(p_q2b, "q2b_team_stacked", w = 11, h = 5.5)

# 2C — Beeswarm of company-level team_size by outcome
swarm <- clean |>
  filter(batch_year <= 2021, !is.na(team_size), team_size > 0, team_size < 1500,
         outcome %in% c("Successful","Failed"))

p_q2c <- swarm |>
  ggplot(aes(x = outcome, y = team_size, color = outcome)) +
  ggbeeswarm::geom_quasirandom(alpha = 0.45, size = 1.4, width = 0.35) +
  stat_summary(fun = median, geom = "crossbar", width = 0.4, color = YC_DARK, linewidth = 0.5) +
  scale_y_log10(breaks = c(1, 5, 10, 25, 100, 500, 1500),
                labels = label_comma()) +
  scale_color_manual(values = outcome_palette[c("Successful","Failed")], guide = "none") +
  labs(title = "Each dot is a company — beeswarm by outcome",
       subtitle = "Team size on log scale; black bar = median",
       x = NULL, y = "Team size (log scale)") +
  theme_yc() +
  theme(panel.grid.major.y = element_line(color = GREY_LIGHT))
save_png(p_q2c, "q2c_team_beeswarm", w = 8.5, h = 6)

# 2D — Connected dot/dumbbell of pp delta from grand mean per team bucket
team_grand <- clean |>
  filter(batch_year <= 2021, !is.na(outcome), !is.na(team_bucket),
         outcome %in% c("Successful", "Failed")) |>
  group_by(team_bucket) |>
  summarise(rate = mean(outcome == "Successful"),
            n_total = n(), .groups = "drop") |>
  mutate(team_bucket = factor(team_bucket,
                              levels = c("1-5","6-10","11-25","26-100","101-500","500+")))
team_overall <- mean(clean$outcome[clean$batch_year <= 2021 &
                                   clean$outcome %in% c("Successful","Failed")] == "Successful")

p_q2d <- team_grand |>
  ggplot(aes(x = team_bucket, y = rate, group = 1)) +
  geom_hline(yintercept = team_overall, color = YC_DARK, linetype = "dashed", linewidth = 0.5) +
  annotate("text", x = 1.2, y = team_overall + 0.04,
           label = sprintf("Overall: %.0f%%", team_overall * 100),
           color = YC_DARK, fontface = "italic", size = 3.6, hjust = 0) +
  scale_x_discrete() +
  geom_line(color = YC_ORANGE, linewidth = 1.2, alpha = 0.6) +
  geom_point(aes(size = n_total), color = YC_ORANGE) +
  geom_text(aes(label = sprintf("%.0f%%", rate * 100)),
            vjust = -1.4, color = YC_DARK, fontface = "bold", size = 4.2) +
  geom_text(aes(label = sprintf("n=%d", n_total)),
            vjust = 2.4, color = GREY_MED, size = 3.2) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1.1), expand = expansion(mult = 0)) +
  scale_size_continuous(range = c(4, 12), guide = "none") +
  labs(title = "Hit rate climbs with team size — connected dots",
       subtitle = "Dot size = cohort count; dashed line = overall mean",
       x = "Team size", y = "Exit hit rate") +
  theme_yc()
save_png(p_q2d, "q2d_team_connected_dots")

message("=== Q1: logit forest ===")

# 3A — Forest plot (refined baseline)
forest_df <- q1_logit |>
  filter(term != "(Intercept)",
         !str_detect(term, "team_bucket500\\+"),
         !is.na(conf.low), !is.na(conf.high), conf.high < 100) |>
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
  )

p_q3a <- forest_df |>
  ggplot(aes(x = estimate, y = label, color = sig)) +
  geom_vline(xintercept = 1, color = GREY_MED, linetype = "dashed") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.25, linewidth = 0.6) +
  geom_point(size = 3) +
  scale_x_log10(breaks = c(0.1, 0.25, 0.5, 1, 2, 4, 10, 30)) +
  scale_color_manual(values = c("n.s." = WARM_GREY, "p < 0.05" = INK_BLUE)) +
  facet_grid(var_group ~ ., scales = "free_y", space = "free_y") +
  labs(title = "Forest plot — odds ratios",
       subtitle = "Baseline: B2B / 1–5 / Non-US / 2005–2014",
       x = "Odds ratio (log scale)", y = NULL, color = NULL) +
  theme_yc() +
  theme(legend.position = "top")
save_png(p_q3a, "q3a_logit_forest", w = 10, h = 7)

# 3B — Bar chart of effect sizes (positive/negative around 1.0 baseline = log-OR)
p_q3b <- forest_df |>
  mutate(log_or = log(estimate),
         direction = if_else(log_or >= 0, "increases odds", "decreases odds")) |>
  ggplot(aes(x = log_or, y = label, fill = direction)) +
  geom_col(width = 0.65) +
  geom_vline(xintercept = 0, color = YC_DARK) +
  geom_text(aes(label = sprintf("%.2f×", estimate),
                hjust = if_else(log_or >= 0, -0.15, 1.15)),
            color = YC_DARK, fontface = "bold", size = 3.8) +
  scale_x_continuous(expand = expansion(mult = 0.2),
                     labels = function(x) paste0(round(exp(x), 2), "×")) +
  scale_fill_manual(values = c("increases odds" = INK_BLUE, "decreases odds" = TERRACOTTA), name = NULL) +
  facet_grid(var_group ~ ., scales = "free_y", space = "free_y") +
  labs(title = "Diverging bars — odds ratio relative to 1×",
       subtitle = "Right of zero: more likely to exit. Left: less likely.",
       x = "Odds ratio", y = NULL) +
  theme_yc() +
  theme(legend.position = "top")
save_png(p_q3b, "q3b_logit_diverging", w = 10, h = 7)

message("=== Q2: industry evolution ===")

evo <- q2_iy |>
  filter(industry != "Unspecified", batch_year >= 2007, batch_year <= 2024)
ind_order <- evo |> group_by(industry) |> summarise(t = sum(n)) |> arrange(desc(t)) |> pull(industry)

# 4A — Stacked area (refined baseline)
p_q4a <- evo |>
  mutate(industry = factor(industry, levels = rev(ind_order))) |>
  ggplot(aes(x = batch_year, y = share, fill = industry)) +
  geom_area(color = CREAM, linewidth = 0.2) +
  scale_y_continuous(labels = label_percent(), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(2008, 2024, 4), expand = c(0, 0)) +
  scale_fill_manual(values = industry_palette) +
  labs(title = "Industry share over time — stacked area",
       subtitle = "Each batch year = 100%; B2B has eaten YC since 2014",
       x = "Batch year", y = "Share of batch", fill = NULL) +
  theme_yc() +
  theme(panel.grid.major.x = element_blank(), legend.position = "right")
save_png(p_q4a, "q4a_evo_stacked_area")

# 4B — Stream graph (centered baseline)
stream_df <- evo |>
  group_by(batch_year) |>
  mutate(total = sum(n)) |>
  ungroup() |>
  mutate(industry = factor(industry, levels = ind_order))

p_q4b <- stream_df |>
  ggplot(aes(x = batch_year, y = n, fill = industry)) +
  geom_area(position = "stack", color = CREAM, linewidth = 0.3) +
  scale_y_continuous(labels = label_comma(), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(2008, 2024, 4), expand = c(0, 0)) +
  scale_fill_manual(values = industry_palette) +
  labs(title = "Absolute count of companies per batch year",
       subtitle = "Color shows industry composition over time",
       x = "Batch year", y = "Companies funded", fill = NULL) +
  theme_yc() +
  theme(panel.grid.major.x = element_blank(), legend.position = "right")
save_png(p_q4b, "q4b_evo_absolute_stack")

# 4C — Faceted small multiples
p_q4c <- evo |>
  filter(industry %in% setdiff(ind_order, "Government")) |>
  mutate(industry = factor(industry, levels = ind_order)) |>
  ggplot(aes(x = batch_year, y = share, color = industry, fill = industry)) +
  geom_area(alpha = 0.25) +
  geom_line(linewidth = 1) +
  facet_wrap(~ industry, ncol = 4) +
  scale_y_continuous(labels = label_percent()) +
  scale_x_continuous(breaks = seq(2008, 2024, 6)) +
  scale_color_manual(values = industry_palette, guide = "none") +
  scale_fill_manual(values = industry_palette, guide = "none") +
  labs(title = "Each industry's share over time — small multiples",
       subtitle = "Same Y axis, easier to compare trajectories",
       x = "Batch year", y = "Share of batch") +
  theme_yc() +
  theme(strip.background = element_rect(fill = CREAM_GRID, color = NA))
save_png(p_q4c, "q4c_evo_small_multiples", w = 11, h = 6)

# 4D — Heatmap industry × year
p_q4d <- evo |>
  ggplot(aes(x = batch_year, y = fct_reorder(industry, share, .fun = sum), fill = share)) +
  geom_tile(color = CREAM, linewidth = 0.5) +
  geom_text(aes(label = if_else(share >= 0.10, sprintf("%.0f%%", share * 100), "")),
            color = CREAM, size = 3.2, fontface = "bold") +
  scale_x_continuous(breaks = seq(2008, 2024, 2), expand = c(0, 0)) +
  scale_fill_gradient(low = CREAM_GRID, high = INK_BLUE, labels = label_percent(),
                      name = "Share") +
  labs(title = "Industry share heatmap by year",
       subtitle = "Darker = bigger share of that batch year",
       x = "Batch year", y = NULL) +
  theme_yc() +
  theme(panel.grid = element_blank(), axis.text.x = element_text(face = "bold"))
save_png(p_q4d, "q4d_evo_heatmap", w = 11, h = 6)

# 4E — Bump chart of rank
bump <- evo |>
  group_by(batch_year) |>
  mutate(rank = rank(-share, ties.method = "first")) |>
  ungroup() |>
  filter(industry %in% ind_order[1:6])

p_q4e <- bump |>
  ggplot(aes(x = batch_year, y = rank, color = industry, group = industry)) +
  geom_line(linewidth = 1.4, alpha = 0.85) +
  geom_point(size = 3.5) +
  scale_y_reverse(breaks = 1:6, labels = paste0("#", 1:6)) +
  scale_x_continuous(breaks = seq(2008, 2024, 2)) +
  scale_color_manual(values = industry_palette) +
  labs(title = "Industry rank by share, year-by-year — bump chart",
       subtitle = "#1 at top; lines crossing = ranks switching",
       x = "Batch year", y = "Rank by share", color = NULL) +
  theme_yc() +
  theme(panel.grid.major.y = element_line(color = GREY_LIGHT, linetype = "dotted"),
        legend.position = "right")
save_png(p_q4e, "q4e_evo_bump", w = 11, h = 6)

message("=== Q2: geography ===")

# 5A — Top countries dumbbell (early vs late era)
geo_era <- clean |>
  filter(!is.na(country), country != "Remote", batch_year >= 2007, batch_year <= 2024) |>
  mutate(era = if_else(batch_year <= 2014, "2007-2014", "2015-2024"))
top_countries <- geo_era |> count(country, sort = TRUE) |> slice_max(n, n = 12) |> pull(country)

dumbbell <- geo_era |>
  filter(country %in% top_countries) |>
  count(country, era) |>
  pivot_wider(names_from = era, values_from = n, values_fill = 0) |>
  mutate(total = `2007-2014` + `2015-2024`,
         country = fct_reorder(country, total))

p_q5a <- dumbbell |>
  ggplot(aes(y = country)) +
  geom_segment(aes(x = `2007-2014`, xend = `2015-2024`, yend = country),
               color = WARM_GREY, linewidth = 1.2) +
  geom_point(aes(x = `2007-2014`), color = WARM_GREY, size = 4) +
  geom_point(aes(x = `2015-2024`), color = TERRACOTTA, size = 4) +
  geom_text(aes(x = `2015-2024`, label = `2015-2024`),
            hjust = -0.5, color = INK, fontface = "bold", size = 3.8) +
  geom_text(aes(x = `2007-2014`, label = `2007-2014`),
            hjust = 1.5, color = WARM_GREY_DK, fontface = "bold", size = 3.8) +
  scale_x_continuous(labels = label_comma(), limits = c(0, NA), expand = expansion(mult = 0.15)) +
  labs(title = "Where YC companies are founded — old era vs new era",
       subtitle = "Grey dot = 2007-2014; terracotta dot = 2015-2024 (top 12 countries by total)",
       x = "Number of companies", y = NULL) +
  theme_yc() +
  theme(panel.grid.major.y = element_line(color = GREY_LIGHT, linetype = "dotted"))
save_png(p_q5a, "q5a_geo_dumbbell", w = 11, h = 6.5)

# 5B — Stacked bar US vs Non-US over years
geo_share <- clean |>
  filter(!is.na(batch_year), batch_year >= 2007, batch_year <= 2024) |>
  mutate(geo = case_when(in_us ~ "USA", is.na(country) ~ "Unknown", TRUE ~ "Non-US")) |>
  count(batch_year, geo) |>
  group_by(batch_year) |>
  mutate(share = n / sum(n)) |>
  ungroup()

p_q5b <- geo_share |>
  mutate(geo = factor(geo, levels = c("USA", "Non-US", "Unknown"))) |>
  ggplot(aes(x = batch_year, y = share, fill = geo)) +
  geom_col(width = 0.85) +
  scale_y_continuous(labels = label_percent(), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(2008, 2024, 2)) +
  scale_fill_manual(values = c(USA = INK_BLUE, "Non-US" = TERRACOTTA, Unknown = WARM_GREY)) +
  labs(title = "US vs Non-US share of each batch",
       subtitle = "Non-US rose from <10% (2008) to ~30%+ (2020s)",
       x = "Batch year", y = "Share of batch", fill = NULL) +
  theme_yc() +
  theme(legend.position = "top")
save_png(p_q5b, "q5b_geo_us_vs_nonus", w = 11, h = 5.5)

# 5C — Top 10 countries treemap (current era)
top_recent <- clean |>
  filter(!is.na(country), country != "Remote", batch_year >= 2018) |>
  count(country, name = "n") |>
  slice_max(n, n = 15)

p_q5c <- top_recent |>
  mutate(label = sprintf("%s\n%d", country, n)) |>
  ggplot(aes(area = n, fill = n, label = label)) +
  geom_treemap(color = CREAM, size = 4) +
  geom_treemap_text(color = CREAM, place = "centre", grow = FALSE,
                    fontface = "bold", reflow = TRUE, size = 16) +
  scale_fill_gradient(low = "#E8B099", high = TERRACOTTA, labels = label_comma(), name = "Companies") +
  labs(title = "Where 2018+ batches come from — treemap",
       subtitle = "Top 15 countries by company count, all batches since 2018") +
  theme_yc()
save_png(p_q5c, "q5c_geo_treemap", w = 11, h = 6.5)

message("=== Q3: era shift ===")

# 6A — Slope chart (refined baseline)
slope <- q3_era_long |> filter(!is.na(industry))
p_q6a <- slope |>
  ggplot(aes(x = era_q3, y = p, group = industry, color = industry)) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 4) +
  geom_text_repel(data = ~filter(.x, era_q3 == "2015-2021"),
                  aes(label = sprintf("%s  %.0f%%", industry, p * 100)),
                  hjust = 0, nudge_x = 0.08, direction = "y",
                  min.segment.length = 0, segment.color = GREY_MED, segment.size = 0.3,
                  size = 4, fontface = "bold", seed = 42) +
  scale_y_continuous(labels = label_percent(), limits = c(0.2, 0.65)) +
  scale_color_manual(values = industry_palette, guide = "none") +
  expand_limits(x = 2.7) +
  labs(title = "Industry hit rate by era — slope chart",
       subtitle = "2005-2014 vs 2015-2021; mature batches only",
       x = "Batch era", y = "Exit hit rate") +
  theme_yc() +
  theme(panel.grid.major.x = element_blank())
save_png(p_q6a, "q6a_era_slope", w = 10, h = 6)

# 6B — Dumbbell (industries on Y axis, 2 dots per row)
p_q6b <- q3_wide |>
  mutate(industry = fct_reorder(industry, `p_2015-2021`)) |>
  ggplot(aes(y = industry)) +
  geom_segment(aes(x = `p_2005-2014`, xend = `p_2015-2021`, yend = industry),
               color = WARM_GREY, linewidth = 1.2) +
  geom_point(aes(x = `p_2005-2014`), color = WARM_GREY, size = 5) +
  geom_point(aes(x = `p_2015-2021`), color = TERRACOTTA, size = 5) +
  geom_text(aes(x = `p_2005-2014`, label = sprintf("%.0f%%", `p_2005-2014` * 100)),
            hjust = 1.4, color = WARM_GREY_DK, fontface = "bold", size = 3.8) +
  geom_text(aes(x = `p_2015-2021`, label = sprintf("%.0f%%", `p_2015-2021` * 100)),
            hjust = -0.4, color = INK, fontface = "bold", size = 3.8) +
  scale_x_continuous(labels = label_percent(), limits = c(0.22, 0.65), expand = expansion(mult = 0.05)) +
  labs(title = "Industry hit rate by era — dumbbell",
       subtitle = "Grey = 2005-2014. Terracotta = 2015-2021. Distance = drop.",
       x = "Exit hit rate", y = NULL) +
  theme_yc() +
  theme(panel.grid.major.y = element_line(color = GREY_LIGHT, linetype = "dotted"))
save_png(p_q6b, "q6b_era_dumbbell", w = 10, h = 5.5)

# 6C — Diverging delta bars
p_q6c <- q3_wide |>
  mutate(delta = `p_2015-2021` - `p_2005-2014`,
         industry = fct_reorder(industry, delta)) |>
  ggplot(aes(x = delta, y = industry, fill = delta < 0)) +
  geom_col(width = 0.65) +
  geom_vline(xintercept = 0, color = YC_DARK) +
  geom_text(aes(label = sprintf("%+.1f pp", delta * 100),
                hjust = if_else(delta >= 0, -0.15, 1.15)),
            color = YC_DARK, fontface = "bold", size = 4.4) +
  scale_x_continuous(labels = label_percent(), expand = expansion(mult = 0.2)) +
  scale_fill_manual(values = c(`TRUE` = TERRACOTTA, `FALSE` = INK_BLUE), guide = "none") +
  labs(title = "Era-shift in hit rate — diverging bars",
       subtitle = "Δ = 2015-2021 rate minus 2005-2014 rate",
       x = "Change in hit rate", y = NULL) +
  theme_yc()
save_png(p_q6c, "q6c_era_diverging", w = 9.5, h = 5.5)

# 6D — Side-by-side grouped bars with CIs
p_q6d <- q3_era_long |>
  mutate(industry = fct_reorder(industry, p, .fun = mean, .desc = TRUE)) |>
  ggplot(aes(x = industry, y = p, fill = era_q3)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = lo, ymax = hi),
                position = position_dodge(0.7), width = 0.18, color = YC_DARK) +
  geom_text(aes(label = sprintf("%.0f%%", p * 100)),
            position = position_dodge(0.7), vjust = -0.5, fontface = "bold", size = 3.8) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 0.7), expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c(`2005-2014` = WARM_GREY, `2015-2021` = TERRACOTTA)) +
  labs(title = "Two eras side-by-side — grouped bars with 95% CIs",
       subtitle = "Sorted by mean hit rate; error bars are Wilson intervals",
       x = NULL, y = "Exit hit rate", fill = "Era") +
  theme_yc() +
  theme(legend.position = "top")
save_png(p_q6d, "q6d_era_grouped_bars", w = 11, h = 6)

message("\nAll done. Files in: ", OUT)
list.files(OUT) |> sort() |> writeLines()
