suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(plotly)
  library(htmlwidgets)
  library(reactable)
  library(htmltools)
})

dir.create("outputs/interactive", recursive = TRUE, showWarnings = FALSE)

YC_ORANGE <- "#F26625"
YC_DARK   <- "#1F2937"

clean            <- read_csv("data/processed/yc_clean.csv", show_col_types = FALSE)
q2_industry_year <- read_csv("data/processed/q2_industry_year.csv", show_col_types = FALSE)
q2_country_year  <- read_csv("data/processed/q2_country_year.csv", show_col_types = FALSE)
q2_top_rate      <- read_csv("data/processed/q2_top_company_rate.csv", show_col_types = FALSE)
q3_era           <- read_csv("data/processed/q3_era_long.csv", show_col_types = FALSE)
q3_wide          <- read_csv("data/processed/q3_era_wide.csv", show_col_types = FALSE)

industry_levels <- q2_industry_year |>
  group_by(industry) |>
  summarise(total = sum(n)) |>
  arrange(desc(total)) |>
  pull(industry)

industry_palette <- c(
  "B2B"                          = "#3B82F6",
  "Consumer"                     = "#F26625",
  "Healthcare"                   = "#10B981",
  "Fintech"                      = "#8B5CF6",
  "Industrials"                  = "#F59E0B",
  "Real Estate and Construction" = "#EF4444",
  "Education"                    = "#06B6D4",
  "Government"                   = "#6B7280",
  "Unspecified"                  = "#D1D5DB"
)

evo_levels <- setdiff(industry_levels, "Unspecified")
evo_data <- q2_industry_year |>
  filter(industry != "Unspecified", batch_year >= 2007, batch_year <= 2024) |>
  mutate(industry = factor(industry, levels = rev(evo_levels)),
         tooltip = sprintf("Batch %d — %s\n%d companies (%.1f%% of batch)",
                           batch_year, industry, n, share * 100))

p_evo <- plot_ly(
  data = evo_data, x = ~batch_year, y = ~share, color = ~industry,
  colors = unname(industry_palette[levels(evo_data$industry)]),
  type = "scatter", mode = "none", stackgroup = "one",
  groupnorm = "percent", text = ~tooltip, hoverinfo = "text"
) |>
  layout(
    title = list(text = "<b>B2B has eaten YC — and Consumer has lost the throne</b>",
                 x = 0.02, xanchor = "left",
                 font = list(size = 18, color = YC_DARK)),
    xaxis = list(title = "Batch year", dtick = 2, gridcolor = "white"),
    yaxis = list(title = "Share of batch", ticksuffix = "%", gridcolor = "white"),
    legend = list(title = list(text = "<b>Industry</b>"), orientation = "v"),
    margin = list(t = 70, l = 70, r = 30, b = 60),
    paper_bgcolor = "white", plot_bgcolor = "#F9FAFB",
    hovermode = "closest"
  ) |>
  config(displaylogo = FALSE,
         modeBarButtonsToRemove = c("autoScale2d","lasso2d","select2d"))

saveWidget(p_evo, "outputs/interactive/q2_industry_evolution.html", selfcontained = TRUE)

iso_map <- c(
  "USA"="USA","United States"="USA","United States of America"="USA",
  "United Kingdom"="GBR","Canada"="CAN","India"="IND","Mexico"="MEX",
  "France"="FRA","Germany"="DEU","Brazil"="BRA","Spain"="ESP",
  "Argentina"="ARG","Singapore"="SGP","Australia"="AUS","Netherlands"="NLD",
  "Israel"="ISR","Japan"="JPN","Sweden"="SWE","Switzerland"="CHE",
  "Nigeria"="NGA","Kenya"="KEN","South Africa"="ZAF","Egypt"="EGY",
  "Indonesia"="IDN","Vietnam"="VNM","Pakistan"="PAK","Philippines"="PHL",
  "Colombia"="COL","Chile"="CHL","Peru"="PER","Estonia"="EST",
  "Ireland"="IRL","Italy"="ITA","Portugal"="PRT","Poland"="POL",
  "Denmark"="DNK","Norway"="NOR","Finland"="FIN","Belgium"="BEL",
  "Turkey"="TUR","South Korea"="KOR","China"="CHN","Hong Kong"="HKG",
  "Taiwan"="TWN","Russia"="RUS","Ukraine"="UKR","UAE"="ARE",
  "United Arab Emirates"="ARE","Saudi Arabia"="SAU","Bangladesh"="BGD",
  "Sri Lanka"="LKA","New Zealand"="NZL","Czech Republic"="CZE","Czechia"="CZE",
  "Romania"="ROU","Hungary"="HUN","Greece"="GRC","Lithuania"="LTU",
  "Latvia"="LVA","Bulgaria"="BGR","Croatia"="HRV","Slovenia"="SVN",
  "Ghana"="GHA","Uganda"="UGA","Tanzania"="TZA","Ethiopia"="ETH",
  "Morocco"="MAR","Tunisia"="TUN","Senegal"="SEN","Rwanda"="RWA",
  "Ecuador"="ECU","Uruguay"="URY","Bolivia"="BOL","Paraguay"="PRY",
  "Costa Rica"="CRI","Panama"="PAN","Guatemala"="GTM","Dominican Republic"="DOM",
  "Cuba"="CUB","Iceland"="ISL","Luxembourg"="LUX","Malta"="MLT",
  "Cyprus"="CYP","Slovakia"="SVK","Serbia"="SRB","Bosnia and Herzegovina"="BIH",
  "North Macedonia"="MKD","Albania"="ALB","Belarus"="BLR","Kazakhstan"="KAZ",
  "Uzbekistan"="UZB","Georgia"="GEO","Armenia"="ARM","Azerbaijan"="AZE",
  "Iran"="IRN","Iraq"="IRQ","Lebanon"="LBN","Jordan"="JOR",
  "Mongolia"="MNG","Nepal"="NPL","Cambodia"="KHM","Laos"="LAO",
  "Malaysia"="MYS","Thailand"="THA","Myanmar"="MMR","Bhutan"="BTN",
  "Austria"="AUT","Algeria"="DZA","Bahrain"="BHR","Namibia"="NAM",
  "Venezuela"="VEN","Zambia"="ZMB","Kyrgyzstan"="KGZ","Puerto Rico"="PRI",
  "Democratic Republic of the Congo"="COD","Ivory Coast"="CIV",
  "London"="GBR","Bengaluru"="IND"
)

country_counts <- clean |>
  filter(!is.na(country), country != "Remote") |>
  count(country, name = "n_companies") |>
  mutate(iso3 = unname(iso_map[country])) |>
  filter(!is.na(iso3))

unmapped <- clean |>
  filter(!is.na(country), country != "Remote") |>
  count(country, name = "n_companies") |>
  filter(!country %in% names(iso_map))
if (nrow(unmapped) > 0) {
  warning("Unmapped countries (", nrow(unmapped), "):")
  print(unmapped)
}

p_map <- plot_geo(country_counts) |>
  add_trace(
    z = ~n_companies, locations = ~iso3, text = ~country,
    color = ~n_companies, colors = c("#FFE4D6", "#F26625", "#7C2D12"),
    hovertemplate = "<b>%{text}</b><br>%{z} YC companies<extra></extra>",
    marker = list(line = list(color = "white", width = 0.5))
  ) |>
  colorbar(title = "Companies", thickness = 12, len = 0.6) |>
  layout(
    title = list(text = "<b>YC has gone global — but the US still dominates</b>",
                 x = 0.02, xanchor = "left",
                 font = list(size = 18, color = YC_DARK)),
    geo = list(
      showframe = FALSE, showcoastlines = TRUE,
      coastlinecolor = "#9CA3AF", projection = list(type = "natural earth"),
      showcountries = TRUE, countrycolor = "#E5E7EB",
      bgcolor = "white"
    ),
    margin = list(t = 70, l = 0, r = 0, b = 30)
  ) |>
  config(displaylogo = FALSE,
         modeBarButtonsToRemove = c("autoScale2d","lasso2d","select2d","hoverClosestGeo"))

saveWidget(p_map, "outputs/interactive/q2_global_map.html", selfcontained = TRUE)

slope_data <- q3_era |>
  mutate(industry = factor(industry, levels = sort(unique(industry)))) |>
  select(industry, era_q3, p, n_total, lo, hi) |>
  rename(rate = p)
slope_colors <- unname(industry_palette[levels(slope_data$industry)])

p_slope <- plot_ly() |>
  add_trace(
    data = slope_data,
    x = ~era_q3, y = ~rate, color = ~industry,
    colors = slope_colors,
    type = "scatter", mode = "lines+markers",
    line = list(width = 4),
    marker = list(size = 12, line = list(color = "white", width = 2)),
    text = ~sprintf("<b>%s</b><br>%s: %.0f%% (n=%d)<br>95%% CI: %.0f–%.0f%%",
                    industry, era_q3, rate * 100, n_total, lo * 100, hi * 100),
    hoverinfo = "text"
  ) |>
  layout(
    title = list(text = "<b>The 2010s playbook stopped paying off</b>",
                 x = 0.02, xanchor = "left",
                 font = list(size = 18, color = YC_DARK)),
    xaxis = list(title = "Batch era", showgrid = FALSE),
    yaxis = list(title = "Exit hit rate", tickformat = ".0%", range = c(0.2, 0.65)),
    hovermode = "closest", paper_bgcolor = "white", plot_bgcolor = "#F9FAFB",
    margin = list(t = 70, l = 70, r = 30, b = 60),
    showlegend = TRUE,
    legend = list(title = list(text = "<b>Industry</b>"))
  ) |>
  config(displaylogo = FALSE)

saveWidget(p_slope, "outputs/interactive/q3_era_slope.html", selfcontained = TRUE)

table_data <- clean |>
  select(name, batch, status, industry, country, team_size, top_company, one_liner, website) |>
  arrange(desc(top_company), desc(team_size), name)

status_badge <- function(value) {
  color <- switch(value,
                  "Acquired" = "#10B981",
                  "Public"   = "#3B82F6",
                  "Active"   = "#F26625",
                  "Inactive" = "#6B7280",
                  "#9CA3AF")
  div(style = sprintf("background:%s;color:white;padding:2px 8px;border-radius:10px;font-size:11px;font-weight:600;display:inline-block",
                      color), value)
}

tbl <- reactable(
  table_data,
  searchable = TRUE, filterable = TRUE, defaultPageSize = 15, pageSizeOptions = c(15, 50, 200),
  showSortable = TRUE,
  highlight = TRUE, striped = TRUE, compact = TRUE, bordered = FALSE,
  defaultColDef = colDef(headerStyle = list(background = "#F3F4F6", fontWeight = "700")),
  columns = list(
    name        = colDef(name = "Company", minWidth = 130, sticky = "left",
                         cell = function(value, index) {
                           url <- table_data$website[index]
                           if (!is.na(url) && nzchar(url)) {
                             tags$a(href = url, target = "_blank", value, style = "color:#1F2937;font-weight:600;text-decoration:none")
                           } else value
                         }),
    batch       = colDef(name = "Batch", maxWidth = 110),
    status      = colDef(name = "Status", maxWidth = 110, cell = function(v) if (is.na(v)) "—" else status_badge(v)),
    industry    = colDef(name = "Industry", maxWidth = 130),
    country     = colDef(name = "Country", maxWidth = 130),
    team_size   = colDef(name = "Team", maxWidth = 80, format = colFormat(separators = TRUE), align = "right"),
    top_company = colDef(name = "Top YC", maxWidth = 80, cell = function(v) if (isTRUE(v)) "★" else ""),
    one_liner   = colDef(name = "One-liner", minWidth = 240),
    website     = colDef(show = FALSE)
  ),
  theme = reactableTheme(
    borderColor = "#E5E7EB",
    stripedColor = "#F9FAFB",
    highlightColor = "#FFF7ED",
    cellPadding = "6px 10px",
    style = list(fontFamily = "system-ui, -apple-system, sans-serif", fontSize = "13px")
  )
)

table_html <- tagList(
  tags$div(style = "padding:16px;font-family:system-ui;",
           tags$h2("YC Company Explorer", style = "margin:0 0 4px 0;color:#1F2937"),
           tags$p(sprintf("All %s companies in the YC directory. Search, filter, and sort.",
                          format(nrow(table_data), big.mark = ",")),
                  style = "color:#6B7280;margin:0 0 16px 0;font-size:14px"),
           tbl)
)
save_html(table_html, "outputs/interactive/yc_explorer.html")

message("Interactive widgets rendered:")
message(paste(" -", list.files("outputs/interactive", full.names = TRUE), collapse = "\n"))
