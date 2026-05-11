# YC Datathon — What Makes a Winning Startup?

A data analysis of **5,880** companies across **46 batches** in the Y Combinator directory (Summer 2005 through Spring 2026 — excludes Summer/Fall 2026 which have only just opened applications). Three questions:

1. **What characteristics distinguish a winning YC startup?** Industry, location, team size, era.
2. **How has YC's portfolio evolved over 20 years?** Composition shifts in industry mix and geography.
3. **Has the recipe for success changed?** Industries that won 2005–2014 vs 2015–2021.

Built for the Lawrence University Datathon. Theme: storytelling with data.

## What's in the box

```
datathon/
├── R/
│   ├── 01_scrape_yc.R          # Algolia → JSON  (~50s, 1 req/sec)
│   ├── 02_clean_wrangle.R      # JSON → tidy CSV
│   ├── 03_eda.R                # sanity checks
│   ├── 04_analysis.R           # answer Q1, Q2, Q3 + logit
│   ├── 05_viz_static.R         # ggplot2 figures
│   ├── 06_viz_interactive.R    # plotly + reactable widgets
│   └── utils.R                 # shared helpers
├── data/
│   ├── raw/yc_raw_<date>.json  # untouched API response
│   ├── raw/scrape_meta.json    # scrape provenance
│   └── processed/
│       ├── yc_clean.csv        # the canonical company-level table
│       ├── yc_industries_long.csv
│       └── q1_*.csv, q2_*.csv, q3_*.csv  # analysis outputs
├── outputs/
│   ├── figures/*.png           # static plots
│   └── interactive/*.html      # plotly + reactable widgets
└── presentation/
    ├── slides.Rmd              # source
    ├── slides.html             # rendered Reveal.js deck (open this)
    └── theme.css               # YC palette + storytelling layout
```

## Reproducing the analysis

System dependencies: **R ≥ 4.5**, **pandoc ≥ 3.x** (for the slides).

```r
install.packages(c(
  "httr2", "jsonlite", "dplyr", "tidyr", "purrr", "stringr", "lubridate",
  "janitor", "readr", "ggplot2", "plotly", "htmlwidgets", "reactable",
  "scales", "broom", "ggrepel", "patchwork", "gt", "rmarkdown", "revealjs",
  "rprojroot"
))
```

Then run from the project root:

```bash
Rscript R/01_scrape_yc.R
Rscript R/02_clean_wrangle.R
Rscript R/03_eda.R
Rscript R/04_analysis.R
Rscript R/05_viz_static.R
Rscript R/06_viz_interactive.R
Rscript -e 'rmarkdown::render("presentation/slides.Rmd")'
```

The full pipeline takes about 2 minutes (most of it the polite scrape).

## Data source & ethics

- **Source:** [ycombinator.com/companies](https://www.ycombinator.com/companies) via its public Algolia search index (`YCCompany_production`, app `45BWZJ1SGC`).
- **Method:** the directory caps pagination at 1,000 results per query, so we facet by **batch** (49 batches, max 399 companies each) to retrieve the full set.
- **Rate limit:** 1 request/second, identifying User-Agent (`Lawrence-University-Datathon-Project (a.mamyrbay@gmail.com)`).
- **Scope:** only fields YC publishes for unauthenticated visitors. No PII beyond founder names already on YC's profile pages. No scraping behind auth walls (no Crunchbase, no LinkedIn).
- **Citation:** if you reuse `data/processed/yc_clean.csv`, please cite YC as source and note the scrape date in `data/raw/scrape_meta.json`.

## Methodological caveats

These are surfaced explicitly in the slide deck — they're the rubric's "methodological rigor" line.

- **Survivorship bias:** the YC directory may de-emphasize Inactive companies; true failure rate is likely higher than the 17.6% observed.
- **Censoring:** companies in batches younger than 5 years (2022–2026) haven't had time to be Acquired or go Public. All "exit hit rates" restrict to mature batches.
- **Self-reported labels:** Status, industry, team size are YC's classification, not audited.
- **Multi-tag industries:** primary industry is YC-assigned; the long-format `yc_industries_long.csv` exposes all tags for tag-level analysis.
- **Perfect separation:** the 500+ team-size bucket has 100% success rate among mature companies — excluded from the logistic regression to avoid divergent coefficients.

## Key findings (preview)

- **Fintech (52%) ≈ B2B (52%)** lead the exit hit rate among mature batches; **Consumer (34%)** trails.
- **USA (46%) ≈ Non-US (45%)** — geography barely matters once you're in YC.
- **Team size is the single strongest predictor**: 26–100 employees vs 1–5 → **7.1×** higher odds of exit (p<0.001).
- The **2015–2021 era exits at 0.71×** the rate of 2005–2014, controlling for everything — the 2010s really were special.
- YC's **portfolio has shifted dramatically**: Consumer was ~35% of batches in 2010, ~10% in 2024. **B2B SaaS now defines YC.**

See `presentation/slides.html` for the full story.

## Tooling

R 4.5 · `httr2` for the API · `dplyr` / `tidyr` / `purrr` / `stringr` for wrangling · `ggplot2` for static plots · `plotly` for interactive (stacked area, world map, slope chart) · `reactable` for the searchable company table · `revealjs` for the deck.
