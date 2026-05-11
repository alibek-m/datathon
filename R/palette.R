# Editorial palette — cream paper, ink-blue + terracotta anchors.
# Designed to read cleanly on a cream slide background (#F5F1EA).
# Sourced by both R/05_viz_static.R and R/07_viz_gallery.R.

CREAM         <- "#F5F1EA"   # paper background
CREAM_GRID    <- "#E8E0D0"   # subtle gridline tone
INK           <- "#1F1A17"   # body text / dark elements
INK_LIGHT     <- "#5A554F"   # subtitle / caption text

TERRACOTTA    <- "#B8472A"   # primary accent
INK_BLUE      <- "#2C5F7C"   # secondary accent
WARM_GREY     <- "#A89E92"   # neutral mid
WARM_GREY_DK  <- "#6B6259"   # darker neutral

TEAL          <- "#4A8B8C"
MUSTARD       <- "#D4A744"
PLUM          <- "#7A4869"
SAGE          <- "#6B8E5E"
SLATE         <- "#5A6470"

# Backward-compat aliases — old code references these names.
YC_ORANGE  <- TERRACOTTA
YC_DARK    <- INK
GREY_MED   <- WARM_GREY
GREY_LIGHT <- CREAM_GRID

industry_palette <- c(
  "B2B"                          = INK_BLUE,
  "Consumer"                     = TERRACOTTA,
  "Healthcare"                   = SAGE,
  "Fintech"                      = PLUM,
  "Industrials"                  = MUSTARD,
  "Real Estate and Construction" = SLATE,
  "Education"                    = TEAL,
  "Government"                   = WARM_GREY_DK
)

outcome_palette <- c(
  Successful = INK_BLUE,
  Surviving  = WARM_GREY,
  Failed     = TERRACOTTA
)

# For diverging bars: above = blue (good / increase), below = terracotta (decline)
direction_palette <- c(above = INK_BLUE, below = TERRACOTTA,
                       `increases odds` = INK_BLUE, `decreases odds` = TERRACOTTA,
                       `TRUE` = TERRACOTTA, `FALSE` = INK_BLUE)

theme_yc <- function(base = 13) {
  ggplot2::theme_minimal(base_size = base) +
    ggplot2::theme(
      plot.background    = ggplot2::element_rect(fill = CREAM, color = NA),
      panel.background   = ggplot2::element_rect(fill = CREAM, color = NA),
      legend.background  = ggplot2::element_rect(fill = CREAM, color = NA),
      legend.key         = ggplot2::element_rect(fill = CREAM, color = NA),
      strip.background   = ggplot2::element_rect(fill = CREAM_GRID, color = NA),
      plot.title         = ggplot2::element_text(face = "bold", size = base + 4, color = INK,
                                                 margin = ggplot2::margin(b = 4)),
      plot.subtitle      = ggplot2::element_text(size = base, color = INK_LIGHT,
                                                 margin = ggplot2::margin(b = 14)),
      plot.caption       = ggplot2::element_text(size = base - 4, color = INK_LIGHT, hjust = 0,
                                                 margin = ggplot2::margin(t = 14)),
      axis.title         = ggplot2::element_text(face = "bold", color = INK),
      axis.text          = ggplot2::element_text(color = INK),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = CREAM_GRID, linewidth = 0.4),
      strip.text         = ggplot2::element_text(face = "bold", size = base - 1, color = INK)
    )
}
