# Common setup for all chapters
# This file is sourced before each chapter

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(knitr)
library(kableExtra)
library(here)
library(grid)

# Source all R functions
source(here("R", "pattern_core.R"))
source(here("R", "pattern_plot.R"))
source(here("R", "pattern_notation.R"))
source(here("R", "measurement_io.R"))
source(here("R", "pattern_print.R"))

# Global knitr options
knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  fig.align = "center",
  fig.pos = "H",
  out.width = "100%",
  dpi = 300,
  dev = "cairo_pdf",  # Better PDF compatibility
  cache = FALSE
)

# Global ggplot theme for pattern drafting
theme_pattern <- function(base_size = 10) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      panel.grid.major = element_line(color = "#E0E0E0", linewidth = 0.2),
      panel.grid.minor = element_line(color = "#F5F5F5", linewidth = 0.1),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.title = element_text(size = base_size),
      plot.title = element_text(size = base_size + 4, face = "bold"),
      plot.subtitle = element_text(size = base_size),
      plot.caption = element_text(size = base_size - 2, color = "grey40"),
      plot.margin = margin(10, 10, 10, 10),
      legend.position = "none"
    )
}

# Set default theme
theme_set(theme_pattern())