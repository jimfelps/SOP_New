############# LIBRARIES AND STUFF ------------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(lubridate)
library(writexl)
library(readxl)
library(patchwork)
library(ggforce)
library(kableExtra)
library(glue)

theme_set(theme_light())

month <- format(Sys.Date(), "%B %Y")  # UPDATE

current_day <- format(Sys.Date(), "%Y-%m-%d")

current_month <- floor_date(Sys.Date(), unit = "month")

current_year <- year(current_month)

ad_colors <- c("#063858", "#186392", "#74b5de", "#bcd3e1")

labels_avq <- c(
  "CII/FT" = "CII/FT",
  "Simple" = "Simple\n < 100",
  "Moderate" = "Moderate\n < 200",
  "Complex" = "Complex\n < 400",
  "High Complexity" = "High\n Complexity\n < 800",
  "Very High Complexity" = "Very High\n Complexity\n 800+"
)

labels <- c(
  "CII/FT" = "CII/FT",
  "Simple (<=100)" = "Simple",
  "Moderate (101-200)" = "Moderate",
  "Complex (201-400)" = "Complex",
  "High Complex (401-800)" = "High\n Complexity",
  "Very High Complex (801+)" = "Very High\n Complexity"
)

oe_labels <- c(
  "No Engineering" = "No Engineering",
  "Simple" = "Simple",
  "Moderate" = "Moderate",
  "Complex" = "Complex",
  "High Complexity" = "High\n Complexity",
  "Very High Complexity" = "Very High\n Complexity"
)

bluescope_lightblue <- "#49b4e6"
bluescope_darkblue <- "#1e7cc1"
bluescope_darkgray <- "#7191a9"
bluescope_lightgray <- "#ffffff"
bluescope_white <- "#feffff"
custom_palette <- c(bluescope_lightblue, bluescope_darkgray)
text_label_size <- 5
text_vjust <- 1
title_text <- 20
subtitle_text <- 16
legend_text <- 14
point_alpha <- 0.9