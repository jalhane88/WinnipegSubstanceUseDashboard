# 01_data_processing.R

#---- Install and load required packages ----
install.packages(c("shiny", "tidyverse", "lubridate", "leaflet", "plotly", "httr", "jsonlite"))

# 01_data_processing.R

# ---- 1. Load Libraries and Define URLs ----
library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)

naloxone_url <- "https://data.winnipeg.ca/resource/qd6b-q49i.json"
substance_use_url <- "https://data.winnipeg.ca/resource/6x82-bz5y.json"

# ---- 2. Download and Parse Data ----
naloxone_raw <- httr::GET(naloxone_url) |>
  httr::content(as = "text", encoding = "UTF-8") |>
  jsonlite::fromJSON(flatten = TRUE) |>
  as_tibble()

substance_use_raw <- httr::GET(substance_use_url) |>
  httr::content(as = "text", encoding = "UTF-8") |>
  jsonlite::fromJSON(flatten = TRUE) |>
  as_tibble()

# ---- 3. Initial Inspection ----
cat("--- Naloxone Data Glimpse ---\n")
glimpse(naloxone_raw)

cat("\n--- Substance Use Data Glimpse ---\n")
glimpse(substance_use_raw)

