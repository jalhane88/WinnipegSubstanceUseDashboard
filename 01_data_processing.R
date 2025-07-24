# 01_data_processing.R

# ---- 1. Load Libraries and Define URLs ----
library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)
library(janitor) # Let's move this library call here for organization

# Storing URLs in variables makes the code cleaner and easier to update.
# We set a high limit to ensure we get all the data, now and in the future.
naloxone_url <- "https://data.winnipeg.ca/resource/qd6b-q49i.json?$limit=200000"
substance_use_url <- "https://data.winnipeg.ca/resource/6x82-bz5y.json?$limit=200000" # <-- UPDATED

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
glimpse(substance_use_raw))

# ---- 4. Clean and Pre-process Data ----

# A great first step is to clean the column names to be valid R names
# (e.g. `naxolone_administrations` from the naloxone dataset is not standard)
# The janitor package is perfect for this.
naloxone_clean <- naloxone_raw |>
  clean_names() |> # a a| a
  rename(naloxone_administrations = naxolone_administrations) |> # Fix the typo in the original data source
  mutate(
    # Convert date from character to a proper datetime object
    dispatch_date = as_datetime(dispatch_date),
    # Convert character numbers to numeric
    naloxone_administrations = as.numeric(naloxone_administrations),
    patient_number = as.numeric(patient_number)
  )

substance_use_clean <- substance_use_raw |>
  clean_names() |>
  mutate(
    dispatch_date = as_datetime(dispatch_date),
    patient_number = as.numeric(patient_number),
    # Convert substance and gender to factors (categorical data)
    substance = as.factor(substance),
    gender = as.factor(gender)
  )

# Let's inspect the cleaned data
cat("--- Cleaned Naloxone Data Glimpse ---\n")
glimpse(naloxone_clean)

cat("\n--- Cleaned Substance Use Data Glimpse ---\n")
glimpse(substance_use_clean)

# ---- 5. Aggregate Substance Data and Merge ----

# First, let's aggregate the substance use data.
# We group by the unique patient-incident identifiers.
# Then, for each patient, we paste all their substances into a single string.
substances_aggregated <- substance_use_clean |>
  group_by(incident_number, dispatch_date, patient_number) |>
  summarise(
    # Paste all substances, separated by a semicolon. Sort them for consistency.
    substances_involved = paste(sort(unique(substance)), collapse = "; "),
    .groups = "drop" # Ungroup after summarising
  )

# Now, we join the aggregated substances to the naloxone data.
# We will also bring across the age and gender from the substance use data,
# as it's likely more complete. We'll grab the first value we find for each patient.
patient_demographics <- substance_use_clean |>
  group_by(incident_number, dispatch_date, patient_number) |>
  summarise(
    age = first(age),
    gender = first(gender)
  )

# Perform the final join
# We start with the naloxone data and bring in the other information.
final_data <- naloxone_clean |>
  left_join(substances_aggregated, by = c("incident_number", "dispatch_date", "patient_number")) |>
  left_join(patient_demographics, by = c("incident_number", "dispatch_date", "patient_number"))

# Let's see our final, merged dataset!
glimpse(final_data)

# ---- 6. Final Selection and Save ----

# Add this line to create the directory if it doesn't already exist
dir.create("data", showWarnings = FALSE)

# We need to clean up the column names after the join.
# We'll keep the .y columns (from the substance_use data) and give them clean names.
app_data <- final_data |>
  select(
    # Keep these columns from the original naloxone data
    incident_number,
    dispatch_date,
    patient_number,
    naloxone_administrations,
    ward,
    neighbourhood_id,
    neighbourhood,
    # Keep these new columns from our joins
    substances_involved,
    # Rename the .y columns to be our main demographic columns
    age = age.y,
    gender = gender.y
  )

# Let's take one last look at our truly final data
glimpse(app_data)

# Now, save this clean data frame to a file in the 'data' folder
# The Shiny app will load this single file.
saveRDS(app_data, file = "data/app_data.rds")

# We can also save it as a CSV if we want to look at it in Excel
# write.csv(app_data, file = "data/app_data.csv", row.names = FALSE)

cat("\nData processing complete. `app_data.rds` saved to the data folder.\n")
