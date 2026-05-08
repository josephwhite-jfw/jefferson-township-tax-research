# ------------------------------------------------------------------------------
# Script: match_parcel_tif_records.R
# Purpose: Match annual parcel files to Jefferson Township TIF records using
#          ParcelNumber and TaxYear.
#
# Notes:
#   This script is not part of the main pipeline because the raw parcel files are
#   large and are not stored in the repository. It is the parcel-matching
#   workflow used to create parcel-level TIF value checks.
#
# Expected raw files:
#   data/raw/Parcel20*.csv
#
# Inputs:
#   data/raw/Parcel20*.csv
#   data/clean/jefferson_tif_details_all_years.csv
#
# Outputs:
#   outputs/diagnostics/matched_parcel_tif_records.csv
#   outputs/diagnostics/unmatched_tif_records.csv
# ------------------------------------------------------------------------------

source(here::here("src", "00_setup.R"))

# ---- Helper functions --------------------------------------------------------

normalize_parcel_number <- function(x) {
  x %>%
    as.character() %>%
    str_replace("-00$", "") %>%
    str_replace_all("-", "") %>%
    str_trim()
}

process_parcel_file <- function(file_path) {
  tax_year <- stringr::str_extract(basename(file_path), "\\d{4}") %>%
    as.integer()
  
  readr::read_csv(
    file_path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  ) %>%
    janitor::clean_names() %>%
    transmute(
      parcel_number = normalize_parcel_number(parcel_id),
      tax_year = tax_year,
      tif_land_value = readr::parse_number(tifmlnd),
      tif_building_value = readr::parse_number(tifmbld),
      tif_total_value = tif_land_value + tif_building_value
    )
}

# ---- Find parcel files -------------------------------------------------------

parcel_files <- list.files(
  path = path_data_raw,
  pattern = "^Parcel20.*\\.csv$",
  full.names = TRUE
)

if (length(parcel_files) == 0) {
  stop("No parcel files found in data/raw/. Expected files like Parcel2024.csv.")
}

message("Found ", length(parcel_files), " parcel files.")

# ---- Combine parcel files ----------------------------------------------------

parcel_data <- parcel_files %>%
  purrr::map_dfr(process_parcel_file)

# ---- Load cleaned TIF records ------------------------------------------------

tif_data <- readr::read_csv(
  here::here("data", "clean", "jefferson_tif_details_all_years.csv"),
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
) %>%
  janitor::clean_names() %>%
  mutate(
    parcel_number = normalize_parcel_number(parcel_number),
    tax_year = as.integer(tax_year)
  )

# ---- Match parcel records to TIF records -------------------------------------

matched_parcel_tif <- tif_data %>%
  inner_join(
    parcel_data,
    by = c("parcel_number", "tax_year")
  )

matched_keys <- matched_parcel_tif %>%
  distinct(parcel_number, tax_year)

unmatched_tif_records <- tif_data %>%
  anti_join(
    matched_keys,
    by = c("parcel_number", "tax_year")
  )

# ---- Save outputs ------------------------------------------------------------

readr::write_csv(
  matched_parcel_tif,
  here::here("outputs", "diagnostics", "matched_parcel_tif_records.csv")
)

readr::write_csv(
  unmatched_tif_records,
  here::here("outputs", "diagnostics", "unmatched_tif_records.csv")
)

# ---- Optional summary by year ------------------------------------------------

parcel_tif_summary <- matched_parcel_tif %>%
  group_by(tax_year) %>%
  summarise(
    matched_records = n(),
    tif_land_value = sum(tif_land_value, na.rm = TRUE),
    tif_building_value = sum(tif_building_value, na.rm = TRUE),
    tif_total_value = sum(tif_total_value, na.rm = TRUE),
    .groups = "drop"
  )

readr::write_csv(
  parcel_tif_summary,
  here::here("outputs", "diagnostics", "parcel_tif_summary_by_year.csv")
)

message("Matched parcel-TIF records successfully.")
message("Matched rows: ", nrow(matched_parcel_tif))
message("Unmatched TIF rows: ", nrow(unmatched_tif_records))
message("Saved outputs to outputs/diagnostics/.")