# ------------------------------------------------------------------------------
# Script: 01_combine_raw_files.R
# Purpose: Combine annual raw TIF and abatement files, filter to Jefferson
#          Township, and save clean CSVs.
# ------------------------------------------------------------------------------

source(here::here("src", "00_setup.R"))

# ---- Helper: read and clean one annual file ----------------------------------

read_annual_file <- function(file_path) {
  tax_year <- extract_tax_year_from_filename(file_path)
  
  readxl::read_excel(
    file_path,
    col_types = "text"
  ) %>%
    janitor::clean_names() %>%
    mutate(tax_year = tax_year)
}

# ---- Combine abatement files -------------------------------------------------

abatement_files <- list.files(
  path = path_data_raw,
  pattern = "^AbatementDetails-TY\\d{4}.*\\.xlsx$",
  full.names = TRUE
)

if (length(abatement_files) == 0) {
  stop("No abatement files found in data/raw/. Expected files like AbatementDetails-TY2024*.xlsx.")
}

abatements_all <- abatement_files %>%
  map_dfr(read_annual_file) %>%
  mutate(
    tax_district = str_sub(as.character(parcel_number), 1, 3),
    tax_district = normalize_tax_district(tax_district)
  ) %>%
  filter(tax_district %in% tax_district_lookup$tax_district)

readr::write_csv(
  abatements_all,
  here::here("data", "clean", "jefferson_abatement_details_all_years.csv")
)

# ---- Combine TIF files -------------------------------------------------------

tif_files <- list.files(
  path = path_data_raw,
  pattern = "^TifDetails-TY\\d{4}.*\\.xlsx$",
  full.names = TRUE
)

if (length(tif_files) == 0) {
  stop("No TIF files found in data/raw/. Expected files like TifDetails-TY2024*.xlsx.")
}

tifs_all <- tif_files %>%
  map_dfr(read_annual_file) %>%
  mutate(
    tax_district = normalize_tax_district(tax_district)
  ) %>%
  filter(tax_district %in% tax_district_lookup$tax_district)

readr::write_csv(
  tifs_all,
  here::here("data", "clean", "jefferson_tif_details_all_years.csv")
)

# ---- Print summary ------------------------------------------------------------

message("Combined raw files successfully.")
message("Abatement rows: ", nrow(abatements_all))
message("TIF rows: ", nrow(tifs_all))
message("Saved cleaned files to data/clean/.")