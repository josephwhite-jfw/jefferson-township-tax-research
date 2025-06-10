# Load libraries
library(readxl)
library(dplyr)
library(stringr)
library(here)
library(purrr)

# ===============================
# 1. Combine Jefferson Abatements
# ===============================

abatement_folder <- here("data-raw")
abatement_files <- list.files(path = abatement_folder, pattern = "^AbatementDetails.*\\.xlsx$", full.names = TRUE)

process_abatement_file <- function(file_path) {
  df <- read_excel(file_path, col_types = "text")
  df %>%
    filter(trimws(Township) == "JEFFERSON TWP") %>%
    mutate(SourceFile = basename(file_path))
}

jefferson_abatements_all_years <- map_dfr(abatement_files, process_abatement_file)

write.csv(jefferson_abatements_all_years, here("data", "Jefferson_Abatement_Details_All_Years.csv"), row.names = FALSE)

cat("✅ Abatement dataset saved with", nrow(jefferson_abatements_all_years), "rows\n")

# ===============================
# 2. Combine Jefferson TIFs
# ===============================

# ===============================
# 2. Combine Jefferson TIFs (overwrite ParcelNumber to strip trailing '-00')
# ===============================

library(readxl)
library(dplyr)
library(stringr)
library(here)
library(purrr)

tif_folder <- here("data-raw")
tif_files <- list.files(path = tif_folder, pattern = "[Tt][Ii][Ff]Details.*\\.xlsx$", full.names = TRUE)

process_tif_file <- function(file_path) {
  df <- read_excel(file_path, col_types = "text")
  
  df %>%
    filter(trimws(Township) == "JEFFERSON TWP") %>%
    mutate(
      SourceFile = basename(file_path),
      ParcelNumber = str_replace(ParcelNumber, "-00$", "")  # Overwrite by stripping only final '-00'
    )
}

jefferson_tifs_all_years <- map_dfr(tif_files, process_tif_file)

write.csv(jefferson_tifs_all_years, here("data", "Jefferson_TIF_Details_All_Years.csv"), row.names = FALSE)

cat("✅ TIF dataset saved with", nrow(jefferson_tifs_all_years), "rows\n")
