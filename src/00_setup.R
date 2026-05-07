# ------------------------------------------------------------------------------
# Script: 00_setup.R
# Purpose: Load packages, define project paths, and store shared functions
#          for the MORPC TIF and abatement revenue analysis.
# ------------------------------------------------------------------------------

# ---- Packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)
library(here)

# ---- Project paths -----------------------------------------------------------

path_data_raw <- here("data", "raw")
path_data_clean <- here("data", "clean")

path_outputs <- here("outputs")
path_outputs_tables <- here("outputs", "tables")
path_outputs_plots <- here("outputs", "plots")
path_outputs_diagnostics <- here("outputs", "diagnostics")

path_docs <- here("docs")

# ---- Create output folders if missing ----------------------------------------

dir.create(path_data_clean, recursive = TRUE, showWarnings = FALSE)
dir.create(path_outputs_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(path_outputs_plots, recursive = TRUE, showWarnings = FALSE)
dir.create(path_outputs_diagnostics, recursive = TRUE, showWarnings = FALSE)

# ---- Tax district to municipality mapping ------------------------------------

tax_district_lookup <- tibble(
  tax_district = c("170", "171", "027", "067", "175"),
  municipality = c(
    "Jefferson Unincorporated",
    "Jefferson Unincorporated",
    "Gahanna",
    "Reynoldsburg",
    "Columbus"
  )
)

# ---- Shared functions --------------------------------------------------------

normalize_tax_district <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    str_pad(width = 3, side = "left", pad = "0")
}

normalize_property_class <- function(x) {
  case_when(
    x %in% c("Res/Ag", "Residential", "Residential/Agricultural", "ResAgr") ~ "ResAgr",
    x %in% c("Com/Ind", "Commercial", "Commercial/Industrial", "ComInd") ~ "ComInd",
    TRUE ~ as.character(x)
  )
}

extract_tax_year_from_filename <- function(file_path) {
  str_extract(basename(file_path), "\\d{4}") %>%
    as.integer()
}