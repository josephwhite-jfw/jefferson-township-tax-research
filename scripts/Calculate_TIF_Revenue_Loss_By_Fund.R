
# -----------------------------------------
# Title: Calculate TIF Revenue Loss by Fund
# -----------------------------------------
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(here)

# Load data
tif <- read_csv(here("data", "Jefferson_TIF_Details_All_Years.csv"))
millage <- read_csv(here("data", "Township_Millage_Table__2014_2024_.csv")) %>%
  mutate(TaxDistrict = str_pad(as.character(TaxDistrict), 3, pad = "0"))

# Standardize TaxDistrict
tif <- tif %>%
  mutate(
    TaxDistrict = str_pad(as.character(TaxDistrict), 3, pad = "0"),
    TaxYear = as.integer(TaxYear),
    PropertyClass = case_when(
      str_starts(TaxableLUC, "5") ~ "ResAgr",
      str_starts(TaxableLUC, "3") | str_starts(TaxableLUC, "4") ~ "ComInd",
      TRUE ~ NA_character_
    )
  )

# Join millage rates
tif_joined <- tif %>%
  left_join(millage, by = c("TaxYear", "TaxDistrict", "PropertyClass")) %>%
  mutate(
    EffectiveBase = AssessedImpr * (TIFPercentage / 100),
    Lost_General = EffectiveBase * (GeneralRate / 1000),
    Lost_Fire = EffectiveBase * (FireRate / 1000),
    Lost_Road = if_else(TaxDistrict == "170", EffectiveBase * (RoadRate / 1000), 0),
    Municipality = case_when(
      TaxDistrict == "170" ~ "Jefferson Unincorporated",
      TaxDistrict == "027" ~ "Gahanna",
      TaxDistrict == "067" ~ "Reynoldsburg",
      TaxDistrict %in% c("175", "171") ~ "Columbus",
      TRUE ~ "Other"
    )
  )

# Summarize and reshape
tif_summary <- tif_joined %>%
  group_by(TaxYear, Municipality) %>%
  summarise(
    General = sum(Lost_General, na.rm = TRUE),
    Fire = sum(Lost_Fire, na.rm = TRUE),
    Road = sum(Lost_Road, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(General, Fire, Road), names_to = "Fund", values_to = "TIF_Loss")

# Save output
write_csv(tif_summary, here("outputs", "accurate_tif_losses.csv"))
