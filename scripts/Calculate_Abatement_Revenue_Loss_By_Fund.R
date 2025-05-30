# -----------------------------------------
# Title: Calculate Abatement Revenue Loss by Fund
# -----------------------------------------
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(here)

# Load data
abatement <- read_csv(here("data", "Jefferson_Abatement_Details_All_Years.csv"))
millage <- read_csv(here("data", "Township_Millage_Table__2014_2024_.csv"))

# Extract TaxDistrict from ParcelNumber
abatement <- abatement %>%
  mutate(
    TaxDistrict = str_pad(str_sub(ParcelNumber, 1, 3), 3, pad = "0"),
    TaxYear = as.integer(TaxYear),
    PropertyClass = "ResAgr"  # Default assumption (mostly CRA)
  )

# PATCH: Add TaxDistrict 171 as a copy of 170 (Columbus)
millage <- millage %>%
  mutate(TaxDistrict = str_pad(TaxDistrict, 3, pad = "0")) %>%
  bind_rows(
    filter(., TaxDistrict == "170") %>% mutate(TaxDistrict = "171")
  )

# Join with millage
abatement_joined <- abatement %>%
  left_join(millage, by = c("TaxYear", "TaxDistrict", "PropertyClass")) %>%
  mutate(
    Lost_General = ForegoneTownship * (GeneralRate / TotalTownshipMillage),
    Lost_Fire = ForegoneTownship * (FireRate / TotalTownshipMillage),
    Lost_Road = if_else(TaxDistrict == "170", ForegoneTownship * (RoadRate / TotalTownshipMillage), 0),
    Municipality = case_when(
      TaxDistrict == "170" ~ "Jefferson Unincorporated",
      TaxDistrict == "027" ~ "Gahanna",
      TaxDistrict == "067" ~ "Reynoldsburg",
      TaxDistrict %in% c("175", "171") ~ "Columbus",
      TRUE ~ "Other"
    )
  )

# Summarize and reshape
abatement_summary <- abatement_joined %>%
  group_by(TaxYear, Municipality) %>%
  summarise(
    General = sum(Lost_General, na.rm = TRUE),
    Fire = sum(Lost_Fire, na.rm = TRUE),
    Road = sum(Lost_Road, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(General, Fire, Road), names_to = "Fund", values_to = "Abatement_Loss")

# Save output
write_csv(abatement_summary, here("outputs", "accurate_abatement_losses.csv"))
