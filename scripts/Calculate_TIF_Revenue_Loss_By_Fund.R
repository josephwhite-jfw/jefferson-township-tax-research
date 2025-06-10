# -----------------------------------------
# Title: Calculate TIF Revenue Loss by Fund
# -----------------------------------------
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(here)

# Load TIF data
tif <- read_csv(here("data", "Matched_Parcel_TIF_Only.csv"))

# Load and patch millage table to include 171 with 170's rates
millage <- read_csv(here("data", "Township_Millage_Table__2014_2024_.csv")) %>%
  mutate(TaxDistrict = str_pad(as.character(TaxDistrict), 3, pad = "0")) %>%
  bind_rows(
    filter(., TaxDistrict == "170") %>% mutate(TaxDistrict = "171")
  )

# Standardize TIF fields (TaxRateType used instead of TaxableLUC)
tif <- tif %>%
  mutate(
    TIFMLND = as.numeric(TIFMLND),
    TIFMBLD = as.numeric(TIFMBLD),
    TIFPercentage = as.numeric(TIFPercentage),
    TaxDistrict = str_pad(as.character(TaxDistrict), 3, pad = "0"),
    TaxYear = as.integer(TaxYear),
    PropertyClass = case_when(
      TaxRateType == "Res/Ag" ~ "ResAgr",
      TaxRateType == "Com/Ind" ~ "ComInd",
      TRUE ~ NA_character_
    )
  )

# Join and calculate losses
tif_joined <- tif %>%
  left_join(millage, by = c("TaxYear", "TaxDistrict", "PropertyClass")) %>%
  mutate(
    EffectiveBase = ((TIFMLND + TIFMBLD) * 0.35) * (TIFPercentage / 100),
    Lost_General = EffectiveBase * (GeneralRate / 1000),
    Lost_Fire = EffectiveBase * (FireRate / 1000),
    Lost_Road = if_else(TaxDistrict %in% c("170", "171"), EffectiveBase * (RoadRate / 1000), 0),
    Municipality = case_when(
      TaxDistrict %in% c("170", "171") ~ "Jefferson Unincorporated",
      TaxDistrict == "027" ~ "Gahanna",
      TaxDistrict == "067" ~ "Reynoldsburg",
      TaxDistrict == "175" ~ "Columbus",
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
