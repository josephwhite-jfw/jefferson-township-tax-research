# -----------------------------------------
# Title: Calculate TIF Revenue Loss by Fund
# -----------------------------------------
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(here)

# Load TIF data
tif <- read_csv(here("data", "Jefferson_TIF_Details_All_Years.csv"))

# Load and patch millage table to include 171 with 170's rates
millage <- read_csv(here("data", "Township_Millage_Table.csv")) %>%
  mutate(TaxDistrict = str_pad(as.character(TaxDistrict), 3, pad = "0")) %>%
  bind_rows(
    filter(., TaxDistrict == "170") %>% mutate(TaxDistrict = "171")
  )

# Standardize TIF fields (TaxRateType used instead of TaxableLUC)
tif <- tif %>%
  mutate(
    TaxDistrict = str_pad(as.character(TaxDistrict), 3, pad = "0"),
    TaxYear = as.integer(TaxYear),
    PropertyClass = case_when(
      TaxRateType == "Res/Ag" ~ "ResAgr",
      TaxRateType == "Com/Ind" ~ "ComInd",
      TRUE ~ NA_character_
    ),
    DivertedTownship = as.numeric(DivertedTownship)
  )

# Join millage rates and compute proportions
tif_joined <- tif %>%
  left_join(millage, by = c("TaxYear", "TaxDistrict", "PropertyClass")) %>%
  mutate(
    TotalRate = GeneralRate + FireRate + RoadRate,
    Lost_General = DivertedTownship * (GeneralRate / TotalRate),
    Lost_Fire    = DivertedTownship * (FireRate / TotalRate),
    Lost_Road    = if_else(TaxDistrict %in% c("170", "171"),
                           DivertedTownship * (RoadRate / TotalRate), 0),
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
