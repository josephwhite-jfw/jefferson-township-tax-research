# -----------------------------------------
# Title: Estimate Jefferson Township Fund-Level Revenue Loss from TIFs (2014–2024)
# -----------------------------------------

# Load libraries
library(dplyr)
library(stringr)
library(readr)
library(tibble)
library(here)

# Load your full TIF dataset
tif_all <- read_csv(here("data", "Jefferson_TIF_Details_All_Years.csv"))

# -----------------------------------------
# Step 1: Clean + classify property type
tif_all <- tif_all %>%
  mutate(
    TaxDistrict = as.character(TaxDistrict),
    TaxYear = as.integer(TaxYear),
    PropertyClass = case_when(
      TaxRateType == "Res/Agr" ~ "ResAgr",
      TaxRateType == "Com/Ind" ~ "ComInd",
      TRUE ~ "Unknown"
    ),
    TIFPercentage = as.numeric(TIFPercentage),
    AssessedTotal = as.numeric(AssessedTotal),
    EffectiveBase = AssessedTotal * (TIFPercentage / 100)
  )

# -----------------------------------------
# Step 2: Millage table by TaxYear × District × PropertyClass
millage_data <- tibble(
  TaxYear = rep(2018:2024, each = 8),
  TaxDistrict = rep(c("170", "175", "027", "067"), times = 7 * 2),
  PropertyClass = rep(c("ResAgr", "ComInd"), each = 4, times = 7),
  GeneralRate = rep(c(1.000, 1.170, 1.620, 1.000), times = 7 * 2),
  FireRate = rep(c(8.2237, 8.2237, 8.2237, 8.2237), times = 7 * 2),
  RoadRate = rep(c(2.5043, 0, 0, 0), times = 7 * 2)
)

# -----------------------------------------
# Step 3: Join millage data
tif_all <- tif_all %>%
  left_join(millage_data, by = c("TaxYear", "TaxDistrict", "PropertyClass"))

# -----------------------------------------
# Step 4: Calculate lost revenue by fund
tif_all <- tif_all %>%
  mutate(
    Lost_General = EffectiveBase * (GeneralRate / 1000),
    Lost_Fire = EffectiveBase * (FireRate / 1000),
    Lost_Road = EffectiveBase * (RoadRate / 1000)
  )

# -----------------------------------------
# Step 5: Summarize by year
loss_summary_by_year <- tif_all %>%
  group_by(TaxYear) %>%
  summarise(
    Total_Lost_General = sum(Lost_General, na.rm = TRUE),
    Total_Lost_Fire = sum(Lost_Fire, na.rm = TRUE),
    Total_Lost_Road = sum(Lost_Road, na.rm = TRUE),
    Total_Lost_Township = Total_Lost_General + Total_Lost_Fire + Total_Lost_Road
  ) %>%
  arrange(TaxYear)

# -----------------------------------------
# Output
print(loss_summary_by_year)

# Optionally save
write_csv(loss_summary_by_year, here("outputs", "tif_fund_loss_by_year.csv"))
