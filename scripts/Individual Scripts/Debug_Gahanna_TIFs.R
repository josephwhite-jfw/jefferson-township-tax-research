library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# Load the TIF dataset
tif <- read_csv("data/Jefferson_TIF_Details_All_Years.csv", col_types = cols(.default = "c"))

# Clean and convert fields
tif <- tif %>%
  mutate(
    TaxYear = as.integer(TaxYear),
    TaxDistrict = str_pad(TaxDistrict, 3, pad = "0")
  )

# Define the columns you care about
diverted_cols <- c(
  "DivertedTownship",
  "DivertedParks",
  "DivertedCityVillage",
  "DivertedZoo",
  "DivertedChildren",
  "DivertedLibrary"
)

# Convert those fields to numeric
tif[diverted_cols] <- lapply(tif[diverted_cols], readr::parse_number)

# Filter for Gahanna parcels for years 2022–2024
g_22_24 <- tif %>%
  filter(TaxDistrict == "027", TaxYear %in% c(2022, 2023, 2024)) %>%
  select(ParcelNumber, TaxYear, all_of(diverted_cols)) %>%
  arrange(ParcelNumber, TaxYear)

# Optional: Pivot longer to analyze changes across all diverted categories
g_long <- g_22_24 %>%
  pivot_longer(
    cols = all_of(diverted_cols),
    names_to = "DivertedType",
    values_to = "Amount"
  )

# Optional: Pivot wider to compare years side-by-side
g_wide <- g_22_24 %>%
  pivot_wider(
    names_from = TaxYear,
    values_from = all_of(diverted_cols),
    names_glue = "{.value}_{TaxYear}"
  )

# Add percent change columns for DivertedTownship
g_wide <- g_wide %>%
  mutate(
    PctChange_Township_22_23 = if_else(
      DivertedTownship_2022 > 0,
      round(100 * (DivertedTownship_2023 - DivertedTownship_2022) / DivertedTownship_2022, 1),
      NA_real_
    ),
    PctChange_Township_23_24 = if_else(
      DivertedTownship_2023 > 0,
      round(100 * (DivertedTownship_2024 - DivertedTownship_2023) / DivertedTownship_2023, 1),
      NA_real_
    )
  )

# View result
print(head(g_wide))

write_csv(g_wide, "outputs/Gahanna_TIF_Diverted_AllTypes_2022_2024.csv")
