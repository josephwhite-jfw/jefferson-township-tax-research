# -----------------------------------------
# Title: Calculate & Export Total County Revenue Diverted to TIFs
# -----------------------------------------
library(dplyr)
library(readr)
library(stringr)
library(here)
library(openxlsx)

# 1) Load raw TIF detail data
tif <- read_csv(here("data", "Jefferson_TIF_Details_All_Years.csv"))

# 2) Zero‐pad TaxDistrict and assign Municipality
tif <- tif %>%
  mutate(
    TaxDistrict = str_pad(as.character(TaxDistrict), 3, pad = "0"),
    TaxYear     = as.integer(TaxYear),
    Municipality = case_when(
      TaxDistrict %in% c("170","171") ~ "Jefferson Unincorporated",
      TaxDistrict == "027"            ~ "Gahanna",
      TaxDistrict == "067"            ~ "Reynoldsburg",
      TaxDistrict == "175"            ~ "Columbus",
      TRUE                             ~ "Other"
    )
  )

# 3) Summarize only the total county diversion per year & municipality
county_totals <- tif %>%
  group_by(TaxYear, Municipality) %>%
  summarise(
    Total_County_Diverted = sum(
      DivertedCountyGen,
      DivertedChildren,
      DivertedADMH,
      DivertedFCBDD,
      DivertedParks,
      DivertedZoo,
      DivertedSenior,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# 4) Build sheet list: one sheet for all municipalities & one per municipality
all_munis <- unique(county_totals$Municipality)
sheet_list <- c(
  Total_County_Diversions = list(county_totals),
  setNames(
    lapply(all_munis, function(m) filter(county_totals, Municipality == m)),
    paste0("CountyTot_", gsub(" ", "", all_munis))
  )
)

# 5) Write to Excel workbook, truncating sheet names >31 chars
wb <- createWorkbook()
for (orig in names(sheet_list)) {
  sheet_name <- if (nchar(orig) > 31) substr(orig, 1, 31) else orig
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, sheet_list[[orig]], startCol = 1, startRow = 1)
}

# 6) Save workbook
saveWorkbook(
  wb,
  here("outputs", "County_Revenue_Diverted_by_TIFs.xlsx"),
  overwrite = TRUE
)
