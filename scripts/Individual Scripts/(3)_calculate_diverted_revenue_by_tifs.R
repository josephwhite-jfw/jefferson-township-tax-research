# -----------------------------------------
# Title: Calculate Diverted Revenue to TIFs by Municipality Fund
# -----------------------------------------
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(here)
library(openxlsx)

# 1) Load data
tif     <- read_csv(here("data", "Jefferson_TIF_Details_All_Years.csv"))
millage <- read_csv(here("data", "Township_Millage_Table.csv")) %>%
  mutate(TaxDistrict = str_pad(as.character(TaxDistrict), 3, pad = "0")) %>%
  bind_rows(
    filter(., TaxDistrict == "170") %>% mutate(TaxDistrict = "171")
  )

# 2) Prepare and join
tif <- tif %>%
  mutate(
    TaxDistrict   = str_pad(as.character(TaxDistrict), 3, pad = "0"),
    TaxYear       = as.integer(TaxYear),
    PropertyClass = case_when(
      TaxRateType == "Res/Ag"  ~ "ResAgr",
      TaxRateType == "Com/Ind" ~ "ComInd",
      TRUE                     ~ NA_character_
    ),
    DivertedTownship = as.numeric(DivertedTownship)
  )

tif_joined <- tif %>%
  left_join(millage, by = c("TaxYear", "TaxDistrict", "PropertyClass")) %>%
  mutate(
    TotalRate   = GeneralRate + FireRate + RoadRate,
    Lost_General = DivertedTownship * (GeneralRate / TotalRate),
    Lost_Fire    = DivertedTownship * (FireRate    / TotalRate),
    Lost_Road    = if_else(
      TaxDistrict %in% c("170","171"),
      DivertedTownship * (RoadRate / TotalRate),
      0
    ),
    Municipality = case_when(
      TaxDistrict %in% c("170","171") ~ "Jefferson Unincorporated",
      TaxDistrict == "027"            ~ "Gahanna",
      TaxDistrict == "067"            ~ "Reynoldsburg",
      TaxDistrict == "175"            ~ "Columbus",
      TRUE                             ~ "Other"
    )
  )

# 3) Summarize & reshape, rename and drop Road for cities
tif_summary <- tif_joined %>%
  group_by(TaxYear, Municipality) %>%
  summarise(
    General = sum(Lost_General, na.rm = TRUE),
    Fire    = sum(Lost_Fire,    na.rm = TRUE),
    Road    = sum(Lost_Road,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols      = c(General, Fire, Road),
    names_to  = "Fund",
    values_to = "Taxes_Diverted_TIF"
  ) %>%
  filter(
    !(Fund == "Road" & Municipality %in% c("Gahanna", "Reynoldsburg", "Columbus"))
  )

# 4) Build sheet list
all_jurisdictions <- unique(tif_summary$Municipality)
sheet_list <- 
  c(
    Final_Total = list(tif_summary),
    setNames(
      lapply(all_jurisdictions, function(m) filter(tif_summary, Municipality == m)),
      paste0("Final_", gsub(" ", "", all_jurisdictions))
    )
  )

# 5) Write to Excel workbook
wb <- createWorkbook()
for (sheet_name in names(sheet_list)) {
  addWorksheet(wb, sheet_name)
  writeData(
    wb, sheet = sheet_name,
    x        = sheet_list[[sheet_name]],
    startCol = 1, startRow = 1
  )
}
saveWorkbook(wb, here("outputs", "TIF_Taxes_Diverted_By_Fund.xlsx"), overwrite = TRUE)
