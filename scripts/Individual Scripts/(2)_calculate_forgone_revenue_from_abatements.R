# -----------------------------------------
# Title: Calculate & Export Abatement Taxes Abated by Fund
# -----------------------------------------
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(here)
library(openxlsx)

# 1) Load data
abatement <- read_csv(here("data", "Jefferson_Abatement_Details_All_Years.csv"))
millage   <- read_csv(here("data", "Township_Millage_Table.csv"))

# 2) Prepare abatement + millage join
abatement <- abatement %>%
  mutate(
    TaxDistrict   = str_pad(str_sub(ParcelNumber, 1, 3), 3, pad = "0"),
    TaxYear       = as.integer(TaxYear),
    PropertyClass = "ResAgr"
  )

millage <- millage %>%
  mutate(TaxDistrict = str_pad(TaxDistrict, 3, pad = "0")) %>%
  bind_rows(
    filter(., TaxDistrict == "170") %>% mutate(TaxDistrict = "171")
  )

abatement_joined <- abatement %>%
  left_join(millage, by = c("TaxYear", "TaxDistrict", "PropertyClass")) %>%
  mutate(
    Lost_General = ForegoneTownship * (GeneralRate / TotalTownshipMillage),
    Lost_Fire    = ForegoneTownship * (FireRate    / TotalTownshipMillage),
    Lost_Road    = if_else(
      TaxDistrict %in% c("170","171"),
      ForegoneTownship * (RoadRate / TotalTownshipMillage),
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

# 3) Summarize & reshape
abatement_summary <- abatement_joined %>%
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
    values_to = "Forgone_Revenue_Abatements"
  ) %>%
  # 4) Drop 'Road' rows for city jurisdictions
  filter(!(Fund == "Road" & Municipality %in% c("Gahanna","Reynoldsburg","Columbus")))

# 5) Split into a list of data frames: one per municipality + total
all_jurisdictions <- unique(abatement_summary$Municipality)
sheet_list <- 
  c(
    Final_Total = list(abatement_summary),
    setNames(
      lapply(all_jurisdictions, function(m) filter(abatement_summary, Municipality == m)),
      paste0("Final_", gsub(" ", "", all_jurisdictions))
    )
  )

# 6) Write to a single Excel workbook
wb <- createWorkbook()
for (sheet_name in names(sheet_list)) {
  addWorksheet(wb, sheet_name)
  writeData(
    wb, sheet = sheet_name,
    x = sheet_list[[sheet_name]],
    startCol = 1, startRow = 1
  )
  
}

# Save
saveWorkbook(wb, here("outputs", "Forgone_Revenue_From_Abatements.xlsx"), overwrite = TRUE)
