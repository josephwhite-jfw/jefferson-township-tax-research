# -----------------------------------------
# Title: Generate & Export Combined Tax Summary
# -----------------------------------------
library(dplyr)
library(readxl)
library(tidyr)
library(here)
library(openxlsx)

# 1) Load the “Final_Total” sheets
tif <- read_excel(
  here("outputs", "TIF_Taxes_Diverted_By_Fund.xlsx"),
  sheet = "Final_Total"
)

abatement <- read_excel(
  here("outputs", "Forgone_Revenue_From_Abatements.xlsx"),
  sheet = "Final_Total"
)
# abatement has a column named "Forgone Revenue"

# 2) Merge, fill missing, and compute total
combined <- full_join(
  tif,
  abatement,
  by = c("TaxYear", "Municipality", "Fund")
) %>%
  mutate(
    Taxes_Diverted_TIF = coalesce(Taxes_Diverted_TIF, 0),
    Forgone_Revenue_Abatements  = coalesce(Forgone_Revenue_Abatements, 0),
    Total_Forgone_and_Diverted = Taxes_Diverted_TIF + Forgone_Revenue_Abatements
  )

# 3) Split into sheets: Combined_Total + one per municipality
all_munis <- unique(combined$Municipality)
sheet_list <- c(
  Combined_Total = list(combined),
  setNames(
    lapply(all_munis, function(m) filter(combined, Municipality == m)),
    paste0("Combined_", gsub(" ", "", all_munis))
  )
)

# 4) Write to Excel workbook, truncating long sheet names to 31 chars
wb <- createWorkbook()
for (orig_name in names(sheet_list)) {
  sheet_name <- if (nchar(orig_name) > 31) substr(orig_name, 1, 31) else orig_name
  addWorksheet(wb, sheet_name)
  writeData(
    wb,
    sheet    = sheet_name,
    x        = sheet_list[[orig_name]],
    startCol = 1,
    startRow = 1
  )
}
saveWorkbook(
  wb,
  here("outputs", "Combined_TIF_Abatement_Taxes.xlsx"),
  overwrite = TRUE
)
