# =======================================================
# Combined Pipeline: From Raw Data to Summary & Plots
# =======================================================

# -----------------------------------------
# Load Libraries
# -----------------------------------------
library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(here)
library(openxlsx)
library(ggplot2)
library(scales)

# -----------------------------------------
# 1. Combine Jefferson Abatements
# -----------------------------------------
abatement_folder <- here("data-raw")
abatement_files  <- list.files(
  path      = abatement_folder,
  pattern   = "^AbatementDetails.*\\.xlsx$",
  full.names = TRUE
)

process_abatement_file <- function(file_path) {
  read_excel(file_path, col_types = "text") %>%
    filter(trimws(Township) == "JEFFERSON TWP") %>%
    mutate(SourceFile = basename(file_path))
}

jefferson_abatements_all_years <- map_dfr(
  abatement_files,
  process_abatement_file
)

write_csv(
  jefferson_abatements_all_years,
  here("data", "Jefferson_Abatement_Details_All_Years.csv")
)
cat("✅ Abatement dataset saved with",
    nrow(jefferson_abatements_all_years), "rows\n")

# -----------------------------------------
# 2. Combine Jefferson TIFs
# -----------------------------------------
tif_folder <- here("data-raw")
tif_files  <- list.files(
  path      = tif_folder,
  pattern   = "[Tt][Ii][Ff]Details.*\\.xlsx$",
  full.names = TRUE
)

process_tif_file <- function(file_path) {
  read_excel(file_path, col_types = "text") %>%
    filter(trimws(Township) == "JEFFERSON TWP") %>%
    mutate(
      SourceFile   = basename(file_path),
      ParcelNumber = str_replace(ParcelNumber, "-00$", "")
    )
}

jefferson_tifs_all_years <- map_dfr(
  tif_files,
  process_tif_file
)

write_csv(
  jefferson_tifs_all_years,
  here("data", "Jefferson_TIF_Details_All_Years.csv")
)
cat("✅ TIF dataset saved with",
    nrow(jefferson_tifs_all_years), "rows\n")

# -----------------------------------------
# 3. Calculate & Export TIF Taxes Diverted by Fund
# -----------------------------------------
tif     <- read_csv(here("data", "Jefferson_TIF_Details_All_Years.csv"))
millage <- read_csv(here("data", "Township_Millage_Table.csv")) %>%
  mutate(TaxDistrict = str_pad(as.character(TaxDistrict), 3, pad = "0")) %>%
  bind_rows(
    filter(., TaxDistrict == "170") %>% mutate(TaxDistrict = "171")
  )

tif_summary <- tif %>%
  mutate(
    TaxDistrict      = str_pad(as.character(TaxDistrict), 3, pad = "0"),
    TaxYear          = as.integer(TaxYear),
    PropertyClass    = case_when(
      TaxRateType == "Res/Ag"  ~ "ResAgr",
      TaxRateType == "Com/Ind" ~ "ComInd",
      TRUE                     ~ NA_character_
    ),
    DivertedTownship = as.numeric(DivertedTownship)
  ) %>%
  left_join(
    millage,
    by = c("TaxYear", "TaxDistrict", "PropertyClass")
  ) %>%
  mutate(
    TotalRate    = GeneralRate + FireRate + RoadRate,
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
  ) %>%
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
  filter(!(Fund == "Road" &
             Municipality %in% c("Gahanna","Reynoldsburg","Columbus")))

# Write TIF output
tif_sheets <- c(
  Final_Total = list(tif_summary),
  setNames(
    lapply(unique(tif_summary$Municipality),
           ~ filter(tif_summary, Municipality == .x)),
    paste0("Final_", gsub(" ", "", unique(tif_summary$Municipality)))
  )
)

wb_tif <- createWorkbook()
for (nm in names(tif_sheets)) {
  addWorksheet(wb_tif, nm)
  writeData(wb_tif, nm, tif_sheets[[nm]], startCol = 1, startRow = 1)
}
saveWorkbook(
  wb_tif,
  here("outputs", "TIF_Taxes_Diverted_By_Fund.xlsx"),
  overwrite = TRUE
)

# -----------------------------------------
# 4. Calculate & Export Abatement Taxes Abated by Fund
# -----------------------------------------
abatement <- read_csv(here("data", "Jefferson_Abatement_Details_All_Years.csv"))
millage   <- read_csv(here("data", "Township_Millage_Table.csv"))

abatement_summary <- abatement %>%
  mutate(
    TaxDistrict   = str_pad(str_sub(ParcelNumber, 1, 3), 3, pad = "0"),
    TaxYear       = as.integer(TaxYear),
    PropertyClass = "ResAgr"
  ) %>%
  left_join(
    millage %>%
      mutate(TaxDistrict = str_pad(TaxDistrict, 3, pad = "0")) %>%
      bind_rows(
        filter(., TaxDistrict == "170") %>% mutate(TaxDistrict = "171")
      ),
    by = c("TaxYear", "TaxDistrict", "PropertyClass")
  ) %>%
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
  ) %>%
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
  filter(!(Fund == "Road" &
             Municipality %in% c("Gahanna","Reynoldsburg","Columbus")))

# Write Abatement output
abatement_sheets <- c(
  Final_Total = list(abatement_summary),
  setNames(
    lapply(unique(abatement_summary$Municipality),
           ~ filter(abatement_summary, Municipality == .x)),
    paste0("Final_", gsub(" ", "", unique(abatement_summary$Municipality)))
  )
)

wb_abatement <- createWorkbook()
for (nm in names(abatement_sheets)) {
  addWorksheet(wb_abatement, nm)
  writeData(wb_abatement, nm, abatement_sheets[[nm]], startCol = 1, startRow = 1)
}
saveWorkbook(
  wb_abatement,
  here("outputs", "Forgone_Revenue_From_Abatements.xlsx"),
  overwrite = TRUE
)

# -----------------------------------------
# 5. Generate & Export Combined Tax Summary
# -----------------------------------------
tif   <- read_excel(
  here("outputs", "TIF_Taxes_Diverted_By_Fund.xlsx"),
  sheet = "Final_Total"
)
abate <- read_excel(
  here("outputs", "Forgone_Revenue_From_Abatements.xlsx"),
  sheet = "Final_Total"
)

combined <- full_join(
  tif, abate,
  by = c("TaxYear", "Municipality", "Fund")
) %>%
  mutate(
    Taxes_Diverted_TIF           = coalesce(Taxes_Diverted_TIF, 0),
    Forgone_Revenue_Abatements   = coalesce(Forgone_Revenue_Abatements, 0),
    Total_Forgone_and_Diverted   = Taxes_Diverted_TIF + Forgone_Revenue_Abatements
  )

combined_sheets <- c(
  Combined_Total = list(combined),
  setNames(
    lapply(unique(combined$Municipality),
           ~ filter(combined, Municipality == .x)),
    paste0("Combined_", gsub(" ", "", unique(combined$Municipality)))
  )
)

wb_combined <- createWorkbook()
for (nm in names(combined_sheets)) {
  sheet_name <- if (nchar(nm) > 31) substr(nm, 1, 31) else nm
  addWorksheet(wb_combined, sheet_name)
  writeData(wb_combined, sheet_name,
            combined_sheets[[nm]], startCol = 1, startRow = 1)
}
saveWorkbook(
  wb_combined,
  here("outputs", "Combined_TIF_Abatement_Taxes.xlsx"),
  overwrite = TRUE
)

# -----------------------------------------
# 6. Save & Display Revenue Loss Plots
# -----------------------------------------
combined <- read_excel(
  here("outputs", "Combined_TIF_Abatement_Taxes.xlsx"),
  sheet = "Combined_Total"
)

loss_long <- combined %>%
  pivot_longer(
    cols      = c(Taxes_Diverted_TIF, Forgone_Revenue_Abatements),
    names_to  = "LossType",
    values_to = "Loss"
  )

loss_summary <- loss_long %>%
  group_by(TaxYear, Fund, Municipality, LossType) %>%
  summarise(
    Total_Loss = sum(Loss, na.rm = TRUE),
    .groups    = "drop"
  )

funds      <- unique(loss_summary$Fund)
loss_types <- unique(loss_summary$LossType)

output_dir <- here("outputs", "plots")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

for (f in funds) {
  for (lt in loss_types) {
    title_text <- if (lt == "Taxes_Diverted_TIF") {
      paste0(f, " fund revenue diverted by TIFs by municipality")
    } else {
      paste0(f, " fund revenue forgone from abatements by municipality")
    }
    
    plot_data <- loss_summary %>%
      filter(Fund == f, LossType == lt)
    
    invalid_rows <- loss_long %>%
      filter(Fund == f, LossType == lt) %>%
      filter(is.na(Loss) | Loss < 0)
    if (nrow(invalid_rows) > 0) {
      message("⚠️ Dropped ", nrow(invalid_rows),
              " invalid rows for ", title_text)
    }
    
    p <- ggplot(plot_data, aes(
      x    = factor(TaxYear),
      y    = Total_Loss,
      fill = Municipality
    )) +
      geom_bar(stat = "identity", position = "stack") +
      labs(
        title = title_text,
        x     = "Tax Year",
        y     = "Revenue Loss ($)",
        fill  = "Municipality"
      ) +
      scale_y_continuous(labels = dollar_format()) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    file_name <- paste0(
      gsub("[/\\\\?%*:|\"<>]", "", title_text),
      ".png"
    )
    ggsave(
      filename = file.path(output_dir, file_name),
      plot     = p,
      width    = 10,
      height   = 6,
      dpi      = 300
    )
    
    print(p)
  }
}