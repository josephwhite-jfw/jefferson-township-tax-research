# ================================================================
# Jefferson Township Tax Research - Full Annual Pipeline
# ================================================================
# Purpose: Combines raw parcel, TIF, and abatement data from Auditor,
# calculates fund-level revenue losses, and exports summary reports
# and visualizations.
# 
# INSTRUCTIONS (for non-coders):
#   1. Update `data-raw/` with the new year’s files
#   2. Open RStudio and this project
#   3. Click "Source" or run this script top to bottom
#   4. Final outputs will be in `outputs/` folder
# ================================================================

# Load libraries
library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(here)
library(ggplot2)
library(scales)

# ===============================
# 1. Combine Jefferson Abatements
# ===============================
abatement_files <- list.files(here("data-raw"), pattern = "^AbatementDetails.*\\.xlsx$", full.names = TRUE)

process_abatement_file <- function(file_path) {
  read_excel(file_path, col_types = "text") %>%
    filter(trimws(Township) == "JEFFERSON TWP") %>%
    mutate(SourceFile = basename(file_path))
}

jefferson_abatements_all_years <- map_dfr(abatement_files, process_abatement_file)
write_csv(jefferson_abatements_all_years, here("data", "Jefferson_Abatement_Details_All_Years.csv"))

# ============================
# 2. Combine Jefferson TIFs
# ============================
tif_files <- list.files(here("data-raw"), pattern = "[Tt][Ii][Ff]Details.*\\.xlsx$", full.names = TRUE)

process_tif_file <- function(file_path) {
  read_excel(file_path, col_types = "text") %>%
    filter(trimws(Township) == "JEFFERSON TWP") %>%
    mutate(
      SourceFile = basename(file_path),
      ParcelNumber = str_replace(ParcelNumber, "-00$", "")
    )
}

jefferson_tifs_all_years <- map_dfr(tif_files, process_tif_file)
write_csv(jefferson_tifs_all_years, here("data", "Jefferson_TIF_Details_All_Years.csv"))

# ================================================
# 3. Calculate Abatement Revenue Loss by Fund
# ================================================
abatement <- read_csv(here("data", "Jefferson_Abatement_Details_All_Years.csv"))
millage <- read_csv(here("data", "Township_Millage_Table.csv"))

abatement <- abatement %>%
  mutate(
    TaxDistrict = str_pad(str_sub(ParcelNumber, 1, 3), 3, pad = "0"),
    TaxYear = as.integer(TaxYear),
    PropertyClass = "ResAgr"
  )

millage <- millage %>%
  mutate(TaxDistrict = str_pad(TaxDistrict, 3, pad = "0")) %>%
  bind_rows(filter(., TaxDistrict == "170") %>% mutate(TaxDistrict = "171"))

abatement_joined <- abatement %>%
  left_join(millage, by = c("TaxYear", "TaxDistrict", "PropertyClass")) %>%
  mutate(
    Lost_General = ForegoneTownship * (GeneralRate / TotalTownshipMillage),
    Lost_Fire    = ForegoneTownship * (FireRate / TotalTownshipMillage),
    Lost_Road    = if_else(TaxDistrict %in% c("170", "171"), ForegoneTownship * (RoadRate / TotalTownshipMillage), 0),
    Municipality = case_when(
      TaxDistrict %in% c("170", "171") ~ "Jefferson Unincorporated",
      TaxDistrict == "027" ~ "Gahanna",
      TaxDistrict == "067" ~ "Reynoldsburg",
      TaxDistrict == "175" ~ "Columbus",
      TRUE ~ "Other"
    )
  )

abatement_summary <- abatement_joined %>%
  group_by(TaxYear, Municipality) %>%
  summarise(
    General = sum(Lost_General, na.rm = TRUE),
    Fire    = sum(Lost_Fire, na.rm = TRUE),
    Road    = sum(Lost_Road, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(General, Fire, Road), names_to = "Fund", values_to = "Abatement_Loss")

write_csv(abatement_summary, here("outputs", "accurate_abatement_losses.csv"))

# ===============================================
# 4. Calculate TIF Revenue Loss by Fund
# ===============================================
tif <- read_csv(here("data", "Jefferson_TIF_Details_All_Years.csv"))
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

millage <- read_csv(here("data", "Township_Millage_Table.csv")) %>%
  mutate(TaxDistrict = str_pad(as.character(TaxDistrict), 3, pad = "0")) %>%
  bind_rows(filter(., TaxDistrict == "170") %>% mutate(TaxDistrict = "171"))

tif_joined <- tif %>%
  left_join(millage, by = c("TaxYear", "TaxDistrict", "PropertyClass")) %>%
  mutate(
    TotalRate   = GeneralRate + FireRate + RoadRate,
    Lost_General = DivertedTownship * (GeneralRate / TotalRate),
    Lost_Fire    = DivertedTownship * (FireRate / TotalRate),
    Lost_Road    = if_else(TaxDistrict %in% c("170", "171"), DivertedTownship * (RoadRate / TotalRate), 0),
    Municipality = case_when(
      TaxDistrict %in% c("170", "171") ~ "Jefferson Unincorporated",
      TaxDistrict == "027" ~ "Gahanna",
      TaxDistrict == "067" ~ "Reynoldsburg",
      TaxDistrict == "175" ~ "Columbus",
      TRUE ~ "Other"
    )
  )

tif_summary <- tif_joined %>%
  group_by(TaxYear, Municipality) %>%
  summarise(
    General = sum(Lost_General, na.rm = TRUE),
    Fire    = sum(Lost_Fire, na.rm = TRUE),
    Road    = sum(Lost_Road, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(General, Fire, Road), names_to = "Fund", values_to = "TIF_Loss")

write_csv(tif_summary, here("outputs", "accurate_tif_losses.csv"))

# ===============================================
# 5. Merge & Summarize Total Losses
# ===============================================
combined <- full_join(
  read_csv(here("outputs", "accurate_tif_losses.csv")),
  read_csv(here("outputs", "accurate_abatement_losses.csv")),
  by = c("TaxYear", "Municipality", "Fund")
) %>%
  mutate(
    TIF_Loss = coalesce(TIF_Loss, 0),
    Abatement_Loss = coalesce(Abatement_Loss, 0),
    Total_Loss = TIF_Loss + Abatement_Loss
  )

write_csv(combined, here("outputs", "accurate_total_losses.csv"))
write_csv(filter(combined, TaxYear == 2024), here("outputs", "accurate_total_losses_2024.csv"))

# ===============================================
# 6. Generate & Save All Loss Plots
# ===============================================
loss_long <- combined %>%
  pivot_longer(c(TIF_Loss, Abatement_Loss), names_to = "LossType", values_to = "Loss")

loss_summary <- loss_long %>%
  group_by(TaxYear, Fund, Municipality, LossType) %>%
  summarise(Total_Loss = sum(Loss, na.rm = TRUE), .groups = "drop")

output_dir <- here("outputs", "plots")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

funds <- unique(loss_summary$Fund)
loss_types <- unique(loss_summary$LossType)

for (f in funds) {
  for (lt in loss_types) {
    loss_type_readable <- ifelse(lt == "TIF_Loss", "TIFs", "Abatements")
    title_text <- paste(f, "Fund Loss from", loss_type_readable, "by Municipality (2014–2024)")
    
    plot_data <- loss_summary %>%
      filter(Fund == f, LossType == lt) %>%
      filter(!is.na(Total_Loss), Total_Loss >= 0)
    
    p <- ggplot(plot_data, aes(x = factor(TaxYear), y = Total_Loss, fill = Municipality)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(
        title = title_text,
        x = "Tax Year",
        y = "Revenue Loss ($)",
        fill = "Municipality"
      ) +
      scale_y_continuous(labels = dollar_format()) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    file_base <- paste0(gsub(" ", "_", f), "_Fund_Loss_from_", loss_type_readable, "_by_Municipality_2014_2024")
    file_base <- gsub("[^A-Za-z0-9_]", "", file_base)
    file_name <- paste0(file_base, ".png")
    
    ggsave(filename = file.path(output_dir, file_name), plot = p, width = 10, height = 6, device = "png")
    print(p)
  }
}

# ✅ DONE! Your summaries and charts are in `outputs/`
