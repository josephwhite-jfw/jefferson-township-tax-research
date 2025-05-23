# -----------------------------
# Title: Accurate Fund-Level Revenue Losses from Abatements (with Property Class, by Municipality)
# -----------------------------

# Load packages
library(dplyr)
library(stringr)
library(readr)
library(tibble)
library(here)

# Load dataset
abatement_all <- read_csv(here("data", "Jefferson_Abatement_Details_All_Years.csv"))

# -----------------------------
# Step 1: Add TaxDistrict and classify PropertyClass
abatement_all <- abatement_all %>%
  mutate(
    TaxDistrict = str_extract(ParcelNumber, "^\\d+"),
    TaxYear = as.integer(TaxYear),
    PropertyClass = case_when(
      str_detect(AbatedLUC, "710") ~ "ResAgr",         # CRA – mostly residential
      str_detect(AbatedLUC, "740|741") ~ "ComInd",     # EZ – mostly commercial/industrial
      TRUE ~ "Unknown"
    )
  )

# -----------------------------
# Step 1b: Add Municipality based on TaxDistrict
abatement_all <- abatement_all %>%
  mutate(
    Municipality = case_when(
      TaxDistrict == "170" ~ "Jefferson Unincorporated",
      TaxDistrict == "027" ~ "Gahanna",
      TaxDistrict == "067" ~ "Reynoldsburg",
      TaxDistrict %in% c("171", "175") ~ "Columbus",
      TRUE ~ "Other"
    )
  )

# -----------------------------
# Step 2: Millage rate table with ResAgr and ComInd entries (TaxYear × TaxDistrict × PropertyClass)
millage_data <- tibble(
  TaxYear = rep(2018:2024, each = 8),
  TaxDistrict = rep(c("170", "175", "027", "067"), times = 7 * 2),
  PropertyClass = rep(c("ResAgr", "ComInd"), each = 4, times = 7),
  EffRateTownship = c(
    # 2018–2024 ResAgr rates
    11.728, 9.394, 9.844, 9.224, 11.57389, 9.67711, 9.72711, 9.10711,
    10.385182, 8.197992, 8.647992, 8.027992, 10.218998, 8.040561, 8.490561, 7.870561,
    12.187597, 10.019635, 10.469635, 9.849635, 9.630280, 7.945525, 8.395525, 7.775525,
    9.677583, 7.983395, 8.433395, 7.813395,
    # 2018–2024 ComInd rates
    13.699376, 11.2, 11.8, 11.2, 13.808519, 11.5, 11.9, 11.1,
    12.541001, 10.182938, 10.632938, 10.012938, 12.818780, 10.493242, 10.943242, 10.323242,
    14.569666, 12.242165, 12.692165, 12.072165, 12.558773, 10.780293, 11.230293, 10.610293,
    12.612427, 10.826662, 11.276662, 10.656662
  ),
  GeneralRate = rep(c(1.000, 1.170, 1.620, 1.000), times = 7 * 2),
  FireRate = rep(c(8.2237, 8.2237, 8.2237, 8.2237), times = 7 * 2),
  RoadRate = rep(c(2.5043, 0, 0, 0), times = 7 * 2)
) %>%
  mutate(
    GeneralShare = GeneralRate / EffRateTownship,
    FireShare = FireRate / EffRateTownship,
    RoadShare = RoadRate / EffRateTownship
  )

# -----------------------------
# Step 3: Join millage data by TaxYear × TaxDistrict × PropertyClass
abatement_all <- abatement_all %>%
  left_join(millage_data, by = c("TaxYear", "TaxDistrict", "PropertyClass"))

# -----------------------------
# Step 4: Compute losses by fund
abatement_all <- abatement_all %>%
  mutate(
    Lost_General = ForegoneTownship * GeneralShare,
    Lost_Fire = ForegoneTownship * FireShare,
    Lost_Road = ForegoneTownship * RoadShare
  )

# -----------------------------
# Step 5: Summarize by year and municipality
loss_summary_by_year_muni_abatement <- abatement_all %>%
  group_by(TaxYear, Municipality) %>%
  summarise(
    Total_Lost_General = sum(Lost_General, na.rm = TRUE),
    Total_Lost_Fire = sum(Lost_Fire, na.rm = TRUE),
    Total_Lost_Road = sum(Lost_Road, na.rm = TRUE),
    Total_Lost_Township = sum(ForegoneTownship, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(TaxYear, Municipality)

# -----------------------------
# Step 6: Output
print(loss_summary_by_year_muni_abatement)

# Optionally write to CSV
write_csv(loss_summary_by_year_muni_abatement, here("outputs", "accurate_abatement_loss_by_year_municipality.csv"))

