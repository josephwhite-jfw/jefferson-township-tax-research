# ------------------------------------------------------------------------------
# Script: 02_calculate_direct_revenue_losses.R
# Purpose: Calculate direct revenue losses from TIF diversions and abatements,
#          allocate losses across township funds, and save CSV outputs.
# ------------------------------------------------------------------------------

source(here::here("src", "00_setup.R"))

# ---- Load cleaned data -------------------------------------------------------

tif_data <- readr::read_csv(
  here::here("data", "clean", "jefferson_tif_details_all_years.csv"),
  show_col_types = FALSE
)

abatement_data <- readr::read_csv(
  here::here("data", "clean", "jefferson_abatement_details_all_years.csv"),
  show_col_types = FALSE
)

millage <- readr::read_csv(
  here::here("data", "clean", "township_millage_table.csv"),
  show_col_types = FALSE
) %>%
  janitor::clean_names() %>%
  mutate(
    tax_year = as.integer(tax_year),
    tax_district = normalize_tax_district(tax_district),
    property_class = normalize_property_class(property_class)
  ) %>%
  bind_rows(
    filter(., tax_district == "170") %>%
      mutate(tax_district = "171")
  ) %>%
  select(
    tax_year,
    tax_district,
    property_class,
    general_rate,
    fire_rate,
    road_rate,
    total_township_millage
  )

# ---- Clean TIF data ----------------------------------------------------------

tif_clean <- tif_data %>%
  janitor::clean_names() %>%
  mutate(
    tax_year = as.integer(tax_year),
    tax_district = normalize_tax_district(tax_district),
    property_class = normalize_property_class(tax_rate_type)
  ) %>%
  left_join(tax_district_lookup, by = "tax_district")

# ---- Calculate TIF diversions by fund ----------------------------------------

tif_diversions_by_fund <- tif_clean %>%
  left_join(
    millage,
    by = c("tax_year", "tax_district", "property_class")
  ) %>%
  mutate(
    total_rate = general_rate + fire_rate + road_rate,
    
    lost_general = diverted_township * (general_rate / total_rate),
    lost_fire = diverted_township * (fire_rate / total_rate),
    lost_road = diverted_township * (road_rate / total_rate),
    
    lost_road = if_else(
      municipality == "Jefferson Unincorporated",
      lost_road,
      0
    )
  ) %>%
  group_by(tax_year, municipality) %>%
  summarise(
    lost_general = sum(lost_general, na.rm = TRUE),
    lost_fire = sum(lost_fire, na.rm = TRUE),
    lost_road = sum(lost_road, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = starts_with("lost_"),
    names_to = "fund",
    values_to = "tif_diverted_revenue"
  ) %>%
  mutate(
    fund = case_when(
      fund == "lost_general" ~ "General",
      fund == "lost_fire" ~ "Fire",
      fund == "lost_road" ~ "Road",
      TRUE ~ fund
    )
  ) %>%
  filter(
    !(fund == "Road" & municipality != "Jefferson Unincorporated")
  )

# ---- Clean abatement data ----------------------------------------------------

abatement_clean <- abatement_data %>%
  janitor::clean_names() %>%
  mutate(
    tax_year = as.integer(tax_year),
    tax_district = normalize_tax_district(tax_district),
    
    # Original abatement script treated abatements as residential/agricultural.
    # If later files include reliable class information, update this line.
    property_class = "ResAgr"
  ) %>%
  left_join(tax_district_lookup, by = "tax_district")

# ---- Calculate abatement losses by fund --------------------------------------

abatement_losses_by_fund <- abatement_clean %>%
  left_join(
    millage,
    by = c("tax_year", "tax_district", "property_class")
  ) %>%
  mutate(
    lost_general = foregone_township * (general_rate / total_township_millage),
    lost_fire = foregone_township * (fire_rate / total_township_millage),
    lost_road = foregone_township * (road_rate / total_township_millage),
    
    lost_road = if_else(
      municipality == "Jefferson Unincorporated",
      lost_road,
      0
    )
  ) %>%
  group_by(tax_year, municipality) %>%
  summarise(
    lost_general = sum(lost_general, na.rm = TRUE),
    lost_fire = sum(lost_fire, na.rm = TRUE),
    lost_road = sum(lost_road, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = starts_with("lost_"),
    names_to = "fund",
    values_to = "abatement_revenue_loss"
  ) %>%
  mutate(
    fund = case_when(
      fund == "lost_general" ~ "General",
      fund == "lost_fire" ~ "Fire",
      fund == "lost_road" ~ "Road",
      TRUE ~ fund
    )
  ) %>%
  filter(
    !(fund == "Road" & municipality != "Jefferson Unincorporated")
  )

# ---- Combine TIF and abatement losses ----------------------------------------

combined_revenue_losses <- full_join(
  tif_diversions_by_fund,
  abatement_losses_by_fund,
  by = c("tax_year", "municipality", "fund")
) %>%
  mutate(
    tif_diverted_revenue = replace_na(tif_diverted_revenue, 0),
    abatement_revenue_loss = replace_na(abatement_revenue_loss, 0),
    total_revenue_loss = tif_diverted_revenue + abatement_revenue_loss
  ) %>%
  arrange(tax_year, municipality, fund)

# ---- Save outputs ------------------------------------------------------------

readr::write_csv(
  tif_diversions_by_fund,
  here::here("outputs", "tables", "tif_diversions_by_fund.csv")
)

readr::write_csv(
  abatement_losses_by_fund,
  here::here("outputs", "tables", "abatement_losses_by_fund.csv")
)

readr::write_csv(
  combined_revenue_losses,
  here::here("outputs", "tables", "combined_revenue_losses.csv")
)

# ---- Print summary -----------------------------------------------------------

message("Calculated direct revenue losses successfully.")
message("TIF diversion rows: ", nrow(tif_diversions_by_fund))
message("Abatement loss rows: ", nrow(abatement_losses_by_fund))
message("Combined loss rows: ", nrow(combined_revenue_losses))
message("Saved outputs to outputs/tables/.")