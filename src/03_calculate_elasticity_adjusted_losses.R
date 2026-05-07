# ------------------------------------------------------------------------------
# Script: 03_calculate_elasticity_adjusted_losses.R
# Purpose: Estimate Fire and Road fund TIF losses using an elasticity-adjusted
#          millage rate method.
#
# Notes:
#   This script restores the original elasticity workbook logic:
#   - Fire rates are averaged across municipal entries.
#   - Road rates use Jefferson Unincorporated only, because the Road fund applies
#     only to the unincorporated township area.
# ------------------------------------------------------------------------------

source(here::here("src", "00_setup.R"))

# ---- Load data ----------------------------------------------------------------

tif_data <- readr::read_csv(
  here::here("data", "clean", "jefferson_tif_details_all_years.csv"),
  show_col_types = FALSE
) %>%
  janitor::clean_names()

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
    municipality,
    tax_district,
    property_class,
    fire_rate,
    road_rate
  )

taxable_values <- readr::read_csv(
  here::here("data", "clean", "township_taxable_values.csv"),
  show_col_types = FALSE
) %>%
  janitor::clean_names() %>%
  mutate(
    tax_year = as.integer(tax_year),
    property_class = normalize_property_class(property_class)
  )

# ---- Clean TIF data -----------------------------------------------------------

tif_clean <- tif_data %>%
  mutate(
    tax_year = as.integer(tax_year),
    tax_district = normalize_tax_district(tax_district),
    property_class = normalize_property_class(tax_rate_type),
    tif_assessed_impr = as.numeric(assessed_impr)
  ) %>%
  left_join(tax_district_lookup, by = "tax_district") %>%
  filter(!is.na(municipality))

# ---- Township taxable base by year and property class -------------------------

township_taxable_base <- taxable_values %>%
  group_by(tax_year, property_class) %>%
  summarise(
    total_township_taxable_value = sum(taxable_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(property_class, tax_year) %>%
  group_by(property_class) %>%
  mutate(
    taxable_value_pct_change =
      total_township_taxable_value / lag(total_township_taxable_value) - 1
  ) %>%
  ungroup()

# ---- Average taxable value changes over selected years ------------------------

fire_anchor_years <- c(2014, 2017, 2020, 2023)
road_anchor_years <- c(2017, 2020, 2023)

avg_taxable_change_fire <- township_taxable_base %>%
  filter(tax_year %in% fire_anchor_years) %>%
  group_by(property_class) %>%
  summarise(
    avg_taxable_value_pct_change = mean(taxable_value_pct_change, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(fund = "Fire")

avg_taxable_change_road <- township_taxable_base %>%
  filter(tax_year %in% road_anchor_years) %>%
  group_by(property_class) %>%
  summarise(
    avg_taxable_value_pct_change = mean(taxable_value_pct_change, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(fund = "Road")

avg_taxable_change <- bind_rows(
  avg_taxable_change_fire,
  avg_taxable_change_road
)

# ---- Fund rates by year and property class -----------------------------------
# Original logic:
#   Fire = average across municipal entries.
#   Road = Jefferson Unincorporated only.

fire_rates <- millage %>%
  group_by(tax_year, property_class) %>%
  summarise(
    rate = mean(fire_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(fund = "Fire")

road_rates <- millage %>%
  filter(municipality == "Jefferson Unincorporated") %>%
  group_by(tax_year, property_class) %>%
  summarise(
    rate = mean(road_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(fund = "Road")

fund_rates <- bind_rows(
  fire_rates,
  road_rates
) %>%
  arrange(fund, property_class, tax_year) %>%
  group_by(fund, property_class) %>%
  mutate(
    rate_pct_change = rate / lag(rate) - 1
  ) %>%
  ungroup()

# ---- Average fund rate changes over selected years ----------------------------

avg_rate_change <- fund_rates %>%
  filter(
    (fund == "Fire" & tax_year %in% fire_anchor_years) |
      (fund == "Road" & tax_year %in% road_anchor_years)
  ) %>%
  group_by(fund, property_class) %>%
  summarise(
    avg_rate_pct_change = mean(rate_pct_change, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Elasticity by fund and property class ------------------------------------

elasticities <- avg_rate_change %>%
  left_join(
    avg_taxable_change,
    by = c("fund", "property_class")
  ) %>%
  mutate(
    elasticity = avg_rate_pct_change / avg_taxable_value_pct_change
  )

# ---- TIF share of township taxable base ---------------------------------------

tif_township_totals <- tif_clean %>%
  group_by(tax_year, property_class) %>%
  summarise(
    tif_township_assessed_impr = sum(tif_assessed_impr, na.rm = TRUE),
    .groups = "drop"
  )

tif_share_of_base <- township_taxable_base %>%
  select(tax_year, property_class, total_township_taxable_value) %>%
  left_join(
    tif_township_totals,
    by = c("tax_year", "property_class")
  ) %>%
  mutate(
    tif_township_assessed_impr = replace_na(tif_township_assessed_impr, 0),
    tif_share_of_base =
      tif_township_assessed_impr / total_township_taxable_value
  )

# ---- Municipal TIF assessed improvements --------------------------------------

municipal_tif_base <- tif_clean %>%
  group_by(tax_year, municipality, tax_district, property_class) %>%
  summarise(
    municipal_tif_assessed_impr = sum(tif_assessed_impr, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Apply adjusted rates -----------------------------------------------------

adjusted_rates <- fund_rates %>%
  select(tax_year, property_class, fund, rate) %>%
  left_join(
    elasticities %>%
      select(fund, property_class, elasticity),
    by = c("fund", "property_class")
  ) %>%
  left_join(
    tif_share_of_base %>%
      select(tax_year, property_class, tif_share_of_base),
    by = c("tax_year", "property_class")
  ) %>%
  mutate(
    tif_share_of_base = replace_na(tif_share_of_base, 0),
    adjustment_factor = 1 + elasticity * tif_share_of_base,
    adjusted_rate = rate * adjustment_factor
  )

elasticity_adjusted_tif_losses <- municipal_tif_base %>%
  left_join(
    adjusted_rates,
    by = c("tax_year", "property_class"),
    relationship = "many-to-many"
  ) %>%
  filter(fund %in% c("Fire", "Road")) %>%
  mutate(
    estimated_loss = adjusted_rate * (municipal_tif_assessed_impr / 1000),
    
    # Road losses apply only to Jefferson Unincorporated.
    estimated_loss = if_else(
      fund == "Road" & municipality != "Jefferson Unincorporated",
      0,
      estimated_loss
    )
  ) %>%
  group_by(tax_year, municipality, fund) %>%
  summarise(
    estimated_loss = sum(estimated_loss, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(
    !(fund == "Road" & municipality != "Jefferson Unincorporated")
  ) %>%
  arrange(tax_year, municipality, fund)

# ---- Save outputs -------------------------------------------------------------

readr::write_csv(
  elasticities,
  here::here("outputs", "diagnostics", "elasticities_by_fund_property_class.csv")
)

readr::write_csv(
  tif_share_of_base,
  here::here("outputs", "diagnostics", "tif_share_of_base.csv")
)

readr::write_csv(
  elasticity_adjusted_tif_losses,
  here::here("outputs", "tables", "elasticity_adjusted_tif_losses.csv")
)

# ---- Print summary ------------------------------------------------------------

message("Calculated elasticity-adjusted TIF losses successfully.")
message("Elasticity rows: ", nrow(elasticities))
message("TIF share rows: ", nrow(tif_share_of_base))
message("Estimated loss rows: ", nrow(elasticity_adjusted_tif_losses))
message("Saved outputs to outputs/tables/ and outputs/diagnostics/.")