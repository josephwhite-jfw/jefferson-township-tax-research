# -----------------------------------------
# Title: Combine TIF and Abatement Revenue Losses by Year and Municipality
# -----------------------------------------

# Join on both TaxYear and Municipality
combined_by_muni <- full_join(
  loss_summary_by_year_muni_abatement %>%
    rename_with(~ paste0(.x, "_Abate"), -c(TaxYear, Municipality)),
  loss_summary_by_year_muni_tif %>%
    rename_with(~ paste0(.x, "_TIF"), -c(TaxYear, Municipality)),
  by = c("TaxYear", "Municipality")
)

# Calculate total losses across TIFs and abatements
combined_muni_totals <- combined_by_muni %>%
  mutate(
    Total_Lost_General = coalesce(Total_Lost_General_Abate, 0) + coalesce(Total_Lost_General_TIF, 0),
    Total_Lost_Fire = coalesce(Total_Lost_Fire_Abate, 0) + coalesce(Total_Lost_Fire_TIF, 0),
    Total_Lost_Road = coalesce(Total_Lost_Road_Abate, 0) + coalesce(Total_Lost_Road_TIF, 0),
    Total_Lost_Township = coalesce(Total_Lost_Township_Abate, 0) + coalesce(Total_Lost_Township_TIF, 0)
  ) %>%
  select(
    TaxYear,
    Municipality,
    Total_Lost_General,
    Total_Lost_Fire,
    Total_Lost_Road,
    Total_Lost_Township
  ) %>%
  arrange(TaxYear, Municipality)

# Output result
print(combined_muni_totals)

# Save to CSV
write_csv(combined_muni_totals, here("outputs", "totals_by_municipality_tif_abatement_loss_by_year.csv"))
