# -----------------------------------------
# Title: Combine TIF and Abatement Revenue Losses by Year
# -----------------------------------------

# Join abatement and TIF data on TaxYear
combined <- full_join(
  loss_summary_by_year_abatement %>%
    rename_with(~ paste0(.x, "_Abate"), -TaxYear),
  loss_summary_by_year_tif %>%
    rename_with(~ paste0(.x, "_TIF"), -TaxYear),
  by = "TaxYear"
)

# Calculate total losses across TIFs and abatements
combined_totals_only <- combined %>%
  mutate(
    Total_Lost_General = Total_Lost_General_Abate + Total_Lost_General_TIF,
    Total_Lost_Fire = Total_Lost_Fire_Abate + Total_Lost_Fire_TIF,
    Total_Lost_Road = Total_Lost_Road_Abate + Total_Lost_Road_TIF,
    Total_Lost_Township = Total_Lost_Township_Abate + Total_Lost_Township_TIF
  ) %>%
  select(
    TaxYear,
    Total_Lost_General,
    Total_Lost_Fire,
    Total_Lost_Road,
    Total_Lost_Township
  ) %>%
  arrange(TaxYear)

# Output result
print(combined_totals_only)

# Save to CSV
write_csv(combined_totals_only, here("outputs", "totals_only_tif_abatement_loss_by_year.csv"))
