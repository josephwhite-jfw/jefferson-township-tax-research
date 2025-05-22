combined_totals_only <- combined %>%
  select(
    TaxYear,
    Total_Lost_General,
    Total_Lost_Fire,
    Total_Lost_Road,
    Total_Lost_Township
  )

print(combined_totals_only)
write_csv(combined_totals_only, here("outputs", "totals_only_tif_abatement_loss_by_year.csv"))