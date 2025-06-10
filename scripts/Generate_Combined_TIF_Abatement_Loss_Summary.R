# -----------------------------------------
# Title: Generate Combined TIF + Abatement Loss Summary
# -----------------------------------------
library(dplyr)
library(readr)
library(tidyr)
library(here)

# Load summaries
tif <- read_csv(here("outputs", "accurate_tif_losses.csv"))
abatement <- read_csv(here("outputs", "accurate_abatement_losses.csv"))

# Merge and calculate totals
combined <- full_join(tif, abatement, by = c("TaxYear", "Municipality", "Fund")) %>%
  mutate(
    TIF_Loss = coalesce(TIF_Loss, 0),
    Abatement_Loss = coalesce(Abatement_Loss, 0),
    Total_Loss = TIF_Loss + Abatement_Loss
  )

# Save outputs
write_csv(combined, here("outputs", "accurate_total_losses.csv"))

# Save 2024 summary separately
combined %>%
  filter(TaxYear == 2024) %>%
  arrange(Municipality, Fund) %>%
  write_csv(here("outputs", "accurate_total_losses_2024.csv"))
