# -----------------------------------------
# Title: Plot Revenue Loss by Fund and Municipality
# -----------------------------------------
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(here)

# Load combined loss data
loss_data <- read_csv(here("outputs", "accurate_total_losses.csv"))

# Summarize total loss per year, fund, and municipality
loss_summary <- loss_data %>%
  group_by(TaxYear, Fund, Municipality) %>%
  summarise(Total_Loss = sum(Total_Loss, na.rm = TRUE), .groups = "drop")

# Plot for each fund
unique_funds <- unique(loss_summary$Fund)

for (f in unique_funds) {
  fund_data <- loss_summary %>% filter(Fund == f)
  
  p <- ggplot(fund_data, aes(x = factor(TaxYear), y = Total_Loss, fill = Municipality)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(
      title = paste(f, "Fund: Revenue Loss by Municipality (2014–2024)"),
      x = "Tax Year",
      y = "Total Revenue Loss ($)",
      fill = "Municipality"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}