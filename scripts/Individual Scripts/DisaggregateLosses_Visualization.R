# -----------------------------------------
# Title: Save and Display Revenue Loss Plots by Fund, Type, and Municipality
# -----------------------------------------
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(here)

# Load the total losses CSV
loss_data <- read_csv(here("outputs", "accurate_total_losses.csv"))

# Convert to long format for TIF vs Abatement split
loss_long <- loss_data %>%
  pivot_longer(
    cols = c(TIF_Loss, Abatement_Loss),
    names_to = "LossType",
    values_to = "Loss"
  )

# Group by year, fund, municipality, and source
loss_summary <- loss_long %>%
  group_by(TaxYear, Fund, Municipality, LossType) %>%
  summarise(Total_Loss = sum(Loss, na.rm = TRUE), .groups = "drop")

# Set a consistent y-axis maximum
y_max <- max(loss_summary$Total_Loss, na.rm = TRUE)

# Get unique fund types and loss types
funds <- unique(loss_summary$Fund)
loss_types <- unique(loss_summary$LossType)

# Create output folder
output_dir <- here("outputs", "plots")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Loop through all fund/loss type combinations
for (f in funds) {
  for (lt in loss_types) {
    
    # Prepare readable title components
    loss_type_readable <- ifelse(lt == "TIF_Loss", "TIFs", "Abatements")
    title_text <- paste(f, "Fund Loss from", loss_type_readable, "by Municipality (2014–2024)")
    
    # Filter for this group and clean data
    plot_data <- loss_summary %>%
      filter(Fund == f, LossType == lt) %>%
      filter(!is.na(Total_Loss), Total_Loss >= 0)
    
    # Warn about any dropped rows
    n_dropped <- sum(is.na(loss_summary$Total_Loss) | loss_summary$Total_Loss < 0)
    if (n_dropped > 0) {
      message("⚠️ Skipped ", n_dropped, " invalid rows for ", title_text)
    }
    
    # Create plot
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
    
    # Build and clean base filename
    file_base <- paste0(
      gsub(" ", "_", f), "_Fund_Loss_from_", loss_type_readable,
      "_by_Municipality_2014_2024"
    )
    file_base <- gsub("[^A-Za-z0-9_]", "", file_base)  # Remove special characters
    
    # Final filename with extension
    file_name <- paste0(file_base, ".png")
    
    # Save the plot
    ggsave(
      filename = file.path(output_dir, file_name),
      plot = p,
      width = 10,
      height = 6,
      device = "png"
    )
    
    # Also show it in RStudio
    print(p)
  }
}
