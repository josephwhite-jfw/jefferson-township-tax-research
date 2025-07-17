# -----------------------------------------
# Title: Save and Display Revenue Loss Plots by Fund, Type, and Municipality
# -----------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(here)

# 1) Load the combined summary from the Excel workbook
combined <- read_excel(
  here("outputs", "Combined_TIF_Abatement_Taxes.xlsx"),
  sheet = "Combined_Total"
)
# combined has columns: TaxYear, Municipality, Fund, Taxes_Diverted_TIF, Forgone Revenue, Total_Taxes

# 2) Pivot to long format for TIF vs Abatement
loss_long <- combined %>%
  pivot_longer(
    cols      = c(Taxes_Diverted_TIF, Forgone_Revenue_Abatements),
    names_to  = "LossType",
    values_to = "Loss"
  )

# 3) Summarize by year, fund, municipality, and source
loss_summary <- loss_long %>%
  group_by(TaxYear, Fund, Municipality, LossType) %>%
  summarise(
    Total_Loss = sum(Loss, na.rm = TRUE),
    .groups    = "drop"
  )

# 4) Determine maximum for consistent y-axis (optional)
y_max <- max(loss_summary$Total_Loss, na.rm = TRUE)

# 5) Unique funds and loss types
funds      <- unique(loss_summary$Fund)
loss_types <- unique(loss_summary$LossType)

# 6) Prepare output directory
output_dir <- here("outputs", "plots")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 7) Loop over fund × loss type combinations
for (f in funds) {
  for (lt in loss_types) {
    
    # Readable label for plot title and filename
    title_text <- if (lt == "Taxes_Diverted_TIF") {
      paste0(f, " Fund Revenue Diverted by TIFs by Municipality")
    } else {
      paste0(f, " Fund Revenue Forgone from Abatements by Municipality")
    }
    
    # Filter data for this combination
    plot_data <- loss_summary %>%
      filter(Fund == f, LossType == lt)
    
    # Warn if any NA or negative were dropped
    invalid_rows <- loss_data <- loss_long %>%
      filter(Fund == f, LossType == lt) %>%
      filter(is.na(Loss) | Loss < 0)
    if (nrow(invalid_rows) > 0) {
      message("⚠️ Dropped ", nrow(invalid_rows),
              " invalid rows for ", title_text)
    }
    
    # Create bar plot
    p <- ggplot(plot_data, aes(
      x    = factor(TaxYear),
      y    = Total_Loss,
      fill = Municipality
    )) +
      geom_bar(stat = "identity", position = "stack") +
      labs(
        title = title_text,
        x     = "Tax Year",
        y     = "Revenue Loss ($)",
        fill  = "Municipality"
      ) +
      scale_y_continuous(
        labels = dollar_format()
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Build safe filename
  
    file_name <- paste0(
      gsub("[/\\\\?%*:|\"<>]", "", title_text),
      ".png"
    )
    
    # Save plot to file
    ggsave(
      filename = file.path(output_dir, file_name),
      plot     = p,
      width    = 10,
      height   = 6,
      dpi      = 300
    )
    
    # Also display it
    print(p)
  }
}
