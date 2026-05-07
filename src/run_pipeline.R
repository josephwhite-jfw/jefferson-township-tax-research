# ------------------------------------------------------------------------------
# Script: run_pipeline.R
# Purpose: Run the full MORPC TIF and abatement revenue analysis pipeline.
# ------------------------------------------------------------------------------

source(here::here("src", "00_setup.R"))

message("Starting MORPC TIF and abatement revenue analysis pipeline...")

source(here::here("src", "01_combine_raw_files.R"))
source(here::here("src", "02_calculate_direct_revenue_losses.R"))
source(here::here("src", "03_calculate_elasticity_adjusted_losses.R"))
source(here::here("src", "04_create_visualizations.R"))

message("Pipeline completed successfully.")