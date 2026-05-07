# ------------------------------------------------------------------------------
# Script: 04_create_visualizations.R
# Purpose: Create final plots from direct and elasticity-adjusted revenue loss
#          outputs.
# ------------------------------------------------------------------------------

source(here::here("src", "00_setup.R"))

combined_losses <- readr::read_csv(
  here::here("outputs", "tables", "combined_revenue_losses.csv"),
  show_col_types = FALSE
)

elasticity_losses <- readr::read_csv(
  here::here("outputs", "tables", "elasticity_adjusted_tif_losses.csv"),
  show_col_types = FALSE
)

# ---- Direct revenue losses: TIF diversions -----------------------------------

tif_plot_data <- combined_losses %>%
  select(tax_year, municipality, fund, tif_diverted_revenue) %>%
  filter(tif_diverted_revenue > 0)

p_tif <- ggplot(
  tif_plot_data,
  aes(
    x = factor(tax_year),
    y = tif_diverted_revenue,
    fill = municipality
  )
) +
  geom_col() +
  facet_wrap(~ fund, scales = "free_y") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    x = "Tax Year",
    y = "TIF Diverted Revenue",
    fill = "Municipality"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  filename = here::here("outputs", "plots", "tif_diverted_revenue_by_fund.png"),
  plot = p_tif,
  width = 10,
  height = 6,
  dpi = 300
)

# ---- Direct revenue losses: abatements ---------------------------------------

abatement_plot_data <- combined_losses %>%
  select(tax_year, municipality, fund, abatement_revenue_loss) %>%
  filter(abatement_revenue_loss > 0)

p_abatement <- ggplot(
  abatement_plot_data,
  aes(
    x = factor(tax_year),
    y = abatement_revenue_loss,
    fill = municipality
  )
) +
  geom_col() +
  facet_wrap(~ fund, scales = "free_y") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    x = "Tax Year",
    y = "Abatement Revenue Loss",
    fill = "Municipality"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  filename = here::here("outputs", "plots", "abatement_revenue_loss_by_fund.png"),
  plot = p_abatement,
  width = 10,
  height = 6,
  dpi = 300
)

# ---- Combined direct revenue losses ------------------------------------------

total_plot_data <- combined_losses %>%
  select(tax_year, municipality, fund, total_revenue_loss) %>%
  filter(total_revenue_loss > 0)

p_total <- ggplot(
  total_plot_data,
  aes(
    x = factor(tax_year),
    y = total_revenue_loss,
    fill = municipality
  )
) +
  geom_col() +
  facet_wrap(~ fund, scales = "free_y") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    x = "Tax Year",
    y = "Total Revenue Loss",
    fill = "Municipality"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  filename = here::here("outputs", "plots", "combined_revenue_loss_by_fund.png"),
  plot = p_total,
  width = 10,
  height = 6,
  dpi = 300
)

# ---- Elasticity-adjusted TIF losses ------------------------------------------

elasticity_plot_data <- elasticity_losses %>%
  filter(estimated_loss > 0)

p_elasticity <- ggplot(
  elasticity_plot_data,
  aes(
    x = factor(tax_year),
    y = estimated_loss,
    fill = municipality
  )
) +
  geom_col() +
  facet_wrap(~ fund, scales = "free_y") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    x = "Tax Year",
    y = "Elasticity-Adjusted Estimated Loss",
    fill = "Municipality"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  filename = here::here("outputs", "plots", "elasticity_adjusted_tif_losses.png"),
  plot = p_elasticity,
  width = 10,
  height = 6,
  dpi = 300
)

message("Created visualizations successfully.")
message("Saved plots to outputs/plots/.")