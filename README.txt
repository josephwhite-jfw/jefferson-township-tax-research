# Jefferson Township Tax Incentive Tracker

This project analyzes the financial impact of Tax Increment Financing (TIF) and property tax abatements on Jefferson Township, Ohio. The purpose is to quantify the annual revenue diverted from township services, including the General, Fire, and Road funds—due to these economic development incentives.

## Project Overview

Franklin County publishes annual datasets detailing properties receiving abatements or located within TIF districts. This project aggregates those records over time and applies township-specific millage rates to calculate estimated revenue loss.

Two primary incentive types are analyzed:
- **TIF (Tax Increment Financing):** Diverts the assessed value growth of a property into dedicated infrastructure or economic development funds.
- **Abatements (e.g., CRA, EZ, TIRC):** Temporarily reduce property taxes owed by owners.

## Methodology

The analysis is performed using R, and includes:
- Cleaning and combining TIF and Abatement data across multiple years
- Matching parcel identifiers and resolving formatting inconsistencies
- Calculating revenue losses using the proportion of each fund’s millage rate
- Generating CSV summaries and bar chart visualizations by fund, year, and municipality

## Outputs

The project produces:
- A comprehensive CSV file showing annual losses by municipality, fund, and tax year
- Separate breakdowns for TIF and Abatement-related losses
- Stacked bar charts illustrating loss trends over time