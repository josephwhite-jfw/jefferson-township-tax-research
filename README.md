# MORPC TIF and Abatement Revenue Analysis

This repo contains an R pipeline for estimating Jefferson Township revenue diverted by Tax Increment Financing (TIF) districts and revenue forgone from property tax abatements.

The project combines Franklin County TIF and abatement files, filters records to Jefferson Township-related tax districts, applies appropraite millage rates, and produces fund-level revenue loss estimates for the General, Fire, and Road funds. The repository also includes an elasticity-adjusted loss model that estimates how Fire and Road fund losses would change after accounting for the effect of TIF value on the taxable base and millage rates.

## Project Overview

Local tax incentives such as TIFs and abatements can reduce or redirect property tax revenue that would otherwise go to local government funds. This project estimates those impacts for Jefferson Township by answering two main questions:

1. How much township revenue was directly diverted by TIFs or forgone through abatements?
2. How large are estimated Fire and Road fund losses when accounting for how TIF value may affect the tax base and fund rates?

## Repository Structure

```text
morpc-tif-abatement-analysis/
│
├── .gitignore
├── JeffersonTownshipTaxResearch.Rproj
├── README.md
│
├── data/
│   ├── raw/
│   │   └── Raw Franklin County TIF and abatement Excel files
│   └── clean/
│       ├── Jefferson_Abatement_Details_All_Years.csv
│       ├── Jefferson_TIF_Details_All_Years.csv
│       ├── township_millage_table.csv
│       └── township_taxable_values.csv
│
├── outputs/
│   ├── diagnostics/
│   │   ├── elasticities_by_fund_property_class.csv
│   │   └── tif_share_of_base.csv
│   ├── plots/
│   │   └── Final visualizations
│   └── tables/
│       ├── abatement_losses_by_fund.csv
│       ├── combined_revenue_losses.csv
│       ├── elasticity_adjusted_tif_losses.csv
│       └── tif_diversions_by_fund.csv
│
├── src/
│   ├── 00_setup.R
│   ├── 01_combine_raw_files.R
│   ├── 02_calculate_direct_revenue_losses.R
│   ├── 03_calculate_elasticity_adjusted_losses.R
│   ├── 04_create_visualizations.R
│   ├── match_parcel_tif_records.R
│   └── run_pipeline.R
│
└── tax-rate-info/
    └── Annual Franklin County tax rate PDFs