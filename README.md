# Jefferson Township TIF and Abatement Revenue Analysis

This repository contains a reproducible R pipeline for estimating Jefferson Township revenue losses associated with Tax Increment Financing (TIF) districts and property tax abatements.

The project combines annual Franklin County TIF and abatement detail files, filters records to Jefferson Township-related tax districts, applies township millage rates, and produces fund-level revenue loss estimates for the General, Fire, and Road funds. The repository also includes an elasticity-adjusted loss model that estimates how Fire and Road fund losses would change after accounting for the effect of TIF value on the taxable base and millage rates.

## Project Overview

Local tax incentives such as TIFs and abatements can reduce or redirect property tax revenue that would otherwise flow to local government funds. This project estimates those impacts for Jefferson Township by answering two main questions:

1. How much township revenue was directly diverted by TIFs or forgone through abatements?
2. How large are estimated Fire and Road fund losses when accounting for how TIF value may affect the tax base and fund rates?

The project produces cleaned datasets, summary tables, diagnostic outputs, and plots that can be used for public finance analysis and reporting.

## Repository Structure

```text
jefferson-township-tax-research/
│
├── README.md
├── jefferson-township-tax-research.Rproj
│
├── data/
│   ├── raw/
│   │   └── Raw Franklin County TIF and abatement files
│   └── clean/
│       └── Cleaned CSV files used by the analysis
│
├── outputs/
│   ├── tables/
│   │   └── Final CSV output tables
│   ├── plots/
│   │   └── Final visualizations
│   └── diagnostics/
│       └── Intermediate checks and diagnostic files
│
└── src/
    ├── 00_setup.R
    ├── 01_combine_raw_files.R
    ├── 02_calculate_direct_revenue_losses.R
    ├── 03_calculate_elasticity_adjusted_losses.R
    ├── 04_create_visualizations.R
    └── run_pipeline.R