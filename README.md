# Racial Disparities in Mortgage Lending: HMDA 2020 Analysis

**Econometric analysis of racial and ethnic discrimination in mortgage approval and interest rate pricing using 2020 Home Mortgage Disclosure Act (HMDA) data.**

---

## Overview

This repository contains a comprehensive empirical analysis of mortgage lending disparities across racial and ethnic groups in the United States. Using 25.6 million mortgage applications from the 2020 HMDA dataset, we examine:

1. **Approval Disparities**: Do minority borrowers face lower approval rates after controlling for creditworthiness?
2. **Pricing Disparities**: Do approved minority borrowers pay higher interest rates for comparable loans?
3. **Within-Lender Patterns**: Do disparities persist even when comparing borrowers at the same financial institution?

### Key Findings

- **NH Black borrowers**: -5.7pp approval gap, +4.7 bps interest rate premium (within-lender)
- **Hispanic borrowers**: -2.5pp approval gap, +2.1 bps rate premium
- **NH Asian borrowers**: +1.8pp approval advantage, -13.0 bps rate discount
- Disparities persist even after controlling for lender fixed effects, suggesting within-institution discrimination

---

## Repository Structure

```
Final_Paper/
├── data/                         # Data files (NOT in GitHub - see setup instructions)
│   ├── 2020_data.csv            # Main analysis data (9.4 GB)
│   ├── 2021_data.csv            # Additional year (9.6 GB)
│   ├── 2022_data.csv            # Additional year (5.7 GB)
│   └── README.md                # Data download instructions
│
├── scripts/                      # R analysis scripts
│   ├── HDMA_Data.R              # Main analysis pipeline (data cleaning, regressions)
│   ├── HDMA_Summary_Stats.R     # Exploratory summary statistics
│   ├── exhibits/                # Descriptive statistics scripts
│   │   ├── Generate_Exhibits.R  # Generates Tables 0-4, Figures 1-9
│   │   └── Quick_Fix_Table0.R   # Quick regeneration of Table 0
│   └── results/                 # Regression results scripts
│       ├── Generate_Results.R   # Generates all results tables and figures
│       └── Generate_Het_Tables.R # Heterogeneity analysis (Tables 5-7)
│
├── output/                       # Generated outputs (LaTeX tables, figures)
│   ├── exhibits/                # Descriptive statistics outputs
│   │   ├── tables/              # CSV + LaTeX tables
│   │   └── figures/             # PNG (300 DPI) + PDF (vector) figures
│   └── results/                 # Regression results outputs
│       ├── tables/              # LaTeX table fragments
│       └── figures/             # Vector PDF figures
│
├── .gitignore                   # Git exclusions (data files, temp files)
└── README.md                    # This file
```

---

## Getting Started

### Prerequisites

**Software:**
- R (version 4.0+)
- RStudio (recommended)

**System Requirements:**
- **RAM**: 16+ GB (to handle 9.4 GB data file)
- **Storage**: 30+ GB free space
- **OS**: Windows, macOS, or Linux

**R Packages:**
Install required packages by running:
```r
install.packages(c(
  "data.table",    # Fast data manipulation
  "fixest",        # High-dimensional fixed effects regressions
  "ggplot2",       # Graphics
  "scales",        # Number formatting
  "gridExtra",     # Multi-panel plots
  "kableExtra",    # LaTeX tables
  "viridis"        # Colorblind-friendly palettes
))
```

### Data Setup

**IMPORTANT:** The data files are **NOT** included in this repository due to their size (24.7 GB total).

1. **Download HMDA 2020 data** from the Consumer Financial Protection Bureau:
   - Visit: https://ffiec.cfpb.gov/data-publication/dynamic-national-loan-level-dataset/2020
   - Download: "Loan/Application Records (LAR) - 2020"
   - Format: CSV

2. **Place the data file:**
   - Save as `2020_data.csv` in the `data/` folder
   - Expected size: ~9.4 GB
   - Expected rows: 25,551,868

3. **Verify data integrity:**
   ```r
   library(data.table)
   dt <- fread("data/2020_data.csv", nrows = 10)
   ncol(dt)  # Should be 99 columns
   ```

See `data/README.md` for detailed instructions.

---

## Usage

### Standard Workflow

Run scripts in this order from the **project root directory**:

#### 1. Main Analysis (Data Cleaning & Regressions)
```r
source("scripts/HDMA_Data.R")
```
- Loads and cleans 2020 HMDA data
- Creates approval and pricing samples
- Estimates regression models with MSA and lender fixed effects
- **Runtime**: ~5-10 minutes
- **Output**: Stores models in R environment

#### 2. Descriptive Exhibits
```r
source("scripts/exhibits/Generate_Exhibits.R")
```
- Generates 7 descriptive tables + 9 figures
- **Runtime**: ~5-10 minutes
- **Output**: `output/exhibits/tables/` and `output/exhibits/figures/`

#### 3. Regression Results
```r
source("scripts/results/Generate_Results.R")
```
- Generates 8 results tables + 3 figures
- Includes heterogeneity analyses
- **Runtime**: ~10-15 minutes
- **Output**: `output/results/tables/` and `output/results/figures/`

### Quick Exploration
For a quick overview of the data without running regressions:
```r
source("scripts/HDMA_Summary_Stats.R")
```

---

## Methodology

### Sample Construction

| Stage | N | Description |
|-------|---|-------------|
| **Raw HMDA 2020** | 25,551,868 | Full 2020 LAR dataset |
| **Clean Mortgage Filter** | 15,227,320 | Conventional, first-lien, owner-occupied, 1-4 units |
| **Approval Sample** | 10,857,663 | action_taken ∈ {1,2,3} |
| **Pricing Sample** | 9,098,913 | Originated loans with valid rates |

**Filters:**
- Conventional loans (not FHA/VA/USDA)
- First-lien only
- Owner-occupied properties
- 1-4 family units
- Excludes HELOCs, reverse mortgages, business-purpose loans

### Race/Ethnicity Groups

Mutually exclusive groups (Hispanic takes precedence):
- **NH White**: Non-Hispanic White
- **NH Black**: Non-Hispanic Black or African American
- **Hispanic**: Hispanic or Latino (any race)
- **NH Asian**: Non-Hispanic Asian
- **Other**: 2+ races, Native American, Pacific Islander, Free Form Text
- **Race NA**: Race not available

### Regression Models

#### Approval (Linear Probability Model)
```
Pr(Approved) = α + β*Race + γ*Controls + FE + ε
```

#### Pricing (OLS on Interest Rate)
```
Rate = α + β*Race + γ*Controls + FE + ε
```

**Controls:**
- Log(income), DTI bands, CLTV bins
- Log(loan amount), log(property value)
- Tract minority share, tract median income
- Loan purpose, occupancy type

**Fixed Effects:**
- Model 1: MSA (Metropolitan Statistical Area) FE
- Model 2: MSA + Lender (LEI) FE

---

## Output Guide

### Descriptive Exhibits

**Tables (CSV + LaTeX):**
- `table0_sample_flow` - Sample construction
- `table1a_summary_approval` - Summary statistics (approval sample)
- `table1b_summary_pricing` - Summary statistics (pricing sample)
- `table2_missingness` - Missing data diagnostics by race
- `table3a_race_comp` - Race composition across samples
- `table3b_dti_comp` - DTI distribution
- `table3c_cltv_comp` - CLTV distribution
- `table4_market_coverage` - MSA and lender coverage

**Figures (PNG 300 DPI + Vector PDF):**
- `fig1_sample_flow` - Visual sample flow diagram
- `fig2_rate_dist` - Interest rate distribution
- `fig3_race_bars` - Race composition bar charts
- `fig4_dti_dist` - DTI band distribution
- `fig5_cltv_dist` - CLTV bin distribution
- `fig6_income_violin` - Income/loan amount by race (violin plots)
- `fig7_msa_pareto` - Top MSAs (Pareto chart)
- `fig8_lender_lorenz` - Lender concentration (Lorenz curve)
- `fig9_missingness` - Missing data by race

### Regression Results

**Tables (LaTeX .tex fragments):**
- `table2_approval` - Approval regressions (MSA FE, MSA+Lender FE)
- `table3_pricing` - Pricing regressions (MSA FE, MSA+Lender FE)
- `table4_synthesis` - Headline disparities summary
- `table5_het_purpose` - Heterogeneity by loan purpose (purchase vs refi)
- `table6_het_tract` - Heterogeneity by tract minority share quartile
- `table7_het_lender` - Heterogeneity by lender type (bank vs nonbank)

**Figures (Vector PDF):**
- `fig_unadj_approval` - Unadjusted approval rates by race
- `fig_unadj_rates` - Unadjusted interest rates by race
- `fig_missingness` - DTI and CLTV missingness by race

### LaTeX Integration

Include tables in your paper:
```latex
\begin{table}[htbp]
  \centering
  \input{output/results/tables/table2_approval.tex}
  \caption{Approval Regressions}
  \label{tab:approval}
\end{table}
```

Include figures:
```latex
\begin{figure}[htbp]
  \centering
  \includegraphics[width=0.85\linewidth]{output/results/figures/fig_unadj_approval.pdf}
  \caption{Unadjusted Approval Rates by Race/Ethnicity}
  \label{fig:approval}
\end{figure}
```

---

## Project Checklist

Before running the analysis:
- [ ] Downloaded 2020_data.csv (9.4 GB)
- [ ] Placed data file in `data/` folder
- [ ] Installed required R packages
- [ ] Verified system has 16+ GB RAM

To reproduce full analysis:
- [ ] Run `scripts/HDMA_Data.R` (main analysis)
- [ ] Run `scripts/exhibits/Generate_Exhibits.R` (descriptive statistics)
- [ ] Run `scripts/results/Generate_Results.R` (regression results)
- [ ] Check `output/` folders for generated tables and figures

```

## Contact

**Author**: Juan Carlos Gonzalez
**Institution**: Yale University, Department of Economics
**Course**: Econ 4438
**Email**: juan.gonzalez@yale.edu

For questions about the code or methodology, please open an issue in this repository.

---

## Acknowledgments

- **Data**: Consumer Financial Protection Bureau (CFPB) for making HMDA data publicly available
- **Methodology**: Inspired by Bartlett et al. (2022), Bhutta & Hizmo (2021), and related mortgage discrimination literature
- **Tools**: Built using R with `data.table`, `fixest`, and `ggplot2`

---

## References

- Bartlett, R., Morse, A., Stanton, R., & Wallace, N. (2022). Consumer-Lending Discrimination in the FinTech Era. *Journal of Financial Economics*, 143(1), 30-56.
- Bhutta, N., & Hizmo, A. (2021). Do Minorities Pay More for Mortgages? *Review of Financial Studies*, 34(2), 763-789.
- CFPB HMDA Data Documentation: https://ffiec.cfpb.gov/documentation/

---

**Last Updated**: December 2024
