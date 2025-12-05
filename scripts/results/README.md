# HMDA 2020 Results Tables & Figures

## 📂 Directory Structure

```
results/
├── Generate_Results.R     # Master script to generate all tables and figures
├── tables/                # Output: LaTeX table fragments (.tex)
│   ├── table0_flow.tex
│   ├── table1_descriptives.tex
│   ├── table2_approval.tex
│   ├── table3_pricing.tex
│   ├── table4_synthesis.tex
│   ├── table5_het_purpose.tex
│   ├── table6_het_tract.tex
│   └── table7_het_lender.tex
└── figures/               # Output: Vector PDF figures
    ├── fig_unadj_approval.pdf
    ├── fig_unadj_rates.pdf
    └── fig_missingness.pdf
```

---

## 🚀 Quick Start

### Generate All Results

From the project root directory:

```r
cd results
Rscript Generate_Results.R
```

**What it does:**
1. Sources `HDMA_Data.R` to load processed data and regression models
2. Generates 8 LaTeX table fragments
3. Generates 3 vector PDF figures
4. Performs sanity checks on all outputs

**Runtime:** ~10-15 minutes (includes running heterogeneity regressions)

---

## 📊 Tables Generated (LaTeX .tex fragments)

### Table 0: Sample Construction and Flow
**File:** `tables/table0_flow.tex`

Shows the sequential filtering process from raw HMDA 2020 to final regression samples.

| Step | N | % of Raw |
|------|---|----------|
| Raw HMDA 2020 | 25,551,868 | 100% |
| Clean filter | 15,227,320 | 59.6% |
| Approval universe | 10,857,663 | 42.5% (87.0% approval) |
| Approval regression | 10,525,077 | 41.2% |
| Pricing universe | 9,098,913 | 35.6% (mean rate 3.175%) |
| Pricing regression | 8,895,511 | 34.8% |

**LaTeX Usage:**
```latex
\begin{table}[htbp]
  \centering
  \input{results/tables/table0_flow.tex}
  \caption{Sample Construction and Flow}
  \label{tab:sample_flow}
\end{table}
```

---

### Table 1: Descriptives and Missingness by Race/Ethnicity
**File:** `tables/table1_descriptives.tex`

Comprehensive descriptive statistics showing:
- Group shares in clean sample (%)
- Unadjusted approval rates (%)
- Mean and median interest rates (%)
- DTI and CLTV missingness rates (%)
- Sample sizes by group

**Key Finding:** NH Black borrowers have 12.1% CLTV missingness vs 3.6% for NH White

**LaTeX Usage:**
```latex
\begin{table}[htbp]
  \centering
  \input{results/tables/table1_descriptives.tex}
  \caption{Descriptive Statistics and Data Quality by Race/Ethnicity}
  \label{tab:descriptives}
\end{table}
```

---

### Table 2: Approval Regressions (Linear Probability Model)
**File:** `tables/table2_approval.tex`

Main approval regression results with two specifications:
- **Column (1):** MSA fixed effects, SE clustered by MSA
- **Column (2):** MSA + Lender fixed effects, SE clustered by LEI

**Dependent variable:** Approved = 1[action_taken = 1]
**Reference group:** NH White
**N:** 10,525,077

**Key Results:**
- NH Black: -7.4 pp (MSA FE) → -5.7 pp (MSA+Lender FE)
- NH Asian: -2.7 pp (MSA FE) → -2.2 pp (MSA+Lender FE)

**LaTeX Usage:**
```latex
\begin{table}[htbp]
  \centering
  \input{results/tables/table2_approval.tex}
  \caption{Conditional Approval Disparities by Race/Ethnicity}
  \label{tab:approval_reg}
\end{table}
```

---

### Table 3: Pricing Regressions (OLS on Interest Rate)
**File:** `tables/table3_pricing.tex`

Main pricing regression results with two specifications:
- **Column (1):** MSA fixed effects, SE clustered by MSA
- **Column (2):** MSA + Lender fixed effects, SE clustered by LEI

**Dependent variable:** Contract interest rate (percentage points)
**Reference group:** NH White
**N:** 8,895,511

**Key Results:**
- NH Black: +0.079 pp = 7.9 bps (MSA FE) → +0.047 pp = 4.7 bps (MSA+Lender FE)
- NH Asian: -0.130 pp = -13.0 bps (MSA FE) → -0.121 pp = -12.1 bps (MSA+Lender FE)

**LaTeX Usage:**
```latex
\begin{table}[htbp]
  \centering
  \input{results/tables/table3_pricing.tex}
  \caption{Conditional Interest Rate Disparities by Race/Ethnicity}
  \label{tab:pricing_reg}
\end{table}
```

---

### Table 4: Synthesis (Headline Disparities)
**File:** `tables/table4_synthesis.tex`

One-glance summary of main results from Tables 2-3:

| Group | Approval (pp) MSA | Approval (pp) MSA+LEI | Rate (bps) MSA | Rate (bps) MSA+LEI |
|-------|-------------------|----------------------|----------------|-------------------|
| NH Black | -7.4 | -5.7 | +7.9 | +4.7 |
| Hispanic | -1.7 | -1.2 | +1.4 | +0.9 |
| NH Asian | -2.7 | -2.2 | -13.0 | -12.1 |

**Note:** Approval gaps in percentage points; rate gaps in basis points (bps = 100 × pp)

**LaTeX Usage:**
```latex
\begin{table}[htbp]
  \centering
  \input{results/tables/table4_synthesis.tex}
  \caption{Summary of Conditional Disparities Across Outcomes}
  \label{tab:synthesis}
\end{table}
```

---

### Table 5: Heterogeneity by Loan Purpose
**File:** `tables/table5_het_purpose.tex`

Tests whether disparities differ by loan purpose (purchase vs refinance).

**Structure:** 4 columns
- Approval, Purchase
- Approval, Refinance
- Rate, Purchase
- Rate, Refinance

**Specification:** MSA + Lender FE, clustered by LEI (consistent with main specs)

**LaTeX Usage:**
```latex
\begin{table}[htbp]
  \centering
  \input{results/tables/table5_het_purpose.tex}
  \caption{Heterogeneity by Loan Purpose}
  \label{tab:het_purpose}
\end{table}
```

---

### Table 6: Heterogeneity by Neighborhood Minority Share
**File:** `tables/table6_het_tract.tex`

Tests whether disparities vary across neighborhood racial composition.

**Structure:** Two panels, 4 quartile columns each
- **Panel A:** Approval regressions by quartile (Q1 = low minority, Q4 = high minority)
- **Panel B:** Pricing regressions by quartile

**Variable:** `tract_minority_population_percent`
**Specification:** MSA + Lender FE, clustered by LEI

**LaTeX Usage:**
```latex
\begin{table}[htbp]
  \centering
  \input{results/tables/table6_het_tract.tex}
  \caption{Heterogeneity by Tract-Level Minority Population Share}
  \label{tab:het_tract}
\end{table}
```

---

### Table 7: Heterogeneity by Lender Type
**File:** `tables/table7_het_lender.tex`

Tests whether disparities differ between banks and nonbank lenders.

**Structure:** Two panels, 2 columns each
- **Panel A:** Approval (Bank vs Nonbank)
- **Panel B:** Pricing (Bank vs Nonbank)

**Classification:**
- Bank = depository institutions (derived_institution_type_flag = 1)
- Nonbank = for-profit/nonprofit nondepositories (flags 2-4)

**Specification:** MSA + Lender FE, clustered by LEI

**LaTeX Usage:**
```latex
\begin{table}[htbp]
  \centering
  \input{results/tables/table7_het_lender.tex}
  \caption{Heterogeneity by Lender Institution Type}
  \label{tab:het_lender}
\end{table}
```

---

## 📈 Figures Generated (Vector PDFs)

### Figure 1: Unadjusted Approval Rates by Group
**File:** `figures/fig_unadj_approval.pdf`

Bar chart showing unconditional approval rates by race/ethnicity.

**Key Insight:** Substantial raw differences before regression adjustment (NH White 89%, NH Black 80%)

**Specifications:**
- Format: Vector PDF (ggplot2)
- Dimensions: 8" × 6"
- Colors: Colorblind-friendly palette

**LaTeX Usage:**
```latex
\begin{figure}[htbp]
  \centering
  \includegraphics[width=0.85\linewidth]{results/figures/fig_unadj_approval.pdf}
  \caption{Unadjusted Approval Rates by Race/Ethnicity}
  \label{fig:unadj_approval}
\end{figure}
```

---

### Figure 2: Unadjusted Interest Rates by Group
**File:** `figures/fig_unadj_rates.pdf`

Bar chart showing mean interest rates by race/ethnicity (originated loans only).

**Key Insight:** NH Asian borrowers receive lowest rates; NH Black highest
**Design:** Bars show means; diamond markers show medians

**LaTeX Usage:**
```latex
\begin{figure}[htbp]
  \centering
  \includegraphics[width=0.85\linewidth]{results/figures/fig_unadj_rates.pdf}
  \caption{Unadjusted Interest Rates by Race/Ethnicity}
  \label{fig:unadj_rates}
\end{figure}
```

---

### Figure 3: Missingness in DTI and CLTV by Group
**File:** `figures/fig_missingness.pdf`

Grouped bar chart showing differential data quality across race/ethnicity groups.

**Key Insight:** NH Black CLTV missingness (12.1%) is 3.4× higher than NH White (3.6%)
**Design:** Two series (DTI orange, CLTV blue)

**LaTeX Usage:**
```latex
\begin{figure}[htbp]
  \centering
  \includegraphics[width=0.85\linewidth]{results/figures/fig_missingness.pdf}
  \caption{Data Missingness by Race/Ethnicity}
  \label{fig:missingness}
\end{figure}
```

---

## 📋 Global Output Conventions

### Formatting Standards

- **Percentages:** 1 decimal place (e.g., 87.0%)
- **Interest rates (regression coefficients):** 3 decimal places in percentage points
- **Basis points conversion:** 1 bp = 0.01 pp (so multiply pp by 100)
- **Standard errors:** In parentheses under coefficients
- **Clustering:** Stated in table notes
- **Fixed effects:** "MSA FE: Yes/No" and "Lender FE: Yes/No" rows
- **Significance stars:** *** p<0.01, ** p<0.05, * p<0.10

### Sample Definitions

**Clean universe:** Conventional, first-lien, owner-occupied, 1-4 unit properties
**Approval sample:** Clean + action_taken ∈ {1, 2, 3} (originated, approved-not-accepted, denied)
**Approval regression sample:** Approval + complete RHS covariates (N=10,525,077)
**Pricing sample:** Clean + action_taken = 1 + 0 < interest_rate < 25
**Pricing regression sample:** Pricing + complete RHS covariates (N=8,895,511)

### Race/Ethnicity Groups (Mutually Exclusive)

1. **NH White** (reference group) - Non-Hispanic White
2. **NH Black** - Non-Hispanic Black/African American
3. **Hispanic** - Hispanic or Latino (any race)
4. **NH Asian** - Non-Hispanic Asian
5. **Other** - Non-Hispanic Other (Native American, Pacific Islander, multi-racial)
6. **Race NA** - Race/ethnicity not available (19.5% of sample)

---

## 🔧 Customization

### Modify Regression Specifications

Edit the formulas in `HDMA_Data.R`:

```r
race_formula <- "i(race_group, ref = 'NH White')"
controls <- "log_income + dti_band + cltv_bin + ..."
```

### Change Table Formatting

Modify kable options in `Generate_Results.R`:

```r
kable(..., booktabs = TRUE, digits = 3, ...)
```

### Adjust Figure Appearance

Edit the `theme_results()` function:

```r
theme_results <- function() {
  theme_minimal(base_size = 12) +
    theme(...)
}
```

---

## ✅ Quality Checks

All outputs include built-in sanity checks:

1. **Sample sizes match specifications**
   - Raw: 25,551,868 ✓
   - Clean: 15,227,320 (59.6%) ✓
   - Approval regression: 10,525,077 ✓
   - Pricing regression: 8,895,511 ✓

2. **Key statistics validated**
   - Overall approval rate: 87.0% ✓
   - Mean interest rate: 3.175% ✓
   - Group shares sum to 100% ✓

3. **Coefficient consistency**
   - Table 4 matches Tables 2-3 exactly ✓
   - Heterogeneity tables use consistent specifications ✓

4. **Figure-table alignment**
   - Fig 1 overall rate matches Table 0 ✓
   - Fig 2 mean rate = 3.175% ✓
   - Fig 3 missingness matches Table 1 ✓

---

## 📚 Dependencies

Required R packages (auto-loaded by script):

- `data.table` - Fast data manipulation
- `fixest` - High-dimensional fixed effects regression
- `ggplot2` - Publication-quality graphics
- `scales` - Number formatting
- `kableExtra` - LaTeX table generation

Install if needed:

```r
install.packages(c("data.table", "fixest", "ggplot2", "scales", "kableExtra"))
```

---

## 🎓 Citation & Notes

### For Academic Paper

When discussing results, note:

- **Approval gaps** are in percentage points (pp)
- **Rate gaps** should be discussed in basis points (bps) for economic interpretation
  - Example: "+4.7 bps for NH Black borrowers (MSA+Lender FE)"
- **Selection into origination** applies to pricing results (rates observed only for approved loans)
- **Differential missingness** in CLTV (especially for NH Black) may affect interpretation

### Standard Table Note Template

```latex
Notes: Sample restricted to conventional, first-lien, owner-occupied mortgages
on 1-4 unit properties. Approval sample: action_taken ∈ {1,2,3}. Pricing sample:
originated loans (action_taken = 1) with valid rates (0 < rate < 25). Reference
group: NH White. Standard errors clustered by [MSA/LEI] in parentheses.
*** p<0.01, ** p<0.05, * p<0.10.
```

---

## 🆘 Troubleshooting

**Error: "Cannot find HDMA_Data.R"**
- Run from `results/` folder or ensure parent directory contains HDMA_Data.R

**Error: "Coefficient not found in model"**
- Check that regression models in HDMA_Data.R completed successfully
- Verify race_group factor levels match specification

**Warning: "Removed rows with NA values"**
- Normal for fixest regressions with incomplete RHS
- Numbers reported in Table 0 reflect post-NA-removal samples

**LaTeX compilation error:**
- Ensure booktabs, threeparttable packages are installed
- Check that table fragments have no preamble (use \input not \include)

---

## 📧 Support

For questions about results generation:
1. Check this README
2. Review specification document: `results_tables_figures_spec.pdf`
3. Verify HDMA_Data.R runs successfully
4. Check console output for specific error messages

---

**Last Updated:** December 2024
**Script Version:** 1.0
**Tested with:** R 4.4.x, fixest 0.12.x, HMDA 2020 data

---

## 📝 Quick Reference: All LaTeX Includes

```latex
% Tables
\input{results/tables/table0_flow.tex}
\input{results/tables/table1_descriptives.tex}
\input{results/tables/table2_approval.tex}
\input{results/tables/table3_pricing.tex}
\input{results/tables/table4_synthesis.tex}
\input{results/tables/table5_het_purpose.tex}
\input{results/tables/table6_het_tract.tex}
\input{results/tables/table7_het_lender.tex}

% Figures
\includegraphics[width=0.85\linewidth]{results/figures/fig_unadj_approval.pdf}
\includegraphics[width=0.85\linewidth]{results/figures/fig_unadj_rates.pdf}
\includegraphics[width=0.85\linewidth]{results/figures/fig_missingness.pdf}
```
