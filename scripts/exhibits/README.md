# HMDA 2020 Descriptive Exhibits for Academic Paper

## 📂 Directory Structure

```
exhibits/
├── Generate_Exhibits.R     # Main script to generate all tables and figures
├── tables/                 # Output: Descriptive statistics tables
│   ├── *.csv              # CSV format (Excel, analysis)
│   └── *.tex              # LaTeX format (for paper)
└── figures/               # Output: Visualization exhibits
    ├── *.png              # PNG format (300 dpi, presentations)
    └── *.pdf              # PDF format (vector, LaTeX papers)
```

## 🚀 Quick Start

### Generate All Exhibits

From the project root directory:

```r
cd exhibits
Rscript Generate_Exhibits.R
```

**What it does:**
1. Sources `HDMA_Data.R` to load and process data
2. Generates 7 tables (CSV + LaTeX)
3. Generates 9 figures (PNG + PDF)
4. Saves everything to `exhibits/tables/` and `exhibits/figures/`

**Runtime:** ~5-10 minutes on standard hardware

---

## 📊 Tables Generated

### Table 0: Sample Construction Flow
**File:** `table0_sample_flow.csv`

Shows the sample selection process from raw HMDA data through clean mortgage filter to approval/pricing samples. Includes approval rates and mean interest rates at each stage.

| Stage | N | % of Raw | Approval Rate | Mean Rate |
|-------|---|----------|---------------|-----------|
| Raw HMDA 2020 | 25,551,868 | 100% | — | — |
| Clean Filter | 15,227,320 | 59.6% | — | — |
| Approval Sample | 10,857,663 | 42.5% | 87.0% | — |
| Pricing Sample | 9,098,913 | 35.6% | — | 3.175% |

---

### Table 1A & 1B: Summary Statistics
**Files:** `table1a_summary_approval.csv`, `table1b_summary_pricing.csv`

Comprehensive summary statistics for all variables used in the analysis, reported overall and by race/ethnicity group. Includes mean, SD, median, 25th/75th percentiles, and N.

**Panel A (Approval Sample):**
- Approval rate
- Income, loan amount, property value
- Tract-level controls
- Loan purpose distribution

**Panel B (Pricing Sample):**
- Interest rate (mean, median, percentiles)
- Same covariates as Panel A for comparability

**Use in paper:** Essential for Table 1 in most economics papers

---

### Table 2: Missingness Diagnostics
**File:** `table2_missingness.csv`

Documents data quality by showing % missing for each variable, overall and by race/ethnicity group.

**Key Finding:** NH Black borrowers have substantially higher CLTV missingness (12.1% vs 3.6% for NH White)

**Variables tracked:**
- Income
- DTI (Debt-to-Income Ratio)
- CLTV (Combined Loan-to-Value)
- Property Value
- Interest Rate

**Use in paper:** Footnote or appendix table documenting data quality

---

### Table 3A-C: Composition of Categorical Variables
**Files:** `table3a_race_composition.csv`, `table3b_dti_distribution.csv`, `table3c_cltv_distribution.csv`

**3A: Race/Ethnicity Composition**
- Shows how sample composition changes from clean → approval → pricing
- Highlights selection effects
- Documents "Race NA" category (19.5% of sample)

**3B: DTI Band Distribution**
- 10 categories preserving HMDA structure
- Shows concentration in 20-45% DTI range

**3C: CLTV Bin Distribution**
- 8 bins from 0-200%
- Documents 31% missingness

**Use in paper:** Appendix or main text to show sample characteristics

---

### Table 4: Market and Lender Coverage
**File:** `table4_market_coverage.csv`

Documents geographic and institutional coverage:

**MSA (Metropolitan Statistical Area) Stats:**
- Number of MSAs covered
- Median/P90 applications per MSA
- Top 10 MSAs market share

**Lender Stats:**
- Number of unique lenders
- Median/P90 applications per lender
- Top 10 lenders market share

**Use in paper:** Shows data isn't driven by a few large metros/lenders

---

## 📈 Figures Generated

### Figure 1: Sample Flow Diagram
**File:** `figure1_sample_flow.png`

Bar chart showing sample construction with counts and retention percentages at each stage.

**Use in paper:** First figure in data section, shows filtering process

---

### Figure 2: Interest Rate Distribution
**File:** `figure2_rate_distribution.png`

Histogram of contract interest rates (trimmed at 99th percentile). Red dashed line shows sample mean (3.175%).

**Key insight:** Most loans cluster around 3%, with long right tail

**Use in paper:** Shows outcome variable distribution, motivates trimming strategy

---

### Figure 3: Race/Ethnicity Composition Across Samples
**File:** `figure3_race_composition.png`

Side-by-side bar chart comparing race distribution across clean, approval, and pricing samples.

**Key insight:** Race NA category is large (19.5%), minimal selection across samples

**Use in paper:** Documents representativeness of final samples

---

### Figure 4: DTI Band Distribution
**File:** `figure4_dti_distribution.png`

Bar chart showing distribution of debt-to-income ratios.

**Key insight:** Most borrowers in 20-45% DTI range; HMDA range categories preserved

**Use in paper:** Shows key control variable distribution

---

### Figure 5: CLTV Bin Distribution
**File:** `figure5_cltv_distribution.png`

Bar chart of combined loan-to-value ratios.

**Key insight:** High concentration in 80-100% CLTV (typical mortgages)

**Use in paper:** Documents down payment patterns

---

### Figure 6: Income & Loan Amount by Race/Ethnicity
**File:** `figure6_income_loan_by_race.png`

Two-panel violin plot (log scale) showing distribution of income and loan amounts by race.

**Key insight:**
- NH White and NH Asian have highest median incomes
- Similar patterns for loan amounts
- Motivates log transformations in regressions

**Use in paper:** Documents demographic differences in observables

---

### Figure 7: Geographic Coverage (Top MSAs)
**File:** `figure7_top_msas.png`

Pareto chart showing top 15 MSAs by application count with cumulative share line.

**Key insight:** Top 15 MSAs account for substantial share but not overwhelming concentration

**Use in paper:** Shows geographic diversity of sample

---

### Figure 8: Lender Concentration
**File:** `figure8_lender_concentration.png`

Lorenz curve showing cumulative market share of top 100 lenders.

**Key insight:** Top 10 lenders dominate market, justifying lender fixed effects

**Use in paper:** Motivates lender FE specification

---

### Figure 9: Missingness by Race/Ethnicity
**File:** `figure9_missingness_by_race.png`

Grouped bar chart showing DTI and CLTV missingness rates by race.

**Key insight:** NH Black borrowers have highest missingness for both variables

**Use in paper:** Data quality section, justifies missing data treatment

---

## 🎨 Figure Specifications

All figures follow academic publication standards:

- **Dimensions:** 8" × 6" (standard journal size)
- **Resolution:** 300 DPI for PNG
- **Format:** Vector PDF for LaTeX
- **Color scheme:** Colorblind-friendly palette
- **Font size:** 12pt base (readable in print)
- **Theme:** Clean, minimal design with clear labels
- **Notes:** Included as captions in figures

### Using Figures in LaTeX

```latex
\begin{figure}[htbp]
  \centering
  \includegraphics[width=0.8\textwidth]{exhibits/figures/figure1_sample_flow.pdf}
  \caption{Sample Construction Flow}
  \label{fig:sample_flow}
\end{figure}
```

### Using Tables in LaTeX

The `.tex` files are ready to \input directly:

```latex
\input{exhibits/tables/table0_sample_flow.tex}
```

Or use the CSV files and format with `stargazer` or `kableExtra`.

---

## 📋 Best Practices for Academic Papers

### What to Include in Your Paper

**Main Text (Data Section):**
1. Table 0 (Sample Flow) - usually in appendix
2. Table 1A or 1B (Summary Stats) - Table 1 in paper
3. Figure 1 (Sample Flow) - optional, can replace Table 0
4. Figure 2 (Rate Distribution) - shows outcome variable
5. Figure 6 (Income/Loan by Race) - motivates controls

**Appendix:**
1. Table 2 (Missingness) - data quality
2. Table 3A-C (Composition) - sample characteristics
3. Table 4 (Market Coverage) - justifies FE approach
4. Figures 7-9 (Coverage, Concentration, Missingness)

### Standard Notes for Tables/Figures

**Sample Description:**
> "Sample restricted to conventional, first-lien, owner-occupied mortgages on 1-4 unit properties (excluding HELOCs, reverse mortgages, and business-purpose loans). Race/ethnicity groups are mutually exclusive with Hispanic defined by ethnicity regardless of race."

**Approval Sample:**
> "Approval sample keeps applications with action_taken ∈ {1,2,3} (originated, approved-not-accepted, or denied)."

**Pricing Sample:**
> "Pricing sample keeps originated loans (action_taken = 1) with 0 < interest_rate < 25."

---

## 🔧 Customization

### Modify Color Scheme

Edit `RACE_COLORS` in `Generate_Exhibits.R`:

```r
RACE_COLORS <- c(
  "NH White" = "#0072B2",
  "NH Black" = "#E69F00",
  "Hispanic" = "#009E73",
  "NH Asian" = "#CC79A7",
  "Other" = "#999999",
  "Race NA" = "#D55E00"
)
```

### Adjust Figure Dimensions

Edit configuration at top of script:

```r
FIG_WIDTH <- 8
FIG_HEIGHT <- 6
```

### Change Output Formats

Modify `save_figure()` function to add other formats (SVG, EPS, etc.)

---

## 📚 Dependencies

Required R packages (installed automatically if using script):

- `data.table` - Fast data manipulation
- `ggplot2` - Publication-quality graphics
- `scales` - Number/percentage formatting
- `gridExtra` - Multi-panel figures
- `kableExtra` - LaTeX table formatting
- `viridis` - Colorblind-friendly palettes

Install missing packages:

```r
install.packages(c("data.table", "ggplot2", "scales", "gridExtra", "kableExtra", "viridis"))
```

---

## 📊 Output Files Summary

| File Type | Count | Formats | Size | Use Case |
|-----------|-------|---------|------|----------|
| Tables | 7 | CSV + LaTeX | ~50KB | Paper tables, appendix |
| Figures | 9 | PNG + PDF | ~1.8MB | Paper figures, presentations |
| **Total** | **16** | **4 formats** | **~1.85MB** | **Complete exhibit package** |

---

## ✅ Quality Checks

All exhibits follow the guidelines from `HMDA_Descriptive_Exhibits_Guide.pdf`:

- ✅ Separate data description from causal claims
- ✅ Always report counts and denominators
- ✅ Show medians and percentiles, not just means
- ✅ Make units explicit (%, $K, basis points)
- ✅ Handle outliers transparently (p99 trimming documented)
- ✅ Document missingness overall and by race
- ✅ Keep visuals simple (one idea per figure)
- ✅ Consistent race group ordering
- ✅ Readable axes and clear notes

---

## 🎓 Citation

If using these exhibits in your paper:

> Descriptive statistics computed using HMDA 2020 public use file. Sample restricted to conventional, first-lien, owner-occupied mortgages. All exhibits generated using R 4.4.x with data.table and ggplot2 packages.

---

## 🆘 Troubleshooting

**Error: "Cannot find 2020_data.csv"**
- Ensure you run from project root or exhibits/ folder
- Check that `2020_data.csv` is in parent directory

**Warning: "Removed rows with missing values"**
- Normal for ggplot2, indicates data cleaning is working
- Check console output for counts

**LaTeX tables not generating:**
- Ensure `kableExtra` package is installed
- Tables will still generate as CSV

**Figures look pixelated:**
- Use PDF versions for LaTeX (vector graphics)
- PNG is 300 DPI, sufficient for most purposes

---

## 📧 Support

For questions about the exhibits or customization:
1. Read the guide: `HMDA_Descriptive_Exhibits_Guide.pdf`
2. Check comments in `Generate_Exhibits.R`
3. Verify data loaded correctly with `HDMA_Data.R`

---

**Last Updated:** December 2024
**Script Version:** 1.0
**Tested with:** R 4.4.x, HMDA 2020 data
