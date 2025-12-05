# ============================================================================
# HMDA 2020: Mortgage Lending Discrimination Analysis
# Examines racial disparities in loan approval and interest rate pricing
# ============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

# ============================================================================
# CONFIGURATION
# ============================================================================

# Data file (relative to project root)
DATA_FILE <- "data/2020_data.csv"

# Columns to load (minimal set for memory efficiency)
REQUIRED_COLS <- c(
  # Identifiers & Fixed Effects
  "lei", "derived_msa_md", "action_taken",
  # Demographics
  "derived_race", "derived_ethnicity",
  # Sample Restrictions
  "loan_type", "loan_purpose", "lien_status", "occupancy_type", "total_units",
  "open_end_line_of_credit", "reverse_mortgage", "business_or_commercial_purpose",
  # Financial Variables
  "loan_amount", "property_value", "income",
  "debt_to_income_ratio", "combined_loan_to_value_ratio", "interest_rate",
  # Neighborhood Controls
  "tract_minority_population_percent", "tract_to_msa_income_percentage"
)

# DTI band levels (for factor ordering)
DTI_LEVELS <- c("<=20", "20-30", "30-36", "36", "36-40",
                "40-45", "45-50", "50-60", ">60", "Missing")

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Safe numeric conversion
to_numeric <- function(x) suppressWarnings(as.numeric(x))

# Create DTI bands following HMDA structure (from DTI_explanation.pdf)
create_dti_bands <- function(dt) {
  dt[, dti_band := fcase(
    # Missing values
    is.na(debt_to_income_ratio) | debt_to_income_ratio %in% c("NA", "Exempt"), "Missing",
    # HMDA pre-defined ranges
    debt_to_income_ratio %in% c("<20%", "20"), "<=20",
    debt_to_income_ratio == "20%-<30%", "20-30",
    debt_to_income_ratio == "30%-<36%", "30-36",
    debt_to_income_ratio == "36", "36",
    debt_to_income_ratio == "50%-60%", "50-60",
    debt_to_income_ratio == ">60%", ">60",
    # Numeric values - bin them
    !is.na(dti_num) & dti_num <= 20, "<=20",
    !is.na(dti_num) & dti_num > 20 & dti_num < 30, "20-30",
    !is.na(dti_num) & dti_num >= 30 & dti_num < 36, "30-36",
    !is.na(dti_num) & dti_num == 36, "36",
    !is.na(dti_num) & dti_num > 36 & dti_num <= 40, "36-40",
    !is.na(dti_num) & dti_num > 40 & dti_num <= 45, "40-45",
    !is.na(dti_num) & dti_num > 45 & dti_num <= 50, "45-50",
    !is.na(dti_num) & dti_num > 50 & dti_num <= 60, "50-60",
    !is.na(dti_num) & dti_num > 60, ">60",
    default = "Missing"
  )]
  dt[, dti_band := factor(dti_band, levels = DTI_LEVELS)]
}

# Print section header
print_header <- function(text, char = "=") {
  width <- 80
  cat("\n", rep(char, width), "\n", sep = "")
  cat(text, "\n")
  cat(rep(char, width), "\n", sep = "")
}

# Print subsection
print_subsection <- function(text) {
  cat("\n--- ", text, " ---\n", sep = "")
}

# ============================================================================
# 1. DATA LOADING & TYPE CONVERSION
# ============================================================================
print_header("LOADING DATA")

dt <- fread(DATA_FILE, select = REQUIRED_COLS, na.strings = c("", "NA", "Exempt"))
cat("Loaded", format(nrow(dt), big.mark = ","), "mortgage applications\n")

# Convert to numeric (handling HMDA string formats)
dt[, `:=`(
  rate_num = to_numeric(interest_rate),
  dti_num = to_numeric(debt_to_income_ratio),
  cltv_num = to_numeric(combined_loan_to_value_ratio),
  inc_num = to_numeric(income),
  pv_num = to_numeric(property_value)
)]

# ============================================================================
# 2. SAMPLE RESTRICTIONS (Clean Mortgage Filter)
# ============================================================================
print_header("APPLYING SAMPLE RESTRICTIONS")

cat("Starting rows:", format(nrow(dt), big.mark = ","), "\n")

# Focus on standard owner-occupied home mortgages
dt_clean <- dt[
  loan_type == 1 &                           # Conventional (not FHA/VA)
  lien_status == 1 &                         # First lien
  occupancy_type == 1 &                      # Owner-occupied
  total_units %in% 1:4 &                     # 1-4 family homes
  open_end_line_of_credit == 2 &             # Not HELOC
  reverse_mortgage == 2 &                    # Not reverse mortgage
  business_or_commercial_purpose == 2        # Not commercial
]

cat("After filters:", format(nrow(dt_clean), big.mark = ","),
    sprintf("(%.1f%% retained)\n", 100 * nrow(dt_clean) / nrow(dt)))

# Free memory
rm(dt)
gc(verbose = FALSE)

# ============================================================================
# 3. FEATURE ENGINEERING
# ============================================================================
print_header("CREATING ANALYSIS VARIABLES")

# Race/Ethnicity (mutually exclusive, Hispanic takes precedence)
cat("Creating race/ethnicity groups...\n")
dt_clean[, race_group := fcase(
  derived_ethnicity == "Hispanic or Latino", "Hispanic",
  derived_race == "White", "NH White",
  derived_race == "Black or African American", "NH Black",
  derived_race == "Asian", "NH Asian",
  derived_race == "Race Not Available", "Race NA",
  default = "Other"
)]
dt_clean[, race_group := factor(race_group, levels = c("NH White", "NH Black",
                                                        "Hispanic", "NH Asian",
                                                        "Other", "Race NA"))]

# DTI bands (HMDA-aware)
cat("Creating DTI bands...\n")
create_dti_bands(dt_clean)

# CLTV bins
cat("Creating CLTV bins...\n")
dt_clean[, cltv_valid := !is.na(cltv_num) & cltv_num > 0 & cltv_num <= 200]
dt_clean[, cltv_bin := fifelse(
  cltv_valid,
  as.character(cut(cltv_num, breaks = c(0, 60, 80, 90, 95, 100, 120, 200),
                   include.lowest = TRUE)),
  "Missing"
)]
dt_clean[, cltv_bin := factor(cltv_bin)]

# Log transformations (handle extreme values)
cat("Creating log-transformed variables...\n")
dt_clean[, `:=`(
  log_income = log(pmax(inc_num, 1)),
  log_loan = log(pmax(loan_amount, 1)),
  log_pv = log(pmax(pv_num, 1))
)]

# ============================================================================
# 4. CREATE ANALYSIS SAMPLES
# ============================================================================
print_header("CREATING REGRESSION DATASETS")

# Approval sample (originated, approved-not-accepted, or denied)
approval_dt <- dt_clean[action_taken %in% 1:3]
approval_dt[, approved := as.numeric(action_taken %in% 1:2)]

cat("Approval sample:", format(nrow(approval_dt), big.mark = ","), "observations\n")
cat("  Approved:", format(sum(approval_dt$approved), big.mark = ","),
    sprintf("(%.1f%%)\n", 100 * mean(approval_dt$approved)))
cat("  Denied:", format(sum(!approval_dt$approved), big.mark = ","),
    sprintf("(%.1f%%)\n", 100 * mean(!approval_dt$approved)))

# Pricing sample (originated loans with valid interest rates)
pricing_dt <- dt_clean[
  action_taken == 1 &
  !is.na(rate_num) &
  rate_num > 0 &
  rate_num < 25
]

cat("\nPricing sample:", format(nrow(pricing_dt), big.mark = ","), "observations\n")
cat("  Mean interest rate:", sprintf("%.3f%%\n", mean(pricing_dt$rate_num)))
cat("  Rate range:", sprintf("%.3f%% - %.3f%%\n",
                             min(pricing_dt$rate_num), max(pricing_dt$rate_num)))

# ============================================================================
# 5. REGRESSION ANALYSIS
# ============================================================================
print_header("RUNNING REGRESSION MODELS")

# Common formula components
race_formula <- "i(race_group, ref = 'NH White')"
controls <- "log_income + dti_band + cltv_bin + log_loan + log_pv +
             tract_minority_population_percent + tract_to_msa_income_percentage +
             factor(loan_purpose)"

# --- APPROVAL MODELS ---
print_subsection("Approval Regressions (Linear Probability Model)")

# MSA fixed effects
cat("\n1. MSA Fixed Effects (comparing across metros)\n")
m_appr_msa <- feols(
  as.formula(paste("approved ~", race_formula, "+", controls, "| derived_msa_md")),
  data = approval_dt,
  vcov = ~ derived_msa_md
)
print(summary(m_appr_msa))

# MSA + Lender fixed effects
cat("\n2. MSA + Lender Fixed Effects (comparing within lenders)\n")
m_appr_lender <- feols(
  as.formula(paste("approved ~", race_formula, "+", controls, "| derived_msa_md + lei")),
  data = approval_dt,
  vcov = ~ lei
)
print(summary(m_appr_lender))

# --- PRICING MODELS ---
print_subsection("Pricing Regressions (Interest Rate)")

# MSA fixed effects
cat("\n1. MSA Fixed Effects (comparing across metros)\n")
m_rate_msa <- feols(
  as.formula(paste("rate_num ~", race_formula, "+", controls, "| derived_msa_md")),
  data = pricing_dt,
  vcov = ~ derived_msa_md
)
print(summary(m_rate_msa))

# MSA + Lender fixed effects
cat("\n2. MSA + Lender Fixed Effects (comparing within lenders)\n")
m_rate_lender <- feols(
  as.formula(paste("rate_num ~", race_formula, "+", controls, "| derived_msa_md + lei")),
  data = pricing_dt,
  vcov = ~ lei
)
print(summary(m_rate_lender))

# ============================================================================
# 6. DATA QUALITY DIAGNOSTICS
# ============================================================================
print_header("DATA QUALITY CHECKS")

# Sample flow
cat("\nSample Flow:\n")
flow_table <- data.table(
  Step = c("Raw data loaded", "Clean mortgage filter", "Approval sample", "Pricing sample"),
  Observations = format(c(25551868, nrow(dt_clean), nrow(approval_dt), nrow(pricing_dt)),
                       big.mark = ",")
)
print(flow_table)

# Missingness by race
cat("\n\nMissingness Rates by Race/Ethnicity:\n")
miss_summary <- approval_dt[, .(
  DTI_missing = sprintf("%.1f%%", mean(dti_band == "Missing") * 100),
  CLTV_missing = sprintf("%.1f%%", mean(cltv_bin == "Missing") * 100)
), by = race_group][order(race_group)]
print(miss_summary)

# Variable distributions
cat("\n\nDTI Band Distribution:\n")
print(dt_clean[, .N, by = dti_band][order(dti_band)])

cat("\n\nRace/Ethnicity Distribution:\n")
print(dt_clean[, .(N = .N, Percent = sprintf("%.1f%%", .N/nrow(dt_clean)*100)),
               by = race_group][order(-N)])

# ============================================================================
# 7. INTERPRETATION GUIDE
# ============================================================================
print_header("INTERPRETATION GUIDE")

cat("
APPROVAL MODEL (Linear Probability Model):
• Race coefficients = percentage-point difference in approval probability
  relative to Non-Hispanic White borrowers
• Example: -0.074 = 7.4 percentage points lower approval rate
• Controls: All observable risk factors (income, DTI, CLTV, etc.)

PRICING MODEL (OLS on Interest Rate):
• Race coefficients = percentage-point difference in interest rate
• Example: 0.079 = 7.9 basis points higher rate (0.079% higher)
• On a $300k 30-year mortgage, 7.9 bps ≈ $14,000 more over loan life

FIXED EFFECTS:
• MSA FE: Compares borrowers in the same metropolitan area
• Lender FE: Compares borrowers at the same financial institution
  (more stringent test - controls for lender-specific policies)

KEY FINDING INTERPRETATION:
If racial disparities persist even with lender fixed effects, it suggests
discrimination occurs within institutions, not just sorting across lenders.
")

print_header("ANALYSIS COMPLETE")

cat("\nModels estimated:\n")
cat("  - m_appr_msa:     Approval with MSA FE\n")
cat("  - m_appr_lender:  Approval with MSA + Lender FE\n")
cat("  - m_rate_msa:     Pricing with MSA FE\n")
cat("  - m_rate_lender:  Pricing with MSA + Lender FE\n\n")

cat("To export results:\n")
cat("  etable(m_appr_msa, m_appr_lender, file = 'output/results/tables/approval_results.tex')\n")
cat("  etable(m_rate_msa, m_rate_lender, file = 'output/results/tables/pricing_results.tex')\n")
