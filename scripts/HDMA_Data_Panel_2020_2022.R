# ============================================================================
# HMDA 2020-2022: Panel Analysis of Racial Disparities Over Time
# Examines how mortgage lending disparities evolve across three years
# ============================================================================
#
# This script:
#   1. Loads and cleans HMDA data for 2020, 2021, 2022
#   2. Runs MSA + Lender FE regressions for each year
#   3. Creates comparison tables showing coefficient evolution
#
# Output:
#   - panel_approval_2020_2022.tex/csv
#   - panel_pricing_2020_2022.tex/csv
# ============================================================================

cat("\n")
cat("================================================================================\n")
cat("HMDA PANEL ANALYSIS: 2020-2022\n")
cat("Analyzing Racial Disparities Across Three Years\n")
cat("================================================================================\n\n")

# Load required packages
suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(kableExtra)
})

# ============================================================================
# CONFIGURATION
# ============================================================================

# Data files
DATA_FILES <- c(
  "2020" = "data/2020_data.csv",
  "2021" = "data/2021_data.csv",
  "2022" = "data/2022_data.csv"
)

# Output directory

OUTPUT_DIR <- "output/results/tables"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Columns to load (same as HDMA_Data.R)
REQUIRED_COLS <- c(
  "lei", "derived_msa_md", "action_taken",
  "derived_race", "derived_ethnicity",
  "loan_type", "loan_purpose", "lien_status", "occupancy_type", "total_units",
  "open_end_line_of_credit", "reverse_mortgage", "business_or_commercial_purpose",
  "loan_amount", "property_value", "income",
  "debt_to_income_ratio", "combined_loan_to_value_ratio", "interest_rate",
  "tract_minority_population_percent", "tract_to_msa_income_percentage"
)

# DTI band levels
DTI_LEVELS <- c("<=20", "20-30", "30-36", "36", "36-40",
                "40-45", "45-50", "50-60", ">60", "Missing")

# Race group levels
RACE_LEVELS <- c("NH White", "NH Black", "Hispanic", "NH Asian", "Other", "Race NA")

# ============================================================================
# HELPER FUNCTIONS (Same as HDMA_Data.R)
# ============================================================================

# Safe numeric conversion
to_numeric <- function(x) suppressWarnings(as.numeric(x))

# Create DTI bands
create_dti_bands <- function(dt) {
  dt[, dti_band := fcase(
    is.na(debt_to_income_ratio) | debt_to_income_ratio %in% c("NA", "Exempt"), "Missing",
    debt_to_income_ratio %in% c("<20%", "20"), "<=20",
    debt_to_income_ratio == "20%-<30%", "20-30",
    debt_to_income_ratio == "30%-<36%", "30-36",
    debt_to_income_ratio == "36", "36",
    debt_to_income_ratio == "50%-60%", "50-60",
    debt_to_income_ratio == ">60%", ">60",
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

# Clean and engineer features
process_year_data <- function(dt, year) {
  cat(sprintf("  Processing %s data...\n", year))

  # Convert to numeric
  dt[, `:=`(
    rate_num = to_numeric(interest_rate),
    dti_num = to_numeric(debt_to_income_ratio),
    cltv_num = to_numeric(combined_loan_to_value_ratio),
    inc_num = to_numeric(income),
    pv_num = to_numeric(property_value)
  )]

  # Apply clean mortgage filter
  dt_clean <- dt[
    loan_type == 1 &
    lien_status == 1 &
    occupancy_type == 1 &
    total_units %in% 1:4 &
    open_end_line_of_credit == 2 &
    reverse_mortgage == 2 &
    business_or_commercial_purpose == 2
  ]

  cat(sprintf("    Clean sample: %s (%.1f%% retained)\n",
              format(nrow(dt_clean), big.mark = ","),
              100 * nrow(dt_clean) / nrow(dt)))

  # Race/ethnicity groups
  dt_clean[, race_group := fcase(
    derived_ethnicity == "Hispanic or Latino", "Hispanic",
    derived_race == "White", "NH White",
    derived_race == "Black or African American", "NH Black",
    derived_race == "Asian", "NH Asian",
    derived_race == "Race Not Available", "Race NA",
    default = "Other"
  )]
  dt_clean[, race_group := factor(race_group, levels = RACE_LEVELS)]

  # DTI bands
  create_dti_bands(dt_clean)

  # CLTV bins
  dt_clean[, cltv_valid := !is.na(cltv_num) & cltv_num > 0 & cltv_num <= 200]
  dt_clean[, cltv_bin := fifelse(
    cltv_valid,
    as.character(cut(cltv_num, breaks = c(0, 60, 80, 90, 95, 100, 120, 200),
                     include.lowest = TRUE)),
    "Missing"
  )]
  dt_clean[, cltv_bin := factor(cltv_bin)]

  # Log transformations
  dt_clean[, `:=`(
    log_income = log(pmax(inc_num, 1)),
    log_loan = log(pmax(loan_amount, 1)),
    log_pv = log(pmax(pv_num, 1))
  )]

  # Create approval sample
  approval_dt <- dt_clean[action_taken %in% 1:3]
  approval_dt[, approved := as.numeric(action_taken %in% 1:2)]

  cat(sprintf("    Approval sample: %s (%.1f%% approval rate)\n",
              format(nrow(approval_dt), big.mark = ","),
              100 * mean(approval_dt$approved)))

  # Create pricing sample
  pricing_dt <- dt_clean[
    action_taken == 1 &
    !is.na(rate_num) &
    rate_num > 0 &
    rate_num < 25
  ]

  cat(sprintf("    Pricing sample: %s (mean rate: %.3f%%)\n",
              format(nrow(pricing_dt), big.mark = ","),
              mean(pricing_dt$rate_num)))

  list(approval = approval_dt, pricing = pricing_dt)
}

# ============================================================================
# MAIN ANALYSIS LOOP
# ============================================================================

cat("\n=== LOADING AND PROCESSING DATA FOR ALL YEARS ===\n\n")

# Store regression models
approval_models <- list()
pricing_models <- list()

# Store summary statistics
summary_stats <- data.table(
  Year = character(),
  N_Approval = numeric(),
  Approval_Rate = numeric(),
  N_Pricing = numeric(),
  Mean_Rate = numeric()
)

# Process each year
for (year in names(DATA_FILES)) {
  cat(sprintf("================================================================================\n"))
  cat(sprintf("YEAR %s\n", year))
  cat(sprintf("================================================================================\n"))

  # Load data
  cat(sprintf("Loading %s data...\n", year))
  dt <- fread(DATA_FILES[year], select = REQUIRED_COLS, na.strings = c("", "NA", "Exempt"))
  cat(sprintf("  Loaded: %s rows\n", format(nrow(dt), big.mark = ",")))

  # Process data
  samples <- process_year_data(dt, year)

  # Free memory
  rm(dt)
  gc(verbose = FALSE)

  # Common formula components
  race_formula <- "i(race_group, ref = 'NH White')"
  controls <- "log_income + dti_band + cltv_bin + log_loan + log_pv +
               tract_minority_population_percent + tract_to_msa_income_percentage +
               factor(loan_purpose)"

  # Approval regression (MSA + Lender FE)
  cat("\n  Running approval regression (MSA + Lender FE)...\n")
  approval_models[[year]] <- feols(
    as.formula(paste("approved ~", race_formula, "+", controls, "| derived_msa_md + lei")),
    data = samples$approval,
    vcov = ~ lei
  )
  cat("Complete\n")

  # Pricing regression (MSA + Lender FE)
  cat("  Running pricing regression (MSA + Lender FE)...\n")
  pricing_models[[year]] <- feols(
    as.formula(paste("rate_num ~", race_formula, "+", controls, "| derived_msa_md + lei")),
    data = samples$pricing,
    vcov = ~ lei
  )
  cat("Complete\n")

  # Store summary stats
  summary_stats <- rbind(summary_stats, data.table(
    Year = year,
    N_Approval = nrow(samples$approval),
    Approval_Rate = mean(samples$approval$approved) * 100,
    N_Pricing = nrow(samples$pricing),
    Mean_Rate = mean(samples$pricing$rate_num)
  ))

  # Free memory
  rm(samples)
  gc(verbose = FALSE)

  cat(sprintf("\nYear %s complete\n\n", year))
}

# ============================================================================
# CREATE COMPARISON TABLES
# ============================================================================

cat("\n")
cat("================================================================================\n")
cat("CREATING COMPARISON TABLES\n")
cat("================================================================================\n\n")

# Coefficient name dictionary
coef_dict <- c(
  "race_group::NH Black" = "NH Black",
  "race_group::Hispanic" = "Hispanic",
  "race_group::NH Asian" = "NH Asian",
  "race_group::Other" = "Other",
  "race_group::Race NA" = "Race NA"
)

# Table 1: Approval Disparities Across Years
cat("Creating Table 1: Approval Disparities (2020-2022)...\n")

approval_table <- etable(
  approval_models[["2020"]],
  approval_models[["2021"]],
  approval_models[["2022"]],
  tex = TRUE,
  style.tex = style.tex("aer"),
  title = NULL,
  dict = coef_dict,
  headers = c("2020", "2021", "2022"),
  notes = c(
    "Dependent variable: Approved = 1[action_taken in {1,2}]. Reference group: NH White.",
    "All columns: MSA + Lender (LEI) fixed effects, standard errors clustered by LEI.",
    paste0("Controls: log(income), DTI bands, CLTV bins, log(loan amount), log(property value),"),
    "tract minority share, tract median income, loan purpose.",
    "Coefficients in percentage points. Negative = lower approval probability for minority group."
  ),
  digits = 3,
  digits.stats = 1,
  keep = "%race_group",
  fixef_sizes = TRUE,
  fixef_sizes.simplify = FALSE
)

# Save approval table
writeLines(approval_table, file.path(OUTPUT_DIR, "panel_approval_2020_2022.tex"))
cat("  Saved: panel_approval_2020_2022.tex\n")

# Create CSV version for Excel
approval_coefs <- data.table(
  `Race/Ethnicity` = c("NH Black", "Hispanic", "NH Asian", "Other", "Race NA")
)

for (year in names(approval_models)) {
  model <- approval_models[[year]]
  coefs <- coef(model)
  ses <- se(model)

  # Extract coefficients for each race group
  for (race in approval_coefs$`Race/Ethnicity`) {
    coef_name <- paste0("race_group::", race)
    if (coef_name %in% names(coefs)) {
      val <- coefs[coef_name]
      se_val <- ses[coef_name]
      # Format: coefficient (se)
      approval_coefs[`Race/Ethnicity` == race, (year) := sprintf("%.4f (%.4f)", val, se_val)]
    }
  }
}

fwrite(approval_coefs, file.path(OUTPUT_DIR, "panel_approval_2020_2022.csv"))
cat("  Saved: panel_approval_2020_2022.csv\n")

# Table 2: Pricing Disparities Across Years
cat("\nCreating Table 2: Pricing Disparities (2020-2022)...\n")

pricing_table <- etable(
  pricing_models[["2020"]],
  pricing_models[["2021"]],
  pricing_models[["2022"]],
  tex = TRUE,
  style.tex = style.tex("aer"),
  title = NULL,
  dict = coef_dict,
  headers = c("2020", "2021", "2022"),
  notes = c(
    "Dependent variable: Contract interest rate (percentage points). Reference group: NH White.",
    "All columns: MSA + Lender (LEI) fixed effects, standard errors clustered by LEI.",
    "Sample: Originated loans (action_taken = 1) with valid rates (0 < rate < 25).",
    paste0("Controls: log(income), DTI bands, CLTV bins, log(loan amount), log(property value),"),
    "tract minority share, tract median income, loan purpose.",
    "Coefficients in percentage points. Multiply by 100 for basis points (bps)."
  ),
  digits = 3,
  digits.stats = 1,
  keep = "%race_group",
  fixef_sizes = TRUE,
  fixef_sizes.simplify = FALSE
)

# Save pricing table
writeLines(pricing_table, file.path(OUTPUT_DIR, "panel_pricing_2020_2022.tex"))
cat("  Saved: panel_pricing_2020_2022.tex\n")

# Create CSV version for Excel
pricing_coefs <- data.table(
  `Race/Ethnicity` = c("NH Black", "Hispanic", "NH Asian", "Other", "Race NA")
)

for (year in names(pricing_models)) {
  model <- pricing_models[[year]]
  coefs <- coef(model)
  ses <- se(model)

  # Extract coefficients for each race group (convert to basis points)
  for (race in pricing_coefs$`Race/Ethnicity`) {
    coef_name <- paste0("race_group::", race)
    if (coef_name %in% names(coefs)) {
      val <- coefs[coef_name] * 100  # Convert to bps
      se_val <- ses[coef_name] * 100
      # Format: coefficient (se)
      pricing_coefs[`Race/Ethnicity` == race, (year) := sprintf("%.2f (%.2f)", val, se_val)]
    }
  }
}

fwrite(pricing_coefs, file.path(OUTPUT_DIR, "panel_pricing_2020_2022.csv"))
cat("  Saved: panel_pricing_2020_2022.csv\n")

# ============================================================================
# SUMMARY STATISTICS TABLE
# ============================================================================

cat("\n")
cat("================================================================================\n")
cat("SUMMARY STATISTICS ACROSS YEARS\n")
cat("================================================================================\n\n")

print(summary_stats)

# Save summary stats
fwrite(summary_stats, file.path(OUTPUT_DIR, "panel_summary_stats_2020_2022.csv"))
cat("\nSaved: panel_summary_stats_2020_2022.csv\n")

# ============================================================================
# COMPLETION SUMMARY
# ============================================================================

cat("\n")
cat("================================================================================\n")
cat("PANEL ANALYSIS COMPLETE\n")
cat("================================================================================\n\n")

cat("TABLES GENERATED:\n")
cat("  panel_approval_2020_2022.tex    - Approval disparities across 3 years\n")
cat("  panel_approval_2020_2022.csv    - Approval coefficients (Excel format)\n")
cat("  panel_pricing_2020_2022.tex     - Pricing disparities across 3 years\n")
cat("  panel_pricing_2020_2022.csv     - Pricing coefficients in bps (Excel format)\n")
cat("  panel_summary_stats_2020_2022.csv - Sample sizes and means\n\n")

cat("OUTPUT LOCATION:\n")
cat("  All files saved to: output/results/tables/\n\n")

cat("LATEX INTEGRATION:\n")
cat("  Include approval table: \\input{output/results/tables/panel_approval_2020_2022.tex}\n")
cat("  Include pricing table:  \\input{output/results/tables/panel_pricing_2020_2022.tex}\n\n")

cat("All regressions completed successfully\n")
cat("Tables ready for publication\n\n")

# ============================================================================
# INTERPRETATION GUIDE
# ============================================================================

cat("================================================================================\n")
cat("INTERPRETATION GUIDE\n")
cat("================================================================================\n\n")

cat("APPROVAL TABLE (percentage points):\n")
cat("  • Each column = one year (2020, 2021, 2022)\n")
cat("  • Each row = racial/ethnic group relative to NH White\n")
cat("  • Negative coefficient = lower approval rate\n")
cat("  • Example: -0.057 = 5.7 percentage points lower approval\n\n")

cat("PRICING TABLE (basis points when multiplied by 100):\n")
cat("  • Each column = one year (2020, 2021, 2022)\n")
cat("  • Each row = racial/ethnic group relative to NH White\n")
cat("  • Positive coefficient = higher interest rate (more expensive)\n")
cat("  • Example: 0.047 pp = 4.7 basis points higher rate\n\n")

cat("KEY QUESTIONS TO INVESTIGATE:\n")
cat("  1. Are disparities widening or narrowing over time?\n")
cat("  2. Did the COVID-19 pandemic (2020) affect some groups more?\n")
cat("  3. Are trends consistent across approval and pricing?\n")
cat("  4. Which groups show the most/least change?\n\n")

cat("================================================================================\n")

cat("\nModels stored in R environment:\n")
cat("  - approval_models$'2020', approval_models$'2021', approval_models$'2022'\n")
cat("  - pricing_models$'2020', pricing_models$'2021', pricing_models$'2022'\n\n")
