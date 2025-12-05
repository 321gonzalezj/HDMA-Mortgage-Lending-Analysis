# Generate Heterogeneity Tables 5-7 Only
# (Tables 0-4 and Figures 1-3 already completed)

cat("================================================================================\n")
cat("GENERATING HETEROGENEITY TABLES (5-7)\n")
cat("================================================================================\n\n")

# Load required packages
library(data.table)
library(fixest)
library(kableExtra)

# Set directories
TABLES_DIR <- "output/results/tables"
if (!dir.exists(TABLES_DIR)) dir.create(TABLES_DIR, recursive = TRUE)

# Source the main data script to get models and data
cat("Loading data and base models...\n")
source("scripts/HDMA_Data.R")

# Helper functions
fmt_num <- function(x) format(x, big.mark = ",", scientific = FALSE)
fmt_pct <- function(x) sprintf("%.1f%%", x * 100)

# Get race formula and controls from HDMA_Data.R environment
race_formula <- "i(race_group, ref = 'NH White')"
controls <- paste(c(
  "log_income",
  "dti_band",
  "cltv_bin",
  "log_loan",
  "log_pv",
  "tract_minority_population_percent",
  "tract_to_msa_income_percentage",
  "factor(loan_purpose)"
), collapse = " + ")

# ============================================================================
# TABLE 5: HETEROGENEITY BY LOAN PURPOSE
# ============================================================================
cat("\n=== Generating Table 5: Heterogeneity by Loan Purpose ===\n")

# Split samples by loan_purpose
approval_purchase <- approval_dt[loan_purpose == "Home purchase"]
approval_refi <- approval_dt[loan_purpose == "Refinancing"]
pricing_purchase <- pricing_dt[loan_purpose == "Home purchase"]
pricing_refi <- pricing_dt[loan_purpose == "Refinancing"]

# Run regressions on subsamples (using MSA FE for memory efficiency)
cat("  Running approval regressions by purpose...\n")
m_appr_purchase <- feols(
  as.formula(paste("approved ~", race_formula, "+", controls, "| derived_msa_md")),
  data = approval_purchase,
  vcov = ~ derived_msa_md
)

m_appr_refi <- feols(
  as.formula(paste("approved ~", race_formula, "+", controls, "| derived_msa_md")),
  data = approval_refi,
  vcov = ~ derived_msa_md
)

cat("  Running pricing regressions by purpose...\n")
m_rate_purchase <- feols(
  as.formula(paste("rate_num ~", race_formula, "+", controls, "| derived_msa_md")),
  data = pricing_purchase,
  vcov = ~ derived_msa_md
)

m_rate_refi <- feols(
  as.formula(paste("rate_num ~", race_formula, "+", controls, "| derived_msa_md")),
  data = pricing_refi,
  vcov = ~ derived_msa_md
)

# Create regression table
table5_tex <- etable(m_appr_purchase, m_appr_refi, m_rate_purchase, m_rate_refi,
                     tex = TRUE,
                     style.tex = style.tex("aer"),
                     title = NULL,
                     dict = c(
                       "race_group::NH Black" = "NH Black",
                       "race_group::Hispanic" = "Hispanic",
                       "race_group::NH Asian" = "NH Asian",
                       "race_group::Other" = "Other",
                       "race_group::Race NA" = "Race NA"
                     ),
                     headers = c("Approval\\nPurchase", "Approval\\nRefi",
                                "Rate\\nPurchase", "Rate\\nRefi"),
                     notes = c(
                       "Heterogeneity by loan purpose (purchase vs refinance).",
                       "All columns: MSA fixed effects, standard errors clustered by MSA.",
                       "Approval dependent variable: 1[action_taken = 1]. Rate dependent variable: percentage points.",
                       "Controls as in main specifications (Tables 2-3)."
                     ),
                     digits = 3,
                     digits.stats = 1,
                     keep = "%race_group",
                     fixef_sizes = TRUE)

writeLines(table5_tex, file.path(TABLES_DIR, "table5_het_purpose.tex"))
cat("Saved: table5_het_purpose.tex\n")

cat(sprintf("  N (purchase approval): %s\n", fmt_num(nrow(approval_purchase))))
cat(sprintf("  N (refi approval): %s\n", fmt_num(nrow(approval_refi))))

# ============================================================================
# TABLE 6: HETEROGENEITY BY TRACT MINORITY SHARE QUARTILE
# ============================================================================
cat("\n=== Generating Table 6: Heterogeneity by Tract Minority Share ===\n")

# Create minority share variable (using tract_minority_population_percent)
# Split into quartiles based on approval sample
approval_dt[, minority_quartile := cut(tract_minority_population_percent,
                                        breaks = quantile(tract_minority_population_percent,
                                                         probs = seq(0, 1, 0.25),
                                                         na.rm = TRUE),
                                        labels = c("Q1 (Low)", "Q2", "Q3", "Q4 (High)"),
                                        include.lowest = TRUE)]

pricing_dt[, minority_quartile := cut(tract_minority_population_percent,
                                       breaks = quantile(tract_minority_population_percent,
                                                        probs = seq(0, 1, 0.25),
                                                        na.rm = TRUE),
                                       labels = c("Q1 (Low)", "Q2", "Q3", "Q4 (High)"),
                                       include.lowest = TRUE)]

# Run regressions by quartile
cat("  Running approval regressions by minority share quartile...\n")
m_appr_q1 <- feols(
  as.formula(paste("approved ~", race_formula, "+", controls, "| derived_msa_md")),
  data = approval_dt[minority_quartile == "Q1 (Low)"],
  vcov = ~ derived_msa_md
)
m_appr_q2 <- feols(
  as.formula(paste("approved ~", race_formula, "+", controls, "| derived_msa_md")),
  data = approval_dt[minority_quartile == "Q2"],
  vcov = ~ derived_msa_md
)
m_appr_q3 <- feols(
  as.formula(paste("approved ~", race_formula, "+", controls, "| derived_msa_md")),
  data = approval_dt[minority_quartile == "Q3"],
  vcov = ~ derived_msa_md
)
m_appr_q4 <- feols(
  as.formula(paste("approved ~", race_formula, "+", controls, "| derived_msa_md")),
  data = approval_dt[minority_quartile == "Q4 (High)"],
  vcov = ~ derived_msa_md
)

cat("  Running pricing regressions by minority share quartile...\n")
m_rate_q1 <- feols(
  as.formula(paste("rate_num ~", race_formula, "+", controls, "| derived_msa_md")),
  data = pricing_dt[minority_quartile == "Q1 (Low)"],
  vcov = ~ derived_msa_md
)
m_rate_q2 <- feols(
  as.formula(paste("rate_num ~", race_formula, "+", controls, "| derived_msa_md")),
  data = pricing_dt[minority_quartile == "Q2"],
  vcov = ~ derived_msa_md
)
m_rate_q3 <- feols(
  as.formula(paste("rate_num ~", race_formula, "+", controls, "| derived_msa_md")),
  data = pricing_dt[minority_quartile == "Q3"],
  vcov = ~ derived_msa_md
)
m_rate_q4 <- feols(
  as.formula(paste("rate_num ~", race_formula, "+", controls, "| derived_msa_md")),
  data = pricing_dt[minority_quartile == "Q4 (High)"],
  vcov = ~ derived_msa_md
)

# Create two-panel table
table6_tex <- etable(list(
                       "Panel A: Approval" = list(m_appr_q1, m_appr_q2, m_appr_q3, m_appr_q4),
                       "Panel B: Pricing" = list(m_rate_q1, m_rate_q2, m_rate_q3, m_rate_q4)
                     ),
                     tex = TRUE,
                     style.tex = style.tex("aer"),
                     title = NULL,
                     dict = c(
                       "race_group::NH Black" = "NH Black",
                       "race_group::Hispanic" = "Hispanic",
                       "race_group::NH Asian" = "NH Asian",
                       "race_group::Other" = "Other",
                       "race_group::Race NA" = "Race NA"
                     ),
                     headers = c("Q1\\n(Low)", "Q2", "Q3", "Q4\\n(High)"),
                     notes = c(
                       "Heterogeneity by tract-level minority population share (quartiles).",
                       "All columns: MSA fixed effects, standard errors clustered by MSA.",
                       "Quartiles based on tract_minority_population_percent distribution.",
                       "Controls as in main specifications (Tables 2-3)."
                     ),
                     digits = 3,
                     digits.stats = 1,
                     keep = "%race_group",
                     fixef_sizes = FALSE)

writeLines(table6_tex, file.path(TABLES_DIR, "table6_het_tract.tex"))
cat("Saved: table6_het_tract.tex\n")

# ============================================================================
# TABLE 7: HETEROGENEITY BY LENDER TYPE
# ============================================================================
cat("\n=== Generating Table 7: Heterogeneity by Lender Type ===\n")

# Create lender type variable
approval_dt[, lender_type := ifelse(derived_institution_type_flag == 1, "Bank", "Nonbank")]
pricing_dt[, lender_type := ifelse(derived_institution_type_flag == 1, "Bank", "Nonbank")]

# Run regressions by lender type
cat("  Running approval regressions by lender type...\n")
m_appr_bank <- feols(
  as.formula(paste("approved ~", race_formula, "+", controls, "| derived_msa_md")),
  data = approval_dt[lender_type == "Bank"],
  vcov = ~ derived_msa_md
)

m_appr_nonbank <- feols(
  as.formula(paste("approved ~", race_formula, "+", controls, "| derived_msa_md")),
  data = approval_dt[lender_type == "Nonbank"],
  vcov = ~ derived_msa_md
)

cat("  Running pricing regressions by lender type...\n")
m_rate_bank <- feols(
  as.formula(paste("rate_num ~", race_formula, "+", controls, "| derived_msa_md")),
  data = pricing_dt[lender_type == "Bank"],
  vcov = ~ derived_msa_md
)

m_rate_nonbank <- feols(
  as.formula(paste("rate_num ~", race_formula, "+", controls, "| derived_msa_md")),
  data = pricing_dt[lender_type == "Nonbank"],
  vcov = ~ derived_msa_md
)

# Create two-panel table
table7_tex <- etable(list(
                       "Panel A: Approval" = list(m_appr_bank, m_appr_nonbank),
                       "Panel B: Pricing" = list(m_rate_bank, m_rate_nonbank)
                     ),
                     tex = TRUE,
                     style.tex = style.tex("aer"),
                     title = NULL,
                     dict = c(
                       "race_group::NH Black" = "NH Black",
                       "race_group::Hispanic" = "Hispanic",
                       "race_group::NH Asian" = "NH Asian",
                       "race_group::Other" = "Other",
                       "race_group::Race NA" = "Race NA"
                     ),
                     headers = c("Bank", "Nonbank"),
                     notes = c(
                       "Heterogeneity by lender institution type (depository vs nondepository).",
                       "All columns: MSA fixed effects, standard errors clustered by MSA.",
                       "Bank = depository (type=1); Nonbank = for-profit/nonprofit nondepository (type=2-4).",
                       "Controls as in main specifications (Tables 2-3)."
                     ),
                     digits = 3,
                     digits.stats = 1,
                     keep = "%race_group",
                     fixef_sizes = FALSE)

writeLines(table7_tex, file.path(TABLES_DIR, "table7_het_lender.tex"))
cat("Saved: table7_het_lender.tex\n")

cat(sprintf("  N (bank approval): %s\n",
            fmt_num(nrow(approval_dt[lender_type == "Bank"]))))
cat(sprintf("  N (nonbank approval): %s\n",
            fmt_num(nrow(approval_dt[lender_type == "Nonbank"]))))

cat("\n================================================================================\n")
cat("HETEROGENEITY TABLES COMPLETE\n")
cat("================================================================================\n")
cat("Generated:\n")
cat("  - table5_het_purpose.tex\n")
cat("  - table6_het_tract.tex\n")
cat("  - table7_het_lender.tex\n")
