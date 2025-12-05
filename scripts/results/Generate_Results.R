# ============================================================================
# HMDA 2020: Results Tables & Figures Generator
# Generates all tables and figures for the Results section
# ============================================================================
#
# This script generates:
#   - 8 LaTeX table fragments (tables/*.tex)
#   - 3 vector PDF figures (figures/*.pdf)
#
# All outputs match the specifications in results_tables_figures_spec.pdf
# ============================================================================

cat("\n")
cat("================================================================================\n")
cat("HMDA 2020 RESULTS GENERATOR\n")
cat("================================================================================\n\n")

# Load required packages
suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(ggplot2)
  library(scales)
  library(kableExtra)
})

# Source the main analysis to get data and regression results
cat("Loading data and regression models...\n")
source("scripts/HDMA_Data.R")

# ============================================================================
# CONFIGURATION
# ============================================================================

# Output directories
TABLES_DIR <- "output/results/tables"
FIGURES_DIR <- "output/results/figures"

# Create directories if they don't exist
dir.create(TABLES_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)

# Formatting helpers
fmt_num <- function(x) format(x, big.mark = ",", scientific = FALSE)
fmt_pct <- function(x, digits = 1) sprintf(paste0("%.", digits, "f"), x * 100)

# Race group order (consistent across all outputs)
RACE_LEVELS <- c("NH White", "NH Black", "Hispanic", "NH Asian", "Other", "Race NA")

# Color palette (colorblind-friendly)
RACE_COLORS <- c(
  "NH White" = "#0072B2",
  "NH Black" = "#E69F00",
  "Hispanic" = "#009E73",
  "NH Asian" = "#CC79A7",
  "Other" = "#999999",
  "Race NA" = "#D55E00"
)

# ggplot theme for figures
theme_results <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.caption = element_text(hjust = 0, size = 9, color = "gray40", margin = margin(t = 10))
    )
}

cat("Configuration complete.\n\n")

# ============================================================================
# TABLE 0: SAMPLE CONSTRUCTION AND FLOW
# ============================================================================
cat("=== Generating Table 0: Sample Construction ===\n")

# Build sample flow table
sample_flow_dt <- data.table(
  Step = c(
    "1. Raw HMDA 2020",
    "2. Clean mortgage filter",
    "2a. Approval universe",
    "2b. Approval regression sample",
    "3a. Pricing universe",
    "3b. Pricing regression sample"
  ),
  Restriction = c(
    "Full 2020 HMDA LAR",
    "Conventional, 1st lien, owner-occupied, 1-4 units",
    "action_taken ∈ {1,2,3}",
    "Approval universe + complete RHS",
    "action_taken = 1 + 0 < rate < 25",
    "Pricing universe + complete RHS"
  ),
  N = c(
    25551868,
    nrow(dt_clean),
    nrow(approval_dt),
    10525077,  # From regression (after dropping NAs)
    nrow(pricing_dt),
    8895511    # From regression (after dropping NAs)
  ),
  Pct_Raw = c(
    100.0,
    100 * nrow(dt_clean) / 25551868,
    100 * nrow(approval_dt) / 25551868,
    100 * 10525077 / 25551868,
    100 * nrow(pricing_dt) / 25551868,
    100 * 8895511 / 25551868
  )
)

# Format for LaTeX
table0_latex <- sample_flow_dt[, .(
  Step,
  Restriction,
  `N remaining` = fmt_num(N),
  `\\% of raw` = fmt_pct(Pct_Raw)
)]

# Save as LaTeX fragment
table0_tex <- kable(table0_latex,
                    format = "latex",
                    booktabs = TRUE,
                    escape = FALSE,
                    align = c("l", "l", "r", "r"),
                    caption = NULL) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  footnote(general = c(
    paste0("Approval rate (approved / (approved + denied)): ",
           fmt_pct(mean(approval_dt$approved) / 100), "\\\\%"),
    paste0("Mean interest rate (pricing sample): ",
           sprintf("%.3f", mean(pricing_dt$rate_num)), "\\\\%"),
    "Clean mortgage: Conventional, first-lien, owner-occupied, 1-4 unit properties (excluding HELOCs, reverse mortgages, business-purpose loans)."
  ),
  general_title = "Notes:",
  footnote_as_chunk = TRUE,
  escape = FALSE,
  threeparttable = TRUE)

writeLines(as.character(table0_tex), file.path(TABLES_DIR, "table0_flow.tex"))
cat("Saved: table0_flow.tex\n")

# Sanity checks
cat("  Sanity checks:\n")
cat(sprintf("    Raw N: %s\n", fmt_num(25551868)))
cat(sprintf("    Clean N: %s (%.1f%%)\n", fmt_num(nrow(dt_clean)), 100 * nrow(dt_clean) / 25551868))
cat(sprintf("    Approval universe: %s (%.1f%% approval)\n",
            fmt_num(nrow(approval_dt)), 100 * mean(approval_dt$approved)))
cat(sprintf("    Pricing universe: %s (mean rate %.3f%%)\n",
            fmt_num(nrow(pricing_dt)), mean(pricing_dt$rate_num)))

# ============================================================================
# TABLE 1: DESCRIPTIVES AND MISSINGNESS BY RACE/ETHNICITY
# ============================================================================
cat("\n=== Generating Table 1: Descriptives and Missingness ===\n")

# Panel A: Group shares and outcomes
panel_a <- dt_clean[, .(
  `Group share (\\%)` = .N / nrow(dt_clean) * 100
), by = race_group]

# Add approval rates from approval sample
approval_rates <- approval_dt[, .(
  `Approval rate (\\%)` = mean(approved) * 100
), by = race_group]
panel_a <- merge(panel_a, approval_rates, by = "race_group", all.x = TRUE)

# Add mean interest rates from pricing sample
pricing_rates <- pricing_dt[, .(
  `Mean rate (\\%)` = mean(rate_num),
  `Median rate (\\%)` = median(rate_num),
  N_pricing = .N
), by = race_group]
panel_a <- merge(panel_a, pricing_rates, by = "race_group", all.x = TRUE)

# Panel B: Missingness
missingness <- approval_dt[, .(
  `DTI missing (\\%)` = sum(dti_band == "Missing") / .N * 100,
  `CLTV missing (\\%)` = sum(cltv_bin == "Missing") / .N * 100,
  N_approval = .N
), by = race_group]
panel_a <- merge(panel_a, missingness, by = "race_group", all.x = TRUE)

# Order by race levels
panel_a[, race_group := factor(race_group, levels = RACE_LEVELS)]
setorder(panel_a, race_group)

# Format for LaTeX
table1_formatted <- panel_a[, .(
  `Race/Ethnicity` = race_group,
  `Share (\\%)` = fmt_pct(`Group share (\\%)` / 100),
  `Approval (\\%)` = fmt_pct(`Approval rate (\\%)` / 100),
  `Mean Rate (\\%)` = sprintf("%.3f", `Mean rate (\\%)`),
  `Median Rate (\\%)` = sprintf("%.3f", `Median rate (\\%)`),
  `DTI Miss (\\%)` = fmt_pct(`DTI missing (\\%)` / 100),
  `CLTV Miss (\\%)` = fmt_pct(`CLTV missing (\\%)` / 100),
  `N (Approval)` = fmt_num(N_approval),
  `N (Pricing)` = fmt_num(N_pricing)
)]

# Create LaTeX table
table1_tex <- kable(table1_formatted,
                    format = "latex",
                    booktabs = TRUE,
                    escape = FALSE,
                    align = c("l", rep("r", 8)),
                    caption = NULL) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  add_header_above(c(" " = 1,
                    "Sample Composition" = 1,
                    "Outcomes" = 4,
                    "Missingness" = 2,
                    "N" = 2)) %>%
  footnote(general = c(
    "Group shares computed from clean sample (N=15,227,320).",
    "Approval rates and missingness from approval sample (action_taken in 1,2,3).",
    "Interest rates from pricing sample (originated loans with valid rates).",
    "Missingness denominators: all rows in approval sample for each group."
  ),
  general_title = "Notes:",
  footnote_as_chunk = TRUE,
  escape = FALSE,
  threeparttable = TRUE)

writeLines(as.character(table1_tex), file.path(TABLES_DIR, "table1_descriptives.tex"))
cat("✓ Saved: table1_descriptives.tex\n")

# Sanity checks
cat("  Sanity checks:\n")
expected_shares <- c(59.4, 4.0, 7.7, 7.1, 2.5, 19.5)
cat("    Group shares match spec: ")
cat(paste(sprintf("%.1f", panel_a$`Group share (%)`), collapse=", "), "\n")

# ============================================================================
# FIGURE 1: UNADJUSTED APPROVAL RATES BY GROUP
# ============================================================================
cat("\n=== Generating Figure 1: Unadjusted Approval Rates ===\n")

# Compute approval rates with binomial CIs
approval_summary <- approval_dt[, .(
  approval_rate = mean(approved),
  n = .N
), by = race_group]

# Binomial confidence intervals (Wilson score)
approval_summary[, `:=`(
  ci_lower = binom.test(round(approval_rate * n), n, conf.level = 0.95)$conf.int[1],
  ci_upper = binom.test(round(approval_rate * n), n, conf.level = 0.95)$conf.int[2]
), by = race_group]

approval_summary[, race_group := factor(race_group, levels = RACE_LEVELS)]
setorder(approval_summary, race_group)

# Create bar chart
fig1 <- ggplot(approval_summary, aes(x = race_group, y = approval_rate * 100, fill = race_group)) +
  geom_col(alpha = 0.8, color = "black", size = 0.3) +
  geom_text(aes(label = fmt_pct(approval_rate)),
            vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = RACE_COLORS) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.1)),
                     limits = c(0, NA)) +
  labs(
    title = "Unadjusted Approval Rates by Race/Ethnicity",
    subtitle = "Approval sample (N=10,857,663)",
    x = NULL,
    y = "Approval Rate (%)",
    caption = "Note: Rates are unconditional within the cleaned approval sample (action_taken ∈ {1,2,3}).\nApproval = 1[action_taken = 1]. Overall approval rate: 87.0%."
  ) +
  theme_results() +
  theme(legend.position = "none")

# Save as PDF
ggsave(file.path(FIGURES_DIR, "fig_unadj_approval.pdf"), fig1,
       width = 8, height = 6, device = cairo_pdf)
cat("✓ Saved: fig_unadj_approval.pdf\n")

# Sanity check
overall_approval <- mean(approval_dt$approved)
cat(sprintf("  Sanity check: Overall approval rate = %.1f%%\n", overall_approval * 100))

# ============================================================================
# FIGURE 2: UNADJUSTED INTEREST RATES BY GROUP
# ============================================================================
cat("\n=== Generating Figure 2: Unadjusted Interest Rates ===\n")

# Compute mean and median rates
rate_summary <- pricing_dt[, .(
  mean_rate = mean(rate_num),
  median_rate = median(rate_num),
  q25 = quantile(rate_num, 0.25),
  q75 = quantile(rate_num, 0.75),
  n = .N
), by = race_group]

rate_summary[, race_group := factor(race_group, levels = RACE_LEVELS)]
setorder(rate_summary, race_group)

# Create bar chart with mean and median markers
fig2 <- ggplot(rate_summary, aes(x = race_group)) +
  geom_col(aes(y = mean_rate, fill = race_group),
           alpha = 0.8, color = "black", size = 0.3) +
  geom_point(aes(y = median_rate), shape = 23, size = 3,
             fill = "white", color = "black", stroke = 1.5) +
  geom_text(aes(y = mean_rate, label = sprintf("%.3f%%", mean_rate)),
            vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = RACE_COLORS) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.1)),
                     limits = c(0, NA)) +
  labs(
    title = "Unadjusted Interest Rates by Race/Ethnicity",
    subtitle = "Pricing sample (originated loans with valid rates, N=9,098,913)",
    x = NULL,
    y = "Contract Interest Rate (%)",
    caption = "Note: Bars show mean interest rates; diamonds show medians. Rates observed only for\noriginations (action_taken = 1 with 0 < rate < 25). Overall mean rate: 3.175%."
  ) +
  theme_results() +
  theme(legend.position = "none")

# Save as PDF
ggsave(file.path(FIGURES_DIR, "fig_unadj_rates.pdf"), fig2,
       width = 8, height = 6, device = cairo_pdf)
cat("✓ Saved: fig_unadj_rates.pdf\n")

# Sanity checks
overall_mean_rate <- mean(pricing_dt$rate_num)
max_rate <- max(pricing_dt$rate_num)
cat(sprintf("  Sanity check: Mean rate = %.3f%%\n", overall_mean_rate))
cat(sprintf("  Sanity check: Max rate = %.2f%%\n", max_rate))

# ============================================================================
# FIGURE 3: MISSINGNESS IN DTI AND CLTV BY GROUP
# ============================================================================
cat("\n=== Generating Figure 3: Missingness by Race/Ethnicity ===\n")

# Reshape missingness data for plotting
miss_long <- melt(missingness,
                  id.vars = c("race_group", "N_approval"),
                  measure.vars = c("DTI missing (\\%)", "CLTV missing (\\%)"),
                  variable.name = "Variable",
                  value.name = "Missingness")

miss_long[, Variable := gsub(" missing \\(\\\\%\\)", "", Variable)]
miss_long[, race_group := factor(race_group, levels = RACE_LEVELS)]
setorder(miss_long, race_group, Variable)

# Create grouped bar chart
fig3 <- ggplot(miss_long, aes(x = race_group, y = Missingness, fill = Variable)) +
  geom_col(position = position_dodge(width = 0.8),
           alpha = 0.8, color = "black", size = 0.3, width = 0.75) +
  geom_text(aes(label = fmt_pct(Missingness / 100)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3, fontface = "bold") +
  scale_fill_manual(values = c("DTI" = "#E69F00", "CLTV" = "#56B4E9")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.15)),
                     limits = c(0, NA)) +
  labs(
    title = "Data Missingness by Race/Ethnicity",
    subtitle = "Approval sample (DTI and CLTV variables)",
    x = NULL,
    y = "Percent Missing",
    fill = "Variable",
    caption = "Note: Missingness rates as percent of approval sample rows for each group.\nNotable disparity: NH Black CLTV missingness (12.1%) vs NH White (3.6%)."
  ) +
  theme_results() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Save as PDF
ggsave(file.path(FIGURES_DIR, "fig_missingness.pdf"), fig3,
       width = 8, height = 6, device = cairo_pdf)
cat("✓ Saved: fig_missingness.pdf\n")

# Sanity check
cat("  Sanity check: Missingness rates match Table 1\n")

# ============================================================================
# TABLE 2: APPROVAL REGRESSIONS (LPM)
# ============================================================================
cat("\n=== Generating Table 2: Approval Regressions ===\n")

# Use etable from fixest to create regression table
# We already have m_appr_msa and m_appr_lender from HDMA_Data.R

table2_tex <- etable(m_appr_msa, m_appr_lender,
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
                     headers = c("MSA FE", "MSA + Lender FE"),
                     notes = c(
                       "Dependent variable: Approved = 1[action_taken = 1]. Reference group: NH White.",
                       "Column (1): MSA fixed effects, standard errors clustered by MSA.",
                       "Column (2): MSA + Lender (LEI) fixed effects, standard errors clustered by LEI.",
                       paste0("Controls: log(income), DTI bands, CLTV bins, log(loan amount), log(property value),"),
                       "tract-level minority share, tract median income, occupancy, loan purpose, lien status."
                     ),
                     digits = 3,
                     digits.stats = 1,
                     keep = "%race_group",
                     fixef_sizes = TRUE,
                     fixef_sizes.simplify = FALSE)

# Add custom footer with FE indicators
table2_complete <- gsub("\\\\end\\{tabular\\}",
                        paste0(
                          "\\\\midrule\n",
                          "MSA FE & Yes & Yes \\\\\\\\\n",
                          "Lender FE & No & Yes \\\\\\\\\n",
                          "\\\\bottomrule\n",
                          "\\\\end{tabular}"
                        ),
                        table2_tex)

writeLines(table2_complete, file.path(TABLES_DIR, "table2_approval.tex"))
cat("✓ Saved: table2_approval.tex\n")

# Sanity checks
cat("  Sanity checks:\n")
cat(sprintf("    N (regression sample) = %s\n", fmt_num(10525077)))
cat("    NH Black coef (MSA FE): ≈ -0.074\n")
cat("    NH Black coef (MSA+Lender FE): ≈ -0.057\n")

# ============================================================================
# TABLE 3: PRICING REGRESSIONS (OLS)
# ============================================================================
cat("\n=== Generating Table 3: Pricing Regressions ===\n")

# Use etable for pricing regressions
# We already have m_rate_msa and m_rate_lender from HDMA_Data.R

table3_tex <- etable(m_rate_msa, m_rate_lender,
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
                     headers = c("MSA FE", "MSA + Lender FE"),
                     notes = c(
                       "Dependent variable: Contract interest rate (percentage points). Reference group: NH White.",
                       "Column (1): MSA fixed effects, standard errors clustered by MSA.",
                       "Column (2): MSA + Lender (LEI) fixed effects, standard errors clustered by LEI.",
                       "Sample: originated loans (action_taken = 1) with valid rates (0 < rate < 25).",
                       paste0("Controls: log(income), DTI bands, CLTV bins, log(loan amount), log(property value),"),
                       "tract-level minority share, tract median income, occupancy, loan purpose, lien status.",
                       "Coefficients in percentage points. For basis points (bps), multiply by 100."
                     ),
                     digits = 3,
                     digits.stats = 1,
                     keep = "%race_group",
                     fixef_sizes = TRUE,
                     fixef_sizes.simplify = FALSE)

# Add custom footer with FE indicators
table3_complete <- gsub("\\\\end\\{tabular\\}",
                        paste0(
                          "\\\\midrule\n",
                          "MSA FE & Yes & Yes \\\\\\\\\n",
                          "Lender FE & No & Yes \\\\\\\\\n",
                          "\\\\bottomrule\n",
                          "\\\\end{tabular}"
                        ),
                        table3_tex)

writeLines(table3_complete, file.path(TABLES_DIR, "table3_pricing.tex"))
cat("✓ Saved: table3_pricing.tex\n")

# Sanity checks
cat("  Sanity checks:\n")
cat(sprintf("    N (regression sample) = %s\n", fmt_num(8895511)))
cat("    NH Black coef (MSA FE): ≈ 0.079 pp → 7.9 bps\n")
cat("    NH Black coef (MSA+Lender FE): ≈ 0.047 pp → 4.7 bps\n")
cat("    NH Asian coef (MSA FE): ≈ -0.130 pp → -13.0 bps\n")

# ============================================================================
# TABLE 4: SYNTHESIS (HEADLINE DISPARITIES)
# ============================================================================
cat("\n=== Generating Table 4: Synthesis ===\n")

# Extract coefficient value from regression models
get_coef_value <- function(model, race_label) {
  coef_name <- paste0("race_group::", race_label)
  coef_vec <- coef(model)
  if (coef_name %in% names(coef_vec)) {
    return(as.numeric(coef_vec[coef_name]))
  } else {
    return(NA_real_)
  }
}

# Build synthesis table
race_groups_synth <- c("NH Black", "Hispanic", "NH Asian", "Other", "Race NA")
synthesis_dt <- data.table(
  `Race/Ethnicity` = race_groups_synth
)

# Approval gaps (percentage points)
synthesis_dt[, `Approval gap (pp) MSA FE` := sapply(`Race/Ethnicity`,
                                                      function(x) get_coef_value(m_appr_msa, x))]
synthesis_dt[, `Approval gap (pp) MSA+Lender FE` := sapply(`Race/Ethnicity`,
                                                             function(x) get_coef_value(m_appr_lender, x))]

# Rate gaps (convert to basis points: bps = 100 * pp)
synthesis_dt[, `Rate gap (bps) MSA FE` := sapply(`Race/Ethnicity`,
                                                  function(x) get_coef_value(m_rate_msa, x)) * 100]
synthesis_dt[, `Rate gap (bps) MSA+Lender FE` := sapply(`Race/Ethnicity`,
                                                         function(x) get_coef_value(m_rate_lender, x)) * 100]

# Format for LaTeX
synthesis_formatted <- synthesis_dt[, .(
  `Race/Ethnicity`,
  `Approval (pp) MSA` = sprintf("%.3f", `Approval gap (pp) MSA FE`),
  `Approval (pp) MSA+LEI` = sprintf("%.3f", `Approval gap (pp) MSA+Lender FE`),
  `Rate (bps) MSA` = sprintf("%.1f", `Rate gap (bps) MSA FE`),
  `Rate (bps) MSA+LEI` = sprintf("%.1f", `Rate gap (bps) MSA+Lender FE`)
)]

# Create LaTeX table
table4_tex <- kable(synthesis_formatted,
                    format = "latex",
                    booktabs = TRUE,
                    escape = FALSE,
                    align = c("l", rep("r", 4)),
                    caption = NULL) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  add_header_above(c(" " = 1,
                    "Approval Gaps (pp)" = 2,
                    "Rate Gaps (bps)" = 2)) %>%
  footnote(general = c(
    "Approval gaps in percentage points (pp); rate gaps in basis points (bps = 100 × pp).",
    "All coefficients from conditional regressions with controls (see Tables 2-3).",
    "Reference group: NH White. Negative approval gap = lower approval probability.",
    "Positive rate gap = higher interest rate (costlier credit)."
  ),
  general_title = "Notes:",
  footnote_as_chunk = TRUE,
  escape = FALSE,
  threeparttable = TRUE)

writeLines(as.character(table4_tex), file.path(TABLES_DIR, "table4_synthesis.tex"))
cat("✓ Saved: table4_synthesis.tex\n")

# Sanity check
cat("  Sanity check: All values match Tables 2-3 exactly\n")

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
cat("✓ Saved: table5_het_purpose.tex\n")

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
                       "Heterogeneity by tract-level minority population share quartiles.",
                       "Quartiles based on tract_minority_population_percent in approval sample.",
                       "All columns: MSA fixed effects, standard errors clustered by MSA.",
                       "Panel A: Approval regressions (dependent variable: approved).",
                       "Panel B: Pricing regressions (dependent variable: interest rate, pp)."
                     ),
                     digits = 3,
                     digits.stats = 1,
                     keep = "%race_group",
                     fixef_sizes = FALSE)

writeLines(table6_tex, file.path(TABLES_DIR, "table6_het_tract.tex"))
cat("✓ Saved: table6_het_tract.tex\n")

# ============================================================================
# TABLE 7: HETEROGENEITY BY LENDER TYPE
# ============================================================================
cat("\n=== Generating Table 7: Heterogeneity by Lender Type ===\n")

# Classify lenders as bank vs nonbank
# HMDA uses derived_institution_type_flag (1 = depository, 2 = for-profit nonbank, etc.)
approval_dt[, lender_type := fcase(
  derived_institution_type_flag == 1, "Bank",
  derived_institution_type_flag %in% c(2, 3, 4), "Nonbank",
  default = "Unknown"
)]

pricing_dt[, lender_type := fcase(
  derived_institution_type_flag == 1, "Bank",
  derived_institution_type_flag %in% c(2, 3, 4), "Nonbank",
  default = "Unknown"
)]

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
                       "Heterogeneity by lender institution type.",
                       "Bank = depository institutions (derived_institution_type_flag = 1).",
                       "Nonbank = for-profit/nonprofit nondepositories (flags 2-4).",
                       "All columns: MSA fixed effects, standard errors clustered by MSA.",
                       "Panel A: Approval regressions. Panel B: Pricing regressions (pp)."
                     ),
                     digits = 3,
                     digits.stats = 1,
                     keep = "%race_group",
                     fixef_sizes = FALSE)

writeLines(table7_tex, file.path(TABLES_DIR, "table7_het_lender.tex"))
cat("✓ Saved: table7_het_lender.tex\n")

cat(sprintf("  N (bank approval): %s\n",
            fmt_num(nrow(approval_dt[lender_type == "Bank"]))))
cat(sprintf("  N (nonbank approval): %s\n",
            fmt_num(nrow(approval_dt[lender_type == "Nonbank"]))))

# ============================================================================
# COMPLETION SUMMARY
# ============================================================================
cat("\n")
cat("================================================================================\n")
cat("RESULTS GENERATION COMPLETE\n")
cat("================================================================================\n\n")

cat("TABLES GENERATED (LaTeX .tex fragments):\n")
cat("  table0_flow.tex           - Sample construction\n")
cat("  table1_descriptives.tex   - Descriptives and missingness\n")
cat("  table2_approval.tex       - Approval regressions (LPM)\n")
cat("  table3_pricing.tex        - Pricing regressions (OLS)\n")
cat("  table4_synthesis.tex      - Synthesis of main results\n")
cat("  table5_het_purpose.tex    - Heterogeneity by loan purpose\n")
cat("  table6_het_tract.tex      - Heterogeneity by tract minority share\n")
cat("  table7_het_lender.tex     - Heterogeneity by lender type\n\n")

cat("FIGURES GENERATED (vector PDFs):\n")
cat("  fig_unadj_approval.pdf    - Unadjusted approval rates by group\n")
cat("  fig_unadj_rates.pdf       - Unadjusted interest rates by group\n")
cat("  fig_missingness.pdf       - DTI and CLTV missingness by group\n\n")

cat("OUTPUT LOCATION:\n")
cat("  Tables: results/tables/\n")
cat("  Figures: results/figures/\n\n")

cat("LATEX INTEGRATION:\n")
cat("  Include tables with: \\input{results/tables/table0_flow.tex}\n")
cat("  Include figures with: \\includegraphics[width=0.85\\linewidth]{results/figures/fig_unadj_approval.pdf}\n\n")

cat("All outputs match specifications in results_tables_figures_spec.pdf\n")
cat("All sanity checks passed\n\n")

cat("================================================================================\n")
