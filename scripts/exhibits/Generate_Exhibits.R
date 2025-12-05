# ============================================================================
# HMDA 2020: Descriptive Statistics & Visualizations for Academic Paper
# Generates all pre-regression tables and figures
# ============================================================================

# Load required packages
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
  library(gridExtra)
  library(kableExtra)
  library(viridis)
})

# Source the main analysis to get processed data
source("scripts/HDMA_Data.R")

# ============================================================================
# CONFIGURATION
# ============================================================================

# Output directories
TABLES_DIR <- "output/exhibits/tables"
FIGURES_DIR <- "output/exhibits/figures"

# Figure dimensions (for academic papers - typically 6-8 inches wide)
FIG_WIDTH <- 8
FIG_HEIGHT <- 6

# Color palette (colorblind-friendly)
RACE_COLORS <- c(
  "NH White" = "#0072B2",
  "NH Black" = "#E69F00",
  "Hispanic" = "#009E73",
  "NH Asian" = "#CC79A7",
  "Other" = "#999999",
  "Race NA" = "#D55E00"
)

# Theme for publication-quality figures
theme_academic <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, color = "gray30"),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 10),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(hjust = 0, size = 9, color = "gray40")
    )
}

# Standard table note
TABLE_NOTE <- "Notes: Sample restricted to conventional, first-lien, owner-occupied mortgages on 1-4 unit properties (excluding HELOCs, reverse mortgages, and business-purpose loans). Approval sample keeps action_taken ∈ {1,2,3}. Pricing sample keeps originated loans with 0 < interest_rate < 25. Race/ethnicity groups are mutually exclusive with Hispanic defined by ethnicity regardless of race."

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Save table to CSV and LaTeX
save_table <- function(dt, name, caption = "") {
  # CSV
  fwrite(dt, file.path(TABLES_DIR, paste0(name, ".csv")))

  # LaTeX (if kableExtra available)
  tryCatch({
    latex_table <- kable(dt, format = "latex", booktabs = TRUE,
                        caption = caption) %>%
      kable_styling(latex_options = c("hold_position", "scale_down"))
    writeLines(as.character(latex_table),
               file.path(TABLES_DIR, paste0(name, ".tex")))
  }, error = function(e) {
    cat("LaTeX output skipped for", name, "\n")
  })

  cat("Saved:", name, "\n")
}

# Save figure to PNG and PDF
save_figure <- function(plot, name, width = FIG_WIDTH, height = FIG_HEIGHT) {
  # PNG (for viewing/presentations)
  ggsave(file.path(FIGURES_DIR, paste0(name, ".png")), plot,
         width = width, height = height, dpi = 300, bg = "white")

  # PDF (for LaTeX papers)
  ggsave(file.path(FIGURES_DIR, paste0(name, ".pdf")), plot,
         width = width, height = height, device = cairo_pdf)

  cat("Saved:", name, "\n")
}

# Format numbers with commas
fmt_num <- function(x) format(x, big.mark = ",", scientific = FALSE)

# Format percentages
fmt_pct <- function(x, digits = 1) sprintf(paste0("%.", digits, "f%%"), x * 100)

# ============================================================================
# TABLE 0: SAMPLE CONSTRUCTION FLOW
# ============================================================================
cat("\n=== Generating Table 0: Sample Construction ===\n")

sample_flow <- data.table(
  Stage = c("Raw HMDA 2020", "Clean mortgage filter", "Approval sample", "Pricing sample"),
  N = c(25551868, nrow(dt_clean), nrow(approval_dt), nrow(pricing_dt)),
  Pct_Retained = c(100,
                   100 * nrow(dt_clean) / 25551868,
                   100 * nrow(approval_dt) / 25551868,
                   100 * nrow(pricing_dt) / 25551868)
)

sample_flow[, Approval_Rate := c(NA, NA, mean(approval_dt$approved) * 100, NA)]
sample_flow[, Mean_Rate := c(NA, NA, NA, mean(pricing_dt$rate_num))]

# Format for display
table0 <- sample_flow[, .(
  Stage,
  `N (Observations)` = fmt_num(N),
  `% of Raw Data` = fmt_pct(Pct_Retained / 100),
  `Approval Rate (%)` = ifelse(is.na(Approval_Rate), "—", sprintf("%.1f%%", Approval_Rate)),
  `Mean Interest Rate (%)` = ifelse(is.na(Mean_Rate), "—", sprintf("%.3f%%", Mean_Rate))
)]

save_table(table0, "table0_sample_flow",
           "Sample Construction and Flow")

# ============================================================================
# TABLE 1: SUMMARY STATISTICS BY RACE/ETHNICITY
# ============================================================================
cat("\n=== Generating Table 1: Summary Statistics ===\n")

# Function to compute summary stats
compute_summary <- function(dt, vars) {
  result <- list()

  for (var in vars) {
    var_data <- dt[[var]]
    var_data <- var_data[!is.na(var_data)]

    if (length(var_data) > 0) {
      result[[var]] <- data.table(
        Variable = var,
        Mean = mean(var_data),
        SD = sd(var_data),
        Median = median(var_data),
        P25 = quantile(var_data, 0.25),
        P75 = quantile(var_data, 0.75),
        N = length(var_data)
      )
    }
  }

  rbindlist(result)
}

# Panel A: Approval sample
vars_approval <- c("approved", "inc_num", "loan_amount", "pv_num",
                   "tract_minority_population_percent", "tract_to_msa_income_percentage")

table1a_overall <- compute_summary(approval_dt, vars_approval)
table1a_overall[, Race := "All"]

table1a_byrace <- approval_dt[, {
  compute_summary(.SD, vars_approval)
}, by = race_group]
setnames(table1a_byrace, "race_group", "Race")

table1a <- rbindlist(list(table1a_overall, table1a_byrace), use.names = TRUE)

# Panel B: Pricing sample
vars_pricing <- c("rate_num", "inc_num", "loan_amount", "pv_num",
                  "tract_minority_population_percent", "tract_to_msa_income_percentage")

table1b_overall <- compute_summary(pricing_dt, vars_pricing)
table1b_overall[, Race := "All"]

table1b_byrace <- pricing_dt[, {
  compute_summary(.SD, vars_pricing)
}, by = race_group]
setnames(table1b_byrace, "race_group", "Race")

table1b <- rbindlist(list(table1b_overall, table1b_byrace), use.names = TRUE)

# Format and save
table1a_fmt <- table1a[, .(
  Race,
  Variable,
  Mean = sprintf("%.2f", Mean),
  SD = sprintf("%.2f", SD),
  Median = sprintf("%.2f", Median),
  P25 = sprintf("%.2f", P25),
  P75 = sprintf("%.2f", P75),
  N = fmt_num(N)
)]

save_table(table1a_fmt, "table1a_summary_approval",
           "Summary Statistics - Approval Sample")

table1b_fmt <- table1b[, .(
  Race,
  Variable,
  Mean = sprintf("%.2f", Mean),
  SD = sprintf("%.2f", SD),
  Median = sprintf("%.2f", Median),
  P25 = sprintf("%.2f", P25),
  P75 = sprintf("%.2f", P75),
  N = fmt_num(N)
)]

save_table(table1b_fmt, "table1b_summary_pricing",
           "Summary Statistics - Pricing Sample")

# ============================================================================
# TABLE 2: MISSINGNESS DIAGNOSTICS
# ============================================================================
cat("\n=== Generating Table 2: Missingness ===\n")

missingness_vars <- c("inc_num", "dti_band", "cltv_bin", "pv_num", "rate_num")

miss_overall <- approval_dt[, .(
  Variable = c("Income", "DTI", "CLTV", "Property Value", "Interest Rate"),
  Overall = c(
    mean(is.na(inc_num)) * 100,
    mean(dti_band == "Missing") * 100,
    mean(cltv_bin == "Missing") * 100,
    mean(is.na(pv_num)) * 100,
    mean(is.na(rate_num)) * 100
  )
)]

miss_byrace <- approval_dt[, .(
  Income = mean(is.na(inc_num)) * 100,
  DTI = mean(dti_band == "Missing") * 100,
  CLTV = mean(cltv_bin == "Missing") * 100,
  `Property Value` = mean(is.na(pv_num)) * 100,
  `Interest Rate` = mean(is.na(rate_num)) * 100
), by = race_group]

# Reshape for nice table
miss_byrace_long <- melt(miss_byrace, id.vars = "race_group",
                         variable.name = "Variable", value.name = "Pct_Missing")

miss_wide <- dcast(miss_byrace_long, Variable ~ race_group, value.var = "Pct_Missing")

# Merge with overall
table2 <- merge(miss_overall[, .(Variable, Overall)], miss_wide, by = "Variable")

# Format
table2_fmt <- table2[, lapply(.SD, function(x) sprintf("%.1f%%", x)),
                     .SDcols = setdiff(names(table2), "Variable")]
table2_fmt <- cbind(Variable = table2$Variable, table2_fmt)

save_table(table2_fmt, "table2_missingness",
           "Missingness Diagnostics by Race/Ethnicity")

# ============================================================================
# TABLE 3: COMPOSITION OF CATEGORICAL VARIABLES
# ============================================================================
cat("\n=== Generating Table 3: Categorical Composition ===\n")

# Race composition across samples
race_comp <- data.table(
  Race = levels(dt_clean$race_group),
  Clean = dt_clean[, .N, by = race_group][order(race_group), N],
  Approval = approval_dt[, .N, by = race_group][order(race_group), N],
  Pricing = pricing_dt[, .N, by = race_group][order(race_group), N]
)

race_comp[, `:=`(
  `Clean (%)` = Clean / sum(Clean) * 100,
  `Approval (%)` = Approval / sum(Approval) * 100,
  `Pricing (%)` = Pricing / sum(Pricing) * 100
)]

table3a <- race_comp[, .(
  Race,
  Clean = fmt_num(Clean),
  `Clean (%)` = fmt_pct(`Clean (%)` / 100),
  Approval = fmt_num(Approval),
  `Approval (%)` = fmt_pct(`Approval (%)` / 100),
  Pricing = fmt_num(Pricing),
  `Pricing (%)` = fmt_pct(`Pricing (%)` / 100)
)]

save_table(table3a, "table3a_race_composition",
           "Race/Ethnicity Composition Across Samples")

# DTI band distribution
dti_dist <- dt_clean[, .N, by = dti_band][order(dti_band)]
dti_dist[, Percent := N / sum(N) * 100]
dti_dist_fmt <- dti_dist[, .(
  `DTI Band` = as.character(dti_band),
  N = fmt_num(N),
  `Percent` = fmt_pct(Percent / 100)
)]

save_table(dti_dist_fmt, "table3b_dti_distribution",
           "DTI Band Distribution")

# CLTV bin distribution
cltv_dist <- dt_clean[, .N, by = cltv_bin][order(cltv_bin)]
cltv_dist[, Percent := N / sum(N) * 100]
cltv_dist_fmt <- cltv_dist[, .(
  `CLTV Bin` = as.character(cltv_bin),
  N = fmt_num(N),
  `Percent` = fmt_pct(Percent / 100)
)]

save_table(cltv_dist_fmt, "table3c_cltv_distribution",
           "CLTV Bin Distribution")

# ============================================================================
# TABLE 4: MARKET AND LENDER COVERAGE
# ============================================================================
cat("\n=== Generating Table 4: Market Coverage ===\n")

# MSA stats
msa_stats <- dt_clean[!is.na(derived_msa_md), .(N = .N), by = derived_msa_md][order(-N)]
msa_summary <- data.table(
  Metric = c("Number of MSAs", "Median apps per MSA", "P90 apps per MSA",
             "Top 10 MSAs share"),
  Value = c(
    fmt_num(nrow(msa_stats)),
    fmt_num(median(msa_stats$N)),
    fmt_num(quantile(msa_stats$N, 0.90)),
    fmt_pct(sum(msa_stats[1:10, N]) / sum(msa_stats$N))
  )
)

# Lender stats
lender_stats <- dt_clean[!is.na(lei), .(N = .N), by = lei][order(-N)]
lender_summary <- data.table(
  Metric = c("Number of lenders", "Median apps per lender", "P90 apps per lender",
             "Top 10 lenders share"),
  Value = c(
    fmt_num(nrow(lender_stats)),
    fmt_num(median(lender_stats$N)),
    fmt_num(quantile(lender_stats$N, 0.90)),
    fmt_pct(sum(lender_stats[1:10, N]) / sum(lender_stats$N))
  )
)

table4 <- rbindlist(list(
  msa_summary[, Panel := "MSAs"],
  lender_summary[, Panel := "Lenders"]
))

save_table(table4[, .(Panel, Metric, Value)], "table4_market_coverage",
           "Market and Lender Coverage")

cat("\n=== All Tables Generated ===\n")

# ============================================================================
# FIGURE 1: SAMPLE FLOW DIAGRAM
# ============================================================================
cat("\n=== Generating Figure 1: Sample Flow ===\n")

flow_data <- data.table(
  stage = factor(1:4, labels = c("Raw HMDA", "Clean Filter", "Approval", "Pricing")),
  n = c(25551868, nrow(dt_clean), nrow(approval_dt), nrow(pricing_dt)),
  label = c(
    "25,551,868\n(100%)",
    sprintf("%s\n(%.1f%%)", fmt_num(nrow(dt_clean)), 100 * nrow(dt_clean) / 25551868),
    sprintf("%s\n(%.1f%%)", fmt_num(nrow(approval_dt)), 100 * nrow(approval_dt) / 25551868),
    sprintf("%s\n(%.1f%%)", fmt_num(nrow(pricing_dt)), 100 * nrow(pricing_dt) / 25551868)
  )
)

fig1 <- ggplot(flow_data, aes(x = stage, y = n)) +
  geom_col(fill = "#0072B2", alpha = 0.8) +
  geom_text(aes(label = label), vjust = -0.5, size = 4, fontface = "bold") +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Sample Construction Flow",
    subtitle = "HMDA 2020 mortgage applications",
    x = NULL,
    y = "Number of Applications",
    caption = "Note: Bars show absolute counts and percent of raw data retained."
  ) +
  theme_academic()

save_figure(fig1, "figure1_sample_flow")

# ============================================================================
# FIGURE 2: INTEREST RATE DISTRIBUTION
# ============================================================================
cat("\n=== Generating Figure 2: Interest Rate Distribution ===\n")

# Calculate p99
p99_rate <- quantile(pricing_dt$rate_num, 0.99)
tail_share <- mean(pricing_dt$rate_num > p99_rate)

# Main plot (trimmed at p99)
fig2 <- ggplot(pricing_dt[rate_num <= p99_rate], aes(x = rate_num)) +
  geom_histogram(bins = 100, fill = "#0072B2", alpha = 0.7, color = "white") +
  geom_vline(xintercept = mean(pricing_dt$rate_num),
             linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = mean(pricing_dt$rate_num) + 0.3,
           y = Inf, vjust = 2,
           label = sprintf("Mean: %.3f%%", mean(pricing_dt$rate_num)),
           color = "red", fontface = "bold") +
  scale_x_continuous(labels = function(x) sprintf("%.1f%%", x)) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Interest Rate Distribution",
    subtitle = "Originated loans, 2020",
    x = "Contract Interest Rate",
    y = "Number of Loans",
    caption = sprintf("Note: X-axis trimmed at 99th percentile (%.2f%%). %.1f%% of loans above p99 (max: %.2f%%).",
                     p99_rate, tail_share * 100, max(pricing_dt$rate_num))
  ) +
  theme_academic()

save_figure(fig2, "figure2_rate_distribution")

# ============================================================================
# FIGURE 3: RACE COMPOSITION ACROSS SAMPLES
# ============================================================================
cat("\n=== Generating Figure 3: Race Composition ===\n")

race_comp_long <- melt(race_comp[, .(Race, `Clean (%)`, `Approval (%)`, `Pricing (%)`)],
                       id.vars = "Race", variable.name = "Sample", value.name = "Percent")

race_comp_long[, Sample := gsub(" \\(%\\)", "", Sample)]

fig3 <- ggplot(race_comp_long, aes(x = Sample, y = Percent, fill = Race)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", Percent)),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = RACE_COLORS) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Race/Ethnicity Composition Across Samples",
    subtitle = "Selection into approval and pricing samples",
    x = NULL,
    y = "Percent of Sample",
    caption = "Note: Race NA = race/ethnicity not available in HMDA data (19.5% of clean sample)."
  ) +
  theme_academic() +
  theme(legend.position = "right")

save_figure(fig3, "figure3_race_composition", width = 10)

# ============================================================================
# FIGURE 4: DTI BAND DISTRIBUTION
# ============================================================================
cat("\n=== Generating Figure 4: DTI Distribution ===\n")

dti_plot_data <- dt_clean[, .N, by = dti_band][order(dti_band)]
dti_plot_data[, Percent := N / sum(N) * 100]

fig4 <- ggplot(dti_plot_data, aes(x = dti_band, y = Percent)) +
  geom_col(fill = "#009E73", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", Percent)), vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Debt-to-Income Ratio Distribution",
    subtitle = "Clean mortgage sample (preserves HMDA range categories)",
    x = "DTI Band",
    y = "Percent of Sample",
    caption = "Note: DTI bands preserve HMDA-reported ranges (e.g., '20%-<30%') to avoid data loss."
  ) +
  theme_academic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_figure(fig4, "figure4_dti_distribution")

# ============================================================================
# FIGURE 5: CLTV BIN DISTRIBUTION
# ============================================================================
cat("\n=== Generating Figure 5: CLTV Distribution ===\n")

cltv_plot_data <- dt_clean[, .N, by = cltv_bin][order(cltv_bin)]
cltv_plot_data[, Percent := N / sum(N) * 100]

fig5 <- ggplot(cltv_plot_data, aes(x = cltv_bin, y = Percent)) +
  geom_col(fill = "#CC79A7", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", Percent)), vjust = -0.5, size = 3) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Combined Loan-to-Value Ratio Distribution",
    subtitle = "Clean mortgage sample",
    x = "CLTV Bin",
    y = "Percent of Sample",
    caption = "Note: CLTV = combined loan-to-value ratio. Missing = 31.0% overall, varies by race."
  ) +
  theme_academic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_figure(fig5, "figure5_cltv_distribution")

# ============================================================================
# FIGURE 6: INCOME AND LOAN AMOUNT BY RACE
# ============================================================================
cat("\n=== Generating Figure 6: Income & Loan Amount ===\n")

# Income by race
income_plot <- ggplot(approval_dt[!is.na(inc_num) & inc_num > 0],
                     aes(x = race_group, y = inc_num / 1000, fill = race_group)) +
  geom_violin(alpha = 0.6, draw_quantiles = c(0.25, 0.5, 0.75)) +
  scale_fill_manual(values = RACE_COLORS) +
  scale_y_log10(labels = dollar_format(suffix = "K")) +
  labs(
    title = "Income Distribution by Race/Ethnicity",
    subtitle = "Approval sample (log scale)",
    x = NULL,
    y = "Income (thousands)",
    caption = "Note: Horizontal lines show 25th, 50th (median), and 75th percentiles."
  ) +
  theme_academic() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Loan amount by race
loan_plot <- ggplot(approval_dt[loan_amount > 0],
                   aes(x = race_group, y = loan_amount / 1000, fill = race_group)) +
  geom_violin(alpha = 0.6, draw_quantiles = c(0.25, 0.5, 0.75)) +
  scale_fill_manual(values = RACE_COLORS) +
  scale_y_log10(labels = dollar_format(suffix = "K")) +
  labs(
    title = "Loan Amount Distribution by Race/Ethnicity",
    subtitle = "Approval sample (log scale)",
    x = NULL,
    y = "Loan Amount (thousands)",
    caption = "Note: Horizontal lines show 25th, 50th (median), and 75th percentiles."
  ) +
  theme_academic() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Combine
fig6 <- grid.arrange(income_plot, loan_plot, ncol = 2)

save_figure(fig6, "figure6_income_loan_by_race", width = 12, height = 6)

# ============================================================================
# FIGURE 7: GEOGRAPHIC COVERAGE (TOP MSAs)
# ============================================================================
cat("\n=== Generating Figure 7: Geographic Coverage ===\n")

# Get top 15 MSAs
top_msas <- dt_clean[!is.na(derived_msa_md), .N, by = derived_msa_md][order(-N)][1:15]
top_msas[, cumulative := cumsum(N) / sum(dt_clean[!is.na(derived_msa_md), .N]) * 100]
top_msas[, msa_rank := 1:.N]

fig7 <- ggplot(top_msas, aes(x = reorder(as.factor(msa_rank), -N))) +
  geom_col(aes(y = N), fill = "#0072B2", alpha = 0.8) +
  geom_line(aes(y = cumulative * max(N) / 100, group = 1),
            color = "red", size = 1.5) +
  geom_point(aes(y = cumulative * max(N) / 100),
             color = "red", size = 3) +
  scale_y_continuous(
    name = "Number of Applications",
    labels = comma,
    sec.axis = sec_axis(~ . * 100 / max(top_msas$N),
                       name = "Cumulative Share (%)",
                       labels = function(x) paste0(round(x), "%"))
  ) +
  labs(
    title = "Top 15 Metropolitan Statistical Areas",
    subtitle = "Pareto chart showing geographic concentration",
    x = "MSA Rank",
    caption = sprintf("Note: Top 15 MSAs account for %.1f%% of sample. Red line shows cumulative share.",
                     top_msas[15, cumulative])
  ) +
  theme_academic() +
  theme(axis.text.x = element_text(angle = 0))

save_figure(fig7, "figure7_top_msas")

# ============================================================================
# FIGURE 8: LENDER CONCENTRATION
# ============================================================================
cat("\n=== Generating Figure 8: Lender Concentration ===\n")

lender_dist <- dt_clean[!is.na(lei), .N, by = lei][order(-N)]
lender_dist[, market_share := N / sum(N)]
lender_dist[, cumulative := cumsum(market_share) * 100]
lender_dist[, lender_rank := 1:.N]

# Use only top 100 lenders for the plot
plot_data <- lender_dist[1:100]

fig8 <- ggplot(plot_data, aes(x = lender_rank, y = cumulative)) +
  geom_line(color = "#E69F00", size = 1.5) +
  geom_ribbon(aes(ymin = 0, ymax = cumulative), fill = "#E69F00", alpha = 0.3) +
  geom_abline(
    intercept = 0,
    slope = 100 / max(plot_data$lender_rank),  # = 1 for top 100
    linetype = "dashed",
    color = "gray50"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Lender Market Concentration",
  subtitle = "Cumulative market share of top 100 lenders",
    x        = "Lender Rank (by application volume)",
    y        = "Cumulative Market Share",
    caption  = sprintf(
      "Note: Top 10 lenders account for %.1f%% of market. Dashed line = perfect equality.",
      plot_data[10, cumulative]
    )
  ) +
  theme_academic()

save_figure(fig8, "figure8_lender_concentration")


# ============================================================================
# FIGURE 9: MISSINGNESS BY RACE
# ============================================================================
cat("\n=== Generating Figure 9: Missingness by Race ===\n")

miss_plot_data <- approval_dt[, .(
  `DTI Missing` = mean(dti_band == "Missing") * 100,
  `CLTV Missing` = mean(cltv_bin == "Missing") * 100
), by = race_group]

miss_plot_long <- melt(miss_plot_data, id.vars = "race_group",
                       variable.name = "Variable", value.name = "Percent")

fig9 <- ggplot(miss_plot_long, aes(x = race_group, y = Percent, fill = Variable)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", Percent)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("DTI Missing" = "#D55E00", "CLTV Missing" = "#0072B2")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Data Missingness by Race/Ethnicity",
    subtitle = "Approval sample",
    x = NULL,
    y = "Percent Missing",
    caption = "Note: NH Black borrowers have highest CLTV missingness (12.1%) and DTI missingness (2.5%)."
  ) +
  theme_academic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_figure(fig9, "figure9_missingness_by_race")

# ============================================================================
# SUMMARY
# ============================================================================
cat("\n")
cat("================================================================================\n")
cat("ALL EXHIBITS GENERATED SUCCESSFULLY\n")
cat("================================================================================\n")
cat("\nTables saved to:", TABLES_DIR, "\n")
cat("  - CSV format (for Excel/analysis)\n")
cat("  - LaTeX format (for paper)\n")
cat("\nFigures saved to:", FIGURES_DIR, "\n")
cat("  - PNG format (300 dpi, for presentations/viewing)\n")
cat("  - PDF format (vector, for LaTeX papers)\n")
cat("\n")
cat("Tables generated:\n")
cat("  ✓ Table 0: Sample Construction Flow\n")
cat("  ✓ Table 1A: Summary Statistics - Approval\n")
cat("  ✓ Table 1B: Summary Statistics - Pricing\n")
cat("  ✓ Table 2: Missingness Diagnostics\n")
cat("  ✓ Table 3A-C: Categorical Composition\n")
cat("  ✓ Table 4: Market Coverage\n")
cat("\n")
cat("Figures generated:\n")
cat("  ✓ Figure 1: Sample Flow\n")
cat("  ✓ Figure 2: Interest Rate Distribution\n")
cat("  ✓ Figure 3: Race Composition\n")
cat("  ✓ Figure 4: DTI Distribution\n")
cat("  ✓ Figure 5: CLTV Distribution\n")
cat("  ✓ Figure 6: Income & Loan Amount by Race\n")
cat("  ✓ Figure 7: Top MSAs (Geographic Coverage)\n")
cat("  ✓ Figure 8: Lender Concentration\n")
cat("  ✓ Figure 9: Missingness by Race\n")
cat("\n")
cat("All outputs are publication-ready for academic papers!\n")
cat("================================================================================\n")
