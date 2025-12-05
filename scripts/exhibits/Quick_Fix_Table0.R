# Quick script to regenerate just Table 0 with formatting fix
library(data.table)

# Load just the sample counts from the saved data
source("scripts/HDMA_Data.R")

# Format helpers
fmt_num <- function(x) format(x, big.mark = ",", scientific = FALSE)
fmt_pct <- function(x, digits = 1) sprintf(paste0("%.", digits, "f%%"), x * 100)

# Create sample flow table
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

# Format for display (WITH FIX: added %% to sprintf)
table0 <- sample_flow[, .(
  Stage,
  `N (Observations)` = fmt_num(N),
  `% of Raw Data` = fmt_pct(Pct_Retained / 100),
  `Approval Rate (%)` = ifelse(is.na(Approval_Rate), "—", sprintf("%.1f%%", Approval_Rate)),
  `Mean Interest Rate (%)` = ifelse(is.na(Mean_Rate), "—", sprintf("%.3f", Mean_Rate))
)]

# Save to CSV
fwrite(table0, "output/exhibits/tables/table0_sample_flow.csv")

cat("Fixed and saved: table0_sample_flow.csv\n")
print(table0)
