# HDMA 2020 Summary Statistics for Presentation
# Focuses on conventional loans and their characteristics

# Load data.table for fast data processing
if (!require("data.table", quietly = TRUE)) {
  install.packages("data.table")
}
library(data.table)

cat("Loading data... This may take a few minutes for the 9.4GB file...\n")

# Load only the columns we need for summary statistics
# This significantly reduces memory usage
columns_needed <- c(
  'loan_type',                           # To filter conventional loans
  'action_taken',                        # To determine approval status
  'derived_ethnicity',                   # Ethnicity information
  'derived_race',                        # Race information
  'loan_amount',                         # For loan statistics
  'interest_rate',                       # For rate statistics
  'income',                              # For income statistics
  'property_value',                      # For property value statistics
  'combined_loan_to_value_ratio',        # CLTV ratio
  'debt_to_income_ratio',                # DTI ratio
  'tract_population',                    # Tract level variables
  'tract_minority_population_percent',
  'tract_to_msa_income_percentage',
  'ffiec_msa_md_median_family_income',
  'tract_owner_occupied_units',
  'tract_one_to_four_family_homes',
  'tract_median_age_of_housing_units'
)

# Load the full dataset with only needed columns
df <- fread('data/2020_data.csv', select = columns_needed)

cat("\n============================================\n")
cat("HDMA 2020 DATASET SUMMARY STATISTICS\n")
cat("============================================\n\n")

# 1. TOTAL OBSERVATIONS
total_obs <- nrow(df)
cat(sprintf("1. TOTAL NUMBER OF OBSERVATIONS: %s\n\n", format(total_obs, big.mark=",")))

# 2. CONVENTIONAL LOANS
# loan_type: 1 = Conventional, 2 = FHA, 3 = VA, 4 = USDA/RHS
conventional_loans <- df[loan_type == 1]
n_conventional <- nrow(conventional_loans)
pct_conventional <- (n_conventional / total_obs) * 100

cat(sprintf("2. CONVENTIONAL LOANS: %s (%.2f%% of all loans)\n\n",
            format(n_conventional, big.mark=","), pct_conventional))

# 3. CONVENTIONAL LOANS BY APPROVAL STATUS
# action_taken: 1 = Originated, 2 = Approved but not accepted, 3 = Denied,
#               4 = Withdrawn, 5 = Incomplete, 6 = Purchased, 7 = Preapproval denied, 8 = Preapproval approved
cat("3. CONVENTIONAL LOANS BY ACTION TAKEN:\n")
action_breakdown <- table(conventional_loans$action_taken)
action_labels <- c(
  "1" = "Loan Originated",
  "2" = "Approved but Not Accepted",
  "3" = "Denied",
  "4" = "Withdrawn by Applicant",
  "5" = "File Closed for Incompleteness",
  "6" = "Purchased by Institution",
  "7" = "Preapproval Request Denied",
  "8" = "Preapproval Approved but Not Accepted"
)

for (i in names(action_breakdown)) {
  label <- action_labels[i]
  count <- action_breakdown[i]
  pct <- (count / n_conventional) * 100
  cat(sprintf("   %s: %s (%.2f%%)\n", label, format(count, big.mark=","), pct))
}

# Create simplified approved vs not approved breakdown
approved <- sum(conventional_loans$action_taken %in% c(1, 2, 6, 8))
not_approved <- sum(conventional_loans$action_taken %in% c(3, 4, 5, 7))
cat(sprintf("\n   SIMPLIFIED BREAKDOWN:\n"))
cat(sprintf("   Approved (Originated/Approved): %s (%.2f%%)\n",
            format(approved, big.mark=","), (approved/n_conventional)*100))
cat(sprintf("   Not Approved (Denied/Withdrawn/Incomplete): %s (%.2f%%)\n\n",
            format(not_approved, big.mark=","), (not_approved/n_conventional)*100))

# 4. ETHNICITY COMPOSITION OF CONVENTIONAL LOANS
cat("4. ETHNICITY COMPOSITION OF CONVENTIONAL LOANS:\n")
ethnicity_breakdown <- table(conventional_loans$derived_ethnicity)
ethnicity_breakdown <- sort(ethnicity_breakdown, decreasing = TRUE)
for (i in seq_along(ethnicity_breakdown)) {
  ethnicity <- names(ethnicity_breakdown)[i]
  count <- ethnicity_breakdown[i]
  pct <- (count / n_conventional) * 100
  cat(sprintf("   %s: %s (%.2f%%)\n", ethnicity, format(count, big.mark=","), pct))
}

# 5. RACE COMPOSITION OF CONVENTIONAL LOANS
cat("\n5. RACE COMPOSITION OF CONVENTIONAL LOANS:\n")
race_breakdown <- table(conventional_loans$derived_race)
race_breakdown <- sort(race_breakdown, decreasing = TRUE)
for (i in seq_along(race_breakdown)) {
  race <- names(race_breakdown)[i]
  count <- race_breakdown[i]
  pct <- (count / n_conventional) * 100
  cat(sprintf("   %s: %s (%.2f%%)\n", race, format(count, big.mark=","), pct))
}

# 6. ADDITIONAL STATISTICS FOR CONVENTIONAL LOANS
cat("\n6. ADDITIONAL STATISTICS FOR CONVENTIONAL LOANS:\n")

# Convert fields to numeric (they may be stored as character with "Exempt" or "NA" values)
conventional_loans$interest_rate_num <- suppressWarnings(as.numeric(conventional_loans$interest_rate))
conventional_loans$property_value_num <- suppressWarnings(as.numeric(conventional_loans$property_value))
conventional_loans$income_num <- suppressWarnings(as.numeric(conventional_loans$income))

cat(sprintf("   Mean Loan Amount: $%s\n",
            format(round(mean(conventional_loans$loan_amount, na.rm=TRUE)), big.mark=",")))
cat(sprintf("   Median Loan Amount: $%s\n",
            format(round(median(conventional_loans$loan_amount, na.rm=TRUE)), big.mark=",")))

# Interest rate statistics
if(sum(!is.na(conventional_loans$interest_rate_num)) > 0) {
  cat(sprintf("   Mean Interest Rate: %.3f%%\n",
              mean(conventional_loans$interest_rate_num, na.rm=TRUE)))
  cat(sprintf("   Median Interest Rate: %.3f%%\n",
              median(conventional_loans$interest_rate_num, na.rm=TRUE)))
} else {
  cat("   Interest Rate: Data not available\n")
}

# Property value statistics
if(sum(!is.na(conventional_loans$property_value_num)) > 0) {
  cat(sprintf("   Mean Property Value: $%s\n",
              format(round(mean(conventional_loans$property_value_num, na.rm=TRUE)), big.mark=",")))
  cat(sprintf("   Median Property Value: $%s\n",
              format(round(median(conventional_loans$property_value_num, na.rm=TRUE)), big.mark=",")))
} else {
  cat("   Property Value: Data not available\n")
}

# Income statistics (income is reported in thousands in HMDA data)
if(sum(!is.na(conventional_loans$income_num)) > 0) {
  cat(sprintf("   Mean Applicant Income: $%s\n",
              format(round(mean(conventional_loans$income_num, na.rm=TRUE)*1000), big.mark=",")))
  cat(sprintf("   Median Applicant Income: $%s\n",
              format(round(median(conventional_loans$income_num, na.rm=TRUE)*1000), big.mark=",")))
} else {
  cat("   Applicant Income: Data not available\n")
}

# 7. APPROVAL RATE BY RACE
cat("\n7. APPROVAL RATE BY RACE (CONVENTIONAL LOANS):\n")
cat("   (Approved = Originated/Approved/Purchased; Not Approved = Denied/Withdrawn/Incomplete)\n\n")

races <- unique(conventional_loans$derived_race)
races <- races[order(races)]

# Store approval rates for charting
approval_rates <- numeric(length(races))
race_labels <- character(length(races))
race_counts <- numeric(length(races))

for (i in seq_along(races)) {
  race <- races[i]
  race_subset <- conventional_loans[derived_race == race]
  n_race <- nrow(race_subset)

  approved_race <- sum(race_subset$action_taken %in% c(1, 2, 6, 8))
  not_approved_race <- sum(race_subset$action_taken %in% c(3, 4, 5, 7))

  approval_rate <- (approved_race / n_race) * 100

  cat(sprintf("   %s:\n", race))
  cat(sprintf("      Total Applications: %s\n", format(n_race, big.mark=",")))
  cat(sprintf("      Approved: %s (%.2f%%)\n", format(approved_race, big.mark=","), approval_rate))
  cat(sprintf("      Not Approved: %s (%.2f%%)\n\n", format(not_approved_race, big.mark=","), 100-approval_rate))

  # Store for chart
  approval_rates[i] <- approval_rate
  race_labels[i] <- race
  race_counts[i] <- n_race
}

# Create approval rate by race chart
cat("\n   Creating approval rate by race chart...\n")

# Shorten labels for better display
race_labels_short <- race_labels
race_labels_short[race_labels == "2 or more minority races"] <- "2+ Minority"
race_labels_short[race_labels == "American Indian or Alaska Native"] <- "Am. Indian/AK Native"
race_labels_short[race_labels == "Black or African American"] <- "Black/African Am."
race_labels_short[race_labels == "Free Form Text Only"] <- "Free Form Text"
race_labels_short[race_labels == "Native Hawaiian or Other Pacific Islander"] <- "Native Hawaiian/PI"
race_labels_short[race_labels == "Race Not Available"] <- "Race Not Available"

png("output/exhibits/figures/Approval_Rate_by_Race.png", width=1400, height=900, res=120)
par(mar=c(10,5,4,2))

# Sort by approval rate for better visualization
sorted_idx <- order(approval_rates, decreasing=TRUE)
approval_rates_sorted <- approval_rates[sorted_idx]
race_labels_sorted <- race_labels_short[sorted_idx]
race_counts_sorted <- race_counts[sorted_idx]

# Color code: higher rates in green, lower in red
colors <- colorRampPalette(c("red", "yellow", "green"))(length(approval_rates_sorted))

barplot(approval_rates_sorted,
        names.arg=race_labels_sorted,
        main="Approval Rates by Race\n(Conventional Loans, 2020 HMDA Data)",
        ylab="Approval Rate (%)",
        col=colors,
        border="white",
        las=2,
        cex.names=0.8,
        ylim=c(0, max(approval_rates_sorted)*1.15))

# Add percentage labels on top of bars
text(x=seq(0.7, by=1.2, length.out=length(approval_rates_sorted)),
     y=approval_rates_sorted,
     labels=paste0(round(approval_rates_sorted, 1), "%"),
     pos=3,
     cex=0.9,
     font=2)

# Add sample size labels below bars
text(x=seq(0.7, by=1.2, length.out=length(approval_rates_sorted)),
     y=rep(0, length(approval_rates_sorted)),
     labels=paste0("n=", format(race_counts_sorted, big.mark=",", scientific=FALSE)),
     pos=1,
     cex=0.6,
     col="gray30",
     xpd=TRUE)

# Add horizontal line for overall average
overall_approval <- (sum(conventional_loans$action_taken %in% c(1, 2, 6, 8)) /
                     nrow(conventional_loans)) * 100
abline(h=overall_approval, col="blue", lwd=2, lty=2)
text(0, overall_approval, paste0("Overall: ", round(overall_approval, 1), "%"),
     pos=3, col="blue", cex=0.8, font=2)

dev.off()

cat("   Approval rate by race chart saved as 'Approval_Rate_by_Race.png'\n")

# 8. INTEREST RATE BY RACE
cat("\n8. MEAN INTEREST RATE BY RACE (CONVENTIONAL LOANS):\n")
cat("   (For originated loans only)\n\n")

# Filter to originated loans only (action_taken == 1)
originated_loans <- conventional_loans[action_taken == 1]
originated_loans$interest_rate_num <- suppressWarnings(as.numeric(originated_loans$interest_rate))

for (race in races) {
  race_subset <- originated_loans[derived_race == race]
  rate_subset <- race_subset$interest_rate_num[!is.na(race_subset$interest_rate_num)]

  n_total <- nrow(race_subset)
  n_with_rate <- length(rate_subset)
  n_missing <- n_total - n_with_rate
  pct_missing <- (n_missing / n_total) * 100

  cat(sprintf("   %s:\n", race))
  if (n_with_rate > 0) {
    cat(sprintf("      Mean Interest Rate: %.3f%%\n", mean(rate_subset)))
    cat(sprintf("      Median Interest Rate: %.3f%%\n", median(rate_subset)))
    cat(sprintf("      Observations with rate data: %s\n", format(n_with_rate, big.mark=",")))
    cat(sprintf("      Missing rate data: %s (%.2f%%)\n\n", format(n_missing, big.mark=","), pct_missing))
  } else {
    cat(sprintf("      No interest rate data available\n\n"))
  }
}

# 9. COMBINED LOAN TO VALUE RATIO
cat("\n9. COMBINED LOAN-TO-VALUE RATIO (CONVENTIONAL LOANS):\n")
conventional_loans$cltv_num <- suppressWarnings(as.numeric(conventional_loans$combined_loan_to_value_ratio))

n_total_cltv <- nrow(conventional_loans)
n_with_cltv <- sum(!is.na(conventional_loans$cltv_num))
n_missing_cltv <- n_total_cltv - n_with_cltv
pct_missing_cltv <- (n_missing_cltv / n_total_cltv) * 100

if (n_with_cltv > 0) {
  # Filter out unreasonable CLTV values (should be between 0 and 200)
  # Some data entry errors may have values in the thousands
  cltv_reasonable <- conventional_loans$cltv_num[!is.na(conventional_loans$cltv_num) &
                                                   conventional_loans$cltv_num > 0 &
                                                   conventional_loans$cltv_num <= 200]
  n_unreasonable <- n_with_cltv - length(cltv_reasonable)

  cat(sprintf("   Mean CLTV: %.2f%%\n", mean(cltv_reasonable)))
  cat(sprintf("   Median CLTV: %.2f%%\n", median(cltv_reasonable)))
  cat(sprintf("   25th Percentile: %.2f%%\n", quantile(cltv_reasonable, 0.25)))
  cat(sprintf("   75th Percentile: %.2f%%\n", quantile(cltv_reasonable, 0.75)))
  cat(sprintf("   Observations with valid CLTV data (0-200%%): %s\n", format(length(cltv_reasonable), big.mark=",")))
  cat(sprintf("   Unreasonable/outlier CLTV values: %s (%.2f%%)\n", format(n_unreasonable, big.mark=","), (n_unreasonable/n_total_cltv)*100))
  cat(sprintf("   Missing CLTV data: %s (%.2f%%)\n", format(n_missing_cltv, big.mark=","), pct_missing_cltv))
} else {
  cat("   No CLTV data available\n")
}

# 10. TRACT-LEVEL VARIABLES
cat("\n10. TRACT-LEVEL VARIABLES (CONVENTIONAL LOANS):\n")

# Tract population
conventional_loans$tract_pop_num <- suppressWarnings(as.numeric(conventional_loans$tract_population))
n_with_pop <- sum(!is.na(conventional_loans$tract_pop_num))
n_missing_pop <- n_total_cltv - n_with_pop
if (n_with_pop > 0) {
  cat(sprintf("   Tract Population:\n"))
  cat(sprintf("      Mean: %s\n", format(round(mean(conventional_loans$tract_pop_num, na.rm=TRUE)), big.mark=",")))
  cat(sprintf("      Median: %s\n", format(round(median(conventional_loans$tract_pop_num, na.rm=TRUE)), big.mark=",")))
  cat(sprintf("      Missing: %s (%.2f%%)\n\n", format(n_missing_pop, big.mark=","), (n_missing_pop/n_total_cltv)*100))
}

# Tract minority population percent
conventional_loans$tract_minority_num <- suppressWarnings(as.numeric(conventional_loans$tract_minority_population_percent))
n_with_minority <- sum(!is.na(conventional_loans$tract_minority_num))
n_missing_minority <- n_total_cltv - n_with_minority
if (n_with_minority > 0) {
  cat(sprintf("   Tract Minority Population Percent:\n"))
  cat(sprintf("      Mean: %.2f%%\n", mean(conventional_loans$tract_minority_num, na.rm=TRUE)))
  cat(sprintf("      Median: %.2f%%\n", median(conventional_loans$tract_minority_num, na.rm=TRUE)))
  cat(sprintf("      Missing: %s (%.2f%%)\n\n", format(n_missing_minority, big.mark=","), (n_missing_minority/n_total_cltv)*100))
}

# Tract to MSA income percentage
conventional_loans$tract_msa_income_num <- suppressWarnings(as.numeric(conventional_loans$tract_to_msa_income_percentage))
n_with_income_pct <- sum(!is.na(conventional_loans$tract_msa_income_num))
n_missing_income_pct <- n_total_cltv - n_with_income_pct
if (n_with_income_pct > 0) {
  cat(sprintf("   Tract to MSA Median Income Ratio:\n"))
  cat(sprintf("      Mean: %.2f%%\n", mean(conventional_loans$tract_msa_income_num, na.rm=TRUE)))
  cat(sprintf("      Median: %.2f%%\n", median(conventional_loans$tract_msa_income_num, na.rm=TRUE)))
  cat(sprintf("      Missing: %s (%.2f%%)\n\n", format(n_missing_income_pct, big.mark=","), (n_missing_income_pct/n_total_cltv)*100))
}

# FFIEC MSA/MD Median Family Income
conventional_loans$ffiec_income_num <- suppressWarnings(as.numeric(conventional_loans$ffiec_msa_md_median_family_income))
n_with_ffiec <- sum(!is.na(conventional_loans$ffiec_income_num))
n_missing_ffiec <- n_total_cltv - n_with_ffiec
if (n_with_ffiec > 0) {
  cat(sprintf("   FFIEC MSA/MD Median Family Income:\n"))
  cat(sprintf("      Mean: $%s\n", format(round(mean(conventional_loans$ffiec_income_num, na.rm=TRUE)), big.mark=",")))
  cat(sprintf("      Median: $%s\n", format(round(median(conventional_loans$ffiec_income_num, na.rm=TRUE)), big.mark=",")))
  cat(sprintf("      Missing: %s (%.2f%%)\n\n", format(n_missing_ffiec, big.mark=","), (n_missing_ffiec/n_total_cltv)*100))
}

# Tract owner occupied units
conventional_loans$tract_owner_occ_num <- suppressWarnings(as.numeric(conventional_loans$tract_owner_occupied_units))
n_with_owner_occ <- sum(!is.na(conventional_loans$tract_owner_occ_num))
n_missing_owner_occ <- n_total_cltv - n_with_owner_occ
if (n_with_owner_occ > 0) {
  cat(sprintf("   Tract Owner-Occupied Units:\n"))
  cat(sprintf("      Mean: %s\n", format(round(mean(conventional_loans$tract_owner_occ_num, na.rm=TRUE)), big.mark=",")))
  cat(sprintf("      Median: %s\n", format(round(median(conventional_loans$tract_owner_occ_num, na.rm=TRUE)), big.mark=",")))
  cat(sprintf("      Missing: %s (%.2f%%)\n\n", format(n_missing_owner_occ, big.mark=","), (n_missing_owner_occ/n_total_cltv)*100))
}

# Tract one to four family homes
conventional_loans$tract_1to4_num <- suppressWarnings(as.numeric(conventional_loans$tract_one_to_four_family_homes))
n_with_1to4 <- sum(!is.na(conventional_loans$tract_1to4_num))
n_missing_1to4 <- n_total_cltv - n_with_1to4
if (n_with_1to4 > 0) {
  cat(sprintf("   Tract One-to-Four Family Homes:\n"))
  cat(sprintf("      Mean: %s\n", format(round(mean(conventional_loans$tract_1to4_num, na.rm=TRUE)), big.mark=",")))
  cat(sprintf("      Median: %s\n", format(round(median(conventional_loans$tract_1to4_num, na.rm=TRUE)), big.mark=",")))
  cat(sprintf("      Missing: %s (%.2f%%)\n\n", format(n_missing_1to4, big.mark=","), (n_missing_1to4/n_total_cltv)*100))
}

# Tract median age of housing units
conventional_loans$tract_age_num <- suppressWarnings(as.numeric(conventional_loans$tract_median_age_of_housing_units))
n_with_age <- sum(!is.na(conventional_loans$tract_age_num))
n_missing_age <- n_total_cltv - n_with_age
if (n_with_age > 0) {
  cat(sprintf("   Tract Median Age of Housing Units:\n"))
  cat(sprintf("      Mean: %.1f years\n", mean(conventional_loans$tract_age_num, na.rm=TRUE)))
  cat(sprintf("      Median: %.1f years\n", median(conventional_loans$tract_age_num, na.rm=TRUE)))
  cat(sprintf("      Missing: %s (%.2f%%)\n", format(n_missing_age, big.mark=","), (n_missing_age/n_total_cltv)*100))
}

# 11. DEBT-TO-INCOME RATIO DISTRIBUTION
cat("\n11. DEBT-TO-INCOME RATIO (CONVENTIONAL LOANS):\n")

# Convert DTI to numeric
conventional_loans$dti_num <- suppressWarnings(as.numeric(conventional_loans$debt_to_income_ratio))

n_with_dti <- sum(!is.na(conventional_loans$dti_num))
n_missing_dti <- n_total_cltv - n_with_dti
pct_missing_dti <- (n_missing_dti / n_total_cltv) * 100

if (n_with_dti > 0) {
  # Filter reasonable DTI values (0-100%)
  dti_reasonable <- conventional_loans$dti_num[!is.na(conventional_loans$dti_num) &
                                                conventional_loans$dti_num >= 0 &
                                                conventional_loans$dti_num <= 100]

  cat(sprintf("   Mean DTI: %.2f%%\n", mean(dti_reasonable)))
  cat(sprintf("   Median DTI: %.2f%%\n", median(dti_reasonable)))
  cat(sprintf("   25th Percentile: %.2f%%\n", quantile(dti_reasonable, 0.25)))
  cat(sprintf("   75th Percentile: %.2f%%\n", quantile(dti_reasonable, 0.75)))
  cat(sprintf("   Observations with valid DTI data: %s\n", format(length(dti_reasonable), big.mark=",")))
  cat(sprintf("   Missing DTI data: %s (%.2f%%)\n\n", format(n_missing_dti, big.mark=","), pct_missing_dti))

  # Create DTI categorical breakdown
  cat("\n   DTI CATEGORICAL BREAKDOWN:\n")

  # Define DTI categories
  dti_categories <- cut(dti_reasonable,
                        breaks=c(0, 20, 30, 36, 43, 50, 100),
                        labels=c("0-20% (Very Low)",
                                "20-30% (Low)",
                                "30-36% (Moderate)",
                                "36-43% (Standard)",
                                "43-50% (High)",
                                "50-100% (Very High)"),
                        include.lowest=TRUE)

  dti_table <- table(dti_categories)
  dti_pct <- prop.table(dti_table) * 100

  for (i in seq_along(dti_table)) {
    cat(sprintf("      %s: %s (%.2f%%)\n",
                names(dti_table)[i],
                format(dti_table[i], big.mark=","),
                dti_pct[i]))
  }

  # Create DTI distribution charts
  cat("\n   Creating DTI distribution charts...\n")

  # Create histogram/density plot
  png("output/exhibits/figures/DTI_Distribution.png", width=1200, height=800, res=120)
  par(mfrow=c(2,1), mar=c(4,4,3,2))

  # Histogram
  hist(dti_reasonable,
       breaks=50,
       main="Distribution of Debt-to-Income Ratio (Conventional Loans)",
       xlab="Debt-to-Income Ratio (%)",
       ylab="Frequency",
       col="steelblue",
       border="white")
  abline(v=median(dti_reasonable), col="red", lwd=2, lty=2)
  abline(v=mean(dti_reasonable), col="darkgreen", lwd=2, lty=2)
  legend("topright",
         legend=c(paste("Median:", round(median(dti_reasonable), 2), "%"),
                  paste("Mean:", round(mean(dti_reasonable), 2), "%")),
         col=c("red", "darkgreen"), lty=2, lwd=2)

  # Box plot
  boxplot(dti_reasonable, horizontal=TRUE,
          main="DTI Box Plot",
          xlab="Debt-to-Income Ratio (%)",
          col="lightblue",
          border="steelblue")

  dev.off()

  # Create categorical bar chart
  png("output/exhibits/figures/DTI_Categorical_Distribution.png", width=1200, height=800, res=120)
  par(mar=c(8,5,4,2))

  # Create bar plot
  barplot(dti_table,
          main="Categorical Distribution of Debt-to-Income Ratio\n(Conventional Loans)",
          ylab="Frequency",
          col="steelblue",
          border="white",
          las=2,  # Rotate labels
          cex.names=0.8)

  # Add percentage labels on top of bars
  text(x=seq(0.7, by=1.2, length.out=length(dti_table)),
       y=dti_table,
       labels=paste0(round(dti_pct, 1), "%"),
       pos=3,
       cex=0.9)

  dev.off()

  cat("   DTI distribution chart saved as 'DTI_Distribution.png'\n")
  cat("   DTI categorical chart saved as 'DTI_Categorical_Distribution.png'\n")
} else {
  cat("   No DTI data available\n")
}

cat("\n============================================\n")
cat("Summary statistics generated successfully!\n")
cat("============================================\n")
