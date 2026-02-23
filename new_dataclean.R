# ==============================================================================
# HCRIS Data Cleaning Script - Create Analytical Dataset
# Purpose: Apply sample restrictions to create final dataset for DiD analysis
# Input: hospdata_full.csv (from hcris_data_prep_cleaned.R)
# Output: hospdata_analytical.csv
# ==============================================================================

# Load required packages -------------------------------------------------------
library(dplyr)
library(stringr)
library(readr)

# Set file paths ---------------------------------------------------------------
data_output_path <- "/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/output/"

# ==============================================================================
# LOAD DATA
# ==============================================================================

hospdata <- read.csv(
  paste0(data_output_path, "hospdata_full.csv"),
  stringsAsFactors = FALSE
)

cat("\n=== STARTING SAMPLE ===\n")
cat(paste("Total observations:", nrow(hospdata), "\n"))
cat(paste("Unique hospitals:", n_distinct(hospdata$mcrnum), "\n"))
cat(paste("Year range:", min(hospdata$year), "to", max(hospdata$year), "\n"))


# ==============================================================================
# FILTER 1: RESTRICT TO ANALYSIS YEARS (2004-2024)
# ==============================================================================

# WHY: Treatment data only available from 2004 onward
# DECISION: start sample when treatment data begins
# NOTE: States with treatment in 2004 are "always treated" in our data

hospdata_clean <- hospdata %>%
  filter(year >= 2003 & year <= 2024)

cat("\n=== FILTER 1: Year Range (2004-2024) ===\n")
cat(paste("Observations removed:", nrow(hospdata) - nrow(hospdata_clean), "\n"))
cat(paste("Observations remaining:", nrow(hospdata_clean), "\n"))
cat(paste("Unique hospitals:", n_distinct(hospdata_clean$mcrnum), "\n"))


# ==============================================================================
# FILTER 2: HOSPITAL TYPE - GENERAL ACUTE CARE AND NON-GOVERNMENTAL
# ==============================================================================

# OBJECTIVE: Keep only private (non-governmental) general acute care hospitals
# METHOD: Use HCRIS variables when available, AHA variables as backup for 2011+

# PART A: Keep only general acute care hospitals
# HCRIS: provtype = 1 (General Short Term)
# AHA: serv = 10 (General acute care)

before_count <- n_distinct(hospdata_clean$mcrnum)

hospdata_clean <- hospdata_clean %>%
  filter(provtype == 1 | serv == 10)

after_count <- n_distinct(hospdata_clean$mcrnum)

cat("\n=== FILTER 2A: General Acute Care Only ===\n")
cat(paste("Hospitals removed:", before_count - after_count, "\n"))
cat(paste("Observations remaining:", nrow(hospdata_clean), "\n"))
cat(paste("Unique hospitals remaining:", after_count, "\n"))


# PART B: Keep only non-governmental hospitals
# HCRIS typectrl: Keep 1-6 (nonprofit & proprietary), Remove 7-13 (governmental)
# AHA cntrl: Keep 21,23,30-33 (nonprofit & for-profit), Remove 12-16,41-48 (governmental)
#
# HCRIS CODES:                           AHA CODES:
# 1 = Nonprofit, Church                  21 = Church operated
# 2 = Nonprofit, Other                   23 = Other not-for-profit
# 3 = Proprietary, Individual            31 = Individual
# 4 = Proprietary, Corporation           33 = Corporation
# 5 = Proprietary, Partnership           32 = Partnership
# 6 = Proprietary, Other                 30 = Other investor-owned
# 7-13 = Governmental (REMOVE)           12-16, 41-48 = Governmental (REMOVE)

before_count <- n_distinct(hospdata_clean$mcrnum)

hospdata_clean <- hospdata_clean %>%
  filter(
    typectrl %in% 1:6 |                           # HCRIS: non-governmental
    cntrl %in% c(21, 23, 30, 31, 32, 33)         # AHA: non-governmental
  )

after_count <- n_distinct(hospdata_clean$mcrnum)

cat("\n=== FILTER 2B: Non-Governmental Only ===\n")
cat(paste("Hospitals removed:", before_count - after_count, "\n"))
cat(paste("Observations remaining:", nrow(hospdata_clean), "\n"))
cat(paste("Unique hospitals remaining:", after_count, "\n"))

# Verify ownership distribution
cat("\nOwnership type breakdown:\n")
hospdata_clean %>%
  mutate(
    ownership = case_when(
      !is.na(typectrl) & typectrl %in% 1:2 ~ "Nonprofit",
      !is.na(typectrl) & typectrl %in% 3:6 ~ "For-profit",
      !is.na(cntrl) & cntrl %in% c(21, 23) ~ "Nonprofit",
      !is.na(cntrl) & cntrl %in% c(30, 31, 32, 33) ~ "For-profit",
      TRUE ~ "Unknown"
    )
  ) %>%
  distinct(mcrnum, ownership) %>%
  count(ownership) %>%
  print()
# ==============================================================================
# FILTER 3: MULTI-STATE HOSPITALS (PLACEHOLDER)
# ==============================================================================

# TODO: Add multi-state hospital filter when code is ready
# PLACEHOLDER CODE:
# 
# # Identify hospitals that appear in multiple states
# multi_state_mcrnums <- hospdata_clean %>%
#   group_by(mcrnum) %>%
#   summarise(n_states = n_distinct(state)) %>%
#   filter(n_states > 1) %>%
#   pull(mcrnum)
# 
# before_count <- n_distinct(hospdata_clean$mcrnum)
# 
# hospdata_clean <- hospdata_clean %>%
#   filter(!mcrnum %in% multi_state_mcrnums)
# 
# after_count <- n_distinct(hospdata_clean$mcrnum)
# 
# cat("\n=== FILTER 3: Multi-State Hospitals ===\n")
# cat(paste("Hospitals removed:", before_count - after_count, "\n"))
# cat(paste("Observations remaining:", nrow(hospdata_clean), "\n"))
# cat(paste("Unique hospitals remaining:", after_count, "\n"))


# ==============================================================================
# FILTER 4: BED COUNT - REMOVE HOSPITALS WITH MISSING BED DATA
# ==============================================================================

# WHY: Bed count is key control variable; missing data suggests data quality issues
# DECISION: Keep only hospitals with complete bed data across all years (0 missing)
# RATIONALE: 94% of hospitals have complete data - strict filter keeps sample clean

# Identify hospitals with ANY missing bed data
hospitals_with_missing_beds <- hospdata_clean %>%
  group_by(mcrnum) %>%
  summarise(
    total_years = n(),
    missing_years = sum(is.na(beds))
  ) %>%
  filter(missing_years > 0) %>%
  pull(mcrnum)

before_count <- n_distinct(hospdata_clean$mcrnum)

hospdata_clean <- hospdata_clean %>%
  filter(!mcrnum %in% hospitals_with_missing_beds)

after_count <- n_distinct(hospdata_clean$mcrnum)

cat("\n=== FILTER 4: Complete Bed Data ===\n")
cat(paste("Hospitals removed (any missing bed data):", before_count - after_count, "\n"))
cat(paste("Observations remaining:", nrow(hospdata_clean), "\n"))
cat(paste("Unique hospitals remaining:", after_count, "\n"))


# ==============================================================================
# FILTER 5: BED COUNT - MINIMUM SIZE THRESHOLD
# ==============================================================================

# WHY: Very small hospitals may have different operational dynamics
# DECISION: Remove hospitals that ever report < 30 beds in any year
# METHOD: Identify hospitals where minimum bed count < 30, remove entire hospital
# RATIONALE: Ensures consistent hospital size; transient drops below 30 suggest data issues

# Calculate bed statistics for each hospital
hospital_bed_stats <- hospdata_clean %>%
  group_by(mcrnum) %>%
  summarise(
    avg_beds = mean(beds, na.rm = TRUE),
    min_beds = min(beds, na.rm = TRUE),
    max_beds = max(beds, na.rm = TRUE)
  )

# Identify hospitals that EVER have < 30 beds
small_hospitals <- hospital_bed_stats %>%
  filter(min_beds < 30) %>%
  pull(mcrnum)

before_count <- n_distinct(hospdata_clean$mcrnum)

hospdata_clean <- hospdata_clean %>%
  filter(!mcrnum %in% small_hospitals)

after_count <- n_distinct(hospdata_clean$mcrnum)

cat("\n=== FILTER 5: Minimum Bed Count (Ever < 30 beds) ===\n")
cat(paste("Hospitals removed (min beds < 30 in any year):", before_count - after_count, "\n"))
cat(paste("Observations remaining:", nrow(hospdata_clean), "\n"))
cat(paste("Unique hospitals remaining:", after_count, "\n"))

# Show bed count distribution for remaining hospitals
cat("\nBed count distribution (remaining hospitals):\n")
hospital_bed_stats %>%
  filter(mcrnum %in% hospdata_clean$mcrnum) %>%
  summarise(
    min_avg = min(avg_beds),
    q25 = quantile(avg_beds, 0.25),
    median = median(avg_beds),
    q75 = quantile(avg_beds, 0.75),
    max_avg = max(avg_beds)
  ) %>%
  print()

# ==============================================================================
# FILTER 4B: MAXIMUM BED COUNT - REMOVE DATA ERRORS
# ==============================================================================

# WHY: Some observations have impossibly high bed counts (e.g., 6+ million)
# DECISION: Remove hospitals that ever report > 3,000 beds
# RATIONALE: Largest US hospitals have ~2,400 beds; 3,000 is a generous upper bound

#diagnostic of the large bed count issue
hospdata %>%
  filter(!is.na(beds)) %>%
  arrange(desc(beds)) %>%
  select(mcrnum, mcrnum, state, year, beds) %>%
  head(20)

hospitals_with_extreme_beds <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(any(beds > 3000, na.rm = TRUE)) %>%
  pull(mcrnum) %>%
  unique()

before_count <- n_distinct(hospdata_clean$mcrnum)

hospdata_clean <- hospdata_clean %>%
  filter(!mcrnum %in% hospitals_with_extreme_beds)

after_count <- n_distinct(hospdata_clean$mcrnum)

cat("\n=== FILTER 4B: Maximum Bed Count (≤ 3,000) ===\n")
cat(paste("Hospitals removed (beds > 3,000 in any year):", before_count - after_count, "\n"))
cat(paste("Observations remaining:", nrow(hospdata_clean), "\n"))
cat(paste("Unique hospitals remaining:", after_count, "\n"))

# ==============================================================================
# ADDITIONAL DATA QUALITY CHECK: REMOVE HOSPITALS WITH BED COUNT = 0
# ==============================================================================

# WHY: Bed count of 0 is impossible for operating hospital
# NOTE: This should be caught by previous filters, but check to be safe

hospitals_with_zero_beds <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(any(beds == 0, na.rm = TRUE)) %>%
  pull(mcrnum) %>%
  unique()

if (length(hospitals_with_zero_beds) > 0) {
  before_count <- n_distinct(hospdata_clean$mcrnum)
  
  hospdata_clean <- hospdata_clean %>%
    filter(!mcrnum %in% hospitals_with_zero_beds)
  
  after_count <- n_distinct(hospdata_clean$mcrnum)
  
  cat("\n=== ADDITIONAL CHECK: Zero Bed Count ===\n")
  cat(paste("Hospitals removed (beds = 0 in any year):", before_count - after_count, "\n"))
  cat(paste("Observations remaining:", nrow(hospdata_clean), "\n"))
  cat(paste("Unique hospitals remaining:", after_count, "\n"))
} else {
  cat("\n=== ADDITIONAL CHECK: Zero Bed Count ===\n")
  cat("No hospitals with zero beds found. ✓\n")
}

cat(paste("  Year range:", min(hospdata$year), "to", max(hospdata$year), "\n"))

# ==============================================================================
# FILTER 7: MINIMUM DISCHARGE VOLUME
# ==============================================================================

# WHY: Hospitals with average < 100 discharges/year likely specialty facilities or data errors
# DECISION: Remove hospitals with average discharges < 100

# Identify low-volume hospitals
low_discharge_hospitals <- hospdata_clean %>%
  group_by(mcrnum) %>%
  summarise(avg_discharges = mean(tot_discharges, na.rm = TRUE)) %>%
  filter(avg_discharges < 100) %>%
  pull(mcrnum)

before_count <- n_distinct(hospdata_clean$mcrnum)

hospdata_clean <- hospdata_clean %>%
  filter(!mcrnum %in% low_discharge_hospitals)

after_count <- n_distinct(hospdata_clean$mcrnum)

cat("\n=== FILTER 7: Minimum Discharge Volume (Avg >= 100) ===\n")
cat(paste("Hospitals removed:", before_count - after_count, "\n"))
cat(paste("Observations remaining:", nrow(hospdata_clean), "\n"))
cat(paste("Unique hospitals remaining:", after_count, "\n"))

# ==============================================================================
# FILTER 8: Other unrealisic values
# ==============================================================================

# negative operating expenses, net patient revenue, charges
hosp_bad_data <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(any(tot_operating_exp < 0, na.rm = TRUE) | any(net_pat_rev < 0, na.rm = TRUE) 
  | any(tot_charges < 0, na.rm = TRUE) | any(mcaid_charges < 0, na.rm = TRUE) | any(cost_to_charge < 0, na.rm = TRUE)
  | cost_to_charge > 2) %>%
  pull(mcrnum) %>%
  unique()

before_count <- n_distinct(hospdata_clean$mcrnum)

hospdata_clean <- hospdata_clean %>%
  filter(!mcrnum %in% hosp_bad_data)

after_count <- n_distinct(hospdata_clean$mcrnum)

cat("\n=== FILTER 8: Remove Hospitals with Unrealistic Financial Values ===\n")
cat(paste("Hospitals removed (negative financial values):", before_count - after_count, "\n"))
cat(paste("Observations remaining:", nrow(hospdata_clean), "\n"))
cat(paste("Unique hospitals remaining:", after_count, "\n"))

# ==============================================================================
# FINAL SAMPLE SUMMARY
# ==============================================================================

cat("\n" , rep("=", 70), "\n", sep = "")
cat("FINAL ANALYTICAL SAMPLE\n")
cat(rep("=", 70), "\n", sep = "")

cat("\nSample composition:\n")
cat(paste("  Total observations:", nrow(hospdata_clean), "\n"))
cat(paste("  Unique hospitals:", n_distinct(hospdata_clean$mcrnum), "\n"))
cat(paste("  Year range:", min(hospdata_clean$year), "to", max(hospdata_clean$year), "\n"))
cat(paste("  Number of years:", n_distinct(hospdata_clean$year), "\n"))

cat("\nHospitals by state:\n")
hospdata_clean %>%
  distinct(mcrnum, state) %>%
  count(state, sort = TRUE) %>%
  head(10) %>%
  print()

cat("\n... (showing top 10 states)\n")

cat("\nHospitals by treatment status:\n")
hospdata_clean %>%
  distinct(mcrnum, state, ever_treat, cohort) %>%
  count(ever_treat) %>%
  mutate(
    status = ifelse(ever_treat == 1, "Ever Treated", "Never Treated"),
    pct = round(n / sum(n) * 100, 1)
  ) %>%
  select(status, n, pct) %>%
  print()

cat("\nTreatment cohorts:\n")
hospdata_clean %>%
  distinct(mcrnum, cohort) %>%
  count(cohort, sort = TRUE) %>%
  print()

# ==============================================================================
# CREATE PRE-TREATMENT BASELINE CHARACTERISTICS
# ==============================================================================

hospdata_clean <- hospdata_clean %>%
  group_by(mcrnum) %>%
  arrange(year) %>%
  mutate(
    # Pre-treatment averages (for treated hospitals)
    # For never-treated (gname == 0), use early years (2004-2006)
    pre_beds_avg = case_when(
      gname == 0 ~ mean(beds[year %in% 2004:2006], na.rm = TRUE),  # Never treated: use 2004-2006
      TRUE ~ mean(beds[year < gname], na.rm = TRUE)                 # Treated: use all years before treatment
    ),
    
    pre_discharges_avg = case_when(
      gname == 0 ~ mean(tot_discharges[year %in% 2004:2006], na.rm = TRUE),
      TRUE ~ mean(tot_discharges[year < gname], na.rm = TRUE)
    ),
    
    # Alternative: Use a fixed baseline period for everyone (more comparable)
    baseline_beds = mean(beds[year %in% 2004:2006], na.rm = TRUE),
    baseline_discharges = mean(tot_discharges[year %in% 2004:2006], na.rm = TRUE),
    
    # Number of pre-treatment years available
    n_pre_years = sum(year < gname, na.rm = TRUE),
    n_baseline_years = sum(year %in% 2004:2006, na.rm = TRUE)
  ) %>%
  ungroup()

# Check the results
cat("\n=== PRE-TREATMENT BASELINE CHARACTERISTICS ===\n")

# Summary by cohort
hospdata_clean %>%
  group_by(cohort) %>%
  summarise(
    n_hospitals = n_distinct(mcrnum),
    mean_pre_beds = mean(pre_beds_avg, na.rm = TRUE),
    mean_baseline_beds = mean(baseline_beds, na.rm = TRUE),
    mean_pre_discharges = mean(pre_discharges_avg, na.rm = TRUE),
    pct_missing_pre_beds = round(mean(is.na(pre_beds_avg)) * 100, 1),
    pct_missing_baseline = round(mean(is.na(baseline_beds)) * 100, 1)
  ) %>%
  print(n = Inf)

# Check for issues
cat("\n=== POTENTIAL ISSUES ===\n")
cat(paste("Hospitals with missing pre_beds_avg:", 
          sum(is.na(hospdata_clean$pre_beds_avg)), "\n"))
cat(paste("Hospitals with missing baseline_beds:", 
          sum(is.na(hospdata_clean$baseline_beds)), "\n"))

# Check hospitals treated very early (might have no pre-treatment data)
early_treated <- hospdata_clean %>%
  filter(gname == 2004) %>%
  distinct(mcrnum, pre_beds_avg, n_pre_years) %>%
  arrange(n_pre_years)

cat("\n=== 2004 COHORT (LIMITED PRE-TREATMENT DATA) ===\n")
cat(paste("2004 cohort has", unique(early_treated$n_pre_years), "pre-treatment years\n"))
cat("Consider using baseline_beds instead for this cohort\n")

# ==============================================================================
# VERIFY TREATMENT VARIABLES ARE INTACT
# ==============================================================================

cat("\n=== TREATMENT VARIABLE CHECK ===\n")

# Check that all required variables exist
required_vars <- c("gname", "cohort", "ever_treat", "post_treat", "time_to_treat")
missing_vars <- setdiff(required_vars, names(hospdata_clean))

if (length(missing_vars) == 0) {
  cat("All treatment variables present: ✓\n")
  cat("  - gname (for C&S)\n")
  cat("  - cohort (readable version)\n")
  cat("  - ever_treat (0/1)\n")
  cat("  - post_treat (0/1)\n")
  cat("  - time_to_treat (years since adoption)\n")
} else {
  cat("WARNING: Missing variables:", paste(missing_vars, collapse = ", "), "\n")
}


# ==============================================================================
# SAVE ANALYTICAL DATASET
# ==============================================================================

write.csv(
  hospdata_clean,
  paste0(data_output_path, "hospdata_analysis.csv"),
  row.names = FALSE
)

cat("\n=== DATA SAVED SUCCESSFULLY ===\n")
cat("Output file: hospdata_analysis.csv\n")
cat("Location:", data_output_path, "\n")

cat("\n", rep("=", 70), "\n", sep = "")
cat("CLEANING COMPLETE!\n")
cat(rep("=", 70), "\n\n", sep = "")


# ==============================================================================
# OPTIONAL: CREATE SUMMARY STATISTICS TABLE
# ==============================================================================

# Uncomment to generate a summary table of key variables

# summary_stats <- hospdata_clean %>%
#   group_by(year) %>%
#   summarise(
#     n_hospitals = n_distinct(mcrnum),
#     n_treated = sum(post_treat),
#     pct_treated = round(mean(post_treat) * 100, 1),
#     avg_beds = round(mean(beds, na.rm = TRUE), 1),
#     avg_ucc_prop = round(mean(ucc_prop, na.rm = TRUE), 3),
#     avg_mcaid_prop = round(mean(mcaid_prop, na.rm = TRUE), 3)
#   )
# 
# print(summary_stats)
# 
# # Save summary stats
# write.csv(
#   summary_stats,
#   paste0(data_output_path, "summary_stats_by_year.csv"),
#   row.names = FALSE
# )