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
# FILTER 1: RESTRICT TO ANALYSIS YEARS (2000-2024)
# ==============================================================================

# WHY: Pre 2000 data is sparse
# DECISION: start sample when treatment data begins
# NOTE: States with treatment in 2004 are "always treated" in our data

hospdata_clean <- hospdata %>%
  filter(year >= 1999 & year <= 2024)

cat("\n=== FILTER 1: Year Range (2000-2024) ===\n")
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

# WHY: Some observations have impossibly high bed counts (e.g., million)
# DECISION: Remove hospitals that ever report > 3,000 beds
# RATIONALE: Largest US hospitals have ~2,400 beds; 3,000 is a generous upper bound

before_avg <- mean(hospdata_clean$beds, na.rm = TRUE)

#identify hospitals with a large bed count issue
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

View(hospdata_clean %>% filter(mcrnum %in% hospitals_with_extreme_beds) %>% select(mcrnum, state, year, beds))
# Looking at these observations shows they are data errors 

# Replace extreme values with average of year before and after
hospdata_clean <- hospdata_clean %>%
  arrange(mcrnum, year) %>%
  group_by(mcrnum) %>%
  mutate(
    beds_lag = lag(beds),      # Year before
    beds_lead = lead(beds),    # Year after
    
    # Replace if > 3000 with average of surrounding years
    beds_corrected = case_when(
      beds > 3000 ~ (beds_lag + beds_lead) / 2,  # Average of before/after
      TRUE ~ beds                                 # Otherwise keep original
    ),
    
    # If still NA (e.g., first/last year), use closest available
    beds_corrected = case_when(
      is.na(beds_corrected) & beds > 3000 ~ coalesce(beds_lag, beds_lead),
      TRUE ~ beds_corrected
    )
  ) %>%
  # Replace original beds with corrected version
  mutate(beds = beds_corrected) %>%
  select(-beds_lag, -beds_lead, -beds_corrected) %>%
  ungroup()


after_avg <- mean(hospdata_clean$beds, na.rm = TRUE)

cat("\n=== CLEAN: Extreme Bed Count Corrections ===\n")

cat(paste("Unique hospitals remaining:", after_count, "\n"))
cat(paste("Average beds before correction:", before_avg, "\n"))
cat(paste("Average beds after correction:", mean(hospdata_clean$beds, na.rm = TRUE), "\n"))

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
# CLEAN: TOT DISCHARGE VOLUME
# ==============================================================================
# WHY: Distribution of Total Discharges contains questionable values 

before_count <- n_distinct(hospdata_clean$mcrnum)

hospitals_with_extreme_discharges <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(any(tot_discharges > 100000, na.rm = TRUE)) %>%
  pull(mcrnum) %>%
  unique()

# View(hospdata_clean %>% filter(mcrnum %in% hospitals_with_extreme_discharges) %>% select(mcrnum, name, state, year, tot_discharges, beds, net_pat_rev))
# Looking at these observations, they dont appear to be errors all the time. 

# manually replace errors with the average of lead/lag year. 
# these mcrnums were found by being sufficiently high compared to other entries for the same hosp 
hospdata_clean <- hospdata_clean %>%
  arrange(mcrnum, year) %>%
  group_by(mcrnum) %>%
  mutate(
    tot_discharges = case_when(
      mcrnum == 210012 & tot_discharges > 100000 ~ (lag(tot_discharges) + lead(tot_discharges)) / 2,
      mcrnum == 380004 & tot_discharges > 100000 ~ (lag(tot_discharges) + lead(tot_discharges)) / 2,
      TRUE ~ tot_discharges
    )
  ) %>%
  ungroup()

hospitals_with_low_discharges <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(any(tot_discharges < 30, na.rm = TRUE)) %>%
  pull(mcrnum) %>%
  unique()

#View(hospdata_clean %>% filter(mcrnum %in% hospitals_with_low_discharges) %>% select(mcrnum, name, state, year, tot_discharges, mcaid_discharges, beds, net_pat_rev))

# after review, thesee cant be fixed so remove them 
hospdata_clean <- hospdata_clean %>%
  filter(!mcrnum %in% hospitals_with_low_discharges)

after_count <- n_distinct(hospdata_clean$mcrnum)

cat("\n=== FILTER 7: Minimum Discharge Volume (Avg >= 100) ===\n")
cat(paste("Hospitals removed:", before_count - after_count, "\n"))
cat(paste("Observations remaining:", nrow(hospdata_clean), "\n"))
cat(paste("Unique hospitals remaining:", after_count, "\n"))

# ==============================================================================
# CLEAN: NET PATIENT REVENUE
# ==============================================================================
# WHY: The distribution of net patient revenue has extreme outliers and unfeasible values 

before_count <- n_distinct(hospdata_clean$mcrnum)
begin_avg <- mean(hospdata_clean$net_pat_rev, na.rm = TRUE)

# NEGATIVES 
hospitals_with_neg_npr <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(any(net_pat_rev < 0, na.rm = TRUE)) %>%
  pull(mcrnum) %>%
  unique()

#View(hospdata_clean %>% filter(mcrnum %in% hospitals_with_neg_npr) %>% select(mcrnum, name, state, year, cohort, net_pat_rev, mcaid_discharges, tot_discharges, beds))

# some are typos. correct to absolute value 
hospdata_clean <- hospdata_clean %>%
  mutate( 
    net_pat_rev = case_when(
      mcrnum == 30083 & net_pat_rev < 0 ~ abs(net_pat_rev), 
      mcrnum == 50457 & net_pat_rev < 0 ~ abs(net_pat_rev),
      mcrnum == 50739 & net_pat_rev < 0 ~ abs(net_pat_rev),
      mcrnum == 60003 & net_pat_rev < 0 ~ abs(net_pat_rev),
      mcrnum == 260023 & net_pat_rev < 0 ~ abs(net_pat_rev),
      mcrnum == 300018 & net_pat_rev < 0 ~ abs(net_pat_rev),
      mcrnum == 100062 & net_pat_rev < 0 ~ (lag(net_pat_rev) + lead(net_pat_rev)) / 2,
      mcrnum == 140231 & net_pat_rev < 0 ~ (lag(net_pat_rev) + lead(net_pat_rev)) / 2,
      mcrnum == 170016 & net_pat_rev < 0 ~ (lag(net_pat_rev) + lead(net_pat_rev)) / 2,
      mcrnum == 390164 & net_pat_rev < 0 ~ (lag(net_pat_rev) + lead(net_pat_rev)) / 2,
      mcrnum == 390236 & net_pat_rev < 0 ~ (lag(net_pat_rev) + lead(net_pat_rev)) / 2,
      mcrnum == 390258 & net_pat_rev < 0 ~ (lag(net_pat_rev) + lead(net_pat_rev)) / 2,
      mcrnum == 440067 & net_pat_rev < 0 ~ (lag(net_pat_rev) + lead(net_pat_rev)) / 2,
      mcrnum == 450539 & net_pat_rev < 0 ~ (lag(net_pat_rev) + lead(net_pat_rev)) / 2,
      mcrnum == 520107 & net_pat_rev < 0 ~ (lag(net_pat_rev) + lead(net_pat_rev)) / 2, 
      TRUE ~ net_pat_rev
    )
  )

# hospitals to remove for incomplete data or unfeasible values
# mcrnum == 50733, 50425, 50139, 450831
hospdata_clean <- hospdata_clean %>%
  filter(!mcrnum %in% c(50733, 50425, 50139, 450831))

# HIGH VALUES 
hospitals_with_high_npr <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(any(net_pat_rev > 3e9, na.rm = TRUE)) %>%
  pull(mcrnum) %>%
  unique()

#View(hospdata_clean %>% filter(mcrnum %in% hospitals_with_high_npr) %>% select(mcrnum, name, state, year, cohort, net_pat_rev, mcaid_discharges, tot_discharges, beds))

# remove hospitals with bad data
hospdata_clean <- hospdata_clean %>%
  filter(!mcrnum %in% c(50055, 440070))

# after review on 2/26, most high values are very high but dont appear to be errors 

after_count <- n_distinct(hospdata_clean$mcrnum)
after_avg <- mean(hospdata_clean$net_pat_rev, na.rm = TRUE)
# summary(hospdata_clean$net_pat_rev)

cat("\n=== CLEAN: Net Patient Revenue Corrections ===\n")
cat(paste("Hospitals removed (unfeasible net patient revenue):", before_count - after_count, "\n"))
cat(paste("Observations remaining:", nrow(hospdata_clean), "\n"))
cat(paste("Unique hospitals remaining:", after_count, "\n"))
cat(paste("Average net patient revenue before correction:", begin_avg, "\n"))
cat(paste("Average net patient revenue after correction:", after_avg, "\n"))

# ==============================================================================
# CLEAN: Uncompensated Care
# ==============================================================================
# WHY: Distribution major skewed right, looking to remove the value of over a billion and fix negatives

before_count <- n_distinct(hospdata_clean$mcrnum)
before_avg <- mean(hospdata_clean$uncomp_care, na.rm = TRUE)

# HIGH VALUES
hospitals_with_high_uncomp_care <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(any(uncomp_care > 1e9, na.rm = TRUE)) %>%
  pull(mcrnum) %>%
  unique()

# none appear to be errors
# View(hospdata_clean %>% filter(mcrnum %in% hospitals_with_high_uncomp_care) %>% select(mcrnum, name, state, year, cohort, uncomp_care, net_pat_rev, mcaid_discharges, tot_discharges, beds))

# NEGATIVE VALUES 
hospitals_with_neg_uncomp_care <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(any(uncomp_care < 0, na.rm = TRUE)) %>%
  pull(mcrnum) %>%
  unique()

#View(hospdata_clean %>% filter(mcrnum %in% hospitals_with_neg_uncomp_care) %>% select(mcrnum, name, state, year, cohort, uncomp_care, net_pat_rev, mcaid_discharges, tot_discharges, beds))

# some are typos. correct to absolute value
hospdata_clean <- hospdata_clean %>%
  mutate( 
    uncomp_care = case_when( 
      mcrnum == 50242 & uncomp_care < 0 ~ abs(uncomp_care),
      mcrnum == 50280 & uncomp_care < 0 ~ abs(uncomp_care),
      mcrnum == 50763 & uncomp_care < 0 ~ abs(uncomp_care),
      mcrnum == 330141 & uncomp_care < 0 ~ abs(uncomp_care),
      mcrnum == 440130 & uncomp_care < 0 ~ abs(uncomp_care),
      mcrnum == 670043 & uncomp_care < 0 ~ abs(uncomp_care),
      mcrnum == 60008 & uncomp_care < 0 ~ lag(uncomp_care) + lead(uncomp_care) / 2,
      mcrnum == 490077 & uncomp_care < 0 ~ lag(uncomp_care) + lead(uncomp_care) / 2,
      TRUE ~ uncomp_care
    ))

# remove hospitals with bad data
hospdata_clean <- hospdata_clean %>%
  filter(!mcrnum %in% c(50257, 330141, 440151))

after_count <- n_distinct(hospdata_clean$mcrnum)
after_avg <- mean(hospdata_clean$uncomp_care, na.rm = TRUE)

cat("\n=== CLEAN: Uncompensated Care Corrections ===\n")
cat(paste("Hospitals removed (unfeasible uncompensated care):", before_count - after_count, "\n"))
cat(paste("Observations remaining:", nrow(hospdata_clean), "\n"))
cat(paste("Unique hospitals remaining:", after_count, "\n"))
cat(paste("Average uncompensated care before correction:", before_avg, "\n"))
cat(paste("Average uncompensated care after correction:", after_avg, "\n"))
# ==============================================================================
# CLEAN: Total Operating Expenses
# ==============================================================================
# WHY: Distirbution is major skewed right with outlier of 7+ billion and some negatives. 

before_avg <- mean(hospdata_clean$tot_operating_exp, na.rm = TRUE)

# HIGH VALUES
hospitals_with_high_operating_exp <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(any(tot_operating_exp > 6e9, na.rm = TRUE)) %>%
  pull(mcrnum) %>%
  unique()

# no large values appear to be errors 
# View(hospdata_clean %>% filter(mcrnum %in% hospitals_with_high_operating_exp) %>% select(mcrnum, name, state, year, cohort, tot_operating_exp, net_pat_rev, mcaid_discharges, tot_discharges, beds))

# NEGATIVE VALUES
hospitals_with_neg_operating_exp <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(any(tot_operating_exp < 0, na.rm = TRUE)) %>%
  pull(mcrnum) %>%
  unique()

#View(hospdata_clean %>% filter(mcrnum %in% hospitals_with_neg_operating_exp) %>% select(mcrnum, name, state, year, cohort, tot_operating_exp, net_pat_rev, mcaid_discharges, tot_discharges, beds))

# filter out these hospitals for now. Its only 3 and they dont have an obvious solution. 
hospdata_clean <- hospdata_clean %>%
  filter(!mcrnum %in% c(50072, 260021, 490088))

# ==============================================================================
# CLEAN: Medicaid Charges
# ==============================================================================
# WHY: Distribution shows a max over 10 billion and negative values. 
# NOTE: 4/7 moving away from charges as an outcome. We dont really know what charges are! 

before_avg <- mean(hospdata_clean$mcaid_charges, na.rm = TRUE)

# HIGH VALUES
hospitals_with_high_mcaid_charges <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(any(mcaid_charges > 1e10, na.rm = TRUE)) %>%
  pull(mcrnum) %>%
  unique()  

# View(hospdata_clean %>% filter(mcrnum %in% hospitals_with_high_mcaid_charges) %>% select(mcrnum, name, state, year, cohort, mcaid_charges, net_pat_rev, mcaid_discharges, tot_discharges, beds))
# Only one mcrnum, looks like a Hospital System

hospdata_clean <- hospdata_clean %>%
  filter(!mcrnum %in% hospitals_with_high_mcaid_charges)

# NEGATIVE VALUES
hospitals_with_neg_mcaid_charges <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(any(mcaid_charges < 0, na.rm = TRUE)) %>%
  pull(mcrnum) %>%
  unique()

#View(hospdata_clean %>% filter(mcrnum %in% hospitals_with_neg_mcaid_charges) %>% select(mcrnum, name, state, year, cohort, mcaid_charges, net_pat_rev, mcaid_discharges, tot_discharges, beds))
# looks like a typo

hospdata_clean <- hospdata_clean %>%
  mutate( 
    mcaid_charges = case_when( 
      mcrnum %in% hospitals_with_neg_mcaid_charges & mcaid_charges < 0 ~ abs(mcaid_charges),
      TRUE ~ mcaid_charges
    ))

# LOW VALUES 
hospitals_with_low_mcaid_charges <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(any(mcaid_charges < mcaid_discharges, na.rm = TRUE)) %>%
  pull(mcrnum) %>%
  unique()

# View(hospdata_clean %>% filter(mcrnum %in% hospitals_with_low_mcaid_charges) %>% select(mcrnum, name, state, year, cohort, mcaid_charges, mcaid_discharges, tot_discharges, beds))
# all look like errors. 

hospdata_clean <- hospdata_clean %>%
  mutate( 
    mcaid_charges = case_when( 
      mcrnum == 30043 & year == 2008 ~ lag(mcaid_charges),
      mcrnum == 30043 & year == 2009 ~ lead(mcaid_charges),
      mcrnum == 140240 & year == 2007 ~ lag(mcaid_charges) + lead(mcaid_charges) / 2,
      mcrnum == 370091 & year == 2004 ~ lead(mcaid_charges),
      mcrnum == 420072 & year == 2003 ~ lead(mcaid_charges),
      TRUE ~ mcaid_charges
    ))

# remove unfixable hospitals
hospdata_clean <- hospdata_clean %>%
  filter(!mcrnum %in% c(140100, 180036))

after_avg <- mean(hospdata_clean$mcaid_charges, na.rm = TRUE)

cat("\n=== CLEAN: Medicaid Charges Corrections ===\n")
cat(paste("Hospitals removed (unfeasible Medicaid charges):", before_count - after_count, "\n"))
cat(paste("Observations remaining:", nrow(hospdata_clean), "\n"))
cat(paste("Unique hospitals remaining:", after_count, "\n"))
cat(paste("Average Medicaid charges before correction:", before_avg, "\n"))
cat(paste("Average Medicaid charges after correction:", after_avg, "\n"))
# ==============================================================================
# FILTER 8: Other unrealisic values
# ==============================================================================

# negative operating expenses, net patient revenue, charges
# cost to charge, cost per discharge
# operating margin (removes 2 hospitals with extreme values) 
# hosp_bad_data <- hospdata_clean %>%
#  group_by(mcrnum) %>%
# filter(any(tot_operating_exp < 0, na.rm = TRUE) | any(net_pat_rev < 0, na.rm = TRUE) 
#  | any(tot_charges < 0, na.rm = TRUE) | any(mcaid_charges < 0, na.rm = TRUE) | any(mcaid_charges > 5e9, na.rm = TRUE) | any(cost_to_charge < 0, na.rm = TRUE)
#  | any(cost_to_charge > 2, na.rm = TRUE) | any(cost_per_discharge > 60000, na.rm = TRUE) | any(op_margin < -1.5, na.rm = TRUE) | any(op_margin > 1.5, na.rm = TRUE) 
#  | any(mcaid_charges < mcaid_discharges, na.rm = TRUE) | any(tot_uncomp_care_charges < 0, na.rm = TRUE)) %>%
#  pull(mcrnum) %>%
#  unique()

#before_count <- n_distinct(hospdata_clean$mcrnum)

#hospdata_clean <- hospdata_clean %>%
  #filter(!mcrnum %in% hosp_bad_data)

#after_count <- n_distinct(hospdata_clean$mcrnum)

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
    # Pre-treatment averages 
    pre_beds_avg = case_when(
      gname == 0 ~ mean(beds, na.rm = TRUE), 
      TRUE ~ mean(beds[year < gname], na.rm = TRUE)                 # Treated: use all years before treatment
    ),
    
    pre_discharges_avg = case_when(
      gname == 0 ~ mean(tot_discharges, na.rm = TRUE),
      TRUE ~ mean(tot_discharges[year < gname], na.rm = TRUE)
    ),
    
    # Alternative: Use a fixed baseline period for everyone (more comparable)
    baseline_beds = mean(beds[year %in% 2000:2006], na.rm = TRUE),
    baseline_discharges = mean(tot_discharges[year %in% 2000:2006], na.rm = TRUE),
    
    # Number of pre-treatment years available
    n_pre_years = sum(year < gname, na.rm = TRUE),
    n_baseline_years = sum(year %in% 2000:2006, na.rm = TRUE)
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
  filter(gname == 0) %>%
  distinct(mcrnum, pre_beds_avg, n_pre_years) %>%
  arrange(n_pre_years)

cat("\n=== Always COHORT (CHECK PRE-TREATMENT DATA) ===\n")
cat(paste("Always cohort has", unique(early_treated$n_pre_years), "pre-treatment years\n"))


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
# OPTION: SAVE ANALYTICAL DATASET
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
cat("COMPLETE: cleaned, income data!\n")
cat(rep("=", 70), "\n\n", sep = "")




