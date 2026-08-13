# ==============================================================================
# HCRIS Data Preparation Script - CLEANED VERSION
# Purpose: Prepare hospital-level panel data for staggered DiD analysis
# Reference where im at: data, dataclean, datafilter, and stats files are cleaned and acceptable.
# ==============================================================================


# Load required packages -------------------------------------------------------
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)

# Set file paths (modify these for your system) -------------------------------
data_input_path <- "/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/input/"
data_output_path <- "/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/output/"


# ==============================================================================
# 1. LOAD AND CLEAN HCRIS DATA
# ==============================================================================

hcris <- read.delim(
  paste0(data_output_path, "HCRIS_data.txt"), 
  stringsAsFactors = FALSE
)

# Clean state abbreviations ----------------------------------------------------
# WHAT: Standardize state names to 2-letter codes
# WHY: Merging requires consistent state identifiers

hcris <- hcris %>%
  mutate(
    state = str_trim(toupper(state)),  # Remove whitespace, make uppercase
    state = recode(state,
      # Fix common misspellings and full names
      "TENNESSEE" = "TN",
      "TE" = "TN",
      "MICHIGAN" = "MI",
      "ILLINOIS" = "IL",
      "CALIFORNIA" = "CA",
      "ARIZONA" = "AZ",
      "MONTANA" = "MT", 
      "NORTH CAROLINA" = "NC",
      "WISCONSIN" = "WI",
      "UTAH" = "UT",
      "KA" = "KS",
      "P." = "PR",
      "AX" = "AZ"
    )
  )

# Filter out non-states and missing data ---------------------------------------
# Remove territories and rows with missing identifiers

hcris <- hcris %>%
  filter(
    !is.na(state),                                          # Remove missing states
    !state %in% c("AS", "GU", "MP", "PR", "VI"),          # Remove territories
    !is.na(provider_number)                                # Remove missing hospital IDs
  )


# ==============================================================================
# 2. LOAD AND PROCESS STATE TAX ADOPTION DATA
# ==============================================================================

tax <- read.csv(
  paste0(data_input_path, "statetaxadopt_kff.csv"), 
  skip = 2  # Skip header rows
)

# Clean tax data structure -----------------------------------------------------
tax <- tax %>%
  select(-Footnotes) %>%           # Remove footnote column
  slice(2:52) %>%                  # Keep state rows only (remove title rows)
  rename(statename = Location) %>%
  mutate(
    # Convert state names to abbreviations
    state = ifelse(
      statename == "District of Columbia", 
      "DC",
      state.abb[match(statename, state.name)]
    )
  )

# Clean year column names ------------------------------------------------------
# Remove "SFY." prefix from year columns to get columns named like "2004", "2005", etc.

tax <- tax %>%
  rename_with(~ str_remove(.x, "^SFY\\."))

# Keep only state and year columns
tax <- tax %>%
  select(state, starts_with("20"))

# Calculate first year of tax adoption ----------------------------------------
# WHAT: Find the earliest year each state adopted a provider tax
# HOW: Look across all year columns for "yes" or "Y", take minimum year
# RESULT: New column "firsttax" = year of adoption or "never"

tax <- tax %>%
  mutate(
    firsttax = apply(
      select(., starts_with("20")),  # Apply across year columns
      1,                              # Row-wise
      function(row) {
        # Find year columns with "yes" or "Y" (case-insensitive)
        years <- names(row)[str_detect(
          as.character(row), 
          regex("yes|^Y$", ignore_case = TRUE)
        )]
        
        # Return "never" if no adoption, otherwise minimum year
        if (length(years) == 0 || any(is.na(as.integer(years)))) {
          "never"
        } else {
          min(as.integer(years))
        }
      }
    )
  )


# ==============================================================================
# 3. [REMOVED] FMAP DATA
# fmap/multiplier (from fmaps_kff.csv) were merged into hospdata but never
# used anywhere downstream in the project -- confirmed via a project-wide
# grep before removing. Dropped to avoid the dependency on that file
# (currently an un-synced OneDrive placeholder that times out on read).
# totaltax and expyear, which DO feed the actual analysis (exp_status,
# descriptive rollout stats), come from tax_totals/expansion below, not
# from fmaps_kff.csv, so removing this section doesn't affect them.
# ==============================================================================

# What does your tax data look like?

tax %>%
  select(state, firsttax) %>%
  filter(firsttax %in% c("2004", "2005", "2006", "never"))

# ==============================================================================
# 4. CREATE CUMULATIVE TAX COUNT (for descriptive stats)
# ==============================================================================

# WHAT: Count how many states have adopted by each year
# WHY: Useful for showing treatment rollout over time

tax_totals <- tax %>%
  pivot_longer(
    cols = matches("^20\\d{2}$"),  # Year columns like 2004, 2005
    names_to = "year",
    values_to = "has_tax"
  ) %>%
  mutate(
    year = as.integer(year),
    has_tax = tolower(has_tax) %in% c("yes", "y")  # Convert to logical
  ) %>%
  group_by(year) %>%
  summarise(
    totaltax = sum(has_tax, na.rm = TRUE),
    .groups = "drop"
  )

# expansion data 
expansion <- read.csv("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/input/expansion.csv", skip = 2)

#drop excess rows and rename states
expansion <- expansion %>% select(-Footnotes)
expansion <- expansion[2:52, ]
expansion <- expansion %>% rename(statename = Location) 
expansion <- expansion %>% rename(expdate = Expansion.Implementation.Date) 
expansion$state <- state.abb[match(expansion$statename, state.name)]
expansion$state[expansion$statename == "District of Columbia"] <- "DC"
expansion <- expansion %>%
  mutate(
    expyear = case_when(
      expdate %in% c("N/A", "", NA) ~ 0,   # keep NAs for non-expansion states
      TRUE ~ year(mdy(expdate))                   # extract the year (month/day/year)
    )) %>% select(state, expdate, expyear)

# ==============================================================================
# 5. MERGE ALL DATA SOURCES
# ==============================================================================

# Create separate state-level dataset for NON-TIME-VARYING variables
state_info <- tax %>%
  select(state, firsttax)  # This is STATE-LEVEL, not state-year

# Merge with HCRIS hospital data
hospdata <- hcris %>%
  # totaltax is year-level only (same value for every state in a given year)
  left_join(tax_totals, by = "year") %>%
  # expyear is state-level only (no year)
  left_join(expansion %>% select(state, expyear), by = "state") %>%
  # firsttax is state-level only (no year)
  left_join(state_info, by = "state") %>%
  rename(mcrnum = provider_number)

# ==============================================================================
# 6. CREATE TREATMENT VARIABLES 
# ==============================================================================

# Convert firsttax to numeric --------------------------------------------------
# Change "never" to NA, others to year number

hospdata <- hospdata %>%
  mutate(
    firsttax_num = case_when(
      firsttax == "never" ~ NA_real_,
      TRUE ~ as.numeric(firsttax)
    )
  )

# Create treatment variables ---------------------------------------------------

hospdata <- hospdata %>%
  mutate(
    # 1. GNAME: Treatment cohort for C&S package
    gname = case_when(
      is.na(firsttax_num) ~ 0,                    # Never treated = 0
      firsttax_num == 2004 ~ 0,                   # 2004 = always treated = 0
      TRUE ~ firsttax_num                         # 2005+ = adoption year
    ),
    
    # 2. COHORT: Character version for readability
    cohort = case_when(
      is.na(firsttax_num) ~ "Never",
      firsttax_num == 2004 ~ "Always (2004)",     # Mark as always treated
      TRUE ~ as.character(firsttax_num)
    ),
    
    # 3. EVER_TREAT: Simple indicator for ever being treated
    ever_treat = case_when(
      is.na(firsttax_num) ~ 0,
      TRUE ~ 1                                    # Includes 2004 cohort
    ),
    
    # 4. POST_TREAT: Is treatment currently active?
    post_treat = case_when(
      is.na(firsttax_num) ~ 0,                    # Never treated = always 0
      firsttax_num == 2004 ~ 1,                   # 2004 cohort = always 1 (even in , 2002)
      year >= firsttax_num ~ 1,                   # After adoption = 1
      TRUE ~ 0                                    # Before adoption = 0
    ),
    
    # 5. TIME_TO_TREAT: Years relative to treatment adoption
    time_to_treat = case_when(
      is.na(firsttax_num) ~ NA_real_,             # Never treated = NA
      firsttax_num == 2004 ~ NA_real_,            # Always treated = NA
      TRUE ~ year - firsttax_num                  # 2005+ cohorts: ,2002→ -3, 2003 → -2, 2004 → -1, etc.
    ),
  

  # 6. EXP_STATUS: Is this state operating under Medicaid expansion in this year?
  # 0 = not expanded in this year
  # 1 = expanded in this year
  # Non-expansion states are coded expyear == 0 (not NA) upstream, so that
  # must be checked explicitly -- otherwise `year >= expyear` (i.e. year >= 0)
  # is true for every real year and every non-expansion state gets marked
  # "expanded" in 100% of its state-years.
  exp_status = case_when(
    is.na(expyear) | expyear == 0 ~ 0,  # Non-expansion (or unknown) states = 0
    year >= expyear ~ 1,                # After expansion = 1
    TRUE ~ 0                           # Before expansion = 0
  ))


# Check 2004 cohort in 2003
hospdata %>%
  filter(cohort == "Always (2004)", year == 2003) %>%
  select(state, year, firsttax_num, cohort, gname, post_treat, ever_treat) %>%
  head(10)
# Should show: cohort = "Always (2004)", post_treat = 1

# Check 2005 cohort in 2001
hospdata %>%
  filter(cohort == "2005", year == 2001) %>%
  select(state, year, firsttax_num, cohort, gname, post_treat, time_to_treat) %>%
  head(10)
# Should show: cohort = "2005", post_treat = 0, time_to_treat = -4


# ==============================================================================
# 7. VERIFY TREATMENT GROUPS
# ==============================================================================

# Check distribution of treatment cohorts --------------------------------------
cat("\n=== TREATMENT COHORT SUMMARY ===\n")

hospdata %>%
  distinct(state, cohort) %>%
  count(cohort) %>%
  arrange(cohort) %>%
  print() 

# Check number of hospitals by treatment status -------------------------------
cat("\n=== HOSPITALS BY TREATMENT STATUS ===\n")

hospdata %>%
  distinct(mcrnum, state, ever_treat) %>%
  count(ever_treat) %>%
  mutate(
    status = ifelse(ever_treat == 1, "Ever Treated", "Never Treated")
  ) %>%
  select(status, n) %>%
  print()

# ==============================================================================
# 8. JOIN AHA: join necessary var from aha. 
# ==============================================================================
# Load AHA data
aha <- read_csv(paste0(data_input_path, "newAHAdata_20052023.csv"))

#cahnge column names to lowercase
colnames(aha) <- tolower(colnames(aha))

# Select only what you need from AHA and prepare for merge
# Service-line offering variables (emdephos, psyemhos, traumhos, adtchos,
# iclabhos, eswlhos) added for the service-line analysis; vem, alchhos,
# psybd, alchbd were already here.
aha_minimal <- aha %>%
  select(id, mcrnum, year, cntrl, serv, rurlhos, sysid, sysname, alchhos, psyhos, obhos, obbd, cicbd, alchbd, psybd, admtot, dctot, ipdtot, vem, vtot,
  suropip, suropop, suroptot, voth, mcrdc, mcdipd, mcddc, sptdc, sptip, thrtdc, thrtip, opotdc, opotip,
  emdephos, psyemhos, traumhos, adtchos, iclabhos, eswlhos)

# Convert aha_minimal mcrnum to match hospdata (integer)
aha_minimal <- aha_minimal %>%
  mutate(mcrnum = as.integer(mcrnum))

# FIX: mcrnum is 100% missing in AHA for 2005-2007 (confirmed against the
# raw file), even though every other AHA field -- including id, AHA's own
# stable internal facility ID -- is fully populated those years. Left as-is,
# the year+mcrnum merge below silently drops all AHA data for 2005-2007,
# for every AHA-sourced variable, not just the new service-line ones.
# Fix: build an id -> mcrnum crosswalk from years where both are present
# (2008+), keep only IDs that map to a single stable mcrnum (94.7% of IDs;
# the remaining ~5% have genuinely inconsistent mcrnum across years and are
# left out rather than guessed at), then backfill mcrnum for 2005-2007 rows
# using that crosswalk before merging.
id_mcrnum_crosswalk <- aha_minimal %>%
  filter(!is.na(id), !is.na(mcrnum)) %>%
  distinct(id, mcrnum) %>%
  group_by(id) %>%
  filter(n_distinct(mcrnum) == 1) %>%
  distinct(id, mcrnum) %>%
  ungroup()

cat("\n=== AHA id -> mcrnum CROSSWALK ===\n")
cat("Stable id->mcrnum mappings:", nrow(id_mcrnum_crosswalk), "\n")

aha_minimal <- aha_minimal %>%
  left_join(id_mcrnum_crosswalk, by = "id", suffix = c("", "_xwalk")) %>%
  mutate(mcrnum = coalesce(mcrnum, mcrnum_xwalk)) %>%
  select(-mcrnum_xwalk)

cat("mcrnum non-missing by year after crosswalk backfill:\n")
aha_minimal %>%
  filter(year %in% 2005:2010) %>%
  group_by(year) %>%
  summarise(n = n(), n_mcrnum_nonmiss = sum(!is.na(mcrnum)), .groups = "drop") %>%
  print()

# Merge on both mcrnum and year
hospdata <- hospdata %>%
  left_join(aha_minimal %>% select(-id), by = c("mcrnum", "year"))

View(hospdata_analysis)
View(aha_minimal)

# Check AHA coverage
cat("\n=== AHA DATA COVERAGE ===\n")
cat(paste("AHA year range:", min(aha_minimal$year), "to", max(aha_minimal$year), "\n"))
cat(paste("Unique hospitals in AHA:", n_distinct(aha_minimal$mcrnum), "\n"))

# ==============================================================================
# 9. CREATE OUTCOME VARIABLES
# ==============================================================================

# Calculate key hospital outcome measures --------------------------------------
# WHAT: Create standardized outcome variables for analysis 

hospdata <- hospdata %>%
  mutate(
    # Uncompensated care as proportion of total charges
    ucc2npr = uncomp_care / net_pat_rev,
    
    # Average cost per discharge
    cost_per_discharge = (cost_to_charge * tot_charges) / tot_discharges,
    
    # Medicaid cost-to-charge ratio
    mcaid_ccr = mcaid_cost / mcaid_charges,
    
    # Medicaid share of charges
    mcaid_prop = mcaid_charges / tot_charges,
    
    # Medicaid share of discharges
    mcaid_prop_discharges = mcaid_discharges / tot_discharges,

    mcaid_prop_dis_aha = mcddc / dctot, 

    # medicaid share of inpatient days 
    mcaid_prop_days = mcdipd / ipdtot,

    # total facility commercial share of discharges
    private_prop_discharges = thrtdc / dctot,

    # commercial share of inpatient days 
    private_prop_days = thrtip / ipdtot,

    # self pay share of discharges
    self_prop_discharges = sptdc / dctot,

    # self pay share of inpatient days
    self_prop_days = sptip / ipdtot,

    # Medicaid cost per medicaid discharge
    mcaid_cost_per_discharge = mcaid_cost / mcaid_discharges,
    
    # Medicare share of discharges
    mcare_prop_discharges = mcare_discharges / tot_discharges,
    
    # Combined Medicare + Medicaid share
    mm_prop_discharges = (mcaid_discharges + mcare_discharges) / tot_discharges,
    
    #  nonmedicaid, non medicare 
    nonm_prop_discharges = 1 - mm_prop_discharges,

    # number of non m discharges 
    nonm_discharges = tot_discharges - (mcaid_discharges + mcare_discharges),

    # operating margin 
    op_margin = (net_pat_rev - tot_operating_exp)/ tot_operating_exp,
  
    # Log variables for skewed outcomes
    log_npr = log(net_pat_rev),  # Add 1 to avoid log(0
    log_op = log(tot_operating_exp))

# ==============================================================================
# 10. SAVE FULL DATASET: no hospitals or years have been dropped yet. 
# ==============================================================================

write.csv(
  hospdata, 
  paste0(data_output_path, "hospdata_full.csv"),
  row.names = FALSE
)

cat("\n=== DATA SAVED SUCCESSFULLY ===\n")
cat("Output file: hospdata_full.csv\n")
cat(paste("Number of hospitals:", n_distinct(hospdata$mcrnum), "\n"))
cat(paste("Years covered:", min(hospdata$year), "to", max(hospdata$year), "\n"))
cat(paste("Total observations:", nrow(hospdata), "\n"))




