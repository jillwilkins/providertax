# ==============================================================================
# HCRIS Data Preparation Script - CLEANED VERSION
# Purpose: Prepare hospital-level panel data for staggered DiD analysis
# ==============================================================================

# Load required packages -------------------------------------------------------
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)

# Set file paths (modify these for your system) -------------------------------
# TIP: Use here::here() package for more portable paths
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
# 3. LOAD AND PROCESS FMAP DATA
# ==============================================================================

fmap <- read.csv(
  paste0(data_input_path, "fmaps_kff.csv"), 
  skip = 2
)

# Clean FMAP data structure ----------------------------------------------------
fmap <- fmap %>%
  select(-Footnotes) %>%
  slice(2:52) %>%
  rename(statename = Location) %>%
  mutate(
    state = ifelse(
      statename == "District of Columbia",
      "DC",
      state.abb[match(statename, state.name)]
    )
  )

# Rename FMAP columns ----------------------------------------------------------
# WHAT: Extract year from column names, create naming like "fmap_2004", "m_2004" format

fmap <- fmap %>%
  rename_with(~ case_when(
    str_detect(.x, "FMAP") ~ paste0("fmap_", str_extract(.x, "\\d{4}")),
    str_detect(.x, "Multiplier") ~ paste0("m_", str_extract(.x, "\\d{4}")),
    TRUE ~ .x
  ))

# Reshape FMAP from wide to long -----------------------------------------------
# WHAT: Convert from one row per state to one row per state-year
# WHY: Easier to merge with panel data structure

# First, convert to numeric
fmap <- fmap %>%
  mutate(across(
    matches("^(fmap|m)_\\d{4}$"), 
    ~ as.numeric(as.character(.))
  ))

# Then pivot to long format
fmap <- fmap %>%
  pivot_longer(
    cols = -state,
    names_to = c(".value", "year"),
    names_pattern = "(fmap|m)_(\\d{4})"  # Separate prefix from year
  ) %>%
  rename(multiplier = m) %>%
  mutate(year = as.integer(year)) %>%
  filter(!is.na(fmap) & !is.na(multiplier))  # Remove rows from title lines


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


# ==============================================================================
# 5. MERGE ALL DATA SOURCES
# ==============================================================================

# Create master state-year dataset ---------------------------------------------
fmap_tax <- fmap %>%
  left_join(
    tax %>% select(state, firsttax),
    by = "state"
  ) %>%
  left_join(
    tax_totals,
    by = "year"
  )

# Merge with HCRIS hospital data -----------------------------------------------
# RESULT: Hospital-year panel with state policy variables

hospdata <- hcris %>%
  left_join(
    fmap_tax %>% select(state, year, fmap, multiplier, firsttax, totaltax),
    by = c("state", "year")
  ) %>%
  rename(mcrnum = provider_number)  # Rename for consistency


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
    # - Year of adoption for treated states
    # - 0 for never-treated states (C&S convention)
    gname = case_when(
      is.na(firsttax_num) ~ 0,           # Never treated = 0
      TRUE ~ firsttax_num                # Otherwise = adoption year
    ),
    
    # 2. COHORT: Character version for readability
    # - "Never" for never-treated
    # - Year as character for treated states
    cohort = case_when(
      is.na(firsttax_num) ~ "Never",
      TRUE ~ as.character(firsttax_num)
    ),
    
    # 3. EVER_TREAT: Simple indicator for ever being treated
    # - 1 if state ever adopts tax
    # - 0 if never adopts
    ever_treat = case_when(
      is.na(firsttax_num) ~ 0,
      TRUE ~ 1
    ),
    
    # 4. POST_TREAT: Is treatment currently active?
    # - 1 if current year >= adoption year
    # - 0 otherwise
    post_treat = case_when(
      is.na(firsttax_num) ~ 0,           # Never treated = always 0
      year >= firsttax_num ~ 1,          # After adoption = 1
      TRUE ~ 0                           # Before adoption = 0
    ),
    
    # 5. TIME_TO_TREAT: Years relative to treatment adoption
    # - Negative = years before treatment
    # - 0 = year of treatment
    # - Positive = years after treatment
    # - NA = never treated (for plotting/event studies)
    time_to_treat = case_when(
      is.na(firsttax_num) ~ NA_real_,    # Never treated = NA
      TRUE ~ year - firsttax_num         # Otherwise = year - adoption year
    )
  )


# ==============================================================================
# 7. CREATE OUTCOME VARIABLES
# ==============================================================================

# Calculate key hospital outcome measures --------------------------------------
# WHAT: Create standardized outcome variables for analysis 

hospdata <- hospdata %>%
  mutate(
    # Uncompensated care as proportion of total charges
    ucc_prop = tot_uncomp_care_charges / tot_charges,
    
    # Average cost per discharge
    cost_per_discharge = (cost_to_charge * tot_charges) / tot_discharges,
    
    # Medicaid cost-to-charge ratio
    mcaid_ccr = mcaid_cost / mcaid_charges,
    
    # Medicaid share of charges
    mcaid_prop = mcaid_charges / tot_charges,
    
    # Medicaid share of discharges
    mcaid_prop_discharges = mcaid_discharges / tot_discharges,
    
    # Medicare share of discharges
    mcare_prop_discharges = mcare_discharges / tot_discharges,
    
    # Combined Medicare + Medicaid share
    mm_prop_discharges = (mcaid_discharges + mcare_discharges) / tot_discharges,
    
    # Private insurance share (residual)
    private_prop_discharges = 1 - mm_prop_discharges
  )

View(hospdata)  # Check the final dataset structure and variables
# ==============================================================================
# 8. VERIFY TREATMENT GROUPS
# ==============================================================================

# Check distribution of treatment cohorts --------------------------------------
cat("\n=== TREATMENT COHORT SUMMARY ===\n")

hospdata %>%
  distinct(state, cohort) %>%
  count(cohort) %>%
  arrange(cohort) %>%
  print()

# as of 2/13, there are 51 "Never" which is a result of the years prior to 2004. 

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
# 9. JOIN AHA: join necessary var from aha. 
# ==============================================================================
# Load AHA data
aha <- read_csv(paste0(data_input_path, "AHAdata_20052023.csv"))

#cahnge column names to lowercase
colnames(aha) <- tolower(colnames(aha))

# Select only what you need from AHA and prepare for merge
aha_minimal <- aha %>%
  select(mcrnum, year, cntrl, serv) 

# Convert aha_minimal mcrnum to match hospdata (integer)
aha_minimal <- aha_minimal %>%
  mutate(mcrnum = as.integer(mcrnum))

# Merge on both mcrnum and year
hospdata <- hospdata %>%
  left_join(aha_minimal, by = c("mcrnum", "year"))

# Check AHA coverage
cat("\n=== AHA DATA COVERAGE ===\n")
cat(paste("AHA year range:", min(aha_minimal$year), "to", max(aha_minimal$year), "\n"))
cat(paste("Unique hospitals in AHA:", n_distinct(aha_minimal$mcrnum), "\n"))

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


