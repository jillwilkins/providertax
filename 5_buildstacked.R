# ==============================================================================
# Stacked DiD Builder 
# This script will create a balanced stacked did data set 
# Following weights and structure from Cengiz et al. 2019 and Wing et al. 2024
# ==============================================================================
library(fixest)
library(purrr)
library(tidyverse)

# load data in if not done already 
hospdata_analysis <- read.csv(paste0(data_output_path, "hospdata_analysis.csv"))

# normalized var per bed
hospdata_analysis <- hospdata_analysis %>%
  mutate(
        mcaid_enroll_bed = medicaid_enrollment / pre_beds_avg, 
        )

# ------------------------------------------------------------------------------
# 0. Parameters
# ------------------------------------------------------------------------------
window = 4      # event window
yr_min   <- 1999    
yr_max   <- 2022     

# ------------------------------------------------------------------------------
# 1. Base data — exclude always-treated, keep never-treated
# ------------------------------------------------------------------------------
hospdata_stack <- hospdata_analysis %>%
  filter(cohort != "Always (2004)")   # never-treated (cohort == "Never (0)") stays

# ------------------------------------------------------------------------------
# 2. Valid cohorts 
#    Cohort j is valid if data covers j - window through j + window
#    With 1999-2022 and window=4: valid cohorts are 2004 through 2017
# ------------------------------------------------------------------------------
cohorts <- hospdata_stack %>%
  filter(gname != 0) %>%                        # treated units only
  distinct(gname) %>%
  pull() %>%
  sort() %>%
  keep(~ (.x - window) >= yr_min &              # enough pre-period data
          (.x + window) <= yr_max)              # enough post-period data

cat("\n=== TREATMENT COHORTS AFTER IC1 ===\n")
cat("Valid cohorts:", cohorts, "\n")

# ------------------------------------------------------------------------------
# 3. Sub-experiment builder — single clean function
# ------------------------------------------------------------------------------
getdata <- function(j, pre_window, post_window) {
  
  n_required <- 2 * window + 1    # hospitals must appear at all event times
  
  # Step 1: pull treated + clean controls for this sub-experiment
  raw <- hospdata_stack %>%
    filter(
      gname == j              |    # treated cohort j
      gname == 0              |    # never-treated controls (gname == 0)
      gname > j + window           # not-yet-treated clean controls
    ) %>%
    filter(
      year >= j - window,          # pre-period: pre_window years before adoption
      year <= j + window           # post-period: post_window years after adoption
    ) %>%
    mutate(
      df       = j,
      rel_year = year - j,                   # event time; 0 = adoption year
      treated  = as.integer(gname == j)      # 1 if treated in THIS sub-experiment
    )
  
  # Step 2: enforce strongly balanced panel
  # Keep only hospitals present at ALL 2*pre_window+1 event times
  keep_hosps <- raw %>%
    group_by(mcrnum) %>%
    summarise(n_yrs = n_distinct(rel_year), .groups = "drop") %>%
    filter(n_yrs == n_required) %>%
    pull(mcrnum)
  
  out <- raw %>% filter(mcrnum %in% keep_hosps)
  
  # Reporting
  cat(sprintf(
    "Sub-exp %d | raw hosps: %4d | balanced: %4d | treated: %3d | control: %4d\n",
    j,
    n_distinct(raw$mcrnum),
    length(keep_hosps),
    out %>% filter(treated == 1) %>% distinct(mcrnum) %>% nrow(),
    out %>% filter(treated == 0) %>% distinct(mcrnum) %>% nrow()
  ))
  
  out
}

# ------------------------------------------------------------------------------
# 4. Build stacked data — single map_df call
# ------------------------------------------------------------------------------
cat("\n=== BUILDING SUB-EXPERIMENTS ===\n")

stacked_data <- map_df(cohorts, ~ getdata(., ))

cat("\n=== STACK SUMMARY ===\n")
cat("Total rows     :", nrow(stacked_data),              "\n")
cat("Total hospitals:", n_distinct(stacked_data$mcrnum), "\n")
cat("Sub-experiments:", n_distinct(stacked_data$df),     "\n")

# ------------------------------------------------------------------------------
# 5. Balance check
# ------------------------------------------------------------------------------
cat("\n=== STACKED DATA BALANCE ===\n")
stacked_data %>%
  group_by(df) %>%
  summarise(
    n_obs       = n(),
    n_hospitals = n_distinct(mcrnum),
    n_treated   = n_distinct(mcrnum[treated == 1]),
    n_control   = n_distinct(mcrnum[treated == 0]),
    .groups     = "drop"
  ) %>%
  print()

cat("\n=== TOTAL SAMPLE ===\n")
hospdata_analysis %>%
  summarise(
    n_obs            = n(),
    n_hospitals      = n_distinct(mcrnum),
    #n_treated        = n_distinct(mcrnum[treate == 1]),
    #n_control        = n_distinct(mcrnum[treated == 0]),
    n_states         = n_distinct(state),
    year_min         = min(year),
    year_max         = max(year)
  ) %>%
  print()

# Hospitals in hospdata_analysis but not in stacked_data
missing_hosps <- hospdata_analysis %>%
  filter(!mcrnum %in% unique(stacked_data$mcrnum)) %>%
  distinct(mcrnum, state, year) %>%
  group_by(mcrnum) %>%
  summarise(
    state    = first(state),
    min_year = min(year),
    max_year = max(year),
    n_years  = n_distinct(year),
    .groups  = "drop"
  )

cat("N missing hospitals:", nrow(missing_hosps), "\n")
summary(missing_hosps$n_years)  # are they short-lived?
table(missing_hosps$state)      # concentrated in certain states?

# ------------------------------------------------------------------------------
# 6. Q-weights — computed on the same stacked_data
# ------------------------------------------------------------------------------
unit_counts <- stacked_data %>%
  distinct(mcrnum, df, treated) %>%
  group_by(df) %>%
  summarise(
    n_treat_a = sum(treated == 1),
    n_ctrl_a  = sum(treated == 0),
    .groups   = "drop"
  )

n_treat_total <- sum(unit_counts$n_treat_a)
n_ctrl_total  <- sum(unit_counts$n_ctrl_a)

stacked_data <- stacked_data %>%
  left_join(unit_counts, by = "df") %>%
  mutate(
    treat_share = n_treat_a / n_treat_total,
    ctrl_share  = n_ctrl_a  / n_ctrl_total,
    Q_weight    = if_else(treated == 1, 1, treat_share / ctrl_share)
  )

cat("\n=== Q-WEIGHT INPUTS ===\n")
unit_counts %>%
  mutate(
    treat_share = n_treat_a / n_treat_total,
    ctrl_share  = n_ctrl_a  / n_ctrl_total,
    Q           = treat_share / ctrl_share
  ) %>%
  print()

# ------------------------------------------------------------------------------
# 7. Q-weight balance diagnostic
# ------------------------------------------------------------------------------
cat("\n=== Q-WEIGHT BALANCE DIAGNOSTIC ===\n")
diag <- stacked_data %>%
  filter(treated == 1) %>%
  group_by(rel_year) %>%
  summarise(
    n_cohorts       = n_distinct(df),
    n_treated_hosps = n_distinct(mcrnum),
    total_Q_weight  = sum(Q_weight),
    .groups         = "drop"
  ) %>%
  arrange(rel_year)

print(diag)

q_range <- range(diag$total_Q_weight)
cat(sprintf(
  "\ntotal_Q_weight range: %.4f to %.4f (diff = %.6f)\n",
  q_range[1], q_range[2], diff(q_range)
))

if (diff(q_range) < 0.001) {
  cat("✓ Balanced — composition stable across all event times\n")
} else {
  cat("✗ Still unbalanced — check which sub-experiments have partial windows\n")
}


# ------------------------------------------------------------------------------
# 8. Derived variables used by downstream scripts
#    in_sys        -> event_studies/scope_calc.R
#    pre_mcaid_dis -> diagnostics/investigate_mcaidrev.R
# ------------------------------------------------------------------------------
stacked_data <- stacked_data %>%
  mutate(in_sys = case_when(sysid > 0 ~ "1", TRUE ~ "0"))

stacked_data <- stacked_data %>%
  mutate(pre_mcaid_dis = mean(mcaid_discharges[rel_year < 0], na.rm = TRUE), .by = mcrnum)

# The stacked data set has been built. Analysis lives in separate scripts.
