# ==============================================================================
# STACKED DIFFERENCE-IN-DIFFERENCES
# Following "A guide on data analysis: Chapter 30"
# ==============================================================================

# recall that the follwoing are already defined 
# cohort = year that the state of hospital i was first treated (2004, always treated)
# gname = numerical version of cohort 
# post_treat = 1 if year >= cohort, 0 otherwise
# ever_treat = 1 if hospital is ever treated, 0 otherwise
# time_to_treat = year - cohort (relative year to treatment)

library(fixest)
library(dplyr)
library(tidyverse)
install.packages("fastDummies")
library(fastDummies)

# ------------------------------------------------------------------------------
#1. get valid treatment cohorts
window <- 5  # Event window (±5 years)

# Get treated cohorts (exclude never-treated and always treated, gname == 0)
cohorts <- hospdata_analysis %>%
  filter(gname != 0) %>%  # Exclude never-treated
  distinct(gname) %>%
  pull() %>%
  sort()

cat("\n=== TREATMENT COHORTS ===\n")
cat("Valid cohorts:", cohorts, "\n")

# ------------------------------------------------------------------------------
#2. function to generate sub experiment data 

getdata <- function(j, window) {
  hospdata_analysis %>%
    filter(
      gname == j |                    # treated units in cohort j
      gname == 0 |                    # never treated, always treated
      gname > j + window              # controls not treated soon after
    ) %>%
    filter(
      year >= j - window &
      year <= j + window              # event window bounds
    ) %>%
    mutate(df = j)                    # sub-experiment indicator
}

# ------------------------------------------------------------------------------
#3. function to generate sub experiment data 
stacked_data <- map_df(cohorts, ~ getdata(., window = window)) %>%
  mutate(
    rel_year = if_else(df == gname, time_to_treat, NA_real_)    # relative year (time until treatment)
  )

# check balance between treated and control in each sub experiment
cat("\n=== STACKED DATA BALANCE ===\n")
stacked_data %>%
  group_by(df) %>%
  summarise(
    cohort_year = first(df),
    n_obs = n(),
    n_hospitals = n_distinct(mcrnum),
    n_treated = n_distinct(mcrnum[gname == df]),
    n_control = n_distinct(mcrnum[gname != df]),
    .groups = "drop"
  ) %>%
  print()

# ------------------------------------------------------------------------------
#4. create relative time dummies  
stacked_data <- map_df(cohorts, ~ getdata(., window = window)) %>%
  mutate(
    rel_year = if_else(df == gname, time_to_treat, NA_real_)
  )

stacked_data <- map_df(cohorts, ~ getdata(., window = window)) %>%
  mutate(
    rel_year = year - df,  # relative year to sub-experiment cohort df
    treated = ifelse(gname == df, 1, 0) #treatment indicator in that sub experiment
  )

# Now create dummies
stacked_data <- stacked_data %>%
  dummy_cols("rel_year", ignore_na = TRUE) %>%
  mutate(across(starts_with("rel_year_"), ~ replace_na(., 0)))

# ------------------------------------------------------------------------------
# 5. estimate stacked DiD with feols

stacked_result <- feols(
  mcare_prop_discharges ~ i(rel_year, treated, ref = -1) | mcrnum^df + year^df,
  data = stacked_data,
  cluster = ~state
)

summary(stacked_result)
iplot(stacked_result, main = "Stacked DiD Event Study")


