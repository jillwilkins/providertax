# ==============================================================================
# SUMMARY STATISTICS FOR HOSPDATA_ANALYSIS
# ==============================================================================

library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)  # Optional: for prettier tables

# Load data if not already in environment
# hospdata_analysis <- read.csv(paste0(data_output_path, "hospdata_analysis.csv"))

# ==============================================================================
# TABLE 1: OVERALL SUMMARY STATISTICS
# ==============================================================================

# Select key variables to summarize
summary_overall <- hospdata_analysis %>%
  summarise(
    # Sample size
    n_obs = n(),
    n_hospitals = n_distinct(mcrnum),
    n_states = n_distinct(state),
    
    # Hospital characteristics
    beds_mean = mean(beds, na.rm = TRUE),
    beds_sd = sd(beds, na.rm = TRUE),
    tot_discharges_mean = mean(tot_discharges, na.rm = TRUE),
    tot_discharges_sd = sd(tot_discharges, na.rm = TRUE),
    
    # Outcome variables
    ucc_prop_mean = mean(ucc_prop, na.rm = TRUE),
    ucc_prop_sd = sd(ucc_prop, na.rm = TRUE),
    
    mcaid_prop_discharges_mean = mean(mcaid_prop_discharges, na.rm = TRUE),
    mcaid_prop_discharges_sd = sd(mcaid_prop_discharges, na.rm = TRUE),
    
    net_pat_rev_mean = mean(net_pat_rev, na.rm = TRUE),
    net_pat_rev_sd = sd(net_pat_rev, na.rm = TRUE),

    tot_operating_exp_mean = mean(tot_operating_exp, na.rm = TRUE),
    tot_operating_exp_sd = sd(tot_operating_exp, na.rm = TRUE),
    
    cost_per_discharge_mean = mean(cost_per_discharge, na.rm = TRUE),
    cost_per_discharge_sd = sd(cost_per_discharge, na.rm = TRUE),
    
    op_margin_mean = mean(op_margin, na.rm = TRUE),
    op_margin_sd = sd(op_margin, na.rm = TRUE),
    
    # Treatment
    ever_treat_pct = mean(ever_treat, na.rm = TRUE) * 100,
    post_treat_pct = mean(post_treat, na.rm = TRUE) * 100
  )

# Reshape for nice table format
table1 <- summary_overall %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  mutate(
    stat_type = ifelse(grepl("_sd$", variable), "SD", "Mean/Count"),
    variable = gsub("_mean$|_sd$|_pct$", "", variable),
    value = round(value, 2)
  ) %>%
  pivot_wider(names_from = stat_type, values_from = value)

cat("\n=== TABLE 1: OVERALL SUMMARY STATISTICS ===\n")
print(table1, n = Inf)


# ==============================================================================
# TABLE 2: SUMMARY STATISTICS BY TREATMENT COHORT
# ==============================================================================

summary_by_cohort <- hospdata_analysis %>% #filter(year <= 2019) %>%  # Limit to pre-2020 for this table
  group_by(cohort) %>%
  summarise(
    n_obs = n(),
    n_hospitals = n_distinct(mcrnum),
    pct_hospitals = round(n_hospitals / n_distinct(hospdata_analysis$mcrnum) * 100, 1),
    n_states = n_distinct(state),
    pct_states = round(n_states / n_distinct(hospdata_analysis$state) * 100, 1),
    
    # Hospital characteristics
    beds_mean = mean(beds, na.rm = TRUE),
    tot_discharges_mean = mean(tot_discharges, na.rm = TRUE),
    
    # Outcome variables
    ucc_prop_mean = mean(ucc_prop, na.rm = TRUE),
    mcaid_prop_discharges_mean = mean(mcaid_prop_discharges, na.rm = TRUE),
    net_pat_rev_mean = mean(net_pat_rev, na.rm = TRUE),
    tot_operating_exp_mean = mean(tot_operating_exp, na.rm = TRUE),
    op_margin_mean = mean(op_margin, na.rm = TRUE),
    cost_per_discharge_mean = mean(cost_per_discharge, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric) & !c(n_obs, n_hospitals, n_states), ~round(., 2)))

cat("\n=== TABLE 2: SUMMARY STATISTICS BY TREATMENT COHORT ===\n")
print(summary_by_cohort, n = Inf)
View(summary_by_cohort)

View(hospdata_analysis %>% filter(cohort == 2005))
# ==============================================================================
# TABLE 3: PRE-TREATMENT VS POST-TREATMENT COMPARISON
# ==============================================================================

# For treated hospitals only, compare pre vs post periods
summary_pre_post <- hospdata_analysis %>%
  filter(ever_treat == 1) %>%  # Only treated hospitals
  mutate(period = ifelse(post_treat == 1, "Post-Treatment", "Pre-Treatment")) %>%
  group_by(period) %>%
  summarise(
    n_obs = n(),
    n_hospitals = n_distinct(mcrnum),
    
    # Hospital characteristics
    beds_mean = mean(beds, na.rm = TRUE),
    beds_sd = sd(beds, na.rm = TRUE),
    
    tot_discharges_mean = mean(tot_discharges, na.rm = TRUE),
    tot_discharges_sd = sd(tot_discharges, na.rm = TRUE),
    
    # Outcome variables
    ucc_prop_mean = mean(ucc_prop, na.rm = TRUE),
    ucc_prop_sd = sd(ucc_prop, na.rm = TRUE),
    
    mcaid_prop_discharges_mean = mean(mcaid_prop_discharges, na.rm = TRUE),
    mcaid_prop_discharges_sd = sd(mcaid_prop_discharges, na.rm = TRUE),
    
    net_pat_rev_mean = mean(net_pat_rev, na.rm = TRUE),
    net_pat_rev_sd = sd(net_pat_rev, na.rm = TRUE),

    tot_operating_exp_mean = mean(tot_operating_exp, na.rm = TRUE),
    tot_operating_exp_sd = sd(tot_operating_exp, na.rm = TRUE), 

    op_margin_mean = mean(op_margin, na.rm = TRUE), 
    op_margin_sd = sd(op_margin, na.rm = TRUE),
    
    cost_per_discharge_mean = mean(cost_per_discharge, na.rm = TRUE),
    cost_per_discharge_sd = sd(cost_per_discharge, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric) & !c(n_obs, n_hospitals), ~round(., 2)))

cat("\n=== TABLE 3: PRE-TREATMENT VS POST-TREATMENT (TREATED HOSPITALS ONLY) ===\n")
print(summary_pre_post, n = Inf)
View(summary_pre_post)

# ==============================================================================
# TABLE 4: TREATED VS NEVER-TREATED COMPARISON (PRE-TREATMENT PERIOD)
# ==============================================================================

# Compare treated and never-treated hospitals in pre-treatment years
# This helps assess baseline balance

summary_balance <- hospdata_analysis %>%
  filter(year <= 2010) %>%  # Pre-treatment for most states
  mutate(group = ifelse(ever_treat == 1, "Ever Treated", "Never Treated")) %>%
  group_by(group) %>%
  summarise(
    n_obs = n(),
    n_hospitals = n_distinct(mcrnum),
    n_states = n_distinct(state),
    
    # Hospital characteristics
    beds_mean = mean(beds, na.rm = TRUE),
    beds_sd = sd(beds, na.rm = TRUE),
    
    tot_discharges_mean = mean(tot_discharges, na.rm = TRUE),
    tot_discharges_sd = sd(tot_discharges, na.rm = TRUE),
    
    # Outcome variables
    ucc_prop_mean = mean(ucc_prop, na.rm = TRUE),
    ucc_prop_sd = sd(ucc_prop, na.rm = TRUE),
    
    mcaid_prop_discharges_mean = mean(mcaid_prop_discharges, na.rm = TRUE),
    mcaid_prop_discharges_sd = sd(mcaid_prop_discharges, na.rm = TRUE),
    
    net_pat_rev_mean = mean(net_pat_rev, na.rm = TRUE),
    net_pat_rev_sd = sd(net_pat_rev, na.rm = TRUE),
    
    cost_per_discharge_mean = mean(cost_per_discharge, na.rm = TRUE),
    cost_per_discharge_sd = sd(cost_per_discharge, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric) & !c(n_obs, n_hospitals, n_states), ~round(., 2)))

cat("\n=== TABLE 4: BALANCE CHECK - TREATED VS NEVER-TREATED (PRE-2011) ===\n")
print(summary_balance, n = Inf)
View(summary_balance)

# ==============================================================================
# TABLE 5: TREATMENT ROLLOUT SUMMARY
# ==============================================================================

treatment_rollout <- hospdata_analysis %>%
  distinct(state, cohort, gname) %>%
  count(cohort) %>%
  arrange(cohort) %>%
  rename(n_states = n)

cat("\n=== TABLE 5: TREATMENT ROLLOUT BY YEAR ===\n")
print(treatment_rollout)


# ==============================================================================
# TABLE 6: SAMPLE COMPOSITION BY YEAR
# ==============================================================================

sample_by_year <- hospdata_analysis %>%
  group_by(year) %>%
  summarise(
    n_hospitals = n_distinct(mcrnum),
    n_treated = sum(post_treat == 1),
    n_control = sum(post_treat == 0),
    pct_treated = round(mean(post_treat) * 100, 1)
  )

cat("\n=== TABLE 6: SAMPLE COMPOSITION BY YEAR ===\n")
print(sample_by_year, n = Inf)


# ==============================================================================
# EXPORT TABLES TO CSV (OPTIONAL)
# ==============================================================================

# Uncomment to save tables
# write.csv(table1, "table1_overall_summary.csv", row.names = FALSE)
# write.csv(summary_by_cohort, "table2_by_cohort.csv", row.names = FALSE)
# write.csv(summary_pre_post, "table3_pre_post.csv", row.names = FALSE)
# write.csv(summary_balance, "table4_balance.csv", row.names = FALSE)
# write.csv(treatment_rollout, "table5_rollout.csv", row.names = FALSE)
# write.csv(sample_by_year, "table6_by_year.csv", row.names = FALSE)


# ==============================================================================
# BONUS: CREATE A FORMATTED TABLE FOR PAPER (OPTIONAL)
# ==============================================================================

# This creates a nicely formatted table combining key statistics
# Useful for Table 1 in your paper

paper_table1 <- hospdata_analysis %>%
  mutate(
    group = case_when(
      ever_treat == 0 ~ "Never Treated",
      post_treat == 0 ~ "Treated (Pre)",
      post_treat == 1 ~ "Treated (Post)"
    )
  ) %>%
  group_by(group) %>%
  summarise(
    `N (hospitals)` = n_distinct(mcrnum),
    `N (obs)` = n(),
    `Beds` = sprintf("%.0f (%.0f)", mean(beds, na.rm = TRUE), sd(beds, na.rm = TRUE)),
    `Total Discharges` = sprintf("%.0f (%.0f)", 
                                  mean(tot_discharges, na.rm = TRUE), 
                                  sd(tot_discharges, na.rm = TRUE)),
    `Uncomp. Care Prop.` = sprintf("%.3f (%.3f)", 
                                    mean(ucc_prop, na.rm = TRUE), 
                                    sd(ucc_prop, na.rm = TRUE)),
    `Medicaid Prop. Disch.` = sprintf("%.3f (%.3f)", 
                                       mean(mcaid_prop_discharges, na.rm = TRUE), 
                                       sd(mcaid_prop_discharges, na.rm = TRUE)),
    `Net Patient Rev. ($M)` = sprintf("%.1f (%.1f)", 
                                       mean(net_pat_rev, na.rm = TRUE) / 1e6, 
                                       sd(net_pat_rev, na.rm = TRUE) / 1e6),
    `Cost per Discharge` = sprintf("%.0f (%.0f)", 
                                    mean(cost_per_discharge, na.rm = TRUE), 
                                    sd(cost_per_discharge, na.rm = TRUE))
  )

cat("\n=== PAPER-READY TABLE: SUMMARY STATISTICS ===\n")
cat("(Format: Mean (SD))\n\n")
print(paper_table1, n = Inf)
View(paper_table1)
# Export
# write.csv(paper_table1, "paper_table1_summary_stats.csv", row.names = FALSE)


cat("\n=== SUMMARY STATISTICS COMPLETE ===\n")
cat("All tables printed above. Uncomment export lines to save as CSV files.\n")



