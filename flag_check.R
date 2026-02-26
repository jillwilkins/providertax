# ==============================================================================
# CREATE FLAG: LARGE YEAR-OVER-YEAR CHANGES IN OPERATING MARGIN
# ==============================================================================

# when testing in the did, i dont know if this really helped with the large standard errors

hospdata_analysis <- hospdata_analysis %>%
  arrange(mcrnum, year) %>%
  group_by(mcrnum) %>%
  mutate(
    # Previous year's operating margin
    op_exp_flag = lag(tot_operating_exp),
    
    # Absolute change from prior year
    op_exp_change = tot_operating_exp - op_exp_flag,
    
    # Percent change from prior year (avoiding division by zero/small numbers)
    op_exp_pct_change = case_when(
      is.na(op_exp_flag) ~ NA_real_,
      abs(op_exp_flag) < 0.01 ~ NA_real_,  # Don't calculate if denominator near zero
      TRUE ~ (tot_operating_exp - op_exp_flag) / abs(op_exp_flag)
    ),
    
    # Flag: 150%+ change from prior year
    flag_op_exp_jump = case_when(
      is.na(op_exp_pct_change) ~ 0,
      abs(op_exp_pct_change) >= 1.50 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  ungroup()

# Check flagged observations
cat("\n=== FLAGGED OBSERVATIONS (150%+ CHANGE) ===\n")
cat(paste("Total observations:", nrow(hospdata_analysis), "\n"))
cat(paste("Flagged observations:", sum(hospdata_analysis$flag_op_exp_jump, na.rm = TRUE), "\n"))
cat(paste("Unique hospitals flagged:", n_distinct(hospdata_analysis$mcrnum[hospdata_analysis$flag_op_exp_jump == 1]), "\n"))
cat(paste("Percent flagged:", 
          round(mean(hospdata_analysis$flag_op_exp_jump, na.rm = TRUE) * 100, 2), "%\n"))

# Show examples
cat("\n=== EXAMPLES OF LARGE JUMPS ===\n")
hospdata_analysis %>%
  filter(flag_op_exp_jump == 1) %>%
  select(mcrnum, state, year, tot_operating_exp, op_exp_flag, op_exp_change, op_exp_pct_change) %>%
  arrange(desc(abs(op_exp_pct_change))) %>%
  head(20) %>%
  mutate(across(where(is.numeric) & !c(mcrnum, year), ~round(., 3))) %>%
  print()

# By cohort
cat("\n=== FLAGGED OBSERVATIONS BY COHORT ===\n")
hospdata_analysis %>%
  group_by(cohort) %>%
  summarise(
    n_obs = n(),
    n_flagged = sum(flag_op_exp_jump, na.rm = TRUE),
    pct_flagged = round(mean(flag_op_exp_jump, na.rm = TRUE) * 100, 2)
  ) %>%
  print(n = Inf)

# 77 hospitals report a percentage change in total_operating_exp of 150% or more. Thats unlikely. 

View(hospdata_clean %>% filter(flag_op_exp_jump == 1) %>% select(mcrnum, state, year, tot_operating_exp, op_exp_flag, op_exp_change, op_exp_pct_change))
