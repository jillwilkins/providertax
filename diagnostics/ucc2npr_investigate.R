# ==============================================================================
# Run stacked DiD for ucc2npr ONLY with custom data filter
# ==============================================================================
summary(stacked_data$ucc2npr)

# Filter your data first
data_ucc2npr <- stacked_data %>%
  filter(
    # Add your filter conditions here, e.g.:
    ucc2npr > 0,  # Only include rows where ucc2npr is greater than 0
    ucc2npr < .3  # Remove major outliers
    # year >= 2010,
    # !is.na(ucc2npr)
  )

# Run the model
result_ucc2npr <- run_stacked_did(
  outcome_var   = "ucc2npr",
  outcome_label = "Uncompensated Care Charges as a Share of Net Patient Revenue",
  data          = data_ucc2npr,        # <-- pass filtered data here
  controls      = "exp_status + median_income_pre",
  # y_scale       = "percent",           # or "comma" depending on how ucc2npr is scaled
  filename      = "events_for_paper/ucc2npr.png",
  width         = 10,
  height        = 8
)

# Access results
summary(result_ucc2npr$model)
result_ucc2npr$coef_data
result_ucc2npr$plot

# ==============================================================================
# Overall ATT (post-treatment average)
# ==============================================================================

att_ucc2npr <- result_ucc2npr$coef_data %>%
  filter(rel_year >= 0) %>%
  summarise(
    att = mean(coef),
    att_se = mean(se)
  ) %>%
  mutate(
    ci_lower = att - 1.96 * att_se,
    ci_upper = att + 1.96 * att_se
  )

cat("Overall ATT (simple average of post-treatment coefficients):\n")
print(att_ucc2npr)

# ==============================================================================
# Baseline mean of ucc2npr (pre-treatment, control group)
# ==============================================================================

baseline_mean <- data_ucc2npr %>%        # or data_ucc2npr if you filtered
  filter(rel_year < 0, treated == 1) %>%
  summarise(
    baseline_mean = mean(ucc2npr, na.rm = TRUE),
    baseline_sd   = sd(ucc2npr,   na.rm = TRUE),
    n             = n()
  )

cat("\nBaseline Mean (pre-treatment, control group):\n")
print(baseline_mean)

# ATT as % of baseline
cat("\nATT as % of baseline mean:\n")
cat(round((att_ucc2npr$att / baseline_mean$baseline_mean) * 100, 2), "%\n")




# PER BED for consistency 
result_uncomp_bed <- run_stacked_did(
  outcome_var   = "uncomp_bed",
  outcome_label = "Uncompensated Care Charges Per Bed",
  data          = stacked_data,     # rename this to data_filtered or whatever you're using
  controls      = "exp_status + median_income_pre",
  y_scale       = "dollar",         # or "comma" depending on units
  filename      = "events_for_paper/uncomp_bed.png",
  width         = 10,
  height        = 8
)

# ATT
att_uncomp_bed <- result_uncomp_bed$coef_data %>%
  filter(rel_year >= 0) %>%
  summarise(
    att = mean(coef),
    att_se = mean(se)
  ) %>%
  mutate(
    ci_lower = att - 1.96 * att_se,
    ci_upper = att + 1.96 * att_se
  )

print(att_uncomp_bed)

# Baseline mean
stacked_data %>%
  filter(rel_year < 0, treated == 0) %>%
  summarise(
    baseline_mean = mean(uncomp_bed, na.rm = TRUE),
    baseline_sd   = sd(uncomp_bed,  na.rm = TRUE)
  )

# att as percentage of baseline
