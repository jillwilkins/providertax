# ==============================================================================
# SUMMARY STATISTICS: MANAGED CARE VS. NON-MANAGED CARE STATES
# States without Medicaid managed care: WY, VT, ME, CT, AK
# ==============================================================================

library(dplyr)
library(tidyr)

# Load data if not already in environment
hospdata_analysis <- read.csv(paste0(data_output_path, "hospdata_analysis.csv"))

no_mco_states <- c("WY", "VT", "ME", "CT", "AK")

hospdata_analysis <- hospdata_analysis %>%
  mutate(mco_group = ifelse(state %in% no_mco_states, "No Managed Care", "Managed Care"))

# Confirm the grouping looks right
hospdata_analysis %>%
  distinct(state, mco_group) %>%
  arrange(mco_group, state) %>%
  print()

# ==============================================================================
# TABLE: SUMMARY STATISTICS BY MANAGED CARE STATUS
# ==============================================================================

summary_by_mco <- hospdata_analysis %>%
  group_by(mco_group) %>%
  summarise(
    n_obs = n(),
    n_hospitals = n_distinct(mcrnum),
    n_states = n_distinct(state),

    # Hospital characteristics
    beds_mean = mean(beds, na.rm = TRUE),
    beds_sd = sd(beds, na.rm = TRUE),
    tot_discharges_mean = mean(tot_discharges, na.rm = TRUE),
    tot_discharges_sd = sd(tot_discharges, na.rm = TRUE),
    pct_rural = mean(rurlhos, na.rm = TRUE) * 100,

    # Medicaid exposure
    mcaid_prop_discharges_mean = mean(mcaid_prop_discharges, na.rm = TRUE),
    mcaid_prop_discharges_sd = sd(mcaid_prop_discharges, na.rm = TRUE),
    mcaid_prop_mean = mean(mcaid_prop, na.rm = TRUE),
    mcaid_prop_sd = sd(mcaid_prop, na.rm = TRUE),
    mcaid_charges_mean = mean(mcaid_charges, na.rm = TRUE),
    mcaid_charges_sd = sd(mcaid_charges, na.rm = TRUE),

    # Financials
    net_pat_rev_mean = mean(net_pat_rev, na.rm = TRUE),
    net_pat_rev_sd = sd(net_pat_rev, na.rm = TRUE),
    tot_operating_exp_mean = mean(tot_operating_exp, na.rm = TRUE),
    tot_operating_exp_sd = sd(tot_operating_exp, na.rm = TRUE),
    cost_per_discharge_mean = mean(cost_per_discharge, na.rm = TRUE),
    cost_per_discharge_sd = sd(cost_per_discharge, na.rm = TRUE),
    op_margin_mean = mean(op_margin, na.rm = TRUE),
    op_margin_sd = sd(op_margin, na.rm = TRUE),

    # Uncompensated care
    tot_uncomp_care_cost_mean = mean(tot_uncomp_care_cost, na.rm = TRUE),
    tot_uncomp_care_cost_sd = sd(tot_uncomp_care_cost, na.rm = TRUE),

    # State-level Medicaid context
    medicaid_enrollment_mean = mean(medicaid_enrollment, na.rm = TRUE),
    eligibility_mean = mean(eligibility, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric) & !c(n_obs, n_hospitals, n_states), ~round(., 2)))

cat("\n=== SUMMARY STATISTICS: MANAGED CARE VS. NON-MANAGED CARE STATES ===\n")
print(summary_by_mco)
View(summary_by_mco)

# ==============================================================================
# T-TESTS: MANAGED CARE VS. NON-MANAGED CARE (MEAN DIFFERENCES)
# ==============================================================================

compare_vars <- c(
  "beds", "tot_discharges", "rurlhos",
  "mcaid_prop_discharges", "mcaid_prop", "mcaid_charges",
  "net_pat_rev", "tot_operating_exp", "cost_per_discharge", "op_margin",
  "tot_uncomp_care_cost", "medicaid_enrollment", "eligibility"
)

ttest_results <- lapply(compare_vars, function(v) {
  t <- t.test(hospdata_analysis[[v]] ~ hospdata_analysis$mco_group)
  tibble(
    variable = v,
    mean_managed_care = t$estimate[["mean in group Managed Care"]],
    mean_no_managed_care = t$estimate[["mean in group No Managed Care"]],
    diff = t$estimate[["mean in group Managed Care"]] - t$estimate[["mean in group No Managed Care"]],
    p_value = t$p.value
  )
}) %>% bind_rows()

cat("\n=== T-TESTS: MEAN DIFFERENCES BY MANAGED CARE STATUS ===\n")
print(ttest_results)
View(ttest_results)
