# ==============================================================================
# MARGIN_UPDATED: operating margin on a net-patient-revenue base
#   margin_updated = (net_pat_rev - tot_operating_exp) / net_pat_rev
# NOW DEFINED IN 1_data.R AS op_margin (the old opex-base op_margin was
# replaced 2026-11-17). Rows with net_pat_rev <= 0 are set to NA (the ratio is undefined or
# sign-flipped there), as are rows with |margin_updated| > 1. A margin
# can't exceed 1 with positive expenses, and values below -1 (expenses
# more than twice revenue; min ~ -40) are treated as reporting errors and
# dropped rather than capped. ~0.4% of hospital-years in hospdata_analysis.
#
# Does NOT rebuild the stacked sample. Requires in the environment:
#   - stacked_data      (5_buildstacked.R)
#   - run_stacked_did() (6_events_paper.R)
# Then:
#   1. Event study, same function/spec as the 6_events_paper.R outcomes
#      -> events_for_paper/margin_updated.png
#   2. Pooled ATT, same spec as results_tables/main_results_table.R
#      (single treated:post coefficient, leads -4..-2 kept, Q-weighted,
#      hospital x cohort and year x cohort FE, state-clustered)
# ==============================================================================

library(dplyr)
library(fixest)

# As of 2026-11-17 the updated formula IS op_margin, defined once in
# 1_data.R. margin_updated is kept as a name so downstream scripts
# (7_quartiles.R) keep working; it is the same column.
stacked_data <- stacked_data %>%
  mutate(
    margin_updated = op_margin,
    post = as.integer(rel_year >= 0)
  )

cat("\nmargin_updated (= op_margin) distribution (stacked rows):\n")
print(summary(stacked_data$margin_updated))

# ------------------------------------------------------------------------------
# 1. Event study
# ------------------------------------------------------------------------------
res_margin_updated <- run_stacked_did(
  outcome_var   = "margin_updated",
  outcome_label = "Operating Margin (NPR base)",
  filename      = "events_for_paper/margin_updated.png",
  width         = 10,
  height        = 8
)

# ------------------------------------------------------------------------------
# 2. Pooled ATT (main results spec)
# ------------------------------------------------------------------------------
pooled_margin_updated <- feols(
  margin_updated ~ treated:post + i(rel_year, treated, ref = -1, keep = -4:-2) +
    exp_status + median_income_pre | mcrnum^df + year^df,
  data = stacked_data, weights = ~Q_weight, cluster = ~state
)

pre_mean <- stacked_data %>%
  filter(treated == 1, rel_year < 0) %>%
  summarise(m = mean(margin_updated, na.rm = TRUE)) %>%
  pull(m)

cat("\n=== margin_updated: pooled ATT (main results spec) ===\n")
cat(sprintf("Pre-treatment mean (treated): %.4f\n", pre_mean))
cat(sprintf("ATT: %.4f (SE %.4f), p = %.3f\n",
            coef(pooled_margin_updated)["treated:post"],
            se(pooled_margin_updated)["treated:post"],
            pvalue(pooled_margin_updated)["treated:post"]))
