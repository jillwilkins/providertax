###########################################################
# Stacked DiD by Quartile of Pre-Treatment Operating Margin
#
# Complements canay_qr.R: Canay's quantile regression shows how the
# tax's effect varies across the OUTCOME's own conditional
# distribution. This asks a different question -- does the ATT vary
# by a hospital's baseline financial health? -- by literally
# subsetting the population into quartiles of PRE-TREATMENT
# (rel_year < 0) operating margin and running the existing stacked
# event-study regression (run_stacked_did(), 6_events_paper.R)
# separately on each quartile. Four independent regressions per
# outcome, not one pooled quartile-interacted model.
#
# Same 6 outcomes as canay_qr.R, so the two sets of results are
# directly comparable outcome-for-outcome:
#   op_bed, npr_bed, mcaid_prop_discharges, uncomp_bed_w95,
#   mcaid_dis_bed, op_margin
#
# Requires stacked_data (5_buildstacked.R) AND run_stacked_did()
# (6_events_paper.R) already defined in the environment.
############################################################

library(dplyr)

# ------------------------------------------------------------------
# pre_op_margin: same convention as pre_mcaid_dis in 5_buildstacked.R
# -- each hospital's own mean op_margin across every rel_year < 0 row
# it contributes, pooled across every sub-experiment (df) it appears
# in as either the treated cohort or a control.
# ------------------------------------------------------------------
stacked_data <- stacked_data %>%
  mutate(pre_op_margin = mean(op_margin[rel_year < 0], na.rm = TRUE), .by = mcrnum)

# ------------------------------------------------------------------
# Quartile cutoffs computed on DISTINCT hospitals, not stacked rows.
# A hospital can appear many times over (once per cohort it served as
# a control for) -- computing quartiles on the raw stacked rows would
# let frequently-reused control hospitals dominate the cutoffs.
# Cutoffs are computed across ALL hospitals (treated + control); a
# hospital's quartile is fixed across every sub-experiment it's part of.
# Q1 = lowest pre-tax operating margin, Q4 = highest.
# ------------------------------------------------------------------
hosp_quartiles <- stacked_data %>%
  distinct(mcrnum, pre_op_margin) %>%
  filter(!is.na(pre_op_margin)) %>%
  mutate(opmargin_quartile = ntile(pre_op_margin, 4))

cat("Hospitals per quartile (Q1 = lowest pre-tax op_margin):\n")
print(table(hosp_quartiles$opmargin_quartile))

stacked_data <- stacked_data %>%
  select(-any_of("opmargin_quartile")) %>%
  left_join(hosp_quartiles %>% select(mcrnum, opmargin_quartile), by = "mcrnum")

# ------------------------------------------------------------------
# Outcomes -- same set and same uncomp_bed_w95 construction as
# canay_qr.R / longdiff.R, so all three approaches line up.
# ------------------------------------------------------------------
if (!"uncomp_bed_w95" %in% names(stacked_data)) {
  winsorize_var <- function(x, probs = c(0.01, 0.95)) {
    bounds <- quantile(x, probs = probs, na.rm = TRUE)
    pmin(pmax(x, bounds[1]), bounds[2])
  }
  stacked_data <- stacked_data %>% mutate(uncomp_bed_w95 = winsorize_var(uncomp_bed, c(0.01, 0.95)))
}

if (!"mcaid_dis_bed" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(mcaid_dis_bed = mcaid_discharges / pre_beds_avg)
}

quartile_outcomes <- c("op_bed", "npr_bed", "mcaid_prop_discharges",
                        "uncomp_bed_w95", "mcaid_dis_bed", "op_margin")

# ------------------------------------------------------------------
# Run run_stacked_did() separately on each quartile's subset. A given
# sub-experiment (df) can end up with only treated or only control
# hospitals once restricted to one quartile -- feols handles this by
# dropping the unidentified fixed-effect groups on its own, which is
# expected here, not an error.
# ------------------------------------------------------------------
dir.create("opmargin_quartile_stackdid", showWarnings = FALSE)

quartile_results <- list()

for (q in 1:4) {
  q_data <- stacked_data %>% filter(opmargin_quartile == q)
  cat("\n\n########## QUARTILE", q, "-- n hospital-year rows:", nrow(q_data),
      "| n hospitals:", n_distinct(q_data$mcrnum),
      "| n treated hospitals:", n_distinct(q_data$mcrnum[q_data$treated == 1]), "##########\n")

  for (outcome in quartile_outcomes) {
    cat("\n--- Quartile", q, "|", outcome, "---\n")
    key <- paste0("q", q, "_", outcome)
    quartile_results[[key]] <- run_stacked_did(
      outcome_var   = outcome,
      outcome_label = outcome,
      data          = q_data,
      filename      = paste0("opmargin_quartile_stackdid/", outcome, "_q", q, ".png"),
      width         = 10,
      height        = 8
    )
  }
}
