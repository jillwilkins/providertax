# ==============================================================================
# TRANSLATIONS: EVENT-STUDY COEFFICIENTS -> REAL-WORLD MAGNITUDES
# Scales per-bed event-study coefficients up to a representative hospital,
# e.g. "how many additional Medicaid patients did the median hospital see
# as a result of the tax." Same Q-weighted pooled spec used throughout.
# Does NOT rebuild the stacked sample -- uses stacked_data (with Q_weight)
# as already built by 5_buildstacked.R. Run that first.
# Requires stacked_data already in the environment.
# ==============================================================================

library(dplyr)
library(fixest)

# mcaid_dis_bed is the same definition 6_events_paper.R adds on top of
# 5_buildstacked.R's output; computed here too in case that hasn't run yet.
if (!"mcaid_dis_bed" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>%
    mutate(mcaid_dis_bed = mcaid_discharges / pre_beds_avg)
}

# ------------------------------------------------------------------------------
# 1. Median pre-treatment bed count, treated hospitals (the "representative
#    hospital" scale used to translate per-bed coefficients)
# ------------------------------------------------------------------------------
median_beds <- stacked_data %>%
  filter(treated == 1, rel_year < 0) %>%
  group_by(mcrnum) %>%
  summarise(avg_beds = mean(beds, na.rm = TRUE), .groups = "drop") %>%
  summarise(median_beds = median(avg_beds, na.rm = TRUE)) %>%
  pull(median_beds)

cat("Median pre-treatment beds (treated hospitals):", median_beds, "\n")

# ------------------------------------------------------------------------------
# 2. Translate a per-bed event-study outcome into "additional units at the
#    representative hospital," by year since adoption and cumulatively
#    across all observed post-treatment years.
# ------------------------------------------------------------------------------
translate_per_bed_outcome <- function(outcome_var, beds = median_beds,
                                       controls = "exp_status + median_income_pre") {
  formula_str <- paste0(outcome_var, " ~ i(rel_year, treated, ref = -1) + ", controls, " | mcrnum^df + year^df")
  model <- feols(as.formula(formula_str), data = stacked_data, weights = ~Q_weight, cluster = ~state)

  cn <- names(coef(model))
  post_terms <- cn[grepl("rel_year::[0-9]+:treated", cn)]

  data.frame(
    rel_year         = as.numeric(gsub(".*::([0-9]+):.*", "\\1", post_terms)),
    coef_per_bed     = as.numeric(coef(model)[post_terms]),
    stringsAsFactors = FALSE
  ) %>%
    arrange(rel_year) %>%
    mutate(
      at_representative_hospital = coef_per_bed * beds,
      cumulative                 = cumsum(at_representative_hospital)
    )
}

# ------------------------------------------------------------------------------
# 3. Additional Medicaid discharges at the median hospital, by years since
#    adoption and cumulatively across all 5 observed post-treatment years
# ------------------------------------------------------------------------------
mcaid_translation <- translate_per_bed_outcome("mcaid_dis_bed")

cat("\n=== ADDITIONAL MEDICAID DISCHARGES AT THE MEDIAN HOSPITAL (", median_beds, "beds ) ===\n")
print(mcaid_translation, row.names = FALSE)

cat("\nCumulative additional Medicaid discharges, years 0-4:",
    round(sum(mcaid_translation$at_representative_hospital), 1), "\n")
