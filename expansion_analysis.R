# ==============================================================================
# PROVIDER TAX EFFECTS AND STATE MEDICAID EXPANSION
# Two checks:
#   1. "Pretend expansion never happened" -- rerun the main stacked DiD
#      restricted to year < 2014, before ACA Medicaid expansion took effect
#      almost everywhere.
#   2. Split the full-timing sample by whether a state ever expanded
#      Medicaid (using the corrected `expyear`, not the old buggy
#      `exp_status`) and compare ATTs side by side.
#
# The full-timing baseline (window=4, 1999-2022, unfiltered) is IDENTICAL to
# 5_buildstacked.R's canonical build, so it just reuses the shared
# `stacked_data` instead of rebuilding it. The pre-2014 and expansion/
# non-expansion splits are genuinely different samples (different cohort
# validity window, or built from a filtered subset of hospitals before the
# balanced-panel step) that 5_buildstacked.R doesn't produce, so
# build_stacked_data() (a parameterized copy of that builder, including
# Q-weights) is still used for those three.
# Requires stacked_data (5_buildstacked.R) and hospdata_analysis already in
# the environment.
# ==============================================================================

library(dplyr)
library(fixest)
library(purrr)

# ------------------------------------------------------------------------------
# 0. Parameterized version of the stacked-DiD builder from 5_buildstacked.R,
#    including Q-weights, so both windows below are built identically.
# ------------------------------------------------------------------------------
build_stacked_data <- function(data, window, yr_min, yr_max) {
  hospdata_stack <- data %>% filter(cohort != "Always (2004)")

  cohorts <- hospdata_stack %>%
    filter(gname != 0) %>%
    distinct(gname) %>%
    pull() %>%
    sort() %>%
    keep(~ (.x - window) >= yr_min & (.x + window) <= yr_max)

  n_required <- 2 * window + 1

  getdata <- function(j) {
    raw <- hospdata_stack %>%
      filter(gname == j | gname == 0 | gname > j + window) %>%
      filter(year >= j - window, year <= j + window) %>%
      mutate(df = j, rel_year = year - j, treated = as.integer(gname == j))
    keep_hosps <- raw %>%
      group_by(mcrnum) %>%
      summarise(n_yrs = n_distinct(rel_year), .groups = "drop") %>%
      filter(n_yrs == n_required) %>%
      pull(mcrnum)
    raw %>% filter(mcrnum %in% keep_hosps)
  }

  out <- map_df(cohorts, getdata)

  unit_counts <- out %>%
    distinct(mcrnum, df, treated) %>%
    group_by(df) %>%
    summarise(n_treat_a = sum(treated == 1), n_ctrl_a = sum(treated == 0), .groups = "drop")
  n_treat_total <- sum(unit_counts$n_treat_a)
  n_ctrl_total  <- sum(unit_counts$n_ctrl_a)

  out %>%
    left_join(unit_counts, by = "df") %>%
    mutate(
      treat_share = n_treat_a / n_treat_total,
      ctrl_share  = n_ctrl_a  / n_ctrl_total,
      Q_weight    = if_else(treated == 1, 1, treat_share / ctrl_share),
      post        = as.integer(rel_year >= 0)
    )
}

# Same pooled spec as table_for_pres.R: single treated:post ATT + pre-trend
# dummies kept, Q-weighted, clustered by state
fit_pooled <- function(outcome_var, data) {
  formula_str <- paste0(
    outcome_var,
    " ~ treated:post + i(rel_year, treated, ref = -1, keep = -4:-2) + exp_status + median_income_pre",
    " | mcrnum^df + year^df"
  )
  feols(as.formula(formula_str), data = data, weights = ~Q_weight, cluster = ~state)
}

extract_att <- function(model, outcome_var) {
  cn <- names(coef(model))
  pt <- cn[grepl("treated.*post|post.*treated", cn)]
  data.frame(
    outcome = outcome_var,
    att     = coef(model)[pt],
    se      = model$se[pt],
    p_value = pvalue(model)[pt]
  )
}

outcomes <- c("op_bed", "npr_bed", "op_margin", "mcaid_prop_discharges",
              "mcaid_dis_bed", "uncomp_bed", "ucc2npr")

# ==============================================================================
# PART 1: FULL TIMING VS. "PRETEND EXPANSION NEVER HAPPENED" (year < 2014)
# ==============================================================================

# Full-timing baseline = the shared stacked_data (window=4, 1999-2022,
# unfiltered) -- same sample 5_buildstacked.R already built. Only need to
# defensively add the derived columns this script's outcome list requires.
if (!"post" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(post = as.integer(rel_year >= 0))
}
if (!"mcaid_dis_bed" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(mcaid_dis_bed = mcaid_discharges / pre_beds_avg)
}
full_stacked <- stacked_data
cat("Full-timing sample: cohorts =", n_distinct(full_stacked$df),
    "| hospitals =", n_distinct(full_stacked$mcrnum), "\n")

pre2014_stacked <- build_stacked_data(hospdata_analysis, window = 4, yr_min = 1999, yr_max = 2013)
cat("Pre-2014 sample: cohorts =", n_distinct(pre2014_stacked$df),
    "| hospitals =", n_distinct(pre2014_stacked$mcrnum), "\n")
cat("Pre-2014 cohorts used:", paste(sort(unique(pre2014_stacked$df)), collapse = ", "), "\n")

results_full <- map_df(outcomes, ~ extract_att(fit_pooled(.x, full_stacked), .x)) %>%
  rename(att_full = att, se_full = se, p_full = p_value)

results_pre2014 <- map_df(outcomes, ~ tryCatch(
  extract_att(fit_pooled(.x, pre2014_stacked), .x),
  error = function(e) data.frame(outcome = .x, att = NA, se = NA, p_value = NA)
)) %>%
  rename(att_pre2014 = att, se_pre2014 = se, p_pre2014 = p_value)

timing_comparison <- results_full %>% left_join(results_pre2014, by = "outcome")

cat("\n=== FULL TIMING VS. PRE-2014-ONLY (\"NO EXPANSION\") ===\n")
print(timing_comparison, digits = 4)

# ==============================================================================
# PART 2: FULL-TIMING RESULTS, EXPANSION VS. NON-EXPANSION STATES
# Ever-expanded defined from the corrected `expyear` (state-level), not the
# old buggy `exp_status`. Classified at the STATE level (any row with a real
# expansion year), not row-wise -- `expyear` is NA for a large share of rows
# even in genuine expansion states (a pre-existing merge gap, unrelated to
# the exp_status fix), so a row-wise check misclassifies states with any NA
# years as "never expanded."
# ==============================================================================

state_expansion <- hospdata_analysis %>%
  group_by(state) %>%
  summarise(ever_expanded = as.integer(any(!is.na(expyear) & expyear > 0)), .groups = "drop")

hospdata_analysis <- hospdata_analysis %>%
  select(-any_of("ever_expanded")) %>%
  left_join(state_expansion, by = "state")

never_exp_states <- state_expansion %>% filter(ever_expanded == 0) %>% arrange(state) %>% pull(state)
cat("\nNever-expansion states:", paste(never_exp_states, collapse = ", "), "\n")

expansion_stacked <- build_stacked_data(
  hospdata_analysis %>% filter(ever_expanded == 1),
  window = 4, yr_min = 1999, yr_max = 2022
)
cat("Expansion-state sample: cohorts =", n_distinct(expansion_stacked$df),
    "| hospitals =", n_distinct(expansion_stacked$mcrnum), "\n")

nonexpansion_stacked <- build_stacked_data(
  hospdata_analysis %>% filter(ever_expanded == 0),
  window = 4, yr_min = 1999, yr_max = 2022
)
cat("Non-expansion-state sample: cohorts =", n_distinct(nonexpansion_stacked$df),
    "| hospitals =", n_distinct(nonexpansion_stacked$mcrnum), "\n")

results_expansion <- map_df(outcomes, ~ tryCatch(
  extract_att(fit_pooled(.x, expansion_stacked), .x),
  error = function(e) data.frame(outcome = .x, att = NA, se = NA, p_value = NA)
)) %>%
  rename(att_expansion = att, se_expansion = se, p_expansion = p_value)

results_nonexpansion <- map_df(outcomes, ~ tryCatch(
  extract_att(fit_pooled(.x, nonexpansion_stacked), .x),
  error = function(e) data.frame(outcome = .x, att = NA, se = NA, p_value = NA)
)) %>%
  rename(att_nonexpansion = att, se_nonexpansion = se, p_nonexpansion = p_value)

expansion_comparison <- results_expansion %>% left_join(results_nonexpansion, by = "outcome")

cat("\n=== EXPANSION VS. NON-EXPANSION STATES (full timing) ===\n")
print(expansion_comparison, digits = 4)

# ==============================================================================
# PART 3: FORMAL TEST -- DOES THE TAX EFFECT DIFFER BY EXPANSION STATUS?
# Triple interaction (treated:post:ever_expanded) on the pooled, full-timing
# sample, instead of eyeballing two separately-estimated split-sample ATTs.
# post:ever_expanded is included so the interaction isn't picking up a
# generic differential post-period trend between expansion and
# non-expansion states that has nothing to do with the tax; ever_expanded
# and treated:ever_expanded are both time-invariant per hospital-df and are
# absorbed by the mcrnum^df fixed effect, so they don't need to be added
# separately.
# ==============================================================================

full_stacked_exp <- full_stacked %>%
  left_join(state_expansion, by = "state")

fit_triple <- function(outcome_var, data) {
  formula_str <- paste0(
    outcome_var,
    " ~ treated:post + treated:post:ever_expanded + post:ever_expanded +",
    " i(rel_year, treated, ref = -1, keep = -4:-2) + exp_status + median_income_pre",
    " | mcrnum^df + year^df"
  )
  feols(as.formula(formula_str), data = data, weights = ~Q_weight, cluster = ~state)
}

extract_triple <- function(model, outcome_var) {
  cn <- names(coef(model))
  base_term  <- cn[grepl("^treated:post$|^post:treated$", cn)]
  diff_term  <- cn[grepl("treated.*post.*ever_expanded|ever_expanded.*treated.*post", cn)]
  data.frame(
    outcome              = outcome_var,
    att_nonexpansion     = coef(model)[base_term],
    se_nonexpansion      = model$se[base_term],
    p_nonexpansion       = pvalue(model)[base_term],
    att_expansion_extra  = coef(model)[diff_term],
    se_expansion_extra   = model$se[diff_term],
    p_expansion_extra    = pvalue(model)[diff_term]
  ) %>%
    mutate(att_expansion_implied = att_nonexpansion + att_expansion_extra)
}

triple_results <- map_df(outcomes, ~ extract_triple(fit_triple(.x, full_stacked_exp), .x))

cat("\n=== TRIPLE-INTERACTION TEST: DOES THE ATT DIFFER BY EXPANSION STATUS? ===\n")
cat("att_nonexpansion = ATT for never-expanded states (the omitted group)\n")
cat("att_expansion_extra = how much MORE (or less) the ATT is for ever-expanded\n")
cat("  states; its p-value is the formal test of a differential effect\n")
cat("att_expansion_implied = att_nonexpansion + att_expansion_extra\n\n")
print(triple_results, digits = 4)
