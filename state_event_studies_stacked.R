# ==============================================================================
# STATE-LEVEL EVENT STUDIES: STACKED DESIGN (uninsured rate, Medicaid
# enrollment, eligibility)
#
# Replaces state_analysis.R's naive `state + year` TWSFE specification with
# the same Q-weighted stacked design used everywhere else in this project
# (5_buildstacked.R) -- staggered treatment timing under plain TWSFE is
# known to produce biased estimates when treatment effects vary by cohort
# or over time (Goodman-Bacon 2021, Sun & Abraham 2021), which is exactly
# why the hospital-level analysis uses the stacked/Q-weighted approach in
# the first place. Since the provider tax is a STATE policy (every
# hospital in a state shares the same adoption year), the same cohort
# structure (gname/df) already used at the hospital level applies directly
# at the state level -- this just re-runs the identical stacking logic
# with `state` as the unit instead of `mcrnum`.
#
# Also fixes the medicaid_enrollment_bed normalization bug found in
# state_analysis.R: medicaid_enrollment (a state TOTAL) was divided by
# pre_beds_avg (a hospital's own AVERAGE bed count, i.e. a typical single
# hospital's size), which isn't a meaningful ratio. Fixed here to divide
# by the state's TOTAL pre-treatment bed capacity (sum across all
# hospitals in the state), giving "Medicaid enrollees per hospital bed of
# capacity in the state" -- a real, interpretable quantity.
#
# Requires hospdata_analysis already in the environment.
# ==============================================================================

library(dplyr)
library(fixest)
library(ggplot2)
library(purrr)
library(haven)

# ------------------------------------------------------------------------------
# 1. Build a clean state-year panel with everything these 3 outcomes need
# ------------------------------------------------------------------------------

# State-level treatment/control variables (constant across a state's hospitals)
state_treatment <- hospdata_analysis %>%
  distinct(state, gname, cohort, firsttax_num)

state_year_controls <- hospdata_analysis %>%
  group_by(state, year) %>%
  summarise(
    exp_status         = first(exp_status),
    median_income_pre  = first(median_income_pre),
    medicaid_enrollment = first(medicaid_enrollment),
    eligibility        = first(eligibility),
    .groups = "drop"
  )

# Total pre-treatment state bed capacity (sum, not mean) -- fixed baseline,
# same convention as pre_beds_avg at the hospital level (not time-varying,
# so it can't mechanically absorb any treatment effect on capacity itself)
state_pre_beds_total <- hospdata_analysis %>%
  distinct(mcrnum, state, pre_beds_avg) %>%
  group_by(state) %>%
  summarise(state_pre_beds_total = sum(pre_beds_avg, na.rm = TRUE), .groups = "drop")

# Uninsured rate: Garthwaite et al. (2015) CPS replication (datauninsured.R)
# NOTE: survey year -> data year is year - 1, and the source file only
# covers survey years 1986-2012, i.e. DATA years 1985-2011 -- ends well
# before several tax-adoption cohorts (2012, 2013, 2014, 2017). Flagged
# explicitly below once cohort coverage is checked.
data_cps_og <- read_dta("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/replication/garth2015/cps_00013.dta")

data_cps <- data_cps_og %>%
  filter(age >= 21 & age < 65) %>%
  mutate(
    year         = year - 1,
    any_ins      = as.integer(hcovany == 2)
  ) %>%
  group_by(statefip, year) %>%
  summarise(
    insured_rate   = weighted.mean(any_ins, wtsupp, na.rm = TRUE),
    uninsured_rate = 1 - insured_rate,
    .groups = "drop"
  ) %>%
  mutate(
    state_name = tigris::fips_codes$state_name[match(sprintf("%02d", as.numeric(statefip)), tigris::fips_codes$state_code)]
  )

# state.abb match (avoids requiring the tigris package just for this)
data_cps <- data_cps %>%
  mutate(state = state.abb[match(state_name, state.name)]) %>%
  filter(!is.na(state)) %>%
  select(state, year, uninsured_rate)

cat("\nuninsured_rate data year range:", min(data_cps$year), "-", max(data_cps$year), "\n")

# ------------------------------------------------------------------------------
# 2. Assemble the state-year panel
# ------------------------------------------------------------------------------
state_panel <- state_year_controls %>%
  left_join(state_treatment, by = "state") %>%
  left_join(state_pre_beds_total, by = "state") %>%
  left_join(data_cps, by = c("state", "year")) %>%
  mutate(medicaid_enrollment_bed = medicaid_enrollment / state_pre_beds_total)

cat("State panel:", nrow(state_panel), "state-years,", n_distinct(state_panel$state), "states\n")

# ------------------------------------------------------------------------------
# 3. State-level stacked builder (same logic as 5_buildstacked.R /
#    expansion_analysis.R's build_stacked_data(), unit = state not mcrnum)
# ------------------------------------------------------------------------------
build_state_stacked <- function(data, window, yr_min, yr_max) {
  base <- data %>% filter(cohort != "Always (2004)")

  cohorts <- base %>%
    filter(gname != 0) %>%
    distinct(gname) %>%
    pull() %>%
    sort() %>%
    keep(~ (.x - window) >= yr_min & (.x + window) <= yr_max)

  n_required <- 2 * window + 1

  getdata <- function(j) {
    raw <- base %>%
      filter(gname == j | gname == 0 | gname > j + window) %>%
      filter(year >= j - window, year <= j + window) %>%
      mutate(df = j, rel_year = year - j, treated = as.integer(gname == j))

    keep_states <- raw %>%
      group_by(state) %>%
      summarise(n_yrs = n_distinct(rel_year), .groups = "drop") %>%
      filter(n_yrs == n_required) %>%
      pull(state)

    raw %>% filter(state %in% keep_states)
  }

  out <- map_df(cohorts, getdata)

  unit_counts <- out %>%
    distinct(state, df, treated) %>%
    group_by(df) %>%
    summarise(n_treat_a = sum(treated == 1), n_ctrl_a = sum(treated == 0), .groups = "drop")
  n_treat_total <- sum(unit_counts$n_treat_a)
  n_ctrl_total  <- sum(unit_counts$n_ctrl_a)

  out %>%
    left_join(unit_counts, by = "df") %>%
    mutate(
      treat_share = n_treat_a / n_treat_total,
      ctrl_share  = n_ctrl_a / n_ctrl_total,
      Q_weight    = if_else(treated == 1, 1, treat_share / ctrl_share)
    )
}

# ------------------------------------------------------------------------------
# 4. Build the stacked state sample. yr_max = 2022 (matches the hospital-
#    level design). Check cohort coverage before committing to a window --
#    uninsured_rate's 2011 cutoff means late cohorts can't contribute a
#    full post-period for THAT outcome specifically.
# ------------------------------------------------------------------------------
window <- 4
state_stacked <- build_state_stacked(state_panel, window = window, yr_min = 1999, yr_max = 2022)

cat("\nCohorts in state-level stack:", paste(sort(unique(state_stacked$df[state_stacked$treated==1])), collapse=", "), "\n")
cat("N treated states:", n_distinct(state_stacked$state[state_stacked$treated==1]), "\n")
cat("N control states:", n_distinct(state_stacked$state[state_stacked$treated==0]), "\n")

cat("\n=== uninsured_rate: N TREATED STATES BY REL_YEAR (coverage check) ===\n")
state_stacked %>% filter(treated==1, !is.na(uninsured_rate)) %>% count(rel_year) %>% as.data.frame() %>% print(row.names=FALSE)

cat("\n=== medicaid_enrollment_bed: N TREATED STATES BY REL_YEAR ===\n")
state_stacked %>% filter(treated==1, !is.na(medicaid_enrollment_bed)) %>% count(rel_year) %>% as.data.frame() %>% print(row.names=FALSE)

cat("\n=== eligibility: N TREATED STATES BY REL_YEAR ===\n")
state_stacked %>% filter(treated==1, !is.na(eligibility)) %>% count(rel_year) %>% as.data.frame() %>% print(row.names=FALSE)

# ==============================================================================
# FUNCTION: RUN STATE-LEVEL STACKED DiD AND CREATE PLOT (same visual
# convention as run_stacked_did(), state^df + year^df instead of mcrnum^df)
# ==============================================================================
run_state_stacked_did <- function(outcome_var,
                                   data = state_stacked,
                                   controls = "exp_status + median_income_pre",
                                   y_scale = "comma",
                                   weights = ~Q_weight,
                                   filename = NULL,
                                   width = 10,
                                   height = 6.5) {

  formula_str <- paste0(
    outcome_var, " ~ i(rel_year, treated, ref = -1) + ",
    controls, " | state^df + year^df"
  )

  cat("\n=== Estimating state-level stacked DiD for:", outcome_var, "===\n")

  result <- feols(as.formula(formula_str), data = data, weights = weights, cluster = ~state)
  print(summary(result))

  coef_data <- data.frame(
    rel_year = as.numeric(gsub(".*::(-?[0-9]+):.*", "\\1",
                               names(coef(result))[grepl("rel_year", names(coef(result)))])),
    coef = coef(result)[grepl("rel_year", names(coef(result)))],
    se = result$se[grepl("rel_year", names(coef(result)))]
  ) %>%
    mutate(ci_lower = coef - 1.96 * se, ci_upper = coef + 1.96 * se) %>%
    bind_rows(data.frame(rel_year = -1, coef = 0, se = 0, ci_lower = 0, ci_upper = 0)) %>%
    arrange(rel_year) %>%
    mutate(period_color = ifelse(rel_year < 0, "Pre-Treatment", "Post-Treatment"))

  p <- ggplot(coef_data, aes(x = rel_year, y = coef, color = period_color)) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.3, size = 0.8) +
    geom_point(size = 3) +
    scale_color_manual(values = c("Pre-Treatment" = "#DC4B4B", "Post-Treatment" = "#2484d2")) +
    geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.4) +
    labs(x = "Years Relative to Treatment", y = " ") +
    scale_x_continuous(breaks = seq(min(coef_data$rel_year), max(coef_data$rel_year), 1)) +
    scale_y_continuous(labels = switch(y_scale,
      "comma" = scales::comma, "percent" = scales::percent_format(accuracy = 0.1), "dollar" = scales::dollar_format())) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
      axis.title.x = element_text(size = 12, margin = margin(t = 10)),
      axis.title.y = element_text(size = 12, margin = margin(r = 10)),
      axis.text = element_text(size = 11, color = "black"),
      panel.grid.major = element_line(color = "#f2f2f2", size = 0.3),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.position = "none"
    )

  print(p)
  if (!is.null(filename)) {
    ggsave(filename, plot = p, width = width, height = height, dpi = 300)
    cat("\nPlot saved to:", filename, "\n")
  }
  list(model = result, coef_data = coef_data, plot = p)
}

# ==============================================================================
# RUN ALL THREE
# ==============================================================================
results_state <- list()

results_state$uninsured_rate <- run_state_stacked_did(
  "uninsured_rate", y_scale = "percent",
  filename = "events_for_paper/uninsured_rate_stacked.png"
)

results_state$medicaid_enrollment_bed <- run_state_stacked_did(
  "medicaid_enrollment_bed", y_scale = "comma",
  filename = "events_for_paper/medicaid_enrollment_stacked.png"
)

results_state$eligibility <- run_state_stacked_did(
  "eligibility", y_scale = "comma",
  filename = "events_for_paper/eligibility_stacked.png"
)

cat("\nDone -- all three state-level stacked event studies saved to events_for_paper/\n")
