# ==============================================================================
# STATE-LEVEL RESULTS TABLE: UNINSURED RATE & MEDICAID ENROLLMENT PER BED CAPACITY
# Same pooled ATT spec as main_results_table.R / service_line_results_table.R
# (single treated:post coefficient, Q-weighted, state x cohort and year x
# cohort fixed effects, controls for exp_status + median_income_pre, SEs
# clustered by state) -- adapted to the state-level stacked design
# (state^df + year^df instead of mcrnum^df).
#
# N States is shown per outcome (not hospitals) -- coverage differs a lot
# between the two (uninsured_rate's CPS source ends 2011; enrollment's
# IPUMS source starts 2007), same reasoning as service_line_results_table.R.
#
# Does NOT rebuild the state-level stacked sample -- uses state_stacked
# (with Q_weight) as already built by state_event_studies_stacked.R. Run
# that first.
# Requires state_stacked already in the environment.
# ==============================================================================

library(dplyr)
library(fixest)
library(purrr)
library(kableExtra)

if (!"post" %in% names(state_stacked)) {
  state_stacked <- state_stacked %>% mutate(post = as.integer(rel_year >= 0))
}

outcome_info <- list(
  list(var = "uninsured_rate",          label = "Uninsured Rate",                          is_pct = TRUE),
  list(var = "medicaid_enrollment_bed", label = "Medicaid Enrollment per Bed Capacity",     is_pct = FALSE)
)

# ------------------------------------------------------------------------------
# Pooled ATT model (identical spec to main_results_table.R, state-level FE)
# ------------------------------------------------------------------------------
fit_pooled_state <- function(outcome_var) {
  formula_str <- paste0(
    outcome_var,
    " ~ treated:post + i(rel_year, treated, ref = -1, keep = -4:-2) + exp_status + median_income_pre",
    " | state^df + year^df"
  )
  feols(as.formula(formula_str), data = state_stacked, weights = ~Q_weight, cluster = ~state)
}

pre_treat_mean_state <- function(outcome_var) {
  state_stacked %>%
    filter(treated == 1, rel_year < 0) %>%
    summarise(m = mean(.data[[outcome_var]], na.rm = TRUE)) %>%
    pull(m)
}

n_states_in_sample <- function(outcome_var) {
  state_stacked %>%
    filter(treated == 1, !is.na(.data[[outcome_var]]), !is.na(exp_status), !is.na(median_income_pre)) %>%
    summarise(n = n_distinct(state)) %>%
    pull(n)
}

# ------------------------------------------------------------------------------
# Run both outcomes
# ------------------------------------------------------------------------------
state_results_raw <- map_df(outcome_info, function(oi) {
  result <- fit_pooled_state(oi$var)
  cn <- names(coef(result))
  pt <- cn[grepl("treated.*post|post.*treated", cn)]

  beta <- as.numeric(coef(result)[pt])
  se   <- as.numeric(result$se[pt])
  p    <- as.numeric(pvalue(result)[pt])

  stars <- case_when(
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE     ~ ""
  )

  data.frame(
    Outcome = oi$label,
    IsPct   = oi$is_pct,
    Mean    = pre_treat_mean_state(oi$var),
    NStates = n_states_in_sample(oi$var),
    Beta    = beta,
    SE      = se,
    Stars   = stars,
    stringsAsFactors = FALSE
  )
})

cat("\n=== STATE-LEVEL OUTCOMES: POOLED ATT ===\n")
print(state_results_raw, digits = 4)

# ------------------------------------------------------------------------------
# Format: percentages for uninsured_rate, plain numbers for enrollment/bed.
# Beta + stars on one row, (SE) on the row underneath.
# ------------------------------------------------------------------------------
state_results_formatted <- state_results_raw %>%
  mutate(
    Mean_fmt   = ifelse(IsPct, sprintf("%.1f%%", Mean * 100), sprintf("%.2f", Mean)),
    Beta_fmt   = ifelse(IsPct, sprintf("%.2fpp", Beta * 100), sprintf("%.3f", Beta)),
    SE_fmt     = ifelse(IsPct, sprintf("%.2fpp", SE * 100), sprintf("%.3f", SE)),
    Beta_Stars = paste0(Beta_fmt, Stars),
    SE_paren   = paste0("(", SE_fmt, ")"),
    NStates_fmt = scales::comma(NStates, accuracy = 1)
  )

state_results_long <- do.call(rbind, lapply(seq_len(nrow(state_results_formatted)), function(i) {
  rbind(
    data.frame(Outcome = state_results_formatted$Outcome[i],
               Mean = state_results_formatted$Mean_fmt[i],
               NStates = state_results_formatted$NStates_fmt[i],
               Estimate = state_results_formatted$Beta_Stars[i], stringsAsFactors = FALSE),
    data.frame(Outcome = "", Mean = "", NStates = "",
               Estimate = state_results_formatted$SE_paren[i], stringsAsFactors = FALSE)
  )
}))

cat("\n=== FORMATTED (BETA / SE STACKED) ===\n")
print(state_results_long)

# ------------------------------------------------------------------------------
# LaTeX table
# ------------------------------------------------------------------------------
state_results_long %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    caption   = "State-Level Outcomes: Pooled Average Treatment Effects on the Treated",
    col.names = c("Outcome", "Pre-Treatment Mean", "N States", "ATT"),
    align     = c("l", "r", "r", "r")
  ) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  add_footnote(
    paste0(
      "Pooled treated x post coefficient from a state-level stacked ",
      "difference-in-differences model (Q-weighted, state x cohort and year x ",
      "cohort fixed effects, controls for state Medicaid expansion status and ",
      "median income, SEs clustered by state). Uninsured Rate from Garthwaite et ",
      "al. (2015)-style CPS replication (data years 1985-2011); Medicaid ",
      "Enrollment per Bed Capacity is state Medicaid enrollment (IPUMS ACS, ",
      "2007+) divided by total pre-treatment hospital bed capacity in the state. ",
      "N States is the number of unique treated states with non-missing data ",
      "contributing to that specific regression -- coverage differs between the ",
      "two outcomes' underlying data sources. * p<0.10, ** p<0.05, *** p<0.01."
    ),
    notation = "none"
  ) %>%
  writeLines("state_results_table.tex")

cat("\nDone -- state_results_table.tex saved.\n")
