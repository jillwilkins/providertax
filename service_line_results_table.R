# ==============================================================================
# SERVICE LINE OFFERINGS: MAIN RESULTS TABLE
# Same pooled ATT spec as main_results_table.R (single treated:post
# coefficient, Q-weighted, hospital x cohort and year x cohort fixed
# effects, controls for exp_status + median_income_pre, SEs clustered by
# state). Ten AHA service-line outcomes -- 7 binary flags (linear
# probability model, reported as percentage points) and 3 counts.
#
# N Hospitals is shown PER OUTCOME, not as a single footnote number like
# main_results_table.R -- coverage varies a lot across these outcomes
# (established this session: AHA field missingness differs substantially
# by variable and by year), so a single blanket hospital count would be
# misleading here in a way it isn't for the main results table's more
# uniformly-covered HCRIS outcomes.
#
# Does NOT rebuild the stacked sample -- uses stacked_data (with Q_weight)
# as already built by 5_buildstacked.R. Run that first.
# Requires stacked_data already in the environment.
# ==============================================================================

library(dplyr)
library(fixest)
library(purrr)
library(kableExtra)

binary_vars <- c("emdephos", "psyemhos", "alchhos", "traumhos", "adtchos", "iclabhos", "eswlhos")
count_vars  <- c("vem", "psybd", "alchbd")

outcome_info <- list(
  list(var = "emdephos", label = "Emergency Department",                       is_binary = TRUE),
  list(var = "psyemhos", label = "Psychiatric Emergency Services",             is_binary = TRUE),
  list(var = "alchhos",  label = "Alcohol/Chemical Dependency Unit",           is_binary = TRUE),
  list(var = "traumhos", label = "Trauma Center",                             is_binary = TRUE),
  list(var = "adtchos",  label = "Adult Day Care Program",                    is_binary = TRUE),
  list(var = "iclabhos", label = "Cardiac Catheterization Lab",               is_binary = TRUE),
  list(var = "eswlhos",  label = "ESWL",                                      is_binary = TRUE),
  list(var = "vem",      label = "Emergency Department Visits",               is_binary = FALSE),
  list(var = "psybd",    label = "Psychiatric Beds",                          is_binary = FALSE),
  list(var = "alchbd",   label = "Alcohol/Chemical Dependency Beds",          is_binary = FALSE)
)

# ------------------------------------------------------------------------------
# Pooled ATT model (identical spec to main_results_table.R)
# ------------------------------------------------------------------------------
if (!"post" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(post = as.integer(rel_year >= 0))
}

fit_pooled <- function(outcome_var) {
  formula_str <- paste0(
    outcome_var,
    " ~ treated:post + i(rel_year, treated, ref = -1, keep = -4:-2) + exp_status + median_income_pre",
    " | mcrnum^df + year^df"
  )
  feols(as.formula(formula_str), data = stacked_data, weights = ~Q_weight, cluster = ~state)
}

pre_treat_mean <- function(outcome_var) {
  stacked_data %>%
    filter(treated == 1, rel_year < 0) %>%
    summarise(m = mean(.data[[outcome_var]], na.rm = TRUE)) %>%
    pull(m)
}

n_hosp_in_sample <- function(outcome_var) {
  stacked_data %>%
    filter(treated == 1, !is.na(.data[[outcome_var]]), !is.na(exp_status), !is.na(median_income_pre)) %>%
    summarise(n = n_distinct(mcrnum)) %>%
    pull(n)
}

# ------------------------------------------------------------------------------
# Run all 10 outcomes
# ------------------------------------------------------------------------------
service_line_raw <- map_df(outcome_info, function(oi) {
  result <- fit_pooled(oi$var)
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
    Outcome   = oi$label,
    IsBinary  = oi$is_binary,
    Mean      = pre_treat_mean(oi$var),
    NHosp     = n_hosp_in_sample(oi$var),
    Beta      = beta,
    SE        = se,
    Stars     = stars,
    stringsAsFactors = FALSE
  )
})

cat("\n=== SERVICE LINE OFFERINGS: POOLED ATT BY OUTCOME ===\n")
print(service_line_raw, digits = 4)

# ------------------------------------------------------------------------------
# Format: binary outcomes as percentage points, counts as plain numbers.
# Coefficient + stars on one row, (SE) on the row underneath, same
# interleaving convention as main_results_table.R.
# ------------------------------------------------------------------------------
service_line_formatted <- service_line_raw %>%
  mutate(
    Mean_fmt = case_when(
      IsBinary            ~ sprintf("%.1f%%", Mean * 100),
      abs(Mean) >= 1000    ~ scales::comma(Mean, accuracy = 1),
      TRUE                ~ sprintf("%.2f", Mean)
    ),
    Beta_fmt = case_when(
      IsBinary            ~ sprintf("%.1fpp", Beta * 100),
      abs(Beta) >= 1000    ~ scales::comma(Beta, accuracy = 1),
      TRUE                ~ sprintf("%.2f", Beta)
    ),
    SE_fmt   = case_when(
      IsBinary            ~ sprintf("%.1fpp", SE * 100),
      abs(SE) >= 1000      ~ scales::comma(SE, accuracy = 1),
      TRUE                ~ sprintf("%.2f", SE)
    ),
    Beta_Stars = paste0(Beta_fmt, Stars),
    SE_paren   = paste0("(", SE_fmt, ")"),
    NHosp_fmt  = scales::comma(NHosp, accuracy = 1)
  )

service_line_long <- do.call(rbind, lapply(seq_len(nrow(service_line_formatted)), function(i) {
  rbind(
    data.frame(Outcome = service_line_formatted$Outcome[i],
               Mean = service_line_formatted$Mean_fmt[i],
               NHosp = service_line_formatted$NHosp_fmt[i],
               Estimate = service_line_formatted$Beta_Stars[i], stringsAsFactors = FALSE),
    data.frame(Outcome = "", Mean = "", NHosp = "",
               Estimate = service_line_formatted$SE_paren[i], stringsAsFactors = FALSE)
  )
}))

cat("\n=== FORMATTED (BETA / SE STACKED) ===\n")
print(service_line_long)

# ------------------------------------------------------------------------------
# LaTeX table
# ------------------------------------------------------------------------------
service_line_long %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    caption   = "Service Line Offerings: Pooled Average Treatment Effects on the Treated",
    col.names = c("Outcome", "Pre-Treatment Mean", "N Hospitals", "ATT"),
    align     = c("l", "r", "r", "r")
  ) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  pack_rows("Service Line Offered (percentage points)", 1, 14) %>%
  pack_rows("Volume / Bed Counts", 15, 20) %>%
  add_footnote(
    paste0(
      "Pooled treated x post coefficient from a stacked difference-in-differences ",
      "model (Q-weighted, hospital x cohort and year x cohort fixed effects, ",
      "controls for state Medicaid expansion status and median income, SEs ",
      "clustered by state). Binary service-line outcomes estimated as a linear ",
      "probability model; coefficients and pre-treatment means reported in ",
      "percentage points. N Hospitals is the number of unique treated hospitals ",
      "with non-missing data contributing to that specific outcome's regression -- ",
      "coverage varies substantially across these AHA-sourced fields, see ",
      "service_line_summary_stats.R notes. * p<0.10, ** p<0.05, *** p<0.01."
    ),
    notation = "none"
  ) %>%
  writeLines("service_line_results_table.tex")

cat("\nDone -- service_line_results_table.tex saved.\n")
