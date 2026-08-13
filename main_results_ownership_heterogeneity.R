# ==============================================================================
# MAIN RESULTS: FOR-PROFIT VS. NON-PROFIT HETEROGENEITY
# Same triple-interaction approach as service_line_ownership_heterogeneity.R
# / expansion_analysis.R Part 3 (treated:post + treated:post:for_profit +
# post:for_profit), applied to main_results_table.R's outcome set instead
# of the service-line outcomes.
#   - Non-Profit: ATT for non-profit hospitals (the omitted group)
#   - For-Profit Diff.: how much MORE (or less) the ATT is for for-profit
#     hospitals; its p-value is the formal test of a differential effect
#   - For-Profit Implied: Non-Profit + For-Profit Diff.
#
# Ownership classification: same convention as service_line_ownership_
# heterogeneity.R (each hospital's own first non-missing typectrl/cntrl,
# fixed per hospital, matching 2_dataclean.R / heterogeneity_win.R).
#
# Does NOT rebuild the stacked sample -- uses stacked_data (with Q_weight)
# as already built by 5_buildstacked.R. Run that first.
# Requires stacked_data already in the environment.
# ==============================================================================

library(dplyr)
library(fixest)
library(purrr)
library(kableExtra)

# ------------------------------------------------------------------------------
# 1. Derived columns this table needs beyond what 5_buildstacked.R provides
#    (same defensive checks as main_results_table.R)
# ------------------------------------------------------------------------------
if (!"post" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(post = as.integer(rel_year >= 0))
}
if (!"nonm_dis_bed" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(nonm_dis_bed = nonm_discharges / pre_beds_avg)
}
if (!"mcaid_dis_bed" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(mcaid_dis_bed = mcaid_discharges / pre_beds_avg)
}
if (!"tot_discharges_bed" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(tot_discharges_bed = tot_discharges / pre_beds_avg)
}

# ------------------------------------------------------------------------------
# 2. Hospital-level ownership classification (same convention as
#    service_line_ownership_heterogeneity.R)
# ------------------------------------------------------------------------------
if (!"for_profit" %in% names(stacked_data)) {
  hosp_ownership <- stacked_data %>%
    group_by(mcrnum) %>%
    summarise(typectrl = first(typectrl), cntrl = first(cntrl), .groups = "drop") %>%
    mutate(
      ownership = case_when(
        !is.na(typectrl) & typectrl %in% 1:2 ~ "Nonprofit",
        !is.na(typectrl) & typectrl %in% 3:6 ~ "For-profit",
        !is.na(cntrl) & cntrl %in% c(21, 23) ~ "Nonprofit",
        !is.na(cntrl) & cntrl %in% c(30, 31, 32, 33) ~ "For-profit",
        TRUE ~ "Unknown"
      ),
      for_profit = case_when(
        ownership == "For-profit" ~ 1,
        ownership == "Nonprofit"  ~ 0,
        TRUE                      ~ NA_real_
      )
    ) %>%
    select(mcrnum, for_profit)

  stacked_data <- stacked_data %>% left_join(hosp_ownership, by = "mcrnum")
}

# ------------------------------------------------------------------------------
# 3. Outcome list: identical to main_results_table.R
# ------------------------------------------------------------------------------
outcome_info <- list(
  list(var = "op_bed",                 label = "Operating Expenses per Bed"),
  list(var = "npr_bed",                label = "Net Patient Revenue per Bed"),
  list(var = "op_margin",              label = "Operating Margin"),
  list(var = "mcaid_prop_discharges",  label = "Medicaid Share of Discharges"),
  list(var = "nonm_prop_discharges",   label = "Non-Public Share of Discharges"),
  list(var = "mcaid_dis_bed",          label = "Medicaid Discharges per Bed"),
  list(var = "nonm_dis_bed",           label = "Non-Public Discharges per Bed"),
  list(var = "tot_discharges_bed",     label = "Total Discharges per Bed"),
  list(var = "uncomp_bed",             label = "Uncompensated Care per Bed"),
  list(var = "ucc2npr",                label = "Uncompensated Care as % of NPR")
)

# ------------------------------------------------------------------------------
# 4. Triple-interaction model (identical spec to service_line_ownership_
#    heterogeneity.R / expansion_analysis.R Part 3)
# ------------------------------------------------------------------------------
fit_triple <- function(outcome_var) {
  formula_str <- paste0(
    outcome_var,
    " ~ treated:post + treated:post:for_profit + post:for_profit +",
    " i(rel_year, treated, ref = -1, keep = -4:-2) + exp_status + median_income_pre",
    " | mcrnum^df + year^df"
  )
  feols(as.formula(formula_str), data = stacked_data %>% filter(!is.na(for_profit)),
        weights = ~Q_weight, cluster = ~state)
}

extract_triple <- function(model, outcome_var) {
  cn <- names(coef(model))
  base_term <- cn[grepl("^treated:post$|^post:treated$", cn)]
  diff_term <- cn[grepl("treated.*post.*for_profit|for_profit.*treated.*post", cn)]

  data.frame(
    outcome              = outcome_var,
    att_nonprofit        = coef(model)[base_term],
    se_nonprofit         = model$se[base_term],
    p_nonprofit          = pvalue(model)[base_term],
    att_forprofit_extra  = coef(model)[diff_term],
    se_forprofit_extra   = model$se[diff_term],
    p_forprofit_extra    = pvalue(model)[diff_term]
  ) %>%
    mutate(att_forprofit_implied = att_nonprofit + att_forprofit_extra)
}

triple_results_main <- map_df(outcome_info, function(oi) extract_triple(fit_triple(oi$var), oi$var))

cat("\n=== MAIN RESULTS, FOR-PROFIT VS. NON-PROFIT: TRIPLE-INTERACTION TEST ===\n")
cat("att_nonprofit = ATT for non-profit hospitals (the omitted group)\n")
cat("att_forprofit_extra = how much MORE (or less) the ATT is for for-profit\n")
cat("  hospitals; its p-value is the formal test of a differential effect\n")
cat("att_forprofit_implied = att_nonprofit + att_forprofit_extra\n\n")
print(triple_results_main, digits = 4)

# ------------------------------------------------------------------------------
# 5. LaTeX table (same 2-column-per-panel style as
#    service_line_ownership_heterogeneity.R / expansion_table.R Panel A)
# ------------------------------------------------------------------------------
stars_for <- function(p) case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")

fmt_cell <- function(beta, se, p) {
  stars <- stars_for(p)
  if (abs(beta) >= 1000) {
    sprintf("%s%s (%s)", scales::comma(beta, accuracy = 1), stars, scales::comma(se, accuracy = 1))
  } else {
    sprintf("%s%s (%s)", sprintf("%.3f", beta), stars, sprintf("%.3f", se))
  }
}

main_ownership_table <- map_df(outcome_info, function(oi) {
  r <- triple_results_main %>% filter(outcome == oi$var)
  data.frame(
    Outcome = oi$label,
    `For-Profit Diff.` = fmt_cell(r$att_forprofit_extra, r$se_forprofit_extra, r$p_forprofit_extra),
    check.names = FALSE, stringsAsFactors = FALSE
  )
})

main_ownership_table %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    caption   = "Main Results: For-Profit vs. Non-Profit Heterogeneity",
    col.names = c("Outcome", "For-Profit Diff."),
    align     = c("l", "r")
  ) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  add_footnote(
    paste0(
      "Triple-interaction model (treated x post x for_profit) on the full ",
      "stacked sample: coefficient shown is the additional effect for ",
      "for-profit hospitals relative to non-profit hospitals (Treated x Post x ",
      "For-Profit), the coefficient of interest for testing a differential ",
      "effect by ownership -- not the ATT for either group on its own (see ",
      "main_results_table.tex for the pooled ATT). Ownership fixed per hospital ",
      "(first non-missing typectrl/cntrl). Q-weighted, hospital x cohort and ",
      "year x cohort fixed effects, SEs clustered by state. * p<0.10, ** p<0.05, ",
      "*** p<0.01."
    ),
    notation = "none"
  ) %>%
  writeLines("main_results_ownership_heterogeneity.tex")

cat("\nDone -- main_results_ownership_heterogeneity.tex saved.\n")
