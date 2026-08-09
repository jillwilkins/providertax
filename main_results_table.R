# ==============================================================================
# MAIN RESULTS TABLE
# Pooled ATT (single treated:post coefficient, Q-weighted, hospital x cohort
# and year x cohort fixed effects) for each primary outcome, with
# significance stars on the coefficient and the SE in parentheses on the
# line underneath. This is the "more sophisticated" ATT: a single pooled
# coefficient with a proper joint SE, not a post-hoc average of separately
# estimated rel_year dummies (see heterogeneity_analysis_notes.md and
# expansion_analysis_notes.md for the fuller writeup of why).
#
# Outcome set is 6_events_paper.R's actual event-study loop (mcaid_prop_
# discharges, nonm_prop_discharges, mcaid_dis_bed, nonm_dis_bed, uncomp_bed,
# ucc2npr, op_bed, npr_bed) plus op_margin and tot_discharges_bed (total
# discharges per bed, mcaid_discharges + nonm_discharges combined, same
# pre_beds_avg normalization). Uses nonm_* (non-Medicare/non-Medicaid), not
# table_for_pres.R's private_* -- private_prop_discharges is ~92% missing
# and fails to estimate; nonm_* doesn't have that problem and is what
# 6_events_paper.R already uses throughout.
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
#    (post, and the mcaid_dis_bed/nonm_dis_bed pair 6_events_paper.R also
#    adds) -- computed defensively, only if not already present.
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

n_hosp <- n_distinct(stacked_data$mcrnum[stacked_data$treated == 1])
cat("Stacked sample: cohorts =", n_distinct(stacked_data$df),
    "| hospitals =", n_distinct(stacked_data$mcrnum),
    "| treated hospitals =", n_hosp, "\n")

# ------------------------------------------------------------------------------
# 2. Outcome list: 6_events_paper.R's event-study set, plus op_margin
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
# 3. Pooled ATT model: single treated:post coefficient, Q-weighted
# ------------------------------------------------------------------------------
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

main_results_raw <- map_df(outcome_info, function(oi) {
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
    Outcome = oi$label,
    Mean    = pre_treat_mean(oi$var),
    Beta    = beta,
    SE      = se,
    Stars   = stars,
    stringsAsFactors = FALSE
  )
})

cat("\n=== MAIN RESULTS: POOLED ATT BY OUTCOME ===\n")
print(main_results_raw, digits = 4)

# ------------------------------------------------------------------------------
# 4. Format: coefficient + stars on one row, (SE) on the row underneath
# ------------------------------------------------------------------------------
main_results_formatted <- main_results_raw %>%
  mutate(
    Mean_fmt = ifelse(abs(Mean) >= 1000, scales::comma(Mean, accuracy = 1), sprintf("%.3f", Mean)),
    Beta_fmt = ifelse(abs(Beta) >= 1000, scales::comma(Beta, accuracy = 1), sprintf("%.3f", Beta)),
    SE_fmt   = ifelse(abs(SE) >= 1000, scales::comma(SE, accuracy = 1), sprintf("%.3f", SE)),
    Beta_Stars = paste0(Beta_fmt, Stars),
    SE_paren   = paste0("(", SE_fmt, ")")
  )

# Interleave beta/SE rows per outcome (a straight bind_rows + arrange can't
# distinguish which blank-Outcome SE row belongs under which beta row, so
# build the interleaving explicitly instead)
main_results_long <- do.call(rbind, lapply(seq_len(nrow(main_results_formatted)), function(i) {
  rbind(
    data.frame(Outcome = main_results_formatted$Outcome[i], Mean = main_results_formatted$Mean_fmt[i],
               Estimate = main_results_formatted$Beta_Stars[i], stringsAsFactors = FALSE),
    data.frame(Outcome = "", Mean = "",
               Estimate = main_results_formatted$SE_paren[i], stringsAsFactors = FALSE)
  )
}))

cat("\n=== FORMATTED (BETA / SE STACKED) ===\n")
print(main_results_long)

# ------------------------------------------------------------------------------
# 5. LaTeX table
# ------------------------------------------------------------------------------
main_results_long %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    caption   = "Main Results: Pooled Average Treatment Effects on the Treated",
    col.names = c("Outcome", "Pre-Treatment Mean", "ATT"),
    align     = c("l", "r", "r")
  ) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  add_footnote(
    paste0(
      "Pooled treated x post coefficient from a stacked difference-in-differences ",
      "model (Q-weighted, hospital x cohort and year x cohort fixed effects, ",
      "controls for state Medicaid expansion status and median income, SEs ",
      "clustered by state). Unique treated hospitals: ", n_hosp, ". ",
      "* p<0.10, ** p<0.05, *** p<0.01."
    ),
    notation = "none"
  ) %>%
  writeLines("main_results_table.tex")

cat("\nDone -- main_results_table.tex saved.\n")
