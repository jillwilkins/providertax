# ==============================================================================
# ALTERNATIVE ESTIMATORS COMPARISON
# Compares the paper's main stacked-regression ATT against two alternative
# DiD estimators, for the four outcomes already used in the HonestDiD
# robustness checks (op_bed, npr_bed, mcaid_prop_discharges, uncomp_bed):
#   - Main (Stacked Regression): treated:post pooled ATT, same spec as
#     main_results_table.R (Q-weighted, hospital x cohort and year x cohort
#     FE, exp_status + median_income_pre controls, clustered by state)
#   - C&S: Callaway & Sant'Anna (2021), via the `did` package
#     (att_gt + aggte, type = "dynamic", event window +/-4, not-yet-treated
#     control group). The window restriction matters: type = "simple" pools
#     ATT(g,t) across the full 1999-2023 calendar panel, so early cohorts
#     get compared against calendar years 15-20+ years past their own
#     treatment date. With nominal dollar outcomes normalized by a bed count
#     fixed at each hospital's earliest pre-period value, that long span
#     lets ordinary cost growth compound into 15-20x-median outliers by
#     2020-2023 for a handful of large/growing hospitals, inflating the C&S
#     SE by an order of magnitude for no substantive reason -- the Main
#     model never makes a comparison outside its own +/-4 window, so it
#     never sees this. Restricting C&S to the same +/-4 window keeps the
#     comparison to Main apples-to-apples.
#   - Staggered TWFE: the naive two-way fixed effects regression (hospital +
#     year FE, single post_treat indicator, no cohort/stacking correction)
#     that the C&S/Sun-Abraham/Goodman-Bacon literature warns can be biased
#     under staggered adoption with heterogeneous treatment effects, since it
#     implicitly uses already-treated units as controls for later-treated
#     ones ("forbidden comparisons"). Included as the naive baseline the
#     other two estimators are meant to improve on.
# All three use RAW (unwinsorized) outcomes, matching main_results_table.R,
# so the "Main (Stacked Regression)" series here reproduces that table's
# published point estimates exactly.
#
# C&S and Staggered TWFE run on hosp_panel (hospdata_analysis, excluding the
# "Always (2004)" left-censored cohort -- same restriction the stacked
# design already applies, since those hospitals have no genuine pre-period --
# and capped at year <= 2022, matching 5_buildstacked.R's hardcoded
# yr_max = 2022, so C&S never sees 2023 data that stacked_data structurally
# never includes either).
# att_gt() requires a time-invariant cluster variable; 7 of 2,945 hospitals
# have a `state` value that changes over time (mcrnum reuse/relocation
# quirk), so each hospital's state is fixed to its first-observed value for
# clustering purposes -- same "fixed per hospital" convention already used
# for ownership elsewhere in this project.
#
# Does NOT rebuild the stacked sample -- uses stacked_data (with Q_weight)
# as already built by 5_buildstacked.R, and hospdata_analysis as already
# built by 4_datamedicaid.R. Run those first.
# ==============================================================================

library(dplyr)
library(fixest)
library(purrr)
library(did)
library(ggplot2)

outcome_info <- list(
  list(var = "op_bed",                label = "Operating Expenses per Bed"),
  list(var = "npr_bed",               label = "Net Patient Revenue per Bed"),
  list(var = "mcaid_prop_discharges", label = "Medicaid Share of Discharges"),
  list(var = "uncomp_bed",            label = "Uncompensated Care per Bed")
)

if (!"post" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(post = as.integer(rel_year >= 0))
}

hosp_panel <- hospdata_analysis %>% filter(cohort != "Always (2004)", year <= 2022)

state_fixed <- hosp_panel %>%
  arrange(mcrnum, year) %>%
  group_by(mcrnum) %>%
  summarise(state_fixed = first(state), .groups = "drop")
hosp_panel <- hosp_panel %>% left_join(state_fixed, by = "mcrnum")

# ------------------------------------------------------------------------------
# Per-estimator fit functions, each returning one row: Estimate, SE
# ------------------------------------------------------------------------------
fit_main <- function(outcome_var) {
  formula_str <- paste0(
    outcome_var,
    " ~ treated:post + i(rel_year, treated, ref = -1, keep = -4:-2) + exp_status + median_income_pre",
    " | mcrnum^df + year^df"
  )
  m <- feols(as.formula(formula_str), data = stacked_data, weights = ~Q_weight, cluster = ~state)
  cn <- names(coef(m))
  pt <- cn[grepl("treated.*post|post.*treated", cn)]
  data.frame(Estimate = as.numeric(coef(m)[pt]), SE = as.numeric(m$se[pt]))
}

fit_cs <- function(outcome_var) {
  cs_data <- hosp_panel %>% filter(!is.na(.data[[outcome_var]]))
  cs_result <- att_gt(
    yname = outcome_var, tname = "year", idname = "mcrnum", gname = "gname",
    data = cs_data, control_group = "notyettreated", clustervars = "state_fixed",
    print_details = FALSE
  )
  # type = "simple" pools ATT(g,t) across the FULL calendar panel (1999-2023),
  # so cohorts get compared against calendar years up to 20+ years from their
  # own treatment date. Combined with nominal dollar outcomes normalized by a
  # bed count fixed at each hospital's earliest pre-period value, that long
  # span lets ordinary cost growth compound into huge magnitudes by 2020-2023
  # for a handful of large/growing hospitals -- inflating the SE by 10-20x
  # relative to Main, which never makes a comparison outside its own +/-4
  # window. Restricting to the same +/-4 event-time window here (dynamic
  # aggregation, then averaging the post-period e=0:4 effects, which is what
  # aggte's own "overall" summary already does for type="dynamic") keeps the
  # comparison to Main apples-to-apples instead of penalizing C&S for
  # something the two estimators structurally don't share.
  cs_agg <- aggte(cs_result, type = "dynamic", min_e = -4, max_e = 4, na.rm = TRUE)
  data.frame(Estimate = as.numeric(cs_agg$overall.att), SE = as.numeric(cs_agg$overall.se))
}

fit_twfe <- function(outcome_var) {
  twfe_data <- hosp_panel %>% filter(!is.na(.data[[outcome_var]]))
  formula_str <- paste0(outcome_var, " ~ post_treat | mcrnum + year")
  m <- feols(as.formula(formula_str), data = twfe_data, cluster = ~state_fixed)
  cn <- names(coef(m))
  pt <- cn[grepl("post_treat", cn)]
  data.frame(Estimate = as.numeric(coef(m)[pt]), SE = as.numeric(m$se[pt]))
}

# ------------------------------------------------------------------------------
# Run all three estimators x four outcomes
# ------------------------------------------------------------------------------
alt_estimator_results <- map_df(outcome_info, function(oi) {
  cat("\n===", oi$label, "===\n")

  cat("  Main (Stacked Regression)...\n")
  r_main <- fit_main(oi$var) %>% mutate(Estimator = "Main (Stacked Regression)")

  cat("  C&S...\n")
  r_cs <- fit_cs(oi$var) %>% mutate(Estimator = "C&S")

  cat("  Staggered TWFE...\n")
  r_twfe <- fit_twfe(oi$var) %>% mutate(Estimator = "Staggered TWFE")

  bind_rows(r_main, r_cs, r_twfe) %>% mutate(Outcome = oi$label)
})

alt_estimator_results <- alt_estimator_results %>%
  mutate(
    lb = Estimate - 1.96 * SE,
    ub = Estimate + 1.96 * SE,
    Outcome   = factor(Outcome, levels = sapply(outcome_info, function(o) o$label)),
    Estimator = factor(Estimator, levels = c("Main (Stacked Regression)", "C&S", "Staggered TWFE"))
  )

cat("\n\n=== ALTERNATIVE ESTIMATORS: RESULTS ===\n")
print(alt_estimator_results, digits = 4)

# ------------------------------------------------------------------------------
# Figure: point + 95% CI, one panel per outcome, color-coded by estimator
# ------------------------------------------------------------------------------
p <- ggplot(alt_estimator_results, aes(x = Estimator, y = Estimate, ymin = lb, ymax = ub, color = Estimator)) +
  geom_pointrange(size = 0.6, linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~ Outcome, scales = "free_y") +
  scale_color_manual(values = c(
    "Main (Stacked Regression)" = "#2484d2",
    "C&S"                        = "gray50",
    "Staggered TWFE"             = "gray50"
  )) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(x = NULL, y = "Estimate and 95% CI", color = NULL) +
  theme_minimal() +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

print(p)
ggsave("alt_estimators_comparison.png", plot = p, width = 10, height = 7, dpi = 300)
cat("\nSaved: alt_estimators_comparison.png\n")
