# ==============================================================================
# ALTERNATIVE ESTIMATORS COMPARISON -- POOLED ATT
# One ATT + 95% CI per estimator, one panel per outcome:
#   - Stacked DiD (main): the estimates REPORTED in
#     results_tables/main_results_table.tex (generated 2026-08-16), entered
#     by hand below rather than re-estimated. Re-running main_results_table.R
#     on the current code + hospdata_analysis.csv (byte-identical to the
#     Aug 16 backup) reproduces the non-per-bed outcomes but not the per-bed
#     ones exactly, so the figure uses the published numbers. Precision is
#     limited to what the table prints (e.g. Medicaid share 0.013 / 0.005).
#   - Callaway & Sant'Anna (2021), `did` package: att_gt with not-yet-treated
#     controls, aggte(type = "dynamic") restricted to e = -4..4, overall ATT
#     = average of the e = 0..4 effects (same +/-4 window as the stacked
#     design; see git history of this file for why the window matters).
#   - Staggered TWFE: naive two-way FE (hospital + year FE) with a single
#     post_treat indicator, no stacking/cohort correction.
#
# Uncompensated care is the winsorized uncomp_bed_w95 (1st/95th pctile), as
# in the main table. For C&S and TWFE the cutoffs are computed on
# stacked_data and applied to hosp_panel.
#
# C&S and TWFE run on hosp_panel: hospdata_analysis excluding the
# "Always (2004)" cohort, capped at year <= 2022 (5_buildstacked.R's yr_max),
# dropping hospitals whose gname changes across their own panel years;
# state fixed to first-observed value for clustering.
#
# Requires stacked_data (5_buildstacked.R) and hospdata_analysis.
# Outputs (in rob_alt/):
#   alt_estimators_comparison.png
#   alt_estimators_comparison.csv
# ==============================================================================

library(dplyr)
library(fixest)
library(purrr)
library(did)
library(ggplot2)

outcome_info <- list(
  list(var = "op_bed",                label = "Operating Expenses per Bed ($)"),
  list(var = "npr_bed",               label = "Net Patient Revenue per Bed ($)"),
  list(var = "mcaid_prop_discharges", label = "Medicaid Share of Discharges"),
  list(var = "uncomp_bed_w95",        label = "Uncompensated Care per Bed ($, winsorized)")
)

est_levels <- c("Stacked DiD (main)", "Callaway & Sant'Anna", "Staggered TWFE")

# ------------------------------------------------------------------------------
# Stacked DiD: published main-table estimates (main_results_table.tex, Aug 16)
# ------------------------------------------------------------------------------
main_published <- data.frame(
  outcome_var = c("op_bed", "npr_bed", "mcaid_prop_discharges", "uncomp_bed_w95"),
  Estimate    = c(35740,    33766,     0.013,                   -13265),
  SE          = c(16844,    16258,     0.005,                    7129)
)

# ------------------------------------------------------------------------------
# Data for C&S and TWFE
# ------------------------------------------------------------------------------
ucc_bounds <- quantile(stacked_data$uncomp_bed, c(0.01, 0.95), na.rm = TRUE)

hosp_panel <- hospdata_analysis %>%
  filter(cohort != "Always (2004)", year <= 2022) %>%
  mutate(uncomp_bed_w95 = pmin(pmax(uncomp_bed, ucc_bounds[1]), ucc_bounds[2]))

unstable_g <- hosp_panel %>%
  group_by(mcrnum) %>%
  summarise(n_g = n_distinct(gname), .groups = "drop") %>%
  filter(n_g > 1) %>%
  pull(mcrnum)
cat("Dropping", length(unstable_g), "hospitals with non-constant gname from hosp_panel:",
    paste(unstable_g, collapse = ", "), "\n")

hosp_panel <- hosp_panel %>%
  filter(!mcrnum %in% unstable_g) %>%
  group_by(mcrnum) %>%
  mutate(state_fixed = first(state[order(year)])) %>%
  ungroup() %>%
  mutate(gname = as.numeric(gname))   # did recodes never-treated to Inf; integer gname can't hold it

# ------------------------------------------------------------------------------
# Estimators
# ------------------------------------------------------------------------------
fit_cs <- function(outcome_var) {
  # Column renamed: aggte() evaluates get(gname) inside a data.table, so a
  # column literally named "gname" shadows the argument and errors.
  cs_result <- att_gt(
    yname = outcome_var, tname = "year", idname = "mcrnum", gname = "first_treat",
    data = hosp_panel %>% filter(!is.na(.data[[outcome_var]])) %>% rename(first_treat = gname),
    control_group = "notyettreated", clustervars = "state_fixed",
    allow_unbalanced_panel = TRUE, print_details = FALSE
  )
  agg <- aggte(cs_result, type = "dynamic", min_e = -4, max_e = 4, na.rm = TRUE)
  data.frame(Estimate = agg$overall.att, SE = agg$overall.se)
}

fit_twfe <- function(outcome_var) {
  m <- feols(as.formula(paste0(outcome_var, " ~ post_treat | mcrnum + year")),
             data = hosp_panel, cluster = ~state_fixed)
  data.frame(Estimate = unname(coef(m)["post_treat"]), SE = unname(se(m)["post_treat"]))
}

alt_estimator_results <- map_df(outcome_info, function(oi) {
  cat("\n===", oi$label, "===\n")
  bind_rows(
    main_published %>% filter(outcome_var == oi$var) %>% select(Estimate, SE) %>%
      mutate(Estimator = est_levels[1]),
    fit_cs(oi$var)   %>% mutate(Estimator = est_levels[2]),
    fit_twfe(oi$var) %>% mutate(Estimator = est_levels[3])
  ) %>% mutate(Outcome = oi$label, outcome_var = oi$var)
}) %>%
  mutate(
    lb = Estimate - 1.96 * SE,
    ub = Estimate + 1.96 * SE,
    Outcome   = factor(Outcome, levels = sapply(outcome_info, function(o) o$label)),
    Estimator = factor(Estimator, levels = est_levels)
  )

cat("\n\n=== ALTERNATIVE ESTIMATORS: POOLED ATT ===\n")
print(as.data.frame(alt_estimator_results), digits = 4)
write.csv(alt_estimator_results, "rob_alt/alt_estimators_comparison.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# Figure: one ATT + 95% CI per estimator, one panel per outcome
# ------------------------------------------------------------------------------
est_colors <- setNames(c("#2a78d6", "#eb6834", "#1baf7a"), est_levels)
est_shapes <- setNames(c(16, 17, 15), est_levels)

p <- ggplot(alt_estimator_results,
            aes(x = Estimator, y = Estimate, ymin = lb, ymax = ub,
                color = Estimator, shape = Estimator)) +
  geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
  geom_errorbar(width = 0, linewidth = 0.8) +
  geom_point(size = 3) +
  facet_wrap(~ Outcome, scales = "free_y", ncol = 2) +
  scale_color_manual(values = est_colors) +
  scale_shape_manual(values = est_shapes) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(x = NULL, y = "ATT and 95% CI", color = NULL, shape = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position    = "top",
    legend.text        = element_text(size = 12),
    axis.text.x        = element_blank(),
    strip.text         = element_text(face = "bold", size = 12, hjust = 0),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing      = unit(1.2, "lines"),
    axis.title         = element_text(color = "gray25")
  )

print(p)
ggsave("rob_alt/alt_estimators_comparison.png", plot = p, width = 10, height = 7, dpi = 300, bg = "white")
cat("\nSaved: rob_alt/alt_estimators_comparison.png\n")
