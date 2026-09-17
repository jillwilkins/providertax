###########################################################
# Long-Difference Quantile Regression
# (renamed from quant_reg.R)
# Quantile difference-in-differences on the long difference (pre/post
# average per hospital x cohort sub-experiment), using stacked_data
# (5_buildstacked.R) to get a comparable control group. rqpd's "pfe"
# method and a plain rq() with factor(mcrnum) dummies were tried first and
# ruled out: pfe returned identical coefficients across every quantile (a
# package limitation, not a data issue), and the mcrnum-dummy version
# threw "possibly singular design" warnings from ~1,800 hospital dummies.
# This long-difference approach needs no individual hospital fixed effects
# at all. ANCOVA and full-stack alternatives were built and compared
# (quantreg_notes.md sections 8-9) but not kept as separate scripts --
# this long-difference version is the one retained going forward.
#
# STANDARD ERRORS are a state-level cluster bootstrap, not rq()'s default
# row-level bootstrap. Two reasons row-level is wrong here: (1) a hospital
# can contribute more than one row to ld_hosp_cohort (once per cohort it
# served as a control for), so rows aren't independent; (2) more
# fundamentally, treatment is assigned at the STATE level -- every
# hospital in an adopting state is treated simultaneously -- so state is
# the correct clustering level regardless (Abadie, Athey, Imbens &
# Wooldridge, 2017, "When Should You Adjust Standard Errors for
# Clustering?"), and it's the level every mean-based DiD model in this
# project already clusters at (5_buildstacked.R, 6_events_paper.R,
# main_results_table.R).
############################################################

options(scipen = 999)  # print numbers plain, not in scientific notation

library(dplyr)
library(quantreg)

# ------------------------------------------------------------------
# State-level cluster bootstrap for the treated coefficient
# ------------------------------------------------------------------
# Resamples whole states with replacement (every row for a resampled
# state comes along together), refits rq() on each resample, and uses the
# SD of the treated coefficient across resamples as its SE. `fit` supplies
# the point estimate (already estimated once on the real data); `formula`
# and `data` are used only to refit during the resampling loop.
cluster_boot_rq <- function(fit, formula, data, tau, cluster_var = "state", B = 500) {
  point_coef <- coef(fit)["treated", ]
  clusters   <- unique(data[[cluster_var]])

  boot_coefs <- matrix(NA_real_, nrow = B, ncol = length(tau))
  for (b in seq_len(B)) {
    sampled_clusters <- sample(clusters, length(clusters), replace = TRUE)
    boot_data <- bind_rows(lapply(sampled_clusters, function(cl) {
      data[data[[cluster_var]] == cl, , drop = FALSE]
    }))
    fit_b <- tryCatch(
      rq(formula, tau = tau, data = boot_data, weights = Q_weight),
      error = function(e) NULL
    )
    if (!is.null(fit_b) && "treated" %in% rownames(coef(fit_b))) {
      boot_coefs[b, ] <- coef(fit_b)["treated", ]
    }
  }

  se <- apply(boot_coefs, 2, sd, na.rm = TRUE)
  p  <- 2 * pnorm(-abs(point_coef / se))

  data.frame(tau = tau, estimate = point_coef, se = se, p_value = p, row.names = NULL)
}

ld_taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)

# ------------------------------------------------------------------
# Pre/Post Average ("Long-Difference") Quantile Regression, WITH a
# control group
# ------------------------------------------------------------------
# A never-treated hospital has no adoption year of its own, so to give it
# a comparable pre/post window we borrow one -- exactly what stacked_data
# already does: every hospital in it, treated or clean control, sits on a
# shared rel_year clock defined relative to its cohort's own adoption
# year. stacked_data itself is only read here, never modified, and
# everything derived from it is named ld_* so it can't collide with
# anything 5_buildstacked.R created.
# ------------------------------------------------------------------

ld_formula <- delta_op_bed ~ treated + exp_status + median_income_pre

ld_hosp_cohort <- stacked_data %>%
  group_by(mcrnum, df) %>%
  summarise(
    treated           = first(treated),
    state             = first(state),
    Q_weight          = first(Q_weight),
    exp_status        = mean(exp_status[rel_year < 0], na.rm = TRUE),
    median_income_pre = first(median_income_pre),
    pre_op_bed        = mean(op_bed[rel_year < 0],  na.rm = TRUE),
    post_op_bed       = mean(op_bed[rel_year >= 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(delta_op_bed = post_op_bed - pre_op_bed) %>%
  filter(!is.na(delta_op_bed))

cat("Long-difference rows:", nrow(ld_hosp_cohort),
    "| treated:", sum(ld_hosp_cohort$treated == 1),
    "| control:", sum(ld_hosp_cohort$treated == 0), "\n")

# treated now has real 0/1 variation, so its coefficient at each tau is an
# actual quantile difference-in-differences estimate.
ld_fit <- rq(ld_formula, tau = ld_taus, data = ld_hosp_cohort, weights = Q_weight)
print(coef(ld_fit))

set.seed(2026)
ld_boot_op_bed <- cluster_boot_rq(ld_fit, ld_formula, ld_hosp_cohort, ld_taus)
cat("\n=== op_bed: state-cluster bootstrap ===\n")
print(ld_boot_op_bed)

# ------------------------------------------------------------------
# Long-Difference Quantile Regression: npr_bed
# ------------------------------------------------------------------
ld_npr_formula <- delta_npr_bed ~ treated + exp_status + median_income_pre

ld_npr_hosp_cohort <- stacked_data %>%
  group_by(mcrnum, df) %>%
  summarise(
    treated           = first(treated),
    state             = first(state),
    Q_weight          = first(Q_weight),
    exp_status        = mean(exp_status[rel_year < 0], na.rm = TRUE),
    median_income_pre = first(median_income_pre),
    pre_npr_bed       = mean(npr_bed[rel_year < 0],  na.rm = TRUE),
    post_npr_bed      = mean(npr_bed[rel_year >= 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(delta_npr_bed = post_npr_bed - pre_npr_bed) %>%
  filter(!is.na(delta_npr_bed))

cat("Long-difference rows (npr_bed):", nrow(ld_npr_hosp_cohort),
    "| treated:", sum(ld_npr_hosp_cohort$treated == 1),
    "| control:", sum(ld_npr_hosp_cohort$treated == 0), "\n")

ld_npr_fit <- rq(ld_npr_formula, tau = ld_taus, data = ld_npr_hosp_cohort, weights = Q_weight)
print(coef(ld_npr_fit))

set.seed(2026)
ld_boot_npr_bed <- cluster_boot_rq(ld_npr_fit, ld_npr_formula, ld_npr_hosp_cohort, ld_taus)
cat("\n=== npr_bed: state-cluster bootstrap ===\n")
print(ld_boot_npr_bed)

# ------------------------------------------------------------------
# Long-Difference Quantile Regression: mcaid_prop_discharges
# ------------------------------------------------------------------
ld_mcaid_formula <- delta_mcaid_prop_dis ~ treated + exp_status + median_income_pre

ld_mcaid_hosp_cohort <- stacked_data %>%
  group_by(mcrnum, df) %>%
  summarise(
    treated             = first(treated),
    state               = first(state),
    Q_weight            = first(Q_weight),
    exp_status          = mean(exp_status[rel_year < 0], na.rm = TRUE),
    median_income_pre   = first(median_income_pre),
    pre_mcaid_prop_dis  = mean(mcaid_prop_discharges[rel_year < 0],  na.rm = TRUE),
    post_mcaid_prop_dis = mean(mcaid_prop_discharges[rel_year >= 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(delta_mcaid_prop_dis = post_mcaid_prop_dis - pre_mcaid_prop_dis) %>%
  filter(!is.na(delta_mcaid_prop_dis))

cat("Long-difference rows (mcaid_prop_discharges):", nrow(ld_mcaid_hosp_cohort),
    "| treated:", sum(ld_mcaid_hosp_cohort$treated == 1),
    "| control:", sum(ld_mcaid_hosp_cohort$treated == 0), "\n")

ld_mcaid_fit <- rq(ld_mcaid_formula, tau = ld_taus, data = ld_mcaid_hosp_cohort, weights = Q_weight)
print(coef(ld_mcaid_fit))

set.seed(2026)
ld_boot_mcaid <- cluster_boot_rq(ld_mcaid_fit, ld_mcaid_formula, ld_mcaid_hosp_cohort, ld_taus)
cat("\n=== mcaid_prop_discharges: state-cluster bootstrap ===\n")
print(ld_boot_mcaid)

# ------------------------------------------------------------------
# Long-Difference Quantile Regression: uncomp_bed (winsorized 1%-95%)
# ------------------------------------------------------------------
# Uses uncomp_bed_w95, not raw uncomp_bed -- matches results_tables/
# main_results_table.R's convention, so this outcome is on the same scale
# as the main results table everywhere in the paper. Built defensively
# (only if missing) with the identical winsorize_var() as that file.
if (!"uncomp_bed_w95" %in% names(stacked_data)) {
  winsorize_var <- function(x, probs = c(0.01, 0.95)) {
    bounds <- quantile(x, probs = probs, na.rm = TRUE)
    pmin(pmax(x, bounds[1]), bounds[2])
  }
  stacked_data <- stacked_data %>% mutate(uncomp_bed_w95 = winsorize_var(uncomp_bed, c(0.01, 0.95)))
}

ld_uncomp_formula <- delta_uncomp_bed_w95 ~ treated + exp_status + median_income_pre

ld_uncomp_hosp_cohort <- stacked_data %>%
  group_by(mcrnum, df) %>%
  summarise(
    treated             = first(treated),
    state               = first(state),
    Q_weight            = first(Q_weight),
    exp_status          = mean(exp_status[rel_year < 0], na.rm = TRUE),
    median_income_pre   = first(median_income_pre),
    pre_uncomp_bed_w95  = mean(uncomp_bed_w95[rel_year < 0],  na.rm = TRUE),
    post_uncomp_bed_w95 = mean(uncomp_bed_w95[rel_year >= 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(delta_uncomp_bed_w95 = post_uncomp_bed_w95 - pre_uncomp_bed_w95) %>%
  filter(!is.na(delta_uncomp_bed_w95))

cat("Long-difference rows (uncomp_bed_w95):", nrow(ld_uncomp_hosp_cohort),
    "| treated:", sum(ld_uncomp_hosp_cohort$treated == 1),
    "| control:", sum(ld_uncomp_hosp_cohort$treated == 0), "\n")

ld_uncomp_fit <- rq(ld_uncomp_formula, tau = ld_taus, data = ld_uncomp_hosp_cohort, weights = Q_weight)
print(coef(ld_uncomp_fit))

set.seed(2026)
ld_boot_uncomp <- cluster_boot_rq(ld_uncomp_fit, ld_uncomp_formula, ld_uncomp_hosp_cohort, ld_taus)
cat("\n=== uncomp_bed_w95: state-cluster bootstrap ===\n")
print(ld_boot_uncomp)

# ------------------------------------------------------------------
# Long-Difference Quantile Regression: op_margin
# ------------------------------------------------------------------
ld_margin_formula <- delta_op_margin ~ treated + exp_status + median_income_pre

ld_margin_hosp_cohort <- stacked_data %>%
  group_by(mcrnum, df) %>%
  summarise(
    treated           = first(treated),
    state             = first(state),
    Q_weight          = first(Q_weight),
    exp_status        = mean(exp_status[rel_year < 0], na.rm = TRUE),
    median_income_pre = first(median_income_pre),
    pre_op_margin     = mean(op_margin[rel_year < 0],  na.rm = TRUE),
    post_op_margin    = mean(op_margin[rel_year >= 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(delta_op_margin = post_op_margin - pre_op_margin) %>%
  filter(!is.na(delta_op_margin))

cat("Long-difference rows (op_margin):", nrow(ld_margin_hosp_cohort),
    "| treated:", sum(ld_margin_hosp_cohort$treated == 1),
    "| control:", sum(ld_margin_hosp_cohort$treated == 0), "\n")

ld_margin_fit <- rq(ld_margin_formula, tau = ld_taus, data = ld_margin_hosp_cohort, weights = Q_weight)
print(coef(ld_margin_fit))

set.seed(2026)
ld_boot_margin <- cluster_boot_rq(ld_margin_fit, ld_margin_formula, ld_margin_hosp_cohort, ld_taus)
cat("\n=== op_margin: state-cluster bootstrap ===\n")
print(ld_boot_margin)

# ------------------------------------------------------------------
# Combined results, all outcomes -- what longdiff_results_table.R reads
# ------------------------------------------------------------------
ld_all_results <- bind_rows(
  cbind(outcome = "op_bed",                ld_boot_op_bed),
  cbind(outcome = "npr_bed",               ld_boot_npr_bed),
  cbind(outcome = "mcaid_prop_discharges", ld_boot_mcaid),
  cbind(outcome = "uncomp_bed_w95",        ld_boot_uncomp),
  cbind(outcome = "op_margin",             ld_boot_margin)
) %>%
  mutate(stars = case_when(
    p_value < 0.01 ~ "***", p_value < 0.05 ~ "**", p_value < 0.10 ~ "*", TRUE ~ ""
  ))

cat("\n\n=== ALL OUTCOMES: treated COEFFICIENT, STATE-CLUSTER BOOTSTRAP ===\n")
print(ld_all_results, digits = 4, row.names = FALSE)
