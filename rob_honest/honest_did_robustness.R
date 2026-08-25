# ==============================================================================
# HONEST DID (RAMBACHAN & ROTH 2023) PARALLEL TRENDS ROBUSTNESS CHECKS
# For four main-results outcomes: op_bed, npr_bed, mcaid_prop_discharges,
# uncomp_bed. Adapts the extraction/sensitivity-analysis pattern already
# prototyped in weights.R, but built on the CANONICAL stacked_data (window=4,
# from 5_buildstacked.R) rather than rebuilding a separate window -- keeps
# this consistent with every other table/figure in the paper, which all sit
# on the same stacked sample.
#
# With window=4, ref=-1, there are 3 usable pre-periods (rel_year -4,-3,-2)
# and 5 post-periods (rel_year 0-4). That's thinner than ideal for HonestDiD
# (more pre-periods gives a better-calibrated bound), but workable -- flagged
# explicitly in the output.
#
# Target estimand: run TWO single-period targets per outcome -- rel_year=0
# (effect at adoption) and rel_year=3 (a later, more mature effect) -- rather
# than an unweighted average across all post-periods. An unweighted average
# treats far-out, thinly-covered periods (which have far fewer contributing
# cohorts and much wider individual SEs -- established repeatedly elsewhere
# this session) as equally important as the well-estimated near-term
# coefficients, which inflates the average's variance and produces a CI not
# comparable to main_results_table.R's pooled treated:post ATT. Single-period
# targets avoid that. Comparing e=0 vs. e=3 directly tests whether these
# effects (which, per the event studies built earlier this session, tend to
# build up over the post-period rather than appear immediately) are more or
# less robust to a parallel-trends violation once they've had time to mature.
#
# For each outcome x target period, reports:
#   - Relative magnitudes (Mbar) sensitivity: how the 95% CI for that
#     period's effect widens as the permitted post-period parallel-trends
#     violation grows (as a multiple of the max pre-period violation)
#   - Breakdown Mbar: the smallest Mbar at which the CI starts to include 0
#     -- i.e. how much of a trends violation (relative to what's observed
#     pre-period) would be needed to overturn significance
#   - Smoothness (FLCI) restriction as a second, complementary bound
#
# Does NOT rebuild the stacked sample -- uses stacked_data (with Q_weight)
# as already built by 5_buildstacked.R. Run that first.
# Requires stacked_data already in the environment.
# ==============================================================================

library(dplyr)
library(fixest)
library(HonestDiD)
library(ggplot2)

outcomes <- list(
  list(var = "op_bed",                label = "Operating Expenses per Bed"),
  list(var = "npr_bed",               label = "Net Patient Revenue per Bed"),
  list(var = "mcaid_prop_discharges", label = "Medicaid Share of Discharges"),
  list(var = "uncomp_bed",            label = "Uncompensated Care per Bed")
)

target_periods <- c(0, 3)
output_dir <- "honest_did_figures"
if (!dir.exists(output_dir)) dir.create(output_dir)

run_honest_did <- function(outcome_var, outcome_label, target_rel_year,
                            data = stacked_data,
                            controls = "exp_status + median_income_pre",
                            mbar_grid = seq(0, 1, by = 0.25),
                            filename = NULL) {

  cat("\n\n========== HONEST DID:", outcome_label, "(", outcome_var, ") -- rel_year =", target_rel_year, "==========\n")

  formula_str <- paste0(
    outcome_var, " ~ i(rel_year, treated, ref = -1) + ", controls,
    " | mcrnum^df + year^df"
  )
  result <- feols(as.formula(formula_str), data = data, weights = ~Q_weight, cluster = ~state)

  cn <- names(coef(result))
  es_coefs <- grep("^rel_year::", cn, value = TRUE)

  betahat <- coef(result)[es_coefs]
  sigma   <- vcov(result, type = "cluster")[es_coefs, es_coefs]

  event_times <- as.numeric(gsub("rel_year::(-?[0-9]+):treated", "\\1", es_coefs))
  ord <- order(event_times)
  betahat <- betahat[ord]; sigma <- sigma[ord, ord]; event_times <- event_times[ord]

  n_pre  <- sum(event_times < -1)
  n_post <- sum(event_times >= 0)

  post_times <- sort(event_times[event_times >= 0])
  if (!target_rel_year %in% post_times) {
    cat("!! rel_year =", target_rel_year, "not available for", outcome_var, "-- skipping\n")
    return(NULL)
  }
  target_idx <- which(post_times == target_rel_year)
  cat("Pre-periods:", n_pre, "| Post-periods:", n_post, "| Target index:", target_idx, "\n")

  l_vec <- basisVector(target_idx, n_post)

  honest_relmag <- createSensitivityResults_relativeMagnitudes(
    betahat = betahat, sigma = sigma,
    numPrePeriods = n_pre, numPostPeriods = n_post,
    Mbarvec = mbar_grid, l_vec = l_vec, alpha = 0.05
  )
  cat("\n--- Relative magnitudes sensitivity ---\n")
  print(honest_relmag)

  breakdown <- honest_relmag %>%
    filter(lb <= 0 & ub >= 0) %>%
    summarise(breakdown_mbar = min(Mbar)) %>%
    pull(breakdown_mbar)
  if (length(breakdown) == 0 || is.na(breakdown)) {
    cat("Breakdown Mbar: CI excludes 0 across the entire tested grid (0 to", max(mbar_grid), ")\n")
  } else {
    cat("Breakdown Mbar:", breakdown, "\n")
  }

  honest_smooth <- tryCatch(
    createSensitivityResults(
      betahat = betahat, sigma = sigma,
      numPrePeriods = n_pre, numPostPeriods = n_post,
      method = "FLCI", l_vec = l_vec, alpha = 0.05
    ),
    error = function(e) {
      cat("Smoothness (FLCI) restriction failed:", conditionMessage(e), "\n")
      NULL
    }
  )
  if (!is.null(honest_smooth)) {
    cat("\n--- Smoothness (FLCI) restriction ---\n")
    print(honest_smooth)
  }

  original_beta <- sum(l_vec * betahat[event_times >= 0])
  original_se   <- sqrt(t(l_vec) %*% sigma[event_times >= 0, event_times >= 0] %*% l_vec)
  original_ci <- data.frame(
    lb = original_beta - 1.96 * original_se,
    ub = original_beta + 1.96 * original_se,
    method = "Standard (Mbar=0)", Mbar = 0
  )

  p <- createSensitivityPlot_relativeMagnitudes(honest_relmag, original_ci) +
    labs(
      title    = paste0(outcome_label, " (rel_year = ", target_rel_year, ")"),
      subtitle = "Mbar = max post-period parallel-trends violation, relative to the largest observed pre-period violation",
      x        = "Mbar",
      y        = paste0("95% Confidence Set for ATT at rel_year = ", target_rel_year)
    ) +
    theme_minimal() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50")

  print(p)
  if (!is.null(filename)) {
    ggsave(filename, plot = p, width = 8, height = 6, dpi = 300)
    cat("Saved:", filename, "\n")
  }

  list(model = result, honest_relmag = honest_relmag, honest_smooth = honest_smooth,
       original_ci = original_ci,
       breakdown_mbar = if (length(breakdown) == 0) NA else breakdown, plot = p)
}

honest_did_results <- list()
for (o in outcomes) {
  for (t in target_periods) {
    key <- paste0(o$var, "_e", t)
    honest_did_results[[key]] <- run_honest_did(
      o$var, o$label, target_rel_year = t,
      filename = file.path(output_dir, paste0("honest_did_", o$var, "_e", t, ".png"))
    )
  }
}

cat("\n\n========== SUMMARY: BREAKDOWN MBAR BY OUTCOME AND TARGET PERIOD ==========\n")
summary_tbl <- do.call(rbind, lapply(outcomes, function(o) {
  do.call(rbind, lapply(target_periods, function(t) {
    key <- paste0(o$var, "_e", t)
    r <- honest_did_results[[key]]
    data.frame(
      Outcome = o$label,
      Rel_Year = t,
      Mbar0_LB = if (is.null(r)) NA else round(r$original_ci$lb, 4),
      Mbar0_UB = if (is.null(r)) NA else round(r$original_ci$ub, 4),
      Breakdown_Mbar = if (is.null(r)) NA else r$breakdown_mbar
    )
  }))
}))
print(summary_tbl, row.names = FALSE)

cat("\nDone -- honest DiD robustness checks complete.\n")
