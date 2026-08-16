# ==============================================================================
# HONEST DID: TWO 2x2 COMPOSITE FIGURES
# Combines the individual honest_did_robustness.R sensitivity plots (rel_year
# = 0 and 3 only) into two quad figures via patchwork:
#   - honestdid_financial.png: op_bed (top row) / npr_bed (bottom row),
#     each at rel_year = 0 (left) and rel_year = 3 (right)
#   - honestdid_patient.png: mcaid_prop_discharges (top row) / uncomp_bed
#     (bottom row), same column layout
# No overall title. Each panel keeps a short title identifying outcome +
# rel_year, but the Mbar-explanation subtitle is dropped and the x-axis is
# labeled "M" instead of "Mbar". Per-panel legends are removed and replaced
# with a single shared legend collected at the bottom of the composite.
#
# Does NOT rebuild the stacked sample -- uses stacked_data (with Q_weight)
# as already built by 5_buildstacked.R. Run that first.
# Requires stacked_data already in the environment.
# ==============================================================================

library(dplyr)
library(fixest)
library(HonestDiD)
library(ggplot2)
library(patchwork)

fit_honest_sensitivity <- function(outcome_var, target_rel_year, data = stacked_data,
                                    controls = "exp_status + median_income_pre",
                                    mbar_grid = seq(0, 1, by = 0.25)) {

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
  target_idx <- which(post_times == target_rel_year)
  l_vec <- basisVector(target_idx, n_post)

  honest_relmag <- createSensitivityResults_relativeMagnitudes(
    betahat = betahat, sigma = sigma,
    numPrePeriods = n_pre, numPostPeriods = n_post,
    Mbarvec = mbar_grid, l_vec = l_vec, alpha = 0.05
  )

  original_beta <- sum(l_vec * betahat[event_times >= 0])
  original_se   <- sqrt(t(l_vec) %*% sigma[event_times >= 0, event_times >= 0] %*% l_vec)
  original_ci <- data.frame(
    lb = original_beta - 1.96 * original_se,
    ub = original_beta + 1.96 * original_se,
    method = "Standard (M=0)", Mbar = 0
  )

  list(honest_relmag = honest_relmag, original_ci = original_ci)
}

# Panel-level plot: title only (outcome + period), no subtitle, x-axis "M",
# individual legend stripped (collected at the composite level instead),
# plain (non-scientific) y-axis labels, extra margin around each panel so
# the combined 2x2 grid has visible breathing room between panels
panel_plot <- function(outcome_var, outcome_label, target_rel_year) {
  fit <- fit_honest_sensitivity(outcome_var, target_rel_year)
  createSensitivityPlot_relativeMagnitudes(fit$honest_relmag, fit$original_ci) +
    labs(
      title = paste0(outcome_label, ", rel_year = ", target_rel_year),
      x     = "M",
      y     = "95% CI",
      color = NULL
    ) +
    scale_y_continuous(labels = scales::label_comma()) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.margin = margin(t = 14, r = 20, b = 14, l = 10)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50")
}

build_quad_figure <- function(top_var, top_label, bottom_var, bottom_label, filename) {
  p_top_left     <- panel_plot(top_var,    top_label,    0)
  p_top_right    <- panel_plot(top_var,    top_label,    3)
  p_bottom_left  <- panel_plot(bottom_var, bottom_label, 0)
  p_bottom_right <- panel_plot(bottom_var, bottom_label, 3)

  # Re-add one legend just to the first panel so patchwork has something to
  # collect -- the rest stay legend-free via panel_plot()'s theme() above
  p_top_left <- p_top_left + theme(legend.position = "bottom")

  combined <- (p_top_left | p_top_right) / (p_bottom_left | p_bottom_right) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

  print(combined)
  ggsave(filename, plot = combined, width = 11, height = 9, dpi = 300)
  cat("Saved:", filename, "\n")
  combined
}

cat("\n=== Building honestdid_financial.png (op_bed / npr_bed) ===\n")
fig_financial <- build_quad_figure(
  "op_bed",  "Operating Expenses per Bed",
  "npr_bed", "Net Patient Revenue per Bed",
  "honestdid_financial.png"
)

cat("\n=== Building honestdid_patient.png (mcaid_prop_discharges / uncomp_bed) ===\n")
fig_patient <- build_quad_figure(
  "mcaid_prop_discharges", "Medicaid Share of Discharges",
  "uncomp_bed",            "Uncompensated Care per Bed",
  "honestdid_patient.png"
)

cat("\nDone -- both quad figures saved.\n")
