###########################################################
# Canay (2011) Two-Step Panel Quantile Regression
# A more literal implementation of Canay's actual estimator than
# longdiff.R's pre/post-average differencing:
#   Step 1: ordinary (mean) panel fixed-effects regression to estimate
#           each hospital's fixed effect alpha_i. Safe to estimate this
#           way because OLS fixed-effects has none of the incidental-
#           parameters problems that sank rqpd::pfe for quantile loss.
#   Step 2: subtract alpha_i-hat from the outcome, then run an ordinary
#           POOLED quantile regression on the adjusted outcome -- this is
#           where the quantile-specific slopes come from.
#
# Uses the raw (unstacked) panel, hosp_panel_pfe -- not stacked_data --
# since staggered timing is handled directly via tax_on + factor(year)
# instead of the cohort-relative rel_year clock. hosp_panel_pfe no longer
# exists elsewhere in the repo (it was removed from quant_reg.R/longdiff.R
# during cleanup), so it's rebuilt here from scratch.
#
# STANDARD ERRORS bootstrap the WHOLE two-step procedure (not just Step 2)
# -- alpha_i-hat is itself an estimated quantity from Step 1, and naive
# Step-2-only SEs would ignore that estimation error (the "generated
# regressor" problem). Resampling is by STATE, not hospital, for the same
# reason longdiff.R's bootstrap was switched to state: treatment is
# assigned at the state level. Each drawn instance of a state gets a
# unique hospital-ID suffix before refitting -- otherwise plm silently
# merges a state drawn twice into one larger group instead of treating it
# as two independent pseudo-draws, understating the bootstrap SE.
#
# Requires stacked_data already in the environment (only used to define
# which hospitals belong to the analysis sample).
############################################################

options(scipen = 999)

library(dplyr)
library(plm)
library(quantreg)

taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)

# ------------------------------------------------------------------
# Rebuild hosp_panel_pfe (raw panel, not stacked)
# ------------------------------------------------------------------
hospdata_analysis <- hospdata_analysis %>%
  mutate(tax_on = as.integer(gname != 0 & year >= gname))

hosp_panel_pfe <- hospdata_analysis %>%
  filter(cohort != "Always (2004)", mcrnum %in% unique(stacked_data$mcrnum)) %>%
  mutate(tax_on = as.integer(gname != 0 & year >= gname))

if (!"uncomp_bed_w95" %in% names(hosp_panel_pfe)) {
  winsorize_var <- function(x, probs = c(0.01, 0.95)) {
    bounds <- quantile(x, probs = probs, na.rm = TRUE)
    pmin(pmax(x, bounds[1]), bounds[2])
  }
  hosp_panel_pfe <- hosp_panel_pfe %>% mutate(uncomp_bed_w95 = winsorize_var(uncomp_bed, c(0.01, 0.95)))
}

if (!"mcaid_dis_bed" %in% names(hosp_panel_pfe)) {
  hosp_panel_pfe <- hosp_panel_pfe %>% mutate(mcaid_dis_bed = mcaid_discharges / pre_beds_avg)
}

cat("hosp_panel_pfe rows:", nrow(hosp_panel_pfe), "| hospitals:", n_distinct(hosp_panel_pfe$mcrnum), "\n")

# ------------------------------------------------------------------
# The two-step procedure, as a function of outcome name -- returns the
# tau-specific tax_on coefficients from Step 2.
# ------------------------------------------------------------------
run_canay <- function(outcome_var, data, tau) {
  step1_formula <- as.formula(paste0(outcome_var, " ~ tax_on + exp_status + median_income_pre + factor(year)"))
  fe_fit <- plm(step1_formula, data = data, index = "mcrnum", model = "within")

  alpha_hat  <- fixef(fe_fit, type = "level")
  adj_col    <- paste0(outcome_var, "_adj")
  data[[adj_col]] <- data[[outcome_var]] - alpha_hat[as.character(data$mcrnum)]

  step2_formula <- as.formula(paste0(adj_col, " ~ tax_on + exp_status + median_income_pre + factor(year)"))
  fit <- rq(step2_formula, tau = tau, data = data)
  coef(fit)["tax_on", ]
}

# ------------------------------------------------------------------
# State-level cluster bootstrap of the whole two-step procedure.
# Each drawn instance of a state gets ALL its hospitals' rows, relabeled
# with a unique suffix per draw so plm never sees the same mcrnum value
# used by two different (independent) pseudo-draws.
# ------------------------------------------------------------------
cluster_boot_canay <- function(outcome_var, data, tau, cluster_var = "state", B = 200) {
  point_coef <- run_canay(outcome_var, data, tau)

  clusters <- unique(data[[cluster_var]])
  boot_coefs <- matrix(NA_real_, nrow = B, ncol = length(tau))

  for (b in seq_len(B)) {
    sampled_clusters <- sample(clusters, length(clusters), replace = TRUE)
    boot_data <- bind_rows(lapply(seq_along(sampled_clusters), function(i) {
      sub <- data[data[[cluster_var]] == sampled_clusters[i], , drop = FALSE]
      sub$mcrnum <- paste0(sub$mcrnum, "_d", i)  # unique id per draw instance
      sub
    }))

    result <- tryCatch(run_canay(outcome_var, boot_data, tau), error = function(e) rep(NA_real_, length(tau)))
    boot_coefs[b, ] <- result
  }

  se <- apply(boot_coefs, 2, sd, na.rm = TRUE)
  p  <- 2 * pnorm(-abs(point_coef / se))
  data.frame(tau = tau, estimate = point_coef, se = se, p_value = p, row.names = NULL)
}

# ------------------------------------------------------------------
# Run for all 5 outcomes (same set as longdiff.R)
# ------------------------------------------------------------------
set.seed(2026)
cat("\n=== op_bed ===\n")
canay_op_bed <- cluster_boot_canay("op_bed", hosp_panel_pfe, taus)
print(canay_op_bed)

set.seed(2026)
cat("\n=== npr_bed ===\n")
canay_npr_bed <- cluster_boot_canay("npr_bed", hosp_panel_pfe, taus)
print(canay_npr_bed)

set.seed(2026)
cat("\n=== mcaid_prop_discharges ===\n")
canay_mcaid <- cluster_boot_canay("mcaid_prop_discharges", hosp_panel_pfe, taus)
print(canay_mcaid)

set.seed(2026)
cat("\n=== uncomp_bed_w95 ===\n")
canay_uncomp <- cluster_boot_canay("uncomp_bed_w95", hosp_panel_pfe, taus)
print(canay_uncomp)

set.seed(2026)
cat("\n=== op_margin ===\n")
canay_margin <- cluster_boot_canay("op_margin", hosp_panel_pfe, taus)
print(canay_margin)

set.seed(2026)
cat("\n=== mcaid_dis_bed ===\n")
canay_mcaid_dis_bed <- cluster_boot_canay("mcaid_dis_bed", hosp_panel_pfe, taus)
print(canay_mcaid_dis_bed)

# ------------------------------------------------------------------
# Combined results, all outcomes
# ------------------------------------------------------------------
canay_all_results <- bind_rows(
  cbind(outcome = "op_bed",                canay_op_bed),
  cbind(outcome = "npr_bed",               canay_npr_bed),
  cbind(outcome = "mcaid_prop_discharges", canay_mcaid),
  cbind(outcome = "uncomp_bed_w95",        canay_uncomp),
  cbind(outcome = "op_margin",             canay_margin),
  cbind(outcome = "mcaid_dis_bed",         canay_mcaid_dis_bed)
) %>%
  mutate(stars = case_when(
    p_value < 0.01 ~ "***", p_value < 0.05 ~ "**", p_value < 0.10 ~ "*", TRUE ~ ""
  ))

cat("\n\n=== ALL OUTCOMES: tax_on COEFFICIENT (Canay two-step), STATE-CLUSTER BOOTSTRAP ===\n")
print(canay_all_results, digits = 4, row.names = FALSE)
