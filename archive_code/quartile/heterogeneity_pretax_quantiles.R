# ==============================================================================
# HETEROGENEITY BY PRE-TAX HOSPITAL TYPE: POOLED QUANTILE-INTERACTED EVENT
# STUDY
#
# Does the tax's effect on the 4 main outcomes (op_bed, npr_bed,
# mcaid_dis_bed, ucc2npr) differ depending on a hospital's PRE-TAX level of
# Medicaid share (mcaid_prop_discharges) or operating margin (op_margin)?
# Two separate univariate splits -- not a joint grid, which would leave too
# few hospitals per cell. Each dimension is run at two granularities
# (deciles and quintiles) so both a smooth and a more stable-per-bin picture
# are available.
#
# Design choice: a single pooled regression per outcome, interacting the
# treated effect with the hospital's quantile bin of the baseline covariate,
# rather than partitioning stacked_data into separate samples per bin. This
# keeps the FULL shared control pool as the counterfactual for every bin,
# and lets us run a Wald test on whether the bin-specific effects actually
# differ from each other (vs. just eyeballing whether point estimates look
# different).
#
# Pre-tax level is defined exactly like pre_beds_avg in 2_dataclean.R: the
# mean value over years before the hospital's own gname (or all years, for
# never-treated hospitals). This makes it a fixed hospital "type" rather
# than something recomputed per sub-experiment -- a not-yet-treated control
# hospital gets the same bin label in every cohort's sub-experiment it
# appears in.
#
# NAMESPACED: builds stacked_data_het as its own object. Never reassigns
# stacked_data itself, so 6_events_paper.R and everything else downstream
# of 5_buildstacked.R is untouched.
#
# Requires hospdata_analysis and stacked_data (from 5_buildstacked.R)
# already in the environment.
# ==============================================================================
library(dplyr)
library(fixest)
library(ggplot2)
library(scales)

# ------------------------------------------------------------------------------
# 1. Pre-tax hospital "type" -- same convention as pre_beds_avg (2_dataclean.R)
# ------------------------------------------------------------------------------
hosp_pretax_chars <- hospdata_analysis %>%
  group_by(mcrnum) %>%
  arrange(year) %>%
  mutate(
    pre_mcaid_share = case_when(
      gname == 0 ~ mean(mcaid_prop_discharges, na.rm = TRUE),
      TRUE ~ mean(mcaid_prop_discharges[year < gname], na.rm = TRUE)
    ),
    pre_op_margin_lvl = case_when(
      gname == 0 ~ mean(op_margin, na.rm = TRUE),
      TRUE ~ mean(op_margin[year < gname], na.rm = TRUE)
    )
  ) %>%
  ungroup() %>%
  distinct(mcrnum, pre_mcaid_share, pre_op_margin_lvl)

cat("Hospitals with pre-tax characteristics:", nrow(hosp_pretax_chars), "\n")
cat("Missing pre_mcaid_share:  ", sum(is.na(hosp_pretax_chars$pre_mcaid_share)), "\n")
cat("Missing pre_op_margin_lvl:", sum(is.na(hosp_pretax_chars$pre_op_margin_lvl)), "\n")

# Bin cutpoints from the distinct hospital-level distribution (not the
# row-duplicated stacked_data), so hospitals appearing in many years/
# sub-experiments don't get overweighted when computing the cutpoints.
hosp_pretax_chars <- hosp_pretax_chars %>%
  mutate(
    mcaidshare_decile   = ntile(pre_mcaid_share, 10),
    mcaidshare_quintile = ntile(pre_mcaid_share, 5),
    opmargin_decile     = ntile(pre_op_margin_lvl, 10),
    opmargin_quintile   = ntile(pre_op_margin_lvl, 5)
  )

# ------------------------------------------------------------------------------
# 2. Join onto stacked_data -- new object, stacked_data itself untouched
# ------------------------------------------------------------------------------
stacked_data_het <- stacked_data %>%
  left_join(hosp_pretax_chars, by = "mcrnum") %>%
  mutate(mcaid_dis_bed = mcaid_discharges / pre_beds_avg) %>%   # in case this runs before 6_events_paper.R
  filter(!is.na(pre_mcaid_share), !is.na(pre_op_margin_lvl))

cat("\nstacked_data_het rows:", nrow(stacked_data_het),
    "| hospitals:", n_distinct(stacked_data_het$mcrnum), "\n")

# Sequential (light -> dark), single-hue, continuous color scale for the
# ORDERED bins -- endpoints from ColorBrewer "Blues" (colorblind-safe),
# distinct from the red/blue pre/post pair used elsewhere in this project
# so the two encodings aren't confused. Continuous (not discrete) so it
# scales cleanly to both 5 and 10 bins without a cluttered legend.
seq_low  <- "#c6dbef"
seq_high <- "#08306b"

# ------------------------------------------------------------------------------
# 3. Helpers: Wald-test contrast matrices for n arbitrary bins
# ------------------------------------------------------------------------------
build_equal_to_first_matrix <- function(n_bins) {
  R <- matrix(0, nrow = n_bins - 1, ncol = n_bins)
  R[, 1] <- -1
  for (k in 2:n_bins) R[k - 1, k] <- 1
  R
}

# ------------------------------------------------------------------------------
# 4. Dynamic quantile-interacted event study
#    .event combines rel_year x bin for treated hospitals (excluding the ref
#    year); every control obs AND every treated obs at rel_year==-1
#    collapses to a single "ref" category -- the omitted baseline, same role
#    -1 plays in i(rel_year, treated, ref=-1) elsewhere in this project.
# ------------------------------------------------------------------------------
run_quantile_event_study <- function(outcome_var, quantile_var, n_bins, outcome_label,
                                      data = stacked_data_het,
                                      controls = "exp_status + median_income_pre",
                                      weights = ~Q_weight,
                                      y_scale = "comma",
                                      filename = NULL) {

  data <- data %>%
    mutate(
      .event = case_when(
        treated == 1 & rel_year != -1 ~ paste0("r", rel_year, "_q", .data[[quantile_var]]),
        TRUE ~ "ref"
      )
    )

  formula_str <- paste0(outcome_var, " ~ i(.event, ref = 'ref') + ", controls,
                        " | mcrnum^df + year^df")

  cat("\n=== Estimating quantile-interacted event study:", outcome_var, "x", quantile_var, "===\n")
  result <- tryCatch(
    feols(as.formula(formula_str), data = data, weights = weights, cluster = ~state),
    error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(result)) return(NULL)
  print(summary(result))

  coef_names <- names(coef(result))
  is_event   <- grepl("^\\.event::r", coef_names)

  coef_data <- data.frame(
    rel_year = as.numeric(gsub("^r(-?[0-9]+)_q[0-9]+$", "\\1", sub("^\\.event::", "", coef_names[is_event]))),
    bin      = as.numeric(gsub("^r-?[0-9]+_q([0-9]+)$", "\\1", sub("^\\.event::", "", coef_names[is_event]))),
    coef     = coef(result)[is_event],
    se       = result$se[is_event]
  ) %>%
    mutate(ci_lower = coef - 1.96 * se, ci_upper = coef + 1.96 * se) %>%
    bind_rows(
      expand.grid(rel_year = -1, bin = 1:n_bins) %>%
        mutate(coef = 0, se = 0, ci_lower = 0, ci_upper = 0)
    ) %>%
    arrange(bin, rel_year)

  p <- ggplot(coef_data, aes(x = rel_year, y = coef, color = bin, group = bin)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "gray50") +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2) +
    scale_color_gradient(
      low = seq_low, high = seq_high,
      breaks = c(1, n_bins), labels = c("Lowest pre-tax", "Highest pre-tax"),
      name = NULL
    ) +
    labs(x = "Years Relative to Treatment", y = outcome_label) +
    scale_x_continuous(breaks = seq(min(coef_data$rel_year), max(coef_data$rel_year), 1)) +
    scale_y_continuous(labels = switch(y_scale,
      "comma" = scales::comma, "percent" = scales::percent_format(accuracy = 0.1),
      "dollar" = scales::dollar_format())) +
    theme_minimal(base_size = 14) +
    theme(
      axis.title.x = element_text(size = 12, margin = margin(t = 10)),
      axis.title.y = element_text(size = 12, margin = margin(r = 10)),
      axis.text    = element_text(size = 11, color = "black"),
      panel.grid.major = element_line(color = "#f2f2f2", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background  = element_rect(fill = "transparent", color = NA),
      legend.position  = "top"
    )

  if (!is.null(filename)) {
    ggsave(filename, plot = p, width = 9, height = 6, dpi = 300)
    cat("Plot saved to:", filename, "\n")
  }

  list(model = result, coef_data = coef_data, plot = p)
}

# ------------------------------------------------------------------------------
# 5. Simplified ATT-per-bin + formal test of heterogeneity
#    Collapses the event-study dynamics to a single post-period ATT per bin
#    (treated x post, by bin), so "is the heterogeneity real" reduces to a
#    small, interpretable Wald test instead of a huge joint test on the
#    full dynamic path.
# ------------------------------------------------------------------------------
run_quantile_att_test <- function(outcome_var, quantile_var, n_bins,
                                   data = stacked_data_het,
                                   controls = "exp_status + median_income_pre",
                                   weights = ~Q_weight) {

  data <- data %>%
    mutate(
      post = as.integer(rel_year >= 0),
      .treat_post_q = case_when(
        treated == 1 & post == 1 ~ paste0("q", .data[[quantile_var]]),
        TRUE ~ "ref"
      )
    )

  formula_str <- paste0(outcome_var, " ~ i(.treat_post_q, ref = 'ref') + ", controls,
                        " | mcrnum^df + year^df")

  result <- tryCatch(
    feols(as.formula(formula_str), data = data, weights = weights, cluster = ~state),
    error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(result)) return(list(atts = NULL, joint = NULL))

  b <- coef(result); V <- vcov(result)
  qnames <- paste0(".treat_post_q::q", 1:n_bins)
  idx <- match(qnames, names(b))

  atts <- data.frame(
    outcome      = outcome_var,
    quantile_var = quantile_var,
    n_bins       = n_bins,
    bin          = 1:n_bins,
    att          = b[idx],
    se           = sqrt(diag(V))[idx]
  )

  if (any(is.na(idx))) {
    cat("WARNING: some bins missing coefficients for", outcome_var, quantile_var, "\n")
    joint <- data.frame(outcome = outcome_var, quantile_var = quantile_var, n_bins = n_bins,
                         chisq_joint = NA, p_joint_all_equal = NA,
                         diff_extreme = NA, se_diff_extreme = NA, p_extreme = NA)
    return(list(atts = atts, joint = joint))
  }

  R <- build_equal_to_first_matrix(n_bins)
  Rb  <- R %*% b[idx]
  RVR <- R %*% V[idx, idx] %*% t(R)
  chisq_joint <- as.numeric(t(Rb) %*% solve(RVR) %*% Rb)
  p_joint <- pchisq(chisq_joint, df = n_bins - 1, lower.tail = FALSE)

  R2 <- matrix(0, nrow = 1, ncol = n_bins); R2[1, 1] <- 1; R2[1, n_bins] <- -1
  diff_extreme <- as.numeric(R2 %*% b[idx])
  se_extreme   <- sqrt(as.numeric(R2 %*% V[idx, idx] %*% t(R2)))
  p_extreme    <- pchisq((diff_extreme / se_extreme)^2, df = 1, lower.tail = FALSE)

  joint <- data.frame(
    outcome = outcome_var, quantile_var = quantile_var, n_bins = n_bins,
    chisq_joint = chisq_joint, p_joint_all_equal = p_joint,
    diff_extreme = diff_extreme, se_diff_extreme = se_extreme, p_extreme = p_extreme
  )

  list(atts = atts, joint = joint)
}

# ------------------------------------------------------------------------------
# 6. Run for all 4 main outcomes x 2 pre-tax dimensions x 2 granularities
# ------------------------------------------------------------------------------
outcomes <- list(
  list(var = "op_bed",        label = "Operating Expenses Per Bed",               y_scale = "dollar"),
  list(var = "npr_bed",       label = "Net Patient Revenue Per Bed",              y_scale = "dollar"),
  list(var = "mcaid_dis_bed", label = "Medicaid Discharges Per Bed",              y_scale = "comma"),
  list(var = "ucc2npr",       label = "Uncompensated Care / Net Patient Revenue", y_scale = "percent")
)

quantile_specs <- list(
  list(var = "mcaidshare_decile",   n_bins = 10, tag = "mcaidshare_decile"),
  list(var = "mcaidshare_quintile", n_bins = 5,  tag = "mcaidshare_quintile"),
  list(var = "opmargin_decile",     n_bins = 10, tag = "opmargin_decile"),
  list(var = "opmargin_quintile",   n_bins = 5,  tag = "opmargin_quintile")
)

atts_all   <- list()
joint_all  <- list()

for (qs in quantile_specs) {
  for (oc in outcomes) {
    key <- paste0(oc$var, "_", qs$tag)

    es <- run_quantile_event_study(
      outcome_var   = oc$var,
      quantile_var  = qs$var,
      n_bins        = qs$n_bins,
      outcome_label = oc$label,
      y_scale       = oc$y_scale,
      filename      = paste0("quartile/heterogeneity_", oc$var, "_by_", qs$tag, ".png")
    )

    att_test <- run_quantile_att_test(
      outcome_var  = oc$var,
      quantile_var = qs$var,
      n_bins       = qs$n_bins
    )
    atts_all[[key]]  <- att_test$atts
    joint_all[[key]] <- att_test$joint
  }
}

atts_all  <- bind_rows(atts_all)
joint_all <- bind_rows(joint_all)

cat("\n=== FORMAL TEST SUMMARY: DOES THE ATT DIFFER ACROSS PRE-TAX BINS? ===\n")
print(joint_all, row.names = FALSE)

write.csv(atts_all,  "quartile/heterogeneity_pretax_quantile_atts.csv", row.names = FALSE)
write.csv(joint_all, "quartile/heterogeneity_pretax_quantile_jointtests.csv", row.names = FALSE)
cat("\nSaved: quartile/heterogeneity_pretax_quantile_atts.csv\n")
cat("Saved: quartile/heterogeneity_pretax_quantile_jointtests.csv\n")
