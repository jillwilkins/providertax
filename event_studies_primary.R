# ==============================================================================
# EVENT STUDIES: PRIMARY OUTCOMES (remake of 6_events_paper.R)
# Same run_stacked_did() function and visual style as 6_events_paper.R
# (red pre-treatment / blue post-treatment, Q-weighted, same fixed effects
# and controls) -- just a bit more landscape than that script's 10x8 default
# (10x6.5 here), and covers a different/expanded outcome set:
#   - op_bed, npr_bed, op_margin, mcaid_prop_discharges, uncomp_bed, ucc2npr
#     (primary outcomes)
#   - ucc_charges: uncompensated care charges as a share of TOTAL charges
#     (tot_uncomp_care_charges / tot_charges -- different denominator than
#     ucc2npr, which is a share of net patient revenue)
#   - log_ucc_charges, log_mcaid_rev: log-transformed, computed here (a
#     handful of non-positive values become NA under log(), same as the
#     pre-existing log_op/log_npr columns already do)
#   - log_op, log_npr: already exist as columns in hospdata_analysis, used
#     as-is
# Overwrites the 5 outcome files this shares a name with in
# events_for_paper/ (op_bed, npr_bed, mcaid_dis, uncomp, ucc2npr) with the
# new landscape format -- that's the point of the "remake." New outcomes
# get new filenames.
# Requires stacked_data already in the environment (5_buildstacked.R).
# ==============================================================================

library(dplyr)
library(fixest)
library(ggplot2)

# ------------------------------------------------------------------------------
# Derived columns needed for the new outcomes, computed defensively
# ------------------------------------------------------------------------------
if (!"mcaid_dis_bed" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(mcaid_dis_bed = mcaid_discharges / pre_beds_avg)
}
if (!"nonm_dis_bed" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(nonm_dis_bed = nonm_discharges / pre_beds_avg)
}
if (!"ucc_charges" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(ucc_charges = tot_uncomp_care_charges / tot_charges)
}
if (!"log_ucc_charges" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(log_ucc_charges = log(tot_uncomp_care_charges))
}
if (!"log_mcaid_rev" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(log_mcaid_rev = log(net_mcaid_rev))
}

# ==============================================================================
# FUNCTION: RUN STACKED DiD AND CREATE PLOT (unchanged from 6_events_paper.R)
# ==============================================================================

run_stacked_did <- function(outcome_var,
                            outcome_label,
                            data = stacked_data,
                            controls = "exp_status + median_income_pre",
                             y_scale     = "comma",
                            weights = ~Q_weight,
                            filename = NULL,
                            width = 10,
                            height = 6.5) {

  # Build formula
  formula_str <- paste0(
    outcome_var, " ~ i(rel_year, treated, ref = -1) + ",
    controls, " | mcrnum^df + year^df"
  )

  # Estimate
  cat("\n=== Estimating Stacked DiD for:", outcome_var, "===\n")

  result <- feols(
    as.formula(formula_str),
    data = data,
    weights = weights,
    cluster = ~state
  )

  # Print summary
  print(summary(result))

  # Extract coefficients
  coef_data <- data.frame(
    rel_year = as.numeric(gsub(".*::(-?[0-9]+):.*", "\\1",
                               names(coef(result))[grepl("rel_year", names(coef(result)))])),
    coef = coef(result)[grepl("rel_year", names(coef(result)))],
    se = result$se[grepl("rel_year", names(coef(result)))]
  ) %>%
    mutate(
      ci_lower = coef - 1.96 * se,
      ci_upper = coef + 1.96 * se
    ) %>%
    bind_rows(data.frame(rel_year = -1, coef = 0, se = 0, ci_lower = 0, ci_upper = 0)) %>%
    arrange(rel_year) %>%
    mutate(
      period_color = ifelse(rel_year < 0, "Pre-Treatment", "Post-Treatment")
    )

  # Create plot
  p <- ggplot(coef_data, aes(x = rel_year, y = coef, color = period_color)) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                  width = 0.3, size = 0.8) +
    geom_point(size = 3) +
    scale_color_manual(values = c("Pre-Treatment" = "#DC4B4B", "Post-Treatment" = "#2484d2")) +
    geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.4) +
    labs(
      x = "Years Relative to Treatment",
      y = " "
    ) +
    scale_x_continuous(
                breaks = seq(min(coef_data$rel_year),
               max(coef_data$rel_year), 1)
) +
scale_y_continuous(
  labels = switch(y_scale,
    "comma"   = scales::comma,
    "percent" = scales::percent_format(accuracy = 0.1),
    "dollar"  = scales::dollar_format()
  )
) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
      axis.title.x = element_text(size = 12, margin = margin(t = 10)),
      axis.title.y = element_text(size = 12, margin = margin(r = 10)),
      axis.text = element_text(size = 11, color = "black"),
      panel.grid.major = element_line(color = "#f2f2f2", size = 0.3),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.position = "none"
    )

  print(p)

  # Save if filename provided
  if (!is.null(filename)) {
    ggsave(filename, plot = p, width = width, height = height, dpi = 300)
    cat("\nPlot saved to:", filename, "\n")
  }

  # Return results
  return(list(
    model = result,
    coef_data = coef_data,
    plot = p
  ))
}

# ==============================================================================
# RUN FOR PRIMARY + NEW OUTCOMES
# ==============================================================================

outcomes_primary <- list(
  list(var = "op_bed",       label = "Operating Expenses Per Bed",
       file = "events_for_paper/op_bed.png",  y_scale = "dollar"),

  list(var = "npr_bed",      label = "Net Patient Revenue Per Bed",
       file = "events_for_paper/npr_bed.png", y_scale = "dollar"),

  list(var = "op_margin",    label = "Operating Margin",
       file = "events_for_paper/op_margin.png", y_scale = "percent"),

  list(var = "mcaid_prop_discharges", label = "Medicaid Share of Discharges",
       file = "events_for_paper/mcaid_dis.png", y_scale = "percent"),

  list(var = "uncomp_bed",   label = "Uncompensated Care Charges Per Bed",
       file = "events_for_paper/uncomp.png", y_scale = "dollar"),

  list(var = "ucc2npr",      label = "Uncompensated Care Charges as a Share of Net Patient Revenue",
       file = "events_for_paper/ucc2npr.png", y_scale = "percent"),

  list(var = "ucc_charges",  label = "Uncompensated Care Charges as a Share of Total Charges",
       file = "events_for_paper/ucc_charges.png", y_scale = "percent"),

  list(var = "log_ucc_charges", label = "Log of Uncompensated Care Charges",
       file = "events_for_paper/log_ucc_charges.png", y_scale = "comma"),

  list(var = "log_op",       label = "Log of Operating Expenses",
       file = "events_for_paper/log_op.png", y_scale = "comma"),

  list(var = "log_npr",      label = "Log of Net Patient Revenue",
       file = "events_for_paper/log_npr.png", y_scale = "comma"),

  list(var = "log_mcaid_rev", label = "Log of Medicaid Revenue",
       file = "events_for_paper/log_mcaid_rev.png", y_scale = "comma")
)

results_primary <- list()

for (outcome in outcomes_primary) {
  results_primary[[outcome$var]] <- run_stacked_did(
    outcome_var   = outcome$var,
    outcome_label = outcome$label,
    y_scale       = outcome$y_scale,
    filename      = outcome$file,
    width         = 10,
    height        = 6.5
  )
}

cat("\nDone -- all primary event studies saved to events_for_paper/\n")
