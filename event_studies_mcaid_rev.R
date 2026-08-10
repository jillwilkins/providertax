# ==============================================================================
# EVENT STUDIES: NET MEDICAID REVENUE OUTCOMES, RAW + WINSORIZED (1%/99%)
# Same run_stacked_did() function and visual style as event_studies_primary.R
# / event_studies_primary_wins.R. Four net Medicaid revenue outcomes, each
# run both raw (unwinsorized) and winsorized at 1%/99% (same level as
# event_studies_primary_wins.R -- NOT the 1%/95% level used for the
# uncompensated care outcomes):
#   - log_mcaid_rev:            log(net_mcaid_rev)
#   - mcaid_rev_bed:            net_mcaid_rev / pre_beds_avg
#   - mcaid_rev_per_mcaid_dis:  net_mcaid_rev / mcaid_discharges (contemporaneous)
#   - mcaid_rev_per_predis:     net_mcaid_rev / pre_mcaid_dis (hospital's own
#                                pre-period average Medicaid discharges --
#                                fixed denominator, avoids the "bad control"
#                                issue of dividing by a contemporaneous,
#                                treatment-affected discharge count)
# Raw versions saved to events_for_paper/, winsorized versions saved to
# events_wins_paper/ -- same folder convention as the other outcomes.
# Requires stacked_data already in the environment (5_buildstacked.R).
# ==============================================================================

library(dplyr)
library(fixest)
library(ggplot2)

# ------------------------------------------------------------------------------
# Derived columns needed, computed defensively
# ------------------------------------------------------------------------------
if (!"log_mcaid_rev" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(log_mcaid_rev = log(net_mcaid_rev))
}
if (!"mcaid_rev_bed" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(mcaid_rev_bed = net_mcaid_rev / pre_beds_avg)
}
if (!"mcaid_rev_per_mcaid_dis" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(mcaid_rev_per_mcaid_dis = net_mcaid_rev / mcaid_discharges)
}
if (!"pre_mcaid_dis" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>%
    mutate(pre_mcaid_dis = mean(mcaid_discharges[rel_year < 0], na.rm = TRUE), .by = mcrnum)
}
if (!"mcaid_rev_per_predis" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(mcaid_rev_per_predis = net_mcaid_rev / pre_mcaid_dis)
}

# ------------------------------------------------------------------------------
# Winsorize each at 1%/99% (same helper/level as event_studies_primary_wins.R)
# ------------------------------------------------------------------------------
winsorize_var <- function(x, probs = c(0.01, 0.99)) {
  lower <- quantile(x, probs[1], na.rm = TRUE)
  upper <- quantile(x, probs[2], na.rm = TRUE)

  x_win <- x
  x_win[x < lower] <- lower
  x_win[x > upper] <- upper

  return(x_win)
}

mcaid_rev_outcomes <- c("log_mcaid_rev", "mcaid_rev_bed", "mcaid_rev_per_mcaid_dis", "mcaid_rev_per_predis")

for (v in mcaid_rev_outcomes) {
  w_col <- paste0(v, "_w")
  if (!w_col %in% names(stacked_data)) {
    stacked_data[[w_col]] <- winsorize_var(stacked_data[[v]])
  }
}

# ==============================================================================
# FUNCTION: RUN STACKED DiD AND CREATE PLOT (unchanged from event_studies_primary.R)
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
# RUN: RAW VERSIONS (events_for_paper/) AND WINSORIZED VERSIONS (events_wins_paper/)
# ==============================================================================

mcaid_rev_info <- list(
  list(var = "log_mcaid_rev",            label = "Log of Medicaid Revenue",
       file_base = "log_mcaid_rev.png",  y_scale = "comma"),

  list(var = "mcaid_rev_bed",            label = "Net Medicaid Revenue per Bed",
       file_base = "mcaid_rev_bed.png",  y_scale = "dollar"),

  list(var = "mcaid_rev_per_mcaid_dis",  label = "Net Medicaid Revenue per Medicaid Discharge",
       file_base = "mcaid_rev_per_mcaid_dis.png", y_scale = "dollar"),

  list(var = "mcaid_rev_per_predis",     label = "Net Medicaid Revenue per Pre-Period Medicaid Discharge",
       file_base = "mcaid_rev_per_predis.png", y_scale = "dollar")
)

results_mcaid_rev <- list()

for (outcome in mcaid_rev_info) {
  # Raw
  results_mcaid_rev[[outcome$var]] <- run_stacked_did(
    outcome_var   = outcome$var,
    outcome_label = outcome$label,
    y_scale       = outcome$y_scale,
    filename      = paste0("events_for_paper/", outcome$file_base),
    width         = 10,
    height        = 6.5
  )

  # Winsorized 1%/99%
  results_mcaid_rev[[paste0(outcome$var, "_w")]] <- run_stacked_did(
    outcome_var   = paste0(outcome$var, "_w"),
    outcome_label = outcome$label,
    y_scale       = outcome$y_scale,
    filename      = paste0("events_wins_paper/", outcome$file_base),
    width         = 10,
    height        = 6.5
  )
}

cat("\nDone -- net Medicaid revenue event studies (raw + 1%/99% winsorized) saved.\n")
