# ==============================================================================
# EVENT STUDIES: UNCOMPENSATED CARE OUTCOMES, WINSORIZED 1%/95%
# Same run_stacked_did() function and visual style as event_studies_primary.R
# / event_studies_primary_wins.R -- restricted to just the uncompensated-care
# related outcomes (uncomp_bed, ucc2npr, ucc_charges, log_ucc_charges), each
# winsorized at 1% bottom / 95% top (more aggressive top-trim than the
# 1%/99% version in event_studies_primary_wins.R) via the same
# winsorize_var() convention as wins.R. Saved separately to
# events_wins95_paper/ so all three versions (unwinsorized, 1/99, 1/95) sit
# side by side.
# Requires stacked_data already in the environment (5_buildstacked.R).
# ==============================================================================

library(dplyr)
library(fixest)
library(ggplot2)

# ------------------------------------------------------------------------------
# Derived columns needed for these outcomes, computed defensively
# ------------------------------------------------------------------------------
if (!"ucc_charges" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(ucc_charges = tot_uncomp_care_charges / tot_charges)
}
if (!"log_ucc_charges" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(log_ucc_charges = log(tot_uncomp_care_charges))
}

# ------------------------------------------------------------------------------
# Winsorize at 1% bottom / 95% top (same helper as wins.R)
# ------------------------------------------------------------------------------
winsorize_var <- function(x, probs = c(0.01, 0.95)) {
  lower <- quantile(x, probs[1], na.rm = TRUE)
  upper <- quantile(x, probs[2], na.rm = TRUE)

  x_win <- x
  x_win[x < lower] <- lower
  x_win[x > upper] <- upper

  return(x_win)
}

ucc_wins95_outcomes <- c("uncomp_bed", "ucc2npr", "ucc_charges", "log_ucc_charges")

for (v in ucc_wins95_outcomes) {
  stacked_data[[paste0(v, "_w95")]] <- winsorize_var(stacked_data[[v]], probs = c(0.01, 0.95))
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
# RUN FOR UNCOMPENSATED CARE OUTCOMES, 1%/95% WINSORIZED
# ==============================================================================

outcomes_ucc_wins95 <- list(
  list(var = "uncomp_bed_w95",   label = "Uncompensated Care Charges Per Bed",
       file = "events_wins95_paper/uncomp.png", y_scale = "dollar"),

  list(var = "ucc2npr_w95",      label = "Uncompensated Care Charges as a Share of Net Patient Revenue",
       file = "events_wins95_paper/ucc2npr.png", y_scale = "percent"),

  list(var = "ucc_charges_w95",  label = "Uncompensated Care Charges as a Share of Total Charges",
       file = "events_wins95_paper/ucc_charges.png", y_scale = "percent"),

  list(var = "log_ucc_charges_w95", label = "Log of Uncompensated Care Charges",
       file = "events_wins95_paper/log_ucc_charges.png", y_scale = "comma")
)

results_ucc_wins95 <- list()

for (outcome in outcomes_ucc_wins95) {
  results_ucc_wins95[[outcome$var]] <- run_stacked_did(
    outcome_var   = outcome$var,
    outcome_label = outcome$label,
    y_scale       = outcome$y_scale,
    filename      = outcome$file,
    width         = 10,
    height        = 6.5
  )
}

cat("\nDone -- uncompensated care event studies (1%/95% winsorized) saved to events_wins95_paper/\n")
