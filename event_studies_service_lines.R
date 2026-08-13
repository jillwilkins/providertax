# ==============================================================================
# EVENT STUDIES: SERVICE LINE OFFERINGS
# Same run_stacked_did() function and visual style as event_studies_primary.R
# / 6_events_paper.R (Q-weighted, red pre-treatment / blue post-treatment).
# Ten AHA service-line outcomes:
#   - Binary flags (emdephos, psyemhos, alchhos, traumhos, adtchos,
#     iclabhos, eswlhos): linear probability model -- the coefficient is a
#     percentage-point change in the likelihood of offering that service,
#     same convention already used for mcaid_prop_discharges etc. elsewhere
#     in this project, not logit/probit.
#   - Counts (vem, psybd, alchbd): coefficient is a change in the raw count.
# Does NOT rebuild the stacked sample -- uses stacked_data (with Q_weight)
# as already built by 5_buildstacked.R. Run that first (hospdata_analysis
# now includes these AHA columns after the pipeline rebuild, so stacked_data
# will carry them through automatically -- no defensive column checks
# needed here).
# Requires stacked_data already in the environment.
# ==============================================================================

library(dplyr)
library(fixest)
library(ggplot2)

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

  formula_str <- paste0(
    outcome_var, " ~ i(rel_year, treated, ref = -1) + ",
    controls, " | mcrnum^df + year^df"
  )

  cat("\n=== Estimating Stacked DiD for:", outcome_var, "===\n")

  result <- feols(
    as.formula(formula_str),
    data = data,
    weights = weights,
    cluster = ~state
  )

  print(summary(result))

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

  if (!is.null(filename)) {
    ggsave(filename, plot = p, width = width, height = height, dpi = 300)
    cat("\nPlot saved to:", filename, "\n")
  }

  return(list(
    model = result,
    coef_data = coef_data,
    plot = p
  ))
}

# ==============================================================================
# RUN FOR ALL 10 SERVICE LINE OUTCOMES
# ==============================================================================

outcomes_service_lines <- list(
  list(var = "emdephos", label = "Emergency Department",
       file = "events_for_paper/emdephos.png", y_scale = "percent"),

  list(var = "psyemhos", label = "Psychiatric Emergency Services",
       file = "events_for_paper/psyemhos.png", y_scale = "percent"),

  list(var = "alchhos",  label = "Alcohol/Chemical Dependency Unit",
       file = "events_for_paper/alchhos.png", y_scale = "percent"),

  list(var = "traumhos", label = "Trauma Center",
       file = "events_for_paper/traumhos.png", y_scale = "percent"),

  list(var = "adtchos",  label = "Adult Day Care Program",
       file = "events_for_paper/adtchos.png", y_scale = "percent"),

  list(var = "iclabhos", label = "Cardiac Catheterization Lab",
       file = "events_for_paper/iclabhos.png", y_scale = "percent"),

  list(var = "eswlhos",  label = "Extracorporeal Shock Wave Lithotripsy (ESWL)",
       file = "events_for_paper/eswlhos.png", y_scale = "percent"),

  list(var = "vem",      label = "Emergency Department Visits",
       file = "events_for_paper/vem.png", y_scale = "comma"),

  list(var = "psybd",    label = "Psychiatric Beds",
       file = "events_for_paper/psybd.png", y_scale = "comma"),

  list(var = "alchbd",   label = "Alcohol/Chemical Dependency Beds",
       file = "events_for_paper/alchbd.png", y_scale = "comma")
)

results_service_lines <- list()

for (outcome in outcomes_service_lines) {
  results_service_lines[[outcome$var]] <- tryCatch(
    run_stacked_did(
      outcome_var   = outcome$var,
      outcome_label = outcome$label,
      y_scale       = outcome$y_scale,
      filename      = outcome$file,
      width         = 10,
      height        = 6.5
    ),
    error = function(e) {
      cat("\n!! FAILED for", outcome$var, ":", conditionMessage(e), "\n")
      NULL
    }
  )
}

cat("\nDone -- all service line event studies saved to events_for_paper/\n")
