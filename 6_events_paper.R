# ==============================================================================
# Script that prepares my event studies to include in third year paper (4/27)
# This is based on the stacked data set created in fix_stacked.R
# The figures are saved under the folder "events_for_paper"
# ==============================================================================

# ==============================================================================
# FUNCTION: RUN STACKED DiD AND CREATE PLOT
# ==============================================================================

run_stacked_did <- function(outcome_var, 
                            outcome_label,
                            data = stacked_data,
                            controls = "exp_status + median_income_pre",
                            #filter_condition = "state != 'HI'"" 
                             y_scale     = "comma",
                            filename = NULL,
                            width = 10,
                            height = 10) {
  
  # Build formula
  formula_str <- paste0(
    outcome_var, " ~ i(rel_year, treated, ref = -1) + ", 
    controls, " | mcrnum^df + year^df"
  )
  
  # Filter data
  #data_filtered <- data %>% filter(eval(parse(text = filter_condition)))
  
  # Estimate
  cat("\n=== Estimating Stacked DiD for:", outcome_var, "===\n")
  
  result <- feols(
    as.formula(formula_str),
    data = data,        # change to data_filtered if you want to apply the filter condition
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
      panel.grid.major = element_line(color = "grey90", size = 0.3),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
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
# RUN FOR MULTIPLE OUTCOMES
# ==============================================================================

# Define outcomes
outcomes <- list(
  list(var = "mcaid_prop_discharges", 
       label = "Medicaid Share of Discharges", 
       file = "events_for_paper/mcaid_dis.png"),

  list(var = "private_prop_discharges", 
       label = "Commercial Share of Discharges", 
       file = "events_for_paper/private_dis.png"),

  list(var = "uncomp_bed", 
       label = "Uncompensated Care Charges Per Bed", 
       file = "events_for_paper/uncomp.png"),
  
  list(var = "op_bed", 
       label = "Operating Expenses Per Bed", 
       file = "events_for_paper/op_bed.png"),

  list(var = "npr_bed", 
       label = "Net Patient Revenue Per Bed", 
       file = "events_for_paper/npr_bed.png") 
)

# Run for all outcomes
results <- list()

for (outcome in outcomes) {
  results[[outcome$var]] <- run_stacked_did(
    outcome_var = outcome$var,
    outcome_label = outcome$label,
    filename = outcome$file,
    width = 10,
    height = 8
  )
}


# =============================================================================
# State level analysis is done in the state_analysis.R script
# State level outcomes 
# =============================================================================

# ELIGIBILITY 

  # Extract coefficients
  coef_data <- data.frame(
    rel_year = as.numeric(gsub(".*::(-?[0-9]+):.*", "\\1", 
                               names(coef(state_result_elig))[grepl("rel_year", names(coef(state_result_elig)))])),
    coef = coef(state_result_elig)[grepl("rel_year", names(coef(state_result_elig)))],
    se = state_result_elig$se[grepl("rel_year", names(coef(state_result_elig)))]
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
  p_elig <- ggplot(coef_data, aes(x = rel_year, y = coef, color = period_color)) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                  width = 0.3, size = 0.8) +
    geom_point(size = 3) +
    scale_color_manual(values = c("Pre-Treatment" = "#DC4B4B", "Post-Treatment" = "#2484d2")) +
    geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.4) +
    labs(
      x = "Years Relative to Treatment",
      y = "Percent of FPL"
    ) +
    scale_x_continuous(
                breaks = seq(min(coef_data$rel_year),
               max(coef_data$rel_year), 1)
) +
scale_y_continuous(
  labels = scales::number_format(accuracy = 0.01)
) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
      axis.title.x = element_text(size = 12, margin = margin(t = 10)),
      axis.title.y = element_text(size = 12, margin = margin(r = 10)),
      axis.text = element_text(size = 11, color = "black"),
      panel.grid.major = element_line(color = "grey90", size = 0.3),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "none"
    )
  
  print(p_elig)

ggsave("events_for_paper/eligibility.png",
       plot  = p_elig,
       width = 10, height = 8, dpi = 300)


# MEDICAID ENROLLMENT PER BED 
  # Extract coefficients
  coef_data_med <- data.frame(
    rel_year = as.numeric(gsub(".*::(-?[0-9]+):.*", "\\1", 
                               names(coef(state_result_med))[grepl("rel_year", names(coef(state_result_med)))])),
    coef = coef(state_result_med)[grepl("rel_year", names(coef(state_result_med)))],
    se = state_result_med$se[grepl("rel_year", names(coef(state_result_med)))]
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
  p_med <- ggplot(coef_data_med, aes(x = rel_year, y = coef, color = period_color)) +
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
                breaks = seq(min(coef_data_med$rel_year),
               max(coef_data_med$rel_year), 1)
) +
scale_y_continuous(
  labels = scales::number_format(accuracy = 0.01)
) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
      axis.title.x = element_text(size = 12, margin = margin(t = 10)),
      axis.title.y = element_text(size = 12, margin = margin(r = 10)),
      axis.text = element_text(size = 11, color = "black"),
      panel.grid.major = element_line(color = "grey90", size = 0.3),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "none"
    )
  
  print(p_med)

ggsave("events_for_paper/medicaid_enrollment.png",
       plot  = p_med,
       width = 10, height = 8, dpi = 300)
