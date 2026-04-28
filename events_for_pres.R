# ==============================================================================
# Script that prepares my event studies to include in Poster Presentation (4/23)
# This is based on the stacked data set created in fix_stacked.R
# The figures are saved under the folder "events_for_pres"
# ==============================================================================

# run the results through my outcomes to include in the presentation 

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
      title = paste(outcome_label),
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
       file = "events_for_pres/mcaid_dis.png"),

  list(var = "private_prop_discharges", 
       label = "Commercial Share of Discharges", 
       file = "events_for_pres/private_dis.png"),

  list(var = "uncomp_bed", 
       label = "Uncompensated Care Charges Per Bed", 
       file = "events_for_pres/uncomp.png"),
  
  list(var = "op_bed", 
       label = "Operating Expenses Per Bed", 
       file = "events_for_pres/op_bed.png"),

  list(var = "npr_bed", 
       label = "Net Patient Revenue Per Bed", 
       file = "events_for_pres/npr_bed.png") 
)

# Run for all outcomes
results <- list()

for (outcome in outcomes) {
  results[[outcome$var]] <- run_stacked_did(
    outcome_var = outcome$var,
    outcome_label = outcome$label,
    filename = outcome$file,
    width = 10,
    height = 10
  )
}

names(coef(results[["mcaid_prop_discharges"]]$model))

results

summary(hospdata_stack$mcaid_discharges)

# PRE PERIOD BASELINE MEANS
pre_treat_means <- stacked_data %>%
  filter(treated == 1, rel_year < 0) %>%
  summarise(
    mn_mcaid  = mean(mcaid_prop_discharges, na.rm = TRUE),
    mn_priv  = mean(private_prop_discharges, na.rm = TRUE),
    mn_uncomp = mean(uncomp_bed,            na.rm = TRUE),
    mn_op     = mean(op_bed,                na.rm = TRUE),
    mn_npr    = mean(npr_bed,               na.rm = TRUE), 
    mn_op_margin = mean(op_margin,               na.rm = TRUE), 
  )

mn_mcaid  <- round(pre_treat_means$mn_mcaid,  3)
mn_uncomp <- round(pre_treat_means$mn_uncomp, 0)
mn_op     <- round(pre_treat_means$mn_op,     0)
mn_npr    <- round(pre_treat_means$mn_npr,     0)
mn_op_margin <- round(pre_treat_means$mn_op_margin, 3)
mn_priv   <- round(pre_treat_means$mn_priv,   3)

print(pre_treat_means)

# ADD Labels for means and att 
# ==============================================================================
# mcaid_prop_discharges
# ==============================================================================
att_mcaid <- results[["mcaid_prop_discharges"]]$coef_data %>%
  filter(rel_year >= 0) %>%
  summarise(att = mean(coef, na.rm = TRUE)) %>%
  pull(att) %>%
  round(3)


# FILL IN THE MEAN VALUE 
label_text_mcaid <- paste0(
  "Mean:  ", formatC(round(mn_mcaid, 3),    format = "f", digits = 3, width = 8), "\n",
  "ATT:    ", formatC(round(att_mcaid, 3), format = "f", digits = 3, width = 8)
)

p_mcaid_labeled <- results[["mcaid_prop_discharges"]]$plot +
  annotate(
    "label",
    x          = max(results[["mcaid_prop_discharges"]]$coef_data$rel_year) - 0.5,
    y          = max(results[["mcaid_prop_discharges"]]$coef_data$ci_upper) * 0.95,
    label      = label_text_mcaid,
    hjust      = 2,
    vjust      = 1,
    size       = 7,
    fontface   = "italic",
    fill       = "#eaf4fc",
    color      = "#17527f",
    label.size    = 0.75,
    lineheight    = 1.3,
    label.padding = unit(0.75, "lines")  # increase from default 0.25
  )

print(p_mcaid_labeled)

ggsave("events_for_pres/mcaid_dis_labeled.png",
       plot  = p_mcaid_labeled,
       width = 10, height = 10, dpi = 300)

# ==============================================================================
# uncomp_bed
# ==============================================================================
att_uncomp <- results[["uncomp_bed"]]$coef_data %>%
  filter(rel_year >= 0) %>%
  summarise(att = mean(coef, na.rm = TRUE)) %>%
  pull(att) %>%
  round(0)

label_text_uncomp <- paste0(
  "  Mean:  ", scales::comma(mn_uncomp), "  \n",
  "  ATT:     ", scales::comma(att_uncomp), "  "
)

p_uncomp_labeled <- results[["uncomp_bed"]]$plot +
  annotate(
    "label",
    x          = max(results[["uncomp_bed"]]$coef_data$rel_year) - 0.5,
    y          = min(results[["uncomp_bed"]]$coef_data$ci_lower) * 0.3,
    label      = label_text_uncomp,
    hjust      = 2,
    vjust      = 3.5,
    size       = 7,
    fontface   = "italic",
    fill       = "#eaf4fc",
    color      = "#17527f",
    label.size    = 0.75,
    lineheight    = 1.3,
    label.padding = unit(0.75, "lines")
  )

print(p_uncomp_labeled)

ggsave("events_for_pres/uncomp_labeled.png",
       plot  = p_uncomp_labeled,
       width = 10, height = 10, dpi = 300)

# ==============================================================================
# op_bed
# ==============================================================================

# generate shared label locations for op and npr 
y_min <- min(
  min(results[["op_bed"]]$coef_data$ci_lower),
  min(results[["npr_bed"]]$coef_data$ci_lower)
)

y_max <- max(
  max(results[["op_bed"]]$coef_data$ci_upper),
  max(results[["npr_bed"]]$coef_data$ci_upper)
)

att_op <- results[["op_bed"]]$coef_data %>%
  filter(rel_year >= 0) %>%
  summarise(att = mean(coef, na.rm = TRUE)) %>%
  pull(att) %>%
  round(0)

label_text_op <- paste0(
  "  Mean:  ", scales::comma(mn_op), "  \n",
  "  ATT:     ", scales::comma(att_op), "  "
)

p_op_labeled <- results[["op_bed"]]$plot +
  annotate(
    "label",
    x = max(results[["op_bed"]]$coef_data$rel_year) - 0.5,
    y = y_max * 0.95,
    label      = label_text_op,
    hjust      = 2,
    vjust      = 1,
    size       = 7,
    fontface   = "italic",
    fill       = "#eaf4fc",
    color      = "#17527f",
    label.size    = 0.75,
    lineheight    = 1.3,
    label.padding = unit(0.75, "lines")
  )

print(p_op_labeled)

ggsave("events_for_pres/op_bed_labeled.png",
       plot  = p_op_labeled,
       width = 10, height = 10, dpi = 300)

# ==============================================================================
# npr_bed
# ==============================================================================
att_npr <- results[["npr_bed"]]$coef_data %>%
  filter(rel_year >= 0) %>%
  summarise(att = mean(coef, na.rm = TRUE)) %>%
  pull(att) %>%
  round(0)

label_text_npr <- paste0(
  "  Mean:  ", scales::comma(mn_npr), "  \n",
  "  ATT:     ", scales::comma(att_npr), "  "
)

p_npr_labeled <- results[["npr_bed"]]$plot +
  annotate(
    "label",
    x = max(results[["npr_bed"]]$coef_data$rel_year) - 0.5,
    y = y_max * 0.95,  # same shared y_max,
    label      = label_text_npr,
    hjust      = 2,
    vjust      = 1,
    size       = 7,
    fontface   = "italic",
    fill       = "#eaf4fc",
    color      = "#17527f",
    label.size    = 0.75,
    lineheight    = 1.3,
    label.padding = unit(0.75, "lines")
  )

print(p_npr_labeled)

ggsave("events_for_pres/npr_bed_labeled.png",
       plot  = p_npr_labeled,
       width = 10, height = 10, dpi = 300)


library(patchwork)

# ------------------------------------------------------------------------------
# Pull the op and npr plots from results list
# ------------------------------------------------------------------------------
p_op_bed  <- results[["op_bed"]]$plot
p_npr_bed <- results[["npr_bed"]]$plot

# ------------------------------------------------------------------------------
# COMBINED: OP AND NPR PLOT 
# ------------------------------------------------------------------------------

# align axis
p_op_labeled  <- p_op_labeled  + coord_cartesian(ylim = c(y_min, y_max))
p_npr_labeled <- p_npr_labeled + coord_cartesian(ylim = c(y_min, y_max))

combined_plot <- p_op_labeled + p_npr_labeled +
  plot_layout(ncol = 2, axes = "collect") +
  plot_annotation(
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5)
    )
  )

print(combined_plot)

ggsave("events_for_pres/op_npr_combined.png",
       plot  = combined_plot,
       width = 14, height = 10, dpi = 300)

# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------
ggsave(
  "events_for_pres/op_npr_combined.png",
  plot   = combined_plot,
  width  = 16,
  height = 10,
  dpi    = 300
)

cat("Saved to events_for_pres/op_npr_combined.png\n")


# ------------------------------------------------------------------------------
# Pull the mcaid and ucc plots from your results list
# ------------------------------------------------------------------------------
p_mcaid_dis  <- results[["mcaid_prop_discharges"]]$plot
p_uncomp_bed <- results[["uncomp_bed"]]$plot

# ------------------------------------------------------------------------------
# COMBINED MCAID AND UNCOMP PLOT
# ------------------------------------------------------------------------------

# Combined plots 
combined_plot <- p_mcaid_labeled + p_uncomp_labeled +
  plot_layout(ncol = 2) +
  plot_annotation(
    # title = "Effects on Hospital Payer Mix",
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5)
    )
  )

print(combined_plot)

# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------
ggsave(
  "events_for_pres/mcaid_uncomp_combined.png",
  plot   = combined_plot,
  width  = 16,
  height = 10,
  dpi    = 300
)

cat("Saved to events_for_pres/mcaid_uncomp_combined.png\n")
# =============================================================================

# =============================================================================
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
       title = "Medicaid Eligibility for Parents",
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

ggsave("events_for_pres/eligibility.png",
       plot  = p_elig,
       width = 10, height = 10, dpi = 300)


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
       title = "Medicaid Enrollment Per Bed",
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

ggsave("events_for_pres/medicaid_enrollment.png",
       plot  = p_med,
       width = 10, height = 10, dpi = 300)

   library(patchwork)

# COMBINE 
combined_state_plot <- p_med + p_elig +
  plot_layout(ncol = 2) +
  plot_annotation(
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5)
    )
  )


print(combined_state_plot)

ggsave("events_for_pres/state_combined.png",
       plot  = combined_state_plot,
       width = 16, height = 10, dpi = 300)


hospdata_analysis %>%
  filter(tot_operating_exp < 50000000) %>%
  summarise(
    n_unique_hospitals = n_distinct(mcrnum),
    n_observations = n()
  )

View(hospdata_analysis)
