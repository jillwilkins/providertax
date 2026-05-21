# ==============================================================================
# Script: Inflation-Adjusted Event Studies for Third Year Paper
# Based on: events_for_paper.R
# Adjusts financial outcomes to 2020 real dollars using Medical CPI
# Figures saved to: events_for_paper/real/
# ==============================================================================

library(tidyverse)
library(fixest)
library(ggplot2)

# ==============================================================================
# 1. GET INFLATION DATA (Medical Care CPI)
# ==============================================================================

# Medical Care CPI (CUUR0000SAM) - Annual Averages
medical_cpi <- tibble(
  year = 2003:2024,
  medical_cpi = c(
    260.0,  # 2003
    269.3,  # 2004
    278.8,  # 2005
    287.5,  # 2006
    296.0,  # 2007
    305.1,  # 2008
    311.4,  # 2009
    317.0,  # 2010
    323.6,  # 2011
    329.8,  # 2012
    336.0,  # 2013
    341.3,  # 2014
    351.1,  # 2015
    364.1,  # 2016
    376.8,  # 2017
    388.4,  # 2018
    401.3,  # 2019
    414.9,  # 2020
    424.9,  # 2021
    450.6,  # 2022
    476.8,  # 2023
    498.1   # 2024 (through Oct, subject to revision)
  )
) %>%
  mutate(
    # Adjustment factor to 2020 dollars
    cpi_2020 = medical_cpi[year == 2020],
    inflation_factor = cpi_2020 / medical_cpi
  )

# View adjustment factors
cat("\n=== INFLATION ADJUSTMENT FACTORS (to 2020 dollars) ===\n")
print(medical_cpi)

# ==============================================================================
# 2. CREATE INFLATION-ADJUSTED STACKED DATA
# ==============================================================================

stacked_data_real <- stacked_data %>%
  left_join(medical_cpi %>% select(year, inflation_factor), by = "year") %>%
  mutate(
    # Adjust all financial variables to 2020 real dollars
    op_bed_real = op_bed * inflation_factor,
    npr_bed_real = npr_bed * inflation_factor,
    uncomp_bed_real = uncomp_bed * inflation_factor,
    
    # If you have these variables, adjust them too:
    # op_exp_real = op_exp * inflation_factor,
    # net_pat_rev_real = net_pat_rev * inflation_factor,
    # tot_pat_rev_real = tot_pat_rev * inflation_factor,
    
    # Non-financial variables don't need adjustment
    # (keep original: mcaid_prop_discharges, mcaid_dis_bed, etc.)
  )

# Verify adjustment
cat("\n=== VERIFICATION: Mean values before/after adjustment ===\n")
stacked_data_real %>%
  filter(!is.na(op_bed), !is.na(op_bed_real)) %>%
  summarise(
    mean_op_bed_nominal = mean(op_bed, na.rm = TRUE),
    mean_op_bed_real = mean(op_bed_real, na.rm = TRUE),
    mean_inflation_factor = mean(inflation_factor, na.rm = TRUE)
  ) %>%
  print()

# ==============================================================================
# 3. CREATE OUTPUT DIRECTORY
# ==============================================================================

if (!dir.exists("events_for_paper/real")) {
  dir.create("events_for_paper/real", recursive = TRUE)
}

# ==============================================================================
# 4. FUNCTION: RUN STACKED DiD (same as before)
# ==============================================================================

run_stacked_did_real <- function(outcome_var, 
                                  outcome_label,
                                  data = stacked_data_real,
                                  controls = "exp_status + median_income_pre",
                                  y_scale = "comma",
                                  filename = NULL,
                                  width = 10,
                                  height = 10) {
  
  # Build formula
  formula_str <- paste0(
    outcome_var, " ~ i(rel_year, treated, ref = -1) + ", 
    controls, " | mcrnum^df + year^df"
  )
  
  # Estimate
  cat("\n=== Estimating Stacked DiD for:", outcome_var, "(REAL) ===\n")
  
  result <- feols(
    as.formula(formula_str),
    data = data,
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
      breaks = seq(min(coef_data$rel_year), max(coef_data$rel_year), 1)
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
# 5. RUN FOR INFLATION-ADJUSTED FINANCIAL OUTCOMES
# ==============================================================================

# Define REAL (inflation-adjusted) outcomes
outcomes_real <- list(
  list(var = "op_bed_real", 
       label = "Operating Expenses Per Bed (2020 $)", 
       file = "events_for_paper/real/op_bed_real.png"),

  list(var = "npr_bed_real", 
       label = "Net Patient Revenue Per Bed (2020 $)", 
       file = "events_for_paper/real/npr_bed_real.png"),

  list(var = "uncomp_bed_real", 
       label = "Uncompensated Care Charges Per Bed (2020 $)", 
       file = "events_for_paper/real/uncomp_real.png")
)

# Run for all real outcomes
results_real <- list()

for (outcome in outcomes_real) {
  results_real[[outcome$var]] <- run_stacked_did_real(
    outcome_var = outcome$var,
    outcome_label = outcome$label,
    filename = outcome$file,
    width = 10,
    height = 8
  )
}

# ==============================================================================
# 6. COMPARISON: NOMINAL VS REAL
# ==============================================================================

# Compare ATT estimates
cat("\n=== COMPARISON: NOMINAL VS REAL ATTs ===\n")

comparison <- tibble(
  outcome = c("Operating Expenses/Bed", "Net Patient Revenue/Bed", "Uncompensated Care/Bed"),
  att_nominal = c(
    mean(coef(results$op_bed$model)[grepl("rel_year::[0-9]+", names(coef(results$op_bed$model)))]),
    mean(coef(results$npr_bed$model)[grepl("rel_year::[0-9]+", names(coef(results$npr_bed$model)))]),
    mean(coef(results$uncomp_bed$model)[grepl("rel_year::[0-9]+", names(coef(results$uncomp_bed$model)))])
  ),
  att_real = c(
    mean(coef(results_real$op_bed_real$model)[grepl("rel_year::[0-9]+", names(coef(results_real$op_bed_real$model)))]),
    mean(coef(results_real$npr_bed_real$model)[grepl("rel_year::[0-9]+", names(coef(results_real$npr_bed_real$model)))]),
    mean(coef(results_real$uncomp_bed_real$model)[grepl("rel_year::[0-9]+", names(coef(results_real$uncomp_bed_real$model)))])
  )
) %>%
  mutate(
    difference = att_real - att_nominal,
    pct_difference = (difference / att_nominal) * 100
  )

print(comparison)

# Save comparison
write_csv(comparison, "events_for_paper/real/nominal_vs_real_comparison.csv")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Nominal results in: events_for_paper/\n")
cat("Real (2020$) results in: events_for_paper/real/\n")