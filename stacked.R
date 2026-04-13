# ==============================================================================
# STACKED DIFFERENCE-IN-DIFFERENCES
# Following "A guide on data analysis: Chapter 30"
# ==============================================================================

# recall that the follwoing are already defined 
# cohort = year that the state of hospital i was first treated (2004, always treated)
# gname = numerical version of cohort 
# post_treat = 1 if year >= cohort, 0 otherwise
# ever_treat = 1 if hospital is ever treated, 0 otherwise
# time_to_treat = year - cohort (relative year to treatment)

library(fixest)
library(dplyr)
library(tidyverse)
install.packages("fastDummies")
library(fastDummies)

# normalized var 
hospdata_analysis <- hospdata_analysis %>%
  mutate(npr_bed = net_pat_rev / pre_beds_avg, 
        op_bed = tot_operating_exp / pre_beds_avg,
        uncomp_bed = uncomp_care/ pre_beds_avg, 
        cash_bed = cash/ pre_beds_avg, 
        mcaid_cost_bed = mcaid_cost/ pre_beds_avg,
        mcaid_enroll_bed = medicaid_enrollment / pre_beds_avg
        )

# ------------------------------------------------------------------------------
#1. get valid treatment cohorts
window <- 5  # Event window (±5 years)

# Exclude always treated 
hospdata_stack <- hospdata_analysis_wins %>%
  filter(cohort != "Always (2004)") 

# Get treated cohorts (exclude never-treated and always treated, gname == 0)
cohorts <- hospdata_stack %>%
  filter(gname != 0, gname != 2004) %>%  # Exclude never-treated and always treated
  distinct(gname) %>%
  pull() %>%
  sort()

cat("\n=== TREATMENT COHORTS ===\n")
cat("Valid cohorts:", cohorts, "\n")

# ------------------------------------------------------------------------------
#2. function to generate sub experiment data 

getdata <- function(j, window) {
  hospdata_stack %>%
    filter(
      gname == j |                    # treated units in cohort j
      gname == 0 |                    # never treated, always treated
      gname > j + 5              # controls not treated soon after
    ) %>%
    filter(
      year >= j - 5 &
      year <= j + 5              # event window bounds
    ) %>%
    mutate(df = j)                    # sub-experiment indicator
}

# ------------------------------------------------------------------------------
#3. function to generate sub experiment data 
stacked_data <- map_df(cohorts, ~ getdata(., window = window)) %>%
  mutate(
    rel_year = if_else(df == gname, time_to_treat, NA_real_)    # relative year (time until treatment)
  )

# check balance between treated and control in each sub experiment
cat("\n=== STACKED DATA BALANCE ===\n")
stacked_data %>%
  group_by(df) %>%
  summarise(
    cohort_year = first(df),
    n_obs = n(),
    n_hospitals = n_distinct(mcrnum),
    n_treated = n_distinct(mcrnum[gname == df]),
    n_control = n_distinct(mcrnum[gname != df]),
    .groups = "drop"
  ) %>%
  print()

# ------------------------------------------------------------------------------
#4. create relative time dummies  
stacked_data <- map_df(cohorts, ~ getdata(., window = window)) %>%
  mutate(
    rel_year = if_else(df == gname, time_to_treat, NA_real_)
  )

stacked_data <- map_df(cohorts, ~ getdata(., window = window)) %>%
  mutate(
    rel_year = year - df,  # relative year to sub-experiment cohort df
    treated = ifelse(gname == df, 1, 0) #treatment indicator in that sub experiment
  )

# Now create dummies
stacked_data <- stacked_data %>%
  dummy_cols("rel_year", ignore_na = TRUE) %>%
  mutate(across(starts_with("rel_year_"), ~ replace_na(., 0)))

# ------------------------------------------------------------------------------
# 5. estimate stacked DiD with feols

stacked_result <- feols(
  op_bed ~ i(rel_year, treated, ref = -1) + median_income_pre + exp_status| mcrnum^df + year^df,
  data = stacked_data %>% filter(year <= 2022), # state != "TX", state != "NJ",
  #weights = ~Q_weight,          # <-- corrective Q-weights here
  cluster = ~state^df) 

summary(stacked_result)
iplot(stacked_result, main = "Stacked DiD Event Study")
colnames(hospdata_analysis)

summary(hospdata_analysis$cash_bed)

# Simple stacked DiD - just the interaction
stacked_simple <- feols(
  log_op ~ treated:post + exp_status + median_income_pre | mcrnum^df + year^df,
  data = stacked_data %>% 
    filter(year <= 2022) %>%
    mutate(post = ifelse(rel_year >= 0, 1, 0)),
  cluster = ~state
)

summary(stacked_simple)

# And what is their combined treated weight?
stacked_data %>%
  filter(treated == 1) %>%
  group_by(rel_year) %>%
  summarise(
    n_cohorts        = n_distinct(df),
    n_treated_hosps  = n_distinct(mcrnum),
    total_Q_weight   = sum(Q_weight),   # should be constant if Q-weights correct
    .groups = "drop"
  ) %>%
  arrange(rel_year) %>%
  print()


# MED enrollment state level 
stacked_data_state <- stacked_data %>%
  group_by(state, year, rel_year, treated, exp_status, df) %>%
  summarise(
    medicaid_enrollment = first(medicaid_enrollment),
    median_income_pre   = first(median_income_pre),
    beds = mean(beds),
    Q_weight = mean(Q_weight, na.rm = TRUE),
    n_hospitals = n_distinct(mcrnum),
    pre_beds_avg        = mean(pre_beds_avg, na.rm = TRUE),  # average across hospitals in state
    .groups = "drop"
  )


state_stacked_result <- feols(
  medicaid_enrollment/pre_beds_avg ~ i(rel_year, treated, ref = -1) + median_income_pre + exp_status | state^df + year^df,
  data = stacked_data_state %>% filter(year < 2022, year > 2007),
  cluster = ~state
)

state_stacked_result <- feols(
  medicaid_enrollment/pre_beds_avg ~ i(rel_year, treated, ref = -1) + 
    median_income_pre + exp_status | state^df + year^df,
  data = stacked_data_state %>% filter(year < 2020, year > 2007),
  cluster = ~state,
  weights = ~Q_weight
)

summary(state_stacked_result)
iplot(state_stacked_result, main = "State Stacked DiD Event Study")


# Test joint significance of pre-treatment coefficients
# Get coefficient names (not indices)
pre_coef_names <- names(coef(state_stacked_result))[grep("rel_year::(-[0-9]+):treated", 
                                                     names(coef(state_stacked_result)))]

cat("\n=== PRE-TREATMENT COEFFICIENTS ===\n")
print(pre_coef_names)

# Test joint significance
wald_test <- wald(state_stacked_result, pre_coef_names)

cat("\n=== JOINT TEST: PRE-TRENDS ===\n")
print(wald_test)

# If p-value < 0.05 → Pre-trends are significant (problem!)
# ==============================================================================
# PRETTY STACKED DiD EVENT STUDY (Clean Style)
# ==============================================================================

library(ggplot2)
library(dplyr)


# ==============================================================================
# STACKED DiD EVENT STUDY (Gradient Red to Blue Style)
# ==============================================================================

library(ggplot2)
library(dplyr)

# Extract coefficients and standard errors
coef_data <- data.frame(
  rel_year = as.numeric(gsub(".*::(-?[0-9]+):.*", "\\1", 
                             names(coef(stacked_result))[grepl("rel_year", names(coef(stacked_result)))])),
  coef = coef(stacked_result)[grepl("rel_year", names(coef(stacked_result)))],
  se = stacked_result$se[grepl("rel_year", names(coef(stacked_result)))]
) %>%
  mutate(
    ci_lower = coef - 1.96 * se,
    ci_upper = coef + 1.96 * se
  ) %>%
  # Add reference period
  bind_rows(data.frame(rel_year = -1, coef = 0, se = 0, ci_lower = 0, ci_upper = 0)) %>%
  arrange(rel_year) %>%
  mutate(
    # Create color variable: red for pre-treatment, blue for post-treatment
    period_color = ifelse(rel_year < 0, "Pre-Treatment", "Post-Treatment")
  )

# Create plot with gradient colors
stacked_event_study_gradient <- ggplot(coef_data, aes(x = rel_year, y = coef, color = period_color)) +
  # Error bars
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.3, size = 0.8) +
  # Points
  geom_point(size = 3) +
  # Color scale: red to blue
  scale_color_manual(values = c("Pre-Treatment" = "#D73027", "Post-Treatment" = "#2C7BB6")) +
  # Reference line at zero
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.4) +
  # Labels
  labs(
    title = "Stacked DiD Event Study: npr per Bed",
    x = "Years Relative to Treatment",
    y = "Effect on Outcome"
  ) +
  # Scales
  scale_x_continuous(breaks = seq(min(coef_data$rel_year), max(coef_data$rel_year), 1)) +
  # Theme
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 11, color = "black"),
    panel.grid.major = element_line(color = "grey90", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"
  )

print(stacked_event_study_gradient)

# Save
ggsave("clustersdid2_npr_bed.png", 
       plot = stacked_event_study_gradient, 
       width = 8, height = 10, dpi = 300)



# ==============================================================================
# FUNCTION: RUN STACKED DiD AND CREATE PLOT
# ==============================================================================

run_stacked_did <- function(outcome_var, 
                            outcome_label,
                            data = stacked_data,
                            controls = "exp_status + median_income_pre",
                            filter_condition = "year <= 2022",
                            filename = NULL,
                            width = 10,
                            height = 7) {
  
  # Build formula
  formula_str <- paste0(
    outcome_var, " ~ i(rel_year, treated, ref = -1) + ", 
    controls, " | mcrnum^df + year^df"
  )
  
  # Filter data
  data_filtered <- data %>% filter(eval(parse(text = filter_condition)))
  
  # Estimate
  cat("\n=== Estimating Stacked DiD for:", outcome_var, "===\n")
  
  result <- feols(
    as.formula(formula_str),
    data = data_filtered,
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
    scale_color_manual(values = c("Pre-Treatment" = "#D73027", "Post-Treatment" = "#2C7BB6")) +
    geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.4) +
    labs(
      title = paste("Stacked DiD Event Study:", outcome_label),
      x = "Years Relative to Treatment",
      y = paste("Effect on", outcome_label)
    ) +
    scale_x_continuous(breaks = seq(min(coef_data$rel_year), max(coef_data$rel_year), 1)) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
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
       file = "figures/sdid_mcaid_share.png"),

  list(var = "mcaid_cost_bed", 
       label = "Medicaid Cost Per Bed", 
       file = "figures/sdid_mcaid_cost.png"),

  list(var = "private_prop_discharges", 
       label = "Commercial Share of Discharges", 
       file = "figures/sdid_commercial.png"),

  list(var = "uncomp_bed", 
       label = "Uncompensated Care Charges Per Bed", 
       file = "figures/sdid_uncomp.png"),
  
  list(var = "op_margin", 
       label = "Operating Margin", 
       file = "figures/sdid_op_margin.png"),

  list(var = "npr_bed", 
       label = "Net Patient Revenue per Bed", 
       file = "figures/sdid_npr.png"),
  
  list(var = "log_npr", 
       label = "Log Net Patient Revenue", 
       file = "figures/sdid_log_npr.png"),

    list(var = "op_bed", 
       label = "Total Operating Expenses per Bed", 
       file = "figures/sdid_tot_op.png"),

  list(var = "log_op", 
       label = "Log Total Operating Expenses",
       file = "figures/sdid_log_op.png"),

  list(var = "cash_bed", 
      label = "Cash per Bed", 
      file = "figures/sdid_cash")
)

# Run for all outcomes
results <- list()

for (outcome in outcomes) {
  results[[outcome$var]] <- run_stacked_did(
    outcome_var = outcome$var,
    outcome_label = outcome$label,
    filename = outcome$file,
    width = 8,
    height = 10
  )
}

View(hospdata_analysis)
# Access individual results
# results$mcaid_prop_discharges_w$model  # The regression
# results$mcaid_prop_discharges_w$plot   # The plot
# results$mcaid_prop_discharges_w$coef_data  # The coefficients