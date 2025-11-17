# Meta --------------------------------------------------------------------
## Author:        Jill Wilkins
## Date Created:  10/9/2025 (11/14/25)
## Date Edited:   11/17/2025
## Goal:          Summary Statistics Table in my 3YP         
## 

library(dplyr)
library(purrr)
library(rlang)
library(kableExtra)

rm(control_results, treated_results, results)

sum_vars <- c(
  "private_prop_discharges", "mcaid_prop_discharges", "ucc_prop", 
  "psyemhos", "alchhos", "cost_to_charge", "rural"
)

var_labels <- c(
  private_prop_discharges = "Private Insurance Discharges",
  mcaid_prop_discharges   = "Medicaid Discharges",
  ucc_prop                = "Uncompensated Care (Prop)",
  psyemhos                = "Psych ED Visits",
  alchhos                 = "Alcohol ED Visits",
  cost_to_charge          = "Cost to Charge Ratio",
  rural                   = "Rural Indicator"
)

control_results <- map_dfr(sum_vars, function(v) {

  hospdata_clean %>%
    # pre tax and never taxed 
    filter(
      treatment_group == "not treated by 2020" |
        (treatment_group == "treated" & year < firsttax),
      !is.na(beds)
    ) %>%
    # hospital-level mean 
    group_by(mcrnum) %>%
    summarise(hosp_mean_y = mean(.data[[v]], na.rm = TRUE), .groups = "drop") %>%
    # overall mean, sd
    summarise(
      variable = v,
      mean_y = mean(hosp_mean_y, na.rm = TRUE),
      sd_y   = sd(hosp_mean_y, na.rm = TRUE)
    )
})

treated_results <- map_dfr(sum_vars, function(v) {

  hospdata_clean %>%
    # filter for treated & taxed
    filter(
      treatment_group == "treated",
      yes_tax == 1,
      !is.na(beds)
    ) %>%
    # hospital-level mean 
    group_by(mcrnum) %>%
    summarise(
      hosp_mean_y = mean(.data[[v]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # overall mean, sd
    summarise(
      variable = v,
      mean_y = mean(hosp_mean_y, na.rm = TRUE),
      sd_y = sd(hosp_mean_y, na.rm = TRUE)
    )
})

control_results <- control_results %>%
  mutate(
    mean_sd_control = sprintf("%.3f (%.3f)", mean_y, sd_y),
    label = var_labels[variable]
  ) %>%
  select(variable, label, mean_sd_control)

treated_results <- treated_results %>%
  mutate(
    mean_sd_treated = sprintf("%.3f (%.3f)", mean_y, sd_y),
    label = var_labels[variable]
  ) %>%
  select(variable, label, mean_sd_treated)

results <- treated_results %>%
  left_join(
    control_results %>% select(variable, mean_sd_treated),
    by = "variable"
  ) %>%
  select(label, mean_sd_control, mean_sd_treated)

results 
kable(results, format = "latex", booktabs = TRUE,
      caption = "Summary Statistics by Treatment Status",
      col.names = c("Variable", "Treated Hospitals", "Not Yet Treated by 2020"),
      align = "lcc")
