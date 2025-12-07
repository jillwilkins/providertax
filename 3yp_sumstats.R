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


# added information for draft 
summary(hospdata_clean$net_pat_rev)

npr_sum <- hospdata_clean %>% filter(!is.na(net_pat_rev), net_pat_rev > 0) %>%
  summarise(
    mean_npr = mean(net_pat_rev, na.rm = TRUE),
    sd_npr = sd (net_pat_rev, na.rm = TRUE), 
    median_npr = median(net_pat_rev, na.rm = TRUE),
    min_npr = min(net_pat_rev, na.rm = TRUE),
    max_npr = max(net_pat_rev, na.rm = TRUE), 
    top_90_npr = quantile (net_pat_rev, 0.9, na.rm = TRUE), 
    top_75_npr = quantile (net_pat_rev, 0.75, na.rm = TRUE)
  )
npr_sum

dens_npr <- ggplot(filter(hospdata_clean, !is.na(net_pat_rev), net_pat_rev > 0, net_pat_rev < 100000000), aes(x = beds)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  labs(title = "Density of NPR",
       x = "NPR", y = "Density") +
  theme_minimal()
print(dens_npr)

ggplot(hospdata_clean %>% filter(net_pat_rev > 0, !is.na(net_pat_rev), net_pat_rev < 3000000000), aes(x = net_pat_rev)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Distribution of Net Patient Revenue",
    x = "Net Patient Revenue (USD)",
    y = "Count of Hospitals"
  ) +
  theme_minimal()

ggplot(hospdata_clean, aes(x = net_pat_rev)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Density of Net Patient Revenue",
    x = "Net Patient Revenue (USD)",
    y = "Density"
  ) +
  theme_minimal()


View(hospdata_clean)
