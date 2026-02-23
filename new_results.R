
# ==============================================================================
# SOLUTION 3: ANALYZE EARLY ADOPTERS ONLY (2004-2012)
# ==============================================================================
View(hospdata_analysis)
# Keep only cohorts that adopted by 2012
hospdata_early <- hospdata_analysis %>%
  filter(
    gname == 0 |  # Never treated
    gname <= 2012, 
    year <= 2014, 
    tot_discharges > 100  # Or treated by 2012
  )

# Check what you have
hospdata_early %>%
  group_by(cohort) %>%
  summarise(
    n_states = n_distinct(state),
    n_hospitals = n_distinct(mcrnum)
  ) %>%
  print(n = Inf)

# Run C&S
result_early <- att_gt(
  yname = "",  # Outcome variable
  tname = "year",
  idname = "mcrnum",
  gname = "gname",
  data = hospdata_early %>% filter(state != "HI", mcaid_prop_discharges > 0),  # Remove HI which has no pre-treatment data
  control_group = "notyettreated",
  xformla = ~ 1,  # Add pre-treatment beds as a covariate
  est_method = "dr",
  clustervars = "state",
  allow_unbalanced_panel = TRUE
)

agg_simple <- aggte(result_early, type = "simple")
summary(agg_simple)

agg_dynamic <- aggte(result_early, type = "dynamic", min_e = -3, max_e = 5)
summary(agg_dynamic)
ggdid(agg_dynamic)

agg_groups <- aggte(result_early, type = "group")
summary(agg_groups)
ggdid(agg_groups)

# Check how many missing
hospdata_early %>%
  summarise(
    total_obs = n(),
    missing_mcaid = sum(is.na(mcaid_prop_discharges)),
    missing_pre_beds = sum(is.na(pre_beds_avg)),
    pct_missing_mcaid = round(missing_mcaid / total_obs * 100, 1)
  )


# diagnostic for se 
# ==============================================================================
# DIAGNOSE: WHY LARGE STANDARD ERRORS?
# ==============================================================================

# 1. How many observations in your analysis?
hospdata_final_clean <- hospdata_analysis %>%
  filter(!is.na(mcaid_prop_discharges))

cat("\n=== SAMPLE SIZE ===\n")
cat(paste("Total observations:", nrow(hospdata_final_clean), "\n"))
cat(paste("Unique hospitals:", n_distinct(hospdata_final_clean$mcrnum), "\n"))
cat(paste("Years:", min(hospdata_final_clean$year), "to", max(hospdata_final_clean$year), "\n"))

# 2. How many clusters (states)?
cat("\n=== CLUSTERING ===\n")
cat(paste("Number of states:", n_distinct(hospdata_final_clean$state), "\n"))

# By treatment status
hospdata_final_clean %>%
  group_by(ever_treat) %>%
  summarise(
    n_states = n_distinct(state),
    n_hospitals = n_distinct(mcrnum),
    n_obs = n()
  ) %>%
  mutate(group = ifelse(ever_treat == 1, "Ever Treated", "Never Treated")) %>%
  print()

# 3. Outcome variability
cat("\n=== OUTCOME VARIABILITY ===\n")
hospdata_final_clean %>%
  summarise(
    mean = mean(mcaid_prop_discharges, na.rm = TRUE),
    sd = sd(mcaid_prop_discharges, na.rm = TRUE),
    cv = sd / mean,  # Coefficient of variation
    min = min(mcaid_prop_discharges, na.rm = TRUE),
    max = max(mcaid_prop_discharges, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>%
  print()

# 4. Within-hospital vs between-hospital variation
library(fixest)
var_decomp <- feols(mcaid_prop_discharges ~ 1 | mcrnum, 
                    data = hospdata_final_clean)

cat("\n=== VARIANCE DECOMPOSITION ===\n")
cat(paste("Total SD:", round(sd(hospdata_final_clean$mcaid_prop_discharges, na.rm = TRUE), 3), "\n"))
cat(paste("Within-hospital SD:", round(sqrt(var_decomp$sigma2), 3), "\n"))
cat(paste("Between-hospital SD:", round(sqrt(var(fitted(var_decomp))), 3), "\n"))

# 5. Sample size by event time
event_time_sample <- hospdata_final_clean %>%
  filter(!is.na(time_to_treat)) %>%
  group_by(time_to_treat) %>%
  summarise(
    n_obs = n(),
    n_hospitals = n_distinct(mcrnum),
    n_states = n_distinct(state)
  ) %>%
  arrange(time_to_treat)

cat("\n=== SAMPLE SIZE BY EVENT TIME ===\n")
print(event_time_sample, n = Inf)

# 6. Are SEs growing over time?
library(ggplot2)
se_data <- data.frame(
  time_to_treat = c(-3, -2, -1, 0, 1, 2, 3, 4, 5),
  se = c(0.0035, 0.0035, 0.0051, 0.0041, 0.0123, 0.0191, 0.0211, 0.0213, 0.0176)
)

ggplot(se_data, aes(x = time_to_treat, y = se)) +
  geom_line(size = 1.2, color = "#D73027") +
  geom_point(size = 3, color = "#D73027") +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  labs(
    title = "Standard Errors by Event Time",
    subtitle = "SEs explode in post-treatment period",
    x = "Years Relative to Treatment",
    y = "Standard Error"
  ) +
  theme_minimal()

ggsave("diagnostics/se_by_event_time.png", width = 8, height = 5, dpi = 300)

View(hospdata_analysis)
