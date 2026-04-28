# ==============================================================================
# This is where i will analyze state level variables
# Currently, medicaid enrollment and eligibiliy criteria 
# ==============================================================================
colnames(stacked_data)

stacked_data_state <- stacked_data %>%
  group_by(state, year, rel_year, treated, exp_status, df) %>%
  summarise(
    medicaid_enrollment = first(medicaid_enrollment),
    median_income_pre   = first(median_income_pre),
    eligibility         = first(eligibility),  # state-level FPL threshold
    beds = mean(beds),
    n_hospitals = n_distinct(mcrnum),
    pre_beds_avg        = mean(pre_beds_avg, na.rm = TRUE),
    .groups = "drop"
  )


state_data <- hospdata_analysis %>%
  group_by(state, year) %>%
  summarise(
    ever_treat = first(ever_treat),
    post_treat = post_treat[1],  # Assuming post_treated is the same for all rows in the group
    medicaid_enrollment = first(medicaid_enrollment),
    median_income_pre   = first(median_income_pre),
    eligibility         = first(eligibility),
    exp_status = exp_status[1],
    firsttax = first(firsttax), 
    pre_beds_avg = mean(pre_beds_avg, na.rm = TRUE), 
    medicaid_enrollment_bed = medicaid_enrollment / pre_beds_avg,  # state-level FPL threshold
    .groups = "drop"
  )

state_data <- state_data %>%
  left_join(panel_hosp, by = c("state", "year"))

colnames(state_data)

state_data <- state_data %>%
  mutate(inp_sup_exp_per_bed = Inpatient_Hospital_Sup_Payments_total / pre_beds_avg) %>%
  ungroup()

summary(state_data$Inpatient_Hospital_Sup_Payments_total)

# NOTES for state data current data availability: 
# Eligibility is available from 2002 onward 
# Expenditures are available from 2002 onward, but some variables are only available from 2008 onward (e.g. supplemental payments)
# Enrollment data is available from 2007 onward, from IPUMS/ACS

colnames(state_data)
View(state_data)

state_data <- state_data %>%
  mutate(
    firsttax = as.numeric(firsttax),
    rel_year = year - firsttax
  ) # whatever your adoption year variable is called


# MEDICIAD ENROLLMENT PER BED EVENT STUDY
state_result_med <- feols(
  medicaid_enrollment_bed ~ post_treat:ever_treat + median_income_pre + exp_status | state + year,
  data = state_data %>% filter(
    year <= 2022, 
    year >= 2007,
    (is.na(rel_year) | (rel_year >= -4 & rel_year <= 5))  # keep never-treated
  ),
  cluster = ~state
)

summary(state_result_med)

state_result_med <- feols(
  medicaid_enrollment_bed ~ i(rel_year, ever_treat, ref = -1) + median_income_pre + exp_status | state + year,
  data = state_data %>% filter(
    year <= 2022, 
    year >= 2007, 
  (is.na(rel_year) | (rel_year >= -4 & rel_year <= 5))),
  cluster = ~state
)
summary(state_result_med)

iplot(state_result_med)

# ELIGIBILITY CRITERIA EVENT STUDY

state_result_elig <- feols(
  eligibility ~ post_treat:ever_treat + median_income_pre + exp_status | state + year,
  data = state_data %>% filter(
    state != "HI",  # Exclude Hawaii if needed
    year <= 2022, 
    year >= 2002, 
    (is.na(rel_year) | (rel_year >= -4 & rel_year <= 5))  # keep never-treated
  ),
  cluster = ~state
)

summary(state_result_elig)

state_result_elig <- feols(
  eligibility ~ i(rel_year, ever_treat, ref = -1) + median_income_pre + exp_status | state + year,
  data = state_data %>% filter(
    year <= 2022, 
    year >= 2002, 
  (is.na(rel_year) | (rel_year >= -4 & rel_year <= 5))),
  cluster = ~state
)

iplot(state_result_elig)

# ==============================================================================
# Inpatient Total Expenditure 
log_inp_reg_result <- feols(
  log_inp_reg ~ i(rel_year, ever_treat, ref = -1) + median_income_pre + exp_status | state + year,
  data = state_data %>% filter(
    year <= 2024, 
    year >= 2002, 
    state != "WY",
    (is.na(rel_year) | (rel_year >= -4 & rel_year <= 5))  # keep never-treated
  ),
  cluster = ~state
)

summary(log_inp_reg_result)

# Inpatient Total Supplemental Expenditure 
log_inp_sup_result <- feols(
  log_inp_sup ~ i(rel_year, ever_treat, ref = -1) + median_income_pre + exp_status | state + year,
  data = state_data %>% filter(
    year <= 2024, 
    year >= 2008, 
    (is.na(rel_year) | (rel_year >= -4 & rel_year <= 5))  # keep never-treated
  ),
  cluster = ~state
)

summary(log_inp_sup_result)

# Inpatient Total Expenditure Per Bed
inp_reg_bed_result <- feols(
  inp_reg_per_bed ~ i(rel_year, ever_treat, ref = -1) + median_income_pre + exp_status | state + year,
  data = state_data %>% filter( # Exclude Hawaii if needed
    year <= 2024, 
    year >= 2002, 
    inp_reg_per_bed >= 0,
    state != "WY",
    (is.na(rel_year) | (rel_year >= -4 & rel_year <= 5))  # keep never-treated
  ),
  cluster = ~state
)

summary(inp_reg_bed_result)

# Inpatient Supplemental Expenditure Per Bed
inp_sup_bed_result <- feols(
  inp_sup_per_bed ~ i(rel_year, ever_treat, ref = -1) + median_income_pre + exp_status | state + year,
  data = state_data %>% filter( # Exclude Hawaii if needed
    year <= 2024, 
    year >= 2008, 
    inp_sup_per_bed >= 0,
    (is.na(rel_year) | (rel_year >= -4 & rel_year <= 5))  # keep never-treated
  ),
  cluster = ~state
)

summary(inp_sup_bed_result)

library(broom)

coef_df <- tidy(log_inp_reg_result, conf.int = TRUE) %>%
  filter(grepl("rel_year", term)) %>%
  mutate(rel_year = as.integer(str_extract(term, "-?\\d+"))) %>%
  # Filter to only rel_year -4 to +5
  filter(rel_year >= -4 & rel_year <= 5) %>%
  # Add reference year
  bind_rows(tibble(rel_year = -1, estimate = 0, 
                   conf.low = 0, conf.high = 0)) %>%
  arrange(rel_year)

event <- ggplot(coef_df, aes(x = rel_year, y = estimate)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "red") +
  labs(title = "Event Study: Inpatient Regular Payments",
       x     = "Years Relative to Treatment",
       y     = "Estimated Effect (Log Inpatient Regular Payments)") +
  theme_minimal()

ggsave("event_study_log_inp_reg.png", event, width = 8, height = 5)





