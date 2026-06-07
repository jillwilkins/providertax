# CPS ATTEMPT 

library(ipumsr)
library(tigris)

ddi_cps <- read_ipums_ddi(paste0(data_input_path, "cps_00004.xml"))
data_cps_full <- read_ipums_micro(ddi_cps)

# make lowercase 
colnames(data_cps_full) <- tolower(colnames(data_cps_full))
names(data_cps_full)

# Uninsurance (age filtered 21-64, matching Garthwaite replication)
data_cps_unins <- data_cps_full %>%
  filter(age >= 21 & age < 65) %>%
  mutate(
    year         = year - 1,
    any_ins      = as.integer(hcovany == 2),
    uninsured    = as.integer(hcovany == 1),
    any_public   = as.integer(hcovpub == 2),
    unemp        = as.integer(empstat >= 20 & empstat <= 22),
    foreign_born = as.integer(citizen == 3),
    statefip     = sprintf("%02d", as.numeric(statefip)),
    state_name   = fips_codes$state_name[match(statefip, fips_codes$state_code)],
    state        = state.abb[match(state_name, state.name)]
  ) %>%
  filter(!is.na(state)) %>%
  group_by(state, statefip, year) %>%
  summarise(
    population     = sum(asecwt, na.rm = TRUE),
    insured_rate   = weighted.mean(any_ins,      asecwt, na.rm = TRUE),
    uninsured_rate = 1 - insured_rate,
    any_public     = weighted.mean(any_public,   asecwt, na.rm = TRUE),
    unemp          = weighted.mean(unemp,        asecwt, na.rm = TRUE),
    foreign_born   = weighted.mean(foreign_born, asecwt, na.rm = TRUE),
    mean_age       = weighted.mean(age,          asecwt, na.rm = TRUE),
    .groups = "drop"
  )

View(data_cps_unins)

# Medicaid enrollment (no age filter)
data_cps_medicaid <- data_cps_full %>%
  mutate(
    year         = year - 1,
    # HINSCAID: main variable (SHADAC summary, comparable across years)
    on_hinscaid  = as.integer(hinscaid == 2),
    # CAIDLY: alternative raw variable
    on_caidly    = as.integer(caidly == 2),
    statefip     = sprintf("%02d", as.numeric(statefip)),
    state_name   = fips_codes$state_name[match(statefip, fips_codes$state_code)],
    state        = state.abb[match(state_name, state.name)]
  ) %>%
  filter(!is.na(state)) %>%
  group_by(state, statefip, year) %>%
  summarise(
    population_all    = sum(asecwt, na.rm = TRUE),
    # HINSCAID counts and share
    n_medicaid        = sum(asecwt * on_hinscaid, na.rm = TRUE),
    medicaid_share    = n_medicaid / population_all,
    # CAIDLY counts and share (for comparison)
    n_medicaid_caidly = sum(asecwt * on_caidly, na.rm = TRUE),
    medicaid_share_caidly = n_medicaid_caidly / population_all,
    .groups = "drop"
  )

# Merge together
data_cps <- data_cps_unins %>%
  left_join(data_cps_medicaid, by = c("state", "statefip", "year"))


# join to state-level data
state_data_cps <- state_data %>%
  left_join(
    data_cps,
    by = c("state", "year")
  )

state_data_cps <- state_data_cps %>%
  mutate(
    firsttax = as.numeric(firsttax),
    rel_year = year - firsttax
  )

# MEDICIAD ENROLLMENT PER BED EVENT STUDY (confirmation with the acs data)
state_result_med_cps <- feols(
  n_medicaid_caidly / pre_beds_avg ~ i(rel_year, ever_treat, ref = -1) + median_income_pre + exp_status | state + year,
  data = state_data_cps %>% filter(is.na(rel_year) | (rel_year >= -4 & rel_year <= 5)),
  cluster = ~state
)

summary(state_result_med_cps)

# MEDICAID ENROLLMENT PER BED 
  # Extract coefficients
  coef_data_medcps <- data.frame(
    rel_year = as.numeric(gsub(".*::(-?[0-9]+):.*", "\\1", 
                               names(coef(state_result_med_cps))[grepl("rel_year", names(coef(state_result_med_cps)))])),
    coef = coef(state_result_med_cps)[grepl("rel_year", names(coef(state_result_med_cps)))],
    se = state_result_med_cps$se[grepl("rel_year", names(coef(state_result_med_cps)))]
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
  p_medcps <- ggplot(coef_data_medcps, aes(x = rel_year, y = coef, color = period_color)) +
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
                breaks = seq(min(coef_data_medcps$rel_year),
               max(coef_data_medcps$rel_year), 1)
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
  
  print(p_medcps)

ggsave("events_for_paper/medicaid_enrollment_cps.png",
       plot  = p_medcps,
       width = 10, height = 8, dpi = 300)

# Results agree to the acs based estimates. 

names(state_data_cps)

View(plot_data)

# EXPLORE UNINSURANCE-CHANGE IN MCAID DIS RELATIONSHIP
# compare changes in medicaid prop dis with changes in uninsured rate (pre vs post)
plot_data <- state_data_cps %>%
  filter(ever_treat == 1, firsttax != 2004) %>%
  group_by(state) %>%
  summarise(
    pre_uninsured_rate   = mean(uninsured_rate[year < firsttax], na.rm = TRUE),
    pre_medicaid_share   = mean(mcaid_prop_discharges[year < firsttax], na.rm = TRUE),
    post_medicaid_share  = mean(mcaid_prop_discharges[year >= firsttax], na.rm = TRUE),
    delta_medicaid_share = post_medicaid_share - pre_medicaid_share, 
    firsttax             = first(firsttax),   # add this to see timing
  ) %>%
  filter(!is.na(delta_medicaid_share), !is.na(pre_uninsured_rate))

mcaid_dis_unins <- ggplot(plot_data, aes(x = pre_uninsured_rate, y = delta_medicaid_share)) +
  geom_point(color = "#185FA5", size = 2.5, alpha = 0.8) +
  geom_text(aes(label = state), hjust = -0.2, vjust = 0.4, size = 3,
            color = "gray30", check_overlap = TRUE) +
  geom_smooth(method = "lm", se = TRUE, color = "#D85A30",
              fill = "#F0997B", alpha = 0.2, linewidth = 0.8) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.02, 0.08))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Pre-tax uninsurance rate vs. change in Medicaid discharge share",
    x = "Pre-tax uninsured rate",
    y = "Change in Medicaid share (post - pre)",
    caption = "Source: CPS ASEC, AHA. Each point is a state."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 13, face = "plain"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(color = "gray50", size = 9)
  )
ggsave("uninsured_vs_mcaid_dis_change.png", mcaid_dis_unins, width = 10, height = 8)


# Check if late adopters are driving the negative relationship
plot_data %>% arrange(pre_uninsured_rate) %>% select(state, firsttax, pre_uninsured_rate, delta_medicaid_share) %>% print(n = Inf)


# High vs low uncompensated care hospitals pre-tax
hospdata_analysis <- hospdata_analysis %>%
  group_by(mcrnum) %>%
  mutate(
    pre_uncomp_share = mean(uncomp_bed[year < firsttax], na.rm = TRUE),
    high_uncomp = pre_uncomp_share > median(pre_uncomp_share, na.rm = TRUE)
  )

