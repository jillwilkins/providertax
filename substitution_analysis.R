# this is the working code for seeing any evidence of substitution as the rate goes up 

state_effects <- stacked_data %>%
  filter(!is.na(mcaid_prop_discharges)) %>%
  group_by(state, mcrnum, treated) %>%
  summarise(
    pre_mean  = mean(mcaid_prop_discharges[rel_year < 0],  na.rm = TRUE),
    post_mean = mean(mcaid_prop_discharges[rel_year >= 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(state, treated) %>%
  summarise(
    pre_mean  = mean(pre_mean,  na.rm = TRUE),
    post_mean = mean(post_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = treated, 
              values_from = c(pre_mean, post_mean),
              names_sep = "_t") %>%
  mutate(
    did_estimate = (post_mean_t1 - pre_mean_t1) - (post_mean_t0 - pre_mean_t0)
  )

 control_trend <- stacked_data %>%
  filter(treated == 0, !is.na(mcaid_prop_discharges)) %>%
  summarise(
    pre_mean_ctrl  = mean(mcaid_prop_discharges[rel_year < 0],  na.rm = TRUE),
    post_mean_ctrl = mean(mcaid_prop_discharges[rel_year >= 0], na.rm = TRUE)
  ) %>%
  mutate(ctrl_trend = post_mean_ctrl - pre_mean_ctrl)

state_effects <- stacked_data %>%
  filter(treated == 1, !is.na(mcaid_prop_discharges)) %>%
  group_by(state) %>%
  summarise(
    pre_mean  = mean(mcaid_prop_discharges[rel_year < 0],  na.rm = TRUE),
    post_mean = mean(mcaid_prop_discharges[rel_year >= 0], na.rm = TRUE),
    n_hospitals = n_distinct(mcrnum),
    .groups = "drop"
  ) %>%
  mutate(
    ctrl_trend   = control_trend$ctrl_trend,
    did_estimate = (post_mean - pre_mean) - ctrl_trend
  )

initial_rates <- hosp_taxamounts %>%
  filter(!is.na(tax_rate)) %>%
  group_by(state) %>%
  slice_min(year, n = 1) %>%        # first year with a non-missing rate
  slice(1) %>%                      # safety: in case of ties
  select(state, year, tax_rate, tax_base, tax_rate2, tax_base2) 

View(initial_rates)

# this process will be manual. I want to use the information i have to plot the mcaid effect and the tax rates. 
# Remove pre-2004 states
initial_rates <- initial_rates %>%
  filter(year >= 2004)

# Get mcaid_prop_discharges at rel_year = -1, 0, 1 and pre/post averages
# Only treated hospitals, joined to their state's initial rate
state_mcaid <- stacked_data %>%
  filter(treated == 1, !is.na(mcaid_prop_discharges)) %>%
  group_by(state) %>%
  summarise(
    rel_neg1   = mean(mcaid_prop_discharges[rel_year == -1], na.rm = TRUE),
    rel_0      = mean(mcaid_prop_discharges[rel_year ==  0], na.rm = TRUE),
    rel_1      = mean(mcaid_prop_discharges[rel_year ==  1], na.rm = TRUE),
    pre_avg    = mean(mcaid_prop_discharges[rel_year <   0], na.rm = TRUE),
    post_avg   = mean(mcaid_prop_discharges[rel_year >=  0], na.rm = TRUE),
    n_hospitals = n_distinct(mcrnum),
    .groups = "drop"
  ) %>%
  mutate(post_minus_pre = post_avg - pre_avg)

# collect needed info
# Step 1: Remove FL and KY
initial_rates <- initial_rates %>%
  filter(!state %in% c("FL", "KY"))

# Step 2: Compute tax_as_npr for ratio states from hosp_taxamounts
ratio_states <- hosp_taxamounts %>%
  filter(
    (state == "AZ" & year == 2014) |
    (state == "MI" & year == 2005) |
    (state == "OH" & year == 2020) |
    (state == "WI" & year == 2008)
  ) %>%
  group_by(state) %>%
  summarise(
    avg_tax_payment = mean(tax_amount_total, na.rm = TRUE),
    avg_net_pat_rev = mean(net_pat_rev,      na.rm = TRUE),
    tax_as_npr      = avg_tax_payment / avg_net_pat_rev * 100,  # as a percent
    .groups = "drop"
  ) %>%
  select(state, tax_as_npr)

# Step 3: Build tax_as_npr for all states
initial_rates <- initial_rates %>%
  left_join(ratio_states, by = "state") %>%
  mutate(
    tax_as_npr = case_when(
      state %in% c("AL", "AR", "GA", "MO", "MS", "OK", "OR", "PA", "VT") 
                                          ~ tax_rate,
      state %in% c("CT", "MA", "NV")     ~ (tax_rate + tax_rate2) / 2,
      state %in% c("AZ", "MI", "OH", "WI") ~ tax_as_npr,  # already joined
      TRUE                                ~ NA_real_
    )
  )

print(initial_rates %>% select(state, year, tax_rate, tax_rate2, tax_as_npr), n = Inf)

ohio_tax_as_npr <- hospdata_analysis %>%
  filter(state == "OH", year == 2020) %>%
  summarise(
    avg_tax_payment = mean(tot_operating_exp * (3.1965 / 100), na.rm = TRUE),
    avg_net_pat_rev = mean(net_pat_rev,                  na.rm = TRUE),
    tax_as_npr      = avg_tax_payment / avg_net_pat_rev * 100
  ) %>%
  pull(tax_as_npr)

# Patch into initial_rates
initial_rates <- initial_rates %>%
  mutate(
    tax_as_npr = if_else(state == "OH", ohio_tax_as_npr, tax_as_npr)
  )

# Verify
initial_rates %>% filter(state == "OH") %>% select(state, tax_rate, tax_as_npr)

View(initial_rates)
View(plot_data)


# scatter plot attempt - DID estimate vs tax_as_npr
# Step 1: Pooled control trend
control_trend <- stacked_data %>%
  filter(treated == 0, !is.na(mcaid_prop_discharges)) %>%
  summarise(
    pre_ctrl  = mean(mcaid_prop_discharges[rel_year < 0],  na.rm = TRUE),
    post_ctrl = mean(mcaid_prop_discharges[rel_year >= 0], na.rm = TRUE)
  ) %>%
  mutate(ctrl_trend = post_ctrl - pre_ctrl) %>%
  pull(ctrl_trend)

cat("Control trend:", control_trend, "\n")

# Step 2: Build plot data
plot_data <- initial_rates %>%
  left_join(state_mcaid, by = "state") %>%
  mutate(did_estimate = post_minus_pre - control_trend) %>%
  filter(!is.na(tax_as_npr), !is.na(did_estimate))

cat("States in plot:", nrow(plot_data), "\n")
print(plot_data %>% select(state, tax_as_npr, post_minus_pre, did_estimate))

# Step 3: Scatter plot
ggplot(plot_data, aes(x = tax_as_npr, y = did_estimate, label = state)) +
  geom_point(size = 3, color = "#2484d2") +
  geom_text(vjust = -0.8, size = 3.5, color = "black") +
  geom_smooth(method = "lm", se = TRUE, color = "#DC4B4B", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    x     = "Tax Rate as Share of Net Patient Revenue (%)",
    y     = "DiD Estimate (Treated minus Control Trend)",
    title = "Tax Rate vs. Effect on Medicaid Share of Discharges"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text        = element_text(size = 11, color = "black"),
    panel.grid.minor = element_blank()
  )

# Step 4: Correlation
cor_val <- cor(plot_data$tax_as_npr, plot_data$did_estimate, use = "complete.obs")
cat(sprintf("\nCorrelation (tax_as_npr vs did_estimate): %.3f\n", cor_val))

# Scatter 2: pre minus post 
ggplot(plot_data, aes(x = tax_as_npr, y = post_minus_pre, label = state)) +
  geom_point(size = 3, color = "#2484d2") +
  geom_text(vjust = -0.8, size = 3.5, color = "black") +
  geom_smooth(method = "lm", se = TRUE, color = "#DC4B4B", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    x     = "Tax Rate as Share of Net Patient Revenue (%)",
    y     = "Post - Pre (Treated Hospitals Only)",
    title = "Tax Rate vs. Effect on Medicaid Share of Discharges"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text        = element_text(size = 11, color = "black"),
    panel.grid.minor = element_blank()
  )

cor_val_raw <- cor(plot_data$tax_as_npr, plot_data$post_minus_pre, use = "complete.obs")
cat(sprintf("\nCorrelation (tax_as_npr vs post_minus_pre): %.3f\n", cor_val_raw))


# Check what's in initial_rates vs state_mcaid
cat("States in initial_rates:", sort(initial_rates$state), "\n")
cat("States in state_mcaid:  ", sort(state_mcaid$state),   "\n")

# Find which states are missing after the join
initial_rates %>%
  left_join(state_mcaid, by = "state") %>%
  filter(is.na(post_minus_pre)) %>%
  select(state, year, tax_as_npr)


tax_rate_vs_mcaid <- ggplot(plot_data, aes(x = tax_as_npr, y = did_estimate, label = state)) +
  geom_smooth(method = "lm", se = TRUE, color = "#DC4B4B", 
              fill = "#DC4B4B", alpha = 0.10,        # lighter confidence band
              linewidth = 0.8) +
  geom_point(size = 5, color = "#2484d2") +
  geom_text(vjust = -0.9, size = 3.8, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  labs(
    x     = "Tax Rate as Share of Net Patient Revenue (%)",
    y     = "DiD Estimate (Treated minus Control Trend)",
    title = "Tax Rate vs. Effect on Medicaid Share of Discharges"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text        = element_text(size = 11, color = "black"),
    axis.title       = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey93"),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.title       = element_text(hjust = 0.5, face = "bold", size = 14)
  )

ggsave("tax_rate_vs_mcaid.png", plot = tax_rate_vs_mcaid, width = 10, height = 10, dpi = 300)


# same state level for uncompenstaed care variables 







# i am not loving this 
# HOSPITAL level 
# tax_as_npr is hospital level tax burden 
# Step 1: Compute hospital-level post_minus_pre
hosp_pre_post <- stacked_data %>%
  filter(treated == 1, !is.na(mcaid_prop_discharges)) %>%
  group_by(mcrnum) %>%
  summarise(
    pre_avg        = mean(mcaid_prop_discharges[rel_year < 0],  na.rm = TRUE),
    post_avg       = mean(mcaid_prop_discharges[rel_year >= 0], na.rm = TRUE),
    post_minus_pre = post_avg - pre_avg,
    .groups = "drop"
  )

# Step 2: Join to hospital-year tax data
hosp_year_plot2 <- hosp_year_tax %>%
  inner_join(hosp_pre_post, by = "mcrnum") %>%
  filter(!is.na(post_minus_pre), !is.na(tax_as_npr_hy))

cat("N hospital-years:", nrow(hosp_year_plot2), "\n")
cat("N hospitals:     ", n_distinct(hosp_year_plot2$mcrnum), "\n")

# Step 3: Plot
ggplot(hosp_year_plot2, aes(x = tax_as_npr_hy, y = post_minus_pre)) +
  geom_point(size = 1.8, color = "#2484d2", alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, color = "#DC4B4B",
              fill = "#DC4B4B", alpha = 0.10, linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  coord_cartesian(xlim = c(NA, 15)) +
  labs(
    x     = "Hospital Tax Payment as Share of Net Patient Revenue (%)",
    y     = "Post - Pre Medicaid Share of Discharges",
    title = "Hospital-Year Tax Burden vs. Change in Medicaid Share of Discharges"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text        = element_text(size = 11, color = "black"),
    axis.title       = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey93"),
    plot.title       = element_text(hjust = 0.5, face = "bold", size = 14)
  )

ggsave("events_for_paper/tax_burden_vs_mcaid_change_hosp_year.png",
       width = 10, height = 8, dpi = 300)