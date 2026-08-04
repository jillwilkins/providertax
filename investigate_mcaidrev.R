# ============================================================
# Diagnostics: net_mcaid_rev
# ============================================================

# summary over time 
stacked_data %>%
  distinct(mcrnum, year, .keep_all = TRUE) %>%
  group_by(year, ever_treat) %>%
  summarise(mean_rev = mean(net_mcaid_rev / mcaid_discharges, na.rm = TRUE), 
            .groups = "drop") %>%
  mutate(group = ifelse(ever_treat == 1, "Ever Treated", "Never Treated")) %>%
  ggplot(aes(x = year, y = mean_rev, color = group)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Medicaid Revenue per Discharge: 2011 Cohort vs Never Treated",
    x     = "Year",
    y     = "Mean Medicaid Revenue per Discharge",
    color = NULL
  ) +
  theme_minimal()

# only 2011
stacked_data %>%
  distinct(mcrnum, year, .keep_all = TRUE) %>%
  filter(cohort == 2011 | ever_treat == 0, mcaid_discharges > 0) %>%
  group_by(year, ever_treat) %>%
  summarise(mean_rev = mean(net_mcaid_rev_wins / mcaid_discharges, na.rm = TRUE), 
            .groups = "drop") %>%
  mutate(group = ifelse(ever_treat == 1, "2011 Cohort", "Never Treated")) %>%
  ggplot(aes(x = year, y = mean_rev, color = group)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "gray50") +
  labs(
    title = "Average Medicaid Revenue per Discharge: 2011 Cohort vs Never Treated",
    x     = "Year",
    y     = "Mean Medicaid Revenue per Discharge",
    color = NULL
  ) +
  theme_minimal()

# distribution 
stacked_data %>%
  mutate(rev_per_dis = net_mcaid_rev / mcaid_discharges) %>%
  summarise(
    p90 = quantile(net_mcaid_rev, 0.90, na.rm = TRUE),
    p95 = quantile(net_mcaid_rev, 0.95, na.rm = TRUE),
    p99 = quantile(net_mcaid_rev, 0.99, na.rm = TRUE),
    p999 = quantile(net_mcaid_rev, 0.999, na.rm = TRUE),
    max  = max(net_mcaid_rev, na.rm = TRUE)
  )

# top 50 hospitals by net_mcaid_rev
before_count <- n_distinct(stacked_data$mcrnum)
begin_avg <- mean(hospdata_analysis$net_mcaid_rev, na.rm = TRUE)

# NEGATIVES 
hospitals_with_neg_mrev <- stacked_data %>%
  group_by(mcrnum) %>%
  filter(any(net_mcaid_rev < 0, na.rm = TRUE)) %>%
  pull(mcrnum) %>%
  unique()

# negatigves are not a big issue here 

# MAX values 
# top 50 hospitals by net_mcaid_rev
hospitals_with_max_mrev <- stacked_data %>%
  slice_max(net_mcaid_rev, n = 50) 

View(stacked_data)
View(hospdata_analysis %>% filter(mcrnum %in% hospitals_with_max_mrev$mcrnum) %>% select(mcrnum, name, year, net_mcaid_rev, net_pat_rev, mcaid_discharges, beds)) 
# suspicious: 50108 discharges drops by 5000 in 2010 
# suspcicious: dropped end 0? avg? 50441 
# wrong replace w avg: 110075 year 2011
# wrong replace w avg: 310009 year 2011 (growth by 10x) 
# suspicious: 390156 year 2012 

stacked_data <- stacked_data %>%
  mutate(net_mcaid_rev = ifelse(mcrnum == 110075 & year == 2011, (lag(net_mcaid_rev) + lead(net_pat_rev)) / 2, net_mcaid_rev)) %>%
  mutate(net_mcaid_rev = ifelse(mcrnum == 310009 & year == 2011, (lag(net_mcaid_rev) + lead(net_pat_rev)) / 2, net_mcaid_rev))

# Winsorizing 
p01 <- quantile(stacked_data$net_mcaid_rev, 0.01, na.rm = TRUE)
p99 <- quantile(stacked_data$net_mcaid_rev, 0.99, na.rm = TRUE)

cat("p01 bound:", p01, "\n")
cat("p99 bound:", p99, "\n")

# winsorize
stacked_data <- stacked_data %>%
  mutate(net_mcaid_rev_wins = case_when(
    is.na(net_mcaid_rev)        ~ NA_real_,
    net_mcaid_rev < p01         ~ p01,
    net_mcaid_rev > p99         ~ p99,
    TRUE                        ~ net_mcaid_rev
  ))

# check
summary(stacked_data$net_mcaid_rev_wins)

# winsorizing the ratio 
# Step 1: construct the ratio first
stacked_data <- stacked_data %>%
  mutate(rev_per_dis = net_mcaid_rev / mcaid_discharges)

# Step 2: check the distribution of the ratio
quantile(stacked_data$rev_per_dis, 
         probs = c(0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99, 0.999), 
         na.rm = TRUE)

# Step 3: winsorize the ratio directly
p01_ratio <- quantile(stacked_data$rev_per_dis, 0.01, na.rm = TRUE)
p99_ratio <- quantile(stacked_data$rev_per_dis, 0.99, na.rm = TRUE)

cat("p01 bound:", p01_ratio, "\n")
cat("p99 bound:", p99_ratio, "\n")

stacked_data <- stacked_data %>%
  mutate(rev_per_dis_wins = case_when(
    is.na(rev_per_dis)        ~ NA_real_,
    rev_per_dis < p01_ratio   ~ p01_ratio,
    rev_per_dis > p99_ratio   ~ p99_ratio,
    TRUE                      ~ rev_per_dis
  ))

# Step 4: replot
stacked_data %>%
  distinct(mcrnum, year, .keep_all = TRUE) %>%
  filter(cohort == 2017 | ever_treat == 0) %>%
  group_by(year, ever_treat) %>%
  summarise(mean_rev = mean(rev_per_dis_wins, na.rm = TRUE), .groups = "drop") %>%
  mutate(group = ifelse(ever_treat == 1, "2017 Cohort", "Never Treated")) %>%
  ggplot(aes(x = year, y = mean_rev, color = group)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 2017, linetype = "dashed", color = "gray50") +
  labs(
    title = "Average Medicaid Revenue per Discharge: 2017 Cohort vs Never Treated",
    x     = "Year",
    y     = "Mean Medicaid Revenue per Discharge (Winsorized)",
    color = NULL
  ) +
  theme_minimal()


# Winsorizing helped, but lets still look at outliers 
# 1. Revenue per discharge after winsorizing — still look at extremes
stacked_data_real %>%
  distinct(mcrnum, year, .keep_all = TRUE) %>%
  mutate(rev_per_dis = net_mcaid_rev_real / mcaid_discharges) %>%
  filter(!is.na(rev_per_dis)) %>%
  summarise(
    n_over_500k  = sum(rev_per_dis > 500000),
    n_over_100k  = sum(rev_per_dis > 100000),
    n_under_100  = sum(rev_per_dis < 100),
    n_zero_dis   = sum(mcaid_discharges == 0),
    n_neg_dis    = sum(mcaid_discharges < 0)
  )

# who are they?
stacked_data_real %>%
  distinct(mcrnum, year, .keep_all = TRUE) %>%
  mutate(rev_per_dis = net_mcaid_rev_real / mcaid_discharges) %>%
  filter(rev_per_dis > 500000) %>%
  select(mcrnum, year, cohort, net_mcaid_rev_real, mcaid_discharges, rev_per_dis) %>%
  arrange(desc(rev_per_dis)) 

# how many unique hospitals are affected?
stacked_data_real %>%
  distinct(mcrnum, year, .keep_all = TRUE) %>%
  filter(mcaid_discharges < 10) %>%
  summarise(
    n_obs      = n(),
    n_hospitals = n_distinct(mcrnum)
  )

# set discharge observations below threshold to NA for the ratio
# but keep them for log revenue analysis
stacked_data_real <- stacked_data_real %>%
  mutate(
    rev_per_dis_clean = case_when(
      mcaid_discharges < 10        ~ NA_real_,
      is.na(mcaid_discharges)      ~ NA_real_,
      TRUE                         ~ net_mcaid_rev_real / mcaid_discharges
    )
  )

# recheck — how many over 500k remain?
stacked_data_real %>%
  distinct(mcrnum, year, .keep_all = TRUE) %>%
  filter(rev_per_dis_clean > 500000) %>%
  nrow()

stacked_data_real %>%
  distinct(mcrnum, year, .keep_all = TRUE) %>%
  filter(rev_per_dis_clean > 500000) %>%
  select(mcrnum, year, cohort, net_mcaid_rev_real, mcaid_discharges, rev_per_dis_clean) %>%
  arrange(desc(rev_per_dis_clean)) 

# 2. Hospitals with extreme year-over-year swings in log revenue
stacked_data_real %>%
  distinct(mcrnum, year, .keep_all = TRUE) %>%
  arrange(mcrnum, year) %>%
  group_by(mcrnum) %>%
  mutate(
    ln_rev      = log(net_mcaid_rev_real),
    ln_rev_lag  = lag(ln_rev),
    yoy_change  = ln_rev - ln_rev_lag
  ) %>%
  filter(abs(yoy_change) > 1) %>%          # >100% change year over year in logs
  select(mcrnum, year, cohort, net_mcaid_rev_real, ln_rev, yoy_change) %>%
  arrange(desc(abs(yoy_change))) 

# 3. Hospitals with very few discharges — unstable ratio
stacked_data %>%
  distinct(mcrnum, year, .keep_all = TRUE) %>%
  filter(mcaid_discharges < 50) %>%
  count(year) %>%
  print(n = Inf)

# 4. Hospitals that appear for only 1-2 years in the panel
stacked_data %>%
  distinct(mcrnum, year) %>%
  count(mcrnum) %>%
  filter(n <= 2) %>%
  nrow()

# --- Summary stats by year ---
stacked_data %>%
  group_by(cohort) %>%
  summarise(
    n          = n(),
    n_missing  = sum(is.na(net_mcaid_rev / mcaid_discharges)),
    n_negative = sum(net_mcaid_rev / mcaid_discharges < 0, na.rm = TRUE),
    mean       = mean(net_mcaid_rev / mcaid_discharges, na.rm = TRUE),
    p10        = quantile(net_mcaid_rev / mcaid_discharges, 0.10, na.rm = TRUE),
    p25        = quantile(net_mcaid_rev / mcaid_discharges, 0.25, na.rm = TRUE),
    median     = median(net_mcaid_rev / mcaid_discharges, na.rm = TRUE),
    p75        = quantile(net_mcaid_rev / mcaid_discharges, 0.75, na.rm = TRUE),
    p99        = quantile(net_mcaid_rev / mcaid_discharges, 0.99, na.rm = TRUE),
    max        = max(net_mcaid_rev / mcaid_discharges, na.rm = TRUE)
  ) %>%
  print(n = Inf)

stacked_data %>%
  group_by(rel_year) %>%
  summarise(
    n          = n(),
    n_missing  = sum(is.na(net_mcaid_rev/ pre_beds_avg)),
    n_negative = sum(net_mcaid_rev / pre_beds_avg < 0, na.rm = TRUE),
    mean       = mean(net_mcaid_rev / pre_beds_avg, na.rm = TRUE),
    p10        = quantile(net_mcaid_rev / pre_beds_avg, 0.10, na.rm = TRUE),
    p25        = quantile(net_mcaid_rev / pre_beds_avg, 0.25, na.rm = TRUE),
    median     = median(net_mcaid_rev / pre_beds_avg, na.rm = TRUE),
    p75        = quantile(net_mcaid_rev / pre_beds_avg, 0.75, na.rm = TRUE),
    p99        = quantile(net_mcaid_rev / pre_beds_avg, 0.99, na.rm = TRUE),
    max        = max(net_mcaid_rev / pre_beds_avg, na.rm = TRUE)
  ) %>%
  print(n = Inf)



# Viewing outliers 
hosp_large_mcaid <- stacked_data %>%
  mutate(mcaid_rev_gt_pat_rev = net_mcaid_rev > net_pat_rev) %>%
  filter(mcaid_rev_gt_pat_rev) %>%
  select(mcrnum, name, year, cohort, net_mcaid_rev, net_pat_rev) %>%
  arrange(desc(net_mcaid_rev - net_pat_rev)) 

View(hosp_large_mcaid)

stacked_data %>%
  group_by(rel_year) %>%
  summarise(
    mean   = mean(net_mcaid_rev / pre_mcaid_dis, na.rm = TRUE),
    median = median(net_mcaid_rev / pre_mcaid_dis, na.rm = TRUE),
    sd     = sd(net_mcaid_rev / pre_mcaid_dis, na.rm = TRUE)
  ) %>%
  print(n = Inf)

stacked_data %>%
  group_by(rel_year, ever_treat) %>%
  summarise(
    mean   = mean((net_mcaid_rev / pre_beds_avg), na.rm = TRUE),
    median = median((net_mcaid_rev / pre_beds_avg), na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(mean, median), names_to = "stat", values_to = "value") %>%
  ggplot(aes(x = rel_year, y = value, color = stat, linetype = factor(ever_treat))) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "black") +
  scale_linetype_manual(values = c("0" = "dashed", "1" = "solid"),
                        labels = c("0" = "Control", "1" = "Treated")) +
  labs(
    title    = "Medicaid Revenue per Bed Over Event Time",
    x        = "Event Time",
    y        = "Medicaid Rev per Bed",
    color    = NULL,
    linetype = NULL
  ) +
  theme_minimal()
