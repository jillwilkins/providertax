# ==============================================================================
# FIGURE: MEDICAID SHARE TRENDS BY TREATMENT STATUS
# ==============================================================================

library(ggplot2)
library(dplyr)

# ------------------------------------------------------------------------------
# Figure 1: Ever Treated vs Never Treated
# ------------------------------------------------------------------------------
# liked this one, informative for the general treand 
trend_ever_never <- hospdata_analysis %>%
  filter(
    !is.na(op_margin),
    year >= 2003 & year <= 2019, 
    state != "HI", 
    firsttax != 2004 # Exclude Hawaii due to data issues
  ) %>%
  mutate(
    treatment_status = ifelse(ever_treat == 1, "Ever Treated", "Never Treated")
  )

fig1_ever_never <- ggplot(
  trend_ever_never,
  aes(x = year, y = op_margin, color = treatment_status, linetype = treatment_status)
) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 2.5) +
  # Add confidence intervals
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3, alpha = 0.5) +
  scale_color_manual(values = c("Ever Treated" = "#83e5f4", "Never Treated" = "#e961cb")) +
  scale_linetype_manual(values = c("Ever Treated" = "solid", "Never Treated" = "dashed")) +
  labs(
    title = "Medicaid Share of Discharges: Treated vs Never-Treated States",
    x = "Year",
    y = "Medicaid Share of Discharges",
    color = "Group",
    linetype = "Group",
    caption = "Error bars show ±1 standard error"
  ) +
  scale_x_continuous(breaks = seq(2004, 2019, 2)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

print(fig1_ever_never)
ggsave("figures/mcaid_trend_ever_vs_never.png", width = 10, height = 6, dpi = 300)

# ==============================================================================
# DIAGNOSTIC: OPERATING MARGIN - NEVER TREATED GROUP
# ==============================================================================

# Summary statistics
cat("\n=== OPERATING MARGIN: NEVER TREATED ===\n")

hospdata_analysis %>%
  filter(cohort == "Never", !is.na(op_margin)) %>%
  summarise(
    n_obs = n(),
    n_hospitals = n_distinct(mcrnum),
    mean = mean(op_margin, na.rm = TRUE),
    median = median(op_margin, na.rm = TRUE),
    sd = sd(op_margin, na.rm = TRUE),
    min = min(op_margin, na.rm = TRUE),
    max = max(op_margin, na.rm = TRUE),
    p25 = quantile(op_margin, 0.25, na.rm = TRUE),
    p75 = quantile(op_margin, 0.75, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric) & !c(n_obs, n_hospitals), ~round(., 4))) %>%
  print()

# Distribution
cat("\n=== PERCENTILES ===\n")
hospdata_analysis %>%
  filter(cohort == "Never", !is.na(op_margin)) %>%
  summarise(
    p01 = quantile(op_margin, 0.01, na.rm = TRUE),
    p05 = quantile(op_margin, 0.05, na.rm = TRUE),
    p10 = quantile(op_margin, 0.10, na.rm = TRUE),
    p25 = quantile(op_margin, 0.25, na.rm = TRUE),
    p50 = quantile(op_margin, 0.50, na.rm = TRUE),
    p75 = quantile(op_margin, 0.75, na.rm = TRUE),
    p90 = quantile(op_margin, 0.90, na.rm = TRUE),
    p95 = quantile(op_margin, 0.95, na.rm = TRUE),
    p99 = quantile(op_margin, 0.99, na.rm = TRUE)
  ) %>%
  mutate(across(everything(), ~round(., 4))) %>%
  print()
View(hospdata_analysis %>% filter(cohort == "Never", !is.na(op_margin)) %>% select(mcrnum, state, year, op_margin) %>% arrange(op_margin))
# By year
cat("\n=== OPERATING MARGIN BY YEAR (NEVER TREATED) ===\n")
hospdata_analysis %>%
  filter(cohort == "Never", !is.na(op_margin)) %>%
  group_by(year) %>%
  summarise(
    n_hospitals = n_distinct(mcrnum),
    mean = mean(op_margin, na.rm = TRUE),
    median = median(op_margin, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric) & !n_hospitals, ~round(., 4))) %>%
  print(n = Inf)

# Histogram
library(ggplot2)

ggplot(hospdata_analysis %>% filter(cohort == "Never", !is.na(op_margin)), 
       aes(x = op_margin)) +
  geom_histogram(bins = 50, fill = "#4575B4", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Operating Margin Distribution: Never-Treated Hospitals",
    x = "Operating Margin",
    y = "Count"
  ) +
  theme_minimal()

ggsave("diagnostics/op_margin_never_treated.png", width = 10, height = 6, dpi = 300)

# ------------------------------------------------------------------------------
# Figure 2: Pre vs Post Treatment (Treated Hospitals Only)
# ------------------------------------------------------------------------------

trend_pre_post <- hospdata_analysis %>%
  filter(
    !is.na(mcaid_prop_discharges),
    year >= 2003 & year <= 2019,
    ever_treat == 1,  # Only treated hospitals
    state != "HI"     # Exclude Hawaii due to data issues
  ) %>%
  mutate(
    period = ifelse(post_treat == 1, "Post-Treatment", "Pre-Treatment")
  )

fig2_pre_post <- ggplot(
  trend_pre_post,
  aes(x = year, y = op_margin, color = period)
) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 2.5) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3, alpha = 0.5) +
  scale_color_manual(values = c("Pre-Treatment" = "#4575B4", "Post-Treatment" = "#75E6FA")) +
  labs(
    title = "Medicaid Share: Pre vs Post Treatment (Treated Hospitals)",
    subtitle = "Each hospital classified based on its own treatment timing",
    x = "Year",
    y = "Medicaid Share of Discharges",
    color = "Period"
  ) +
  scale_x_continuous(breaks = seq(2004, 2019, 2)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, face = "italic"),
    panel.grid.minor = element_blank()
  )

print(fig2_pre_post)
ggsave("figures/mcaid_trend_pre_vs_post.png", width = 10, height = 6, dpi = 300)


# ------------------------------------------------------------------------------
# Figure 3: Key Treatment Cohorts
# ------------------------------------------------------------------------------

# Select major cohorts
key_cohorts <- c("2005", "2009", "2011", "Never")

trend_cohorts <- hospdata_analysis %>%
  filter(
    !is.na(mcaid_prop_discharges),
    year >= 2004 & year <= 2019,
    cohort %in% key_cohorts
  ) %>%
  mutate(
    dis_per_bed = mcaid_discharges / beds,  # Medicaid discharges per bed
    cohort_label = factor(
      cohort,
      levels = c("2005", "2009", "2011", "Never"),
      labels = c("Treated 2005", "Treated 2009", "Treated 2011", "Never Treated")
    )
  )


fig3_cohorts <- ggplot(
  trend_cohorts,
  aes(x = year, y = dis_per_bed, color = cohort_label)
) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  # Add vertical lines for treatment timing
  geom_vline(xintercept = 2005, linetype = "dotted", alpha = 0.3, color = "#D73027") +
  geom_vline(xintercept = 2009, linetype = "dotted", alpha = 0.3, color = "#FEE08B") +
  geom_vline(xintercept = 2011, linetype = "dotted", alpha = 0.3, color = "#1A9850") +
  annotate("text", x = 2005, y = 0.35, label = "2005", size = 3, angle = 90, vjust = -0.5) +
  annotate("text", x = 2009, y = 0.35, label = "2009", size = 3, angle = 90, vjust = -0.5) +
  annotate("text", x = 2011, y = 0.35, label = "2011", size = 3, angle = 90, vjust = -0.5) +
  scale_color_manual(
    values = c("Treated 2005" = "#D73027", "Treated 2009" = "#FEE08B", 
               "Treated 2011" = "#1A9850", "Never Treated" = "#4575B4")
  ) +
  labs(
    title = "Medicaid Share Trends by Treatment Cohort",
    x = "Year",
    y = "Medicaid Share of Discharges",
    color = "Treatment Cohort",
    caption = "Vertical dotted lines indicate treatment adoption year"
  ) +
  scale_x_continuous(breaks = seq(2004, 2019, 2)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

print(fig3_cohorts)
ggsave("figures/mcaid_trend_by_cohort.png", width = 10, height = 6, dpi = 300)


# ------------------------------------------------------------------------------
# Figure 4: All Treatment Cohorts (Spaghetti Plot)
# ------------------------------------------------------------------------------

trend_all_cohorts <- hospdata_analysis %>%
  filter(
    !is.na(ucc_prop),
    year >= 2004 & year <= 2019, 
    cohort >= 2010 # Exclude never-treated for clarity
  )

fig4_all_cohorts <- ggplot(
  trend_all_cohorts,
  aes(x = year, y = ucc_prop, color = cohort, group = cohort)
) +
  stat_summary(fun = mean, geom = "line", size = 0.8, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", size = 1.5, alpha = 0.7) +
  # Add never-treated as bold dashed line
  stat_summary(
    data = hospdata_analysis %>% 
      filter(!is.na(ucc_prop), year >= 2004 & year <= 2019, cohort == "Never"),
    aes(x = year, y = ucc_prop),
    fun = mean, geom = "line", size = 1.5, linetype = "dashed", color = "black",
    inherit.aes = FALSE
  ) +
  labs(
    title = "Medicaid Share Trends: All Treatment Cohorts",
    subtitle = "Black dashed line = Never Treated",
    x = "Year",
    y = "Medicaid Share of Discharges",
    color = "Treatment\nCohort"
  ) +
  scale_x_continuous(breaks = seq(2004, 2019, 2)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, face = "italic"),
    panel.grid.minor = element_blank()
  )

print(fig4_all_cohorts)
ggsave("figures/mcaid_trend_all_cohorts.png", width = 12, height = 6, dpi = 300)


# ------------------------------------------------------------------------------
# BONUS: Summary Statistics Table
# ------------------------------------------------------------------------------

summary_by_treatment <- hospdata_analysis %>%
  filter(!is.na(mcaid_prop_discharges), year >= 2004 & year <= 2019) %>%
  group_by(ever_treat) %>%
  summarise(
    n_hospitals = n_distinct(mcrnum),
    n_obs = n(),
    mean_mcaid = mean(mcaid_prop_discharges, na.rm = TRUE),
    sd_mcaid = sd(mcaid_prop_discharges, na.rm = TRUE),
    min_mcaid = min(mcaid_prop_discharges, na.rm = TRUE),
    max_mcaid = max(mcaid_prop_discharges, na.rm = TRUE)
  ) %>%
  mutate(
    group = ifelse(ever_treat == 1, "Ever Treated", "Never Treated"),
    across(c(mean_mcaid, sd_mcaid, min_mcaid, max_mcaid), ~round(., 3))
  ) %>%
  select(group, n_hospitals, n_obs, mean_mcaid, sd_mcaid, min_mcaid, max_mcaid)

cat("\n=== MEDICAID SHARE SUMMARY BY TREATMENT STATUS (2004-2019) ===\n")
print(summary_by_treatment)

View(hospdata_analysis)


# ==============================================================================
# FIGURE: EVENT STUDY USING EVENT TIME (MANUAL VERSION)
# ==============================================================================

library(ggplot2)
library(dplyr)

# ------------------------------------------------------------------------------
# Figure 1: Simple Event Study (Mean by Event Time)
# ------------------------------------------------------------------------------

event_study_data <- hospdata_analysis %>%
  filter(
    !is.na(mcaid_prop_discharges),
    !is.na(time_to_treat),  # Only hospitals that are ever treated
    time_to_treat >= -5 & time_to_treat <= 10  # Focus on reasonable window
  ) %>%
  group_by(time_to_treat) %>%
  summarise(
    mean_mcaid = mean(mcaid_prop_discharges, na.rm = TRUE),
    se = sd(mcaid_prop_discharges, na.rm = TRUE) / sqrt(n()),
    n_obs = n(),
    n_hospitals = n_distinct(mcrnum)
  )

fig_event_study <- ggplot(event_study_data, aes(x = time_to_treat, y = mean_mcaid)) +
  geom_point(size = 3, color = "#D73027") +
  geom_line(size = 1, color = "#D73027") +
  geom_errorbar(aes(ymin = mean_mcaid - 1.96*se, ymax = mean_mcaid + 1.96*se),
                width = 0.2, alpha = 0.5, color = "#D73027") +
  # Add vertical line at treatment
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "black", size = 1) +
  # Add horizontal reference line at t=-1
  geom_hline(yintercept = event_study_data$mean_mcaid[event_study_data$time_to_treat == -1],
             linetype = "dotted", alpha = 0.5, color = "gray50") +
  # Shade pre-treatment period
  annotate("rect", xmin = -5.5, xmax = -0.5, ymin = -Inf, ymax = Inf,
           alpha = 0.1, fill = "blue") +
  annotate("text", x = -3, y = max(event_study_data$mean_mcaid), 
           label = "Pre-Treatment", size = 4, color = "blue") +
  annotate("text", x = 5, y = max(event_study_data$mean_mcaid), 
           label = "Post-Treatment", size = 4, color = "red") +
  labs(
    title = "Event Study: Medicaid Share of Discharges",
    subtitle = "All treatment cohorts aligned by time relative to treatment adoption",
    x = "Years Relative to Treatment (0 = Treatment Year)",
    y = "Medicaid Share of Discharges",
    caption = "Dashed line = treatment adoption. Error bars = 95% CI.\nDotted line = mean at t=-1 (reference period)."
  ) +
  scale_x_continuous(breaks = seq(-5, 10, 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, face = "italic"),
    panel.grid.minor = element_blank()
  )

print(fig_event_study)
ggsave("figures/event_study_mcaid_manual.png", width = 10, height = 6, dpi = 300)


# ------------------------------------------------------------------------------
# Figure 2: Event Study with Never-Treated as Reference
# ------------------------------------------------------------------------------

# Calculate never-treated mean for each year
never_treated_means <- hospdata_analysis %>%
  filter(cohort == "Never", !is.na(mcaid_prop_discharges)) %>%
  group_by(year) %>%
  summarise(never_mean = mean(mcaid_prop_discharges, na.rm = TRUE))

# Merge and calculate difference
event_study_diff <- hospdata_analysis %>%
  filter(
    !is.na(mcaid_prop_discharges),
    !is.na(time_to_treat),
    time_to_treat >= -5 & time_to_treat <= 10
  ) %>%
  left_join(never_treated_means, by = "year") %>%
  mutate(mcaid_diff = mcaid_prop_discharges - never_mean) %>%
  group_by(time_to_treat) %>%
  summarise(
    mean_diff = mean(mcaid_diff, na.rm = TRUE),
    se = sd(mcaid_diff, na.rm = TRUE) / sqrt(n()),
    n_obs = n()
  )

fig_event_diff <- ggplot(event_study_diff, aes(x = time_to_treat, y = mean_diff)) +
  geom_point(size = 3, color = "#D73027") +
  geom_line(size = 1, color = "#D73027") +
  geom_errorbar(aes(ymin = mean_diff - 1.96*se, ymax = mean_diff + 1.96*se),
                width = 0.2, alpha = 0.5, color = "#D73027") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "black", size = 1) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray50", size = 0.5) +
  labs(
    title = "Event Study: Medicaid Share Relative to Never-Treated",
    subtitle = "Difference between treated and never-treated hospitals",
    x = "Years Relative to Treatment (0 = Treatment Year)",
    y = "Difference in Medicaid Share\n(Treated - Never Treated)",
    caption = "Dashed line = treatment adoption. Error bars = 95% CI."
  ) +
  scale_x_continuous(breaks = seq(-5, 10, 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, face = "italic"),
    panel.grid.minor = element_blank()
  )

print(fig_event_diff)
ggsave("figures/event_study_mcaid_diff.png", width = 10, height = 6, dpi = 300)


# ------------------------------------------------------------------------------
# Figure 3: Event Study by Cohort (Faceted)
# ------------------------------------------------------------------------------

# Select key cohorts
key_cohorts <- c("2005", "2009", "2011")

event_study_by_cohort <- hospdata_analysis %>%
  filter(
    !is.na(mcaid_prop_discharges),
    !is.na(time_to_treat),
    cohort %in% key_cohorts,
    time_to_treat >= -5 & time_to_treat <= 10
  ) %>%
  group_by(cohort, time_to_treat) %>%
  summarise(
    mean_mcaid = mean(mcaid_prop_discharges, na.rm = TRUE),
    se = sd(mcaid_prop_discharges, na.rm = TRUE) / sqrt(n()),
    n_obs = n(),
    .groups = "drop"
  )

fig_event_by_cohort <- ggplot(event_study_by_cohort, 
                               aes(x = time_to_treat, y = mean_mcaid, color = cohort)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
  facet_wrap(~cohort, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("2005" = "#D73027", "2009" = "#FEE08B", "2011" = "#1A9850")) +
  labs(
    title = "Event Study by Treatment Cohort",
    x = "Years Relative to Treatment",
    y = "Medicaid Share of Discharges",
    color = "Cohort"
  ) +
  scale_x_continuous(breaks = seq(-5, 10, 2)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 12)
  )

print(fig_event_by_cohort)
ggsave("figures/event_study_by_cohort.png", width = 8, height = 10, dpi = 300)


# ------------------------------------------------------------------------------
# SIMPLE: outcome var over time 
# -------------------

# plot mcaid_discharges over time 
fig_mcaid_over_time <- hospdata_analysis %>%
  filter(!is.na(mcaid_discharges)) %>%
  group_by(year) %>%
  summarise(mean_mcaid = mean(mcaid_discharges, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean_mcaid)) +
  geom_line(size = 1, color = "#D73027") +
  geom_point(size = 2, color = "#D73027") +
  labs(
    title = "Average Medicaid Discharges Over Time",
    x = "Year",
    y = "Average Medicaid Discharges"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(fig_mcaid_over_time)
ggsave("figures/mcaid_discharges_over_time.png", width = 8, height = 6, dpi = 300)

# total discharges over time 
fig_total_discharges <- hospdata_analysis %>%
  filter(!is.na(tot_discharges)) %>%
  group_by(year) %>%
  summarise(mean_total = mean(tot_discharges, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean_total)) +
  geom_line(size = 1, color = "#1A9850") +
  geom_point(size = 2, color = "#1A9850") +
  labs(
    title = "Average Total Discharges Over Time",
    x = "Year",
    y = "Average Total Discharges"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(fig_total_discharges)

View(hospdata_analysis)
