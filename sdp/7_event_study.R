###########################################################
# SDP preliminary analysis
# Step 7: mcaid_revdis by EVENT TIME (rel_year = year - sdp_year)
# instead of calendar year, for states that adopt an SDP.
#
# Excludes GA, SC, NJ, WV (sdp_adopt=1 but sdp_year unknown) --
# can't place them on an event-time axis without a year.
#
# Endpoints binned at rel_year in [-9, 4]: all 36 known-year adopter
# states have data from -9 to -3; coverage drops off fast beyond
# that (2 states at -17, 4 states at +5) so those tails are pooled
# into the -9 / +4 endpoints rather than shown as noisy single
# points.
#
# Never-adopt states have no event time, so they're shown as flat
# reference lines (their own 2008-2022 winsorized mean / median)
# rather than a line over event time.
#
# Same winsorization convention as prior scripts: pooled 5th/95th
# pct over the 2008-2022 calendar sample.
###########################################################

library(dplyr)
library(ggplot2)

data_output_path <- "/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/output/"

hospdata_analysis <- read.csv(paste0(data_output_path, "hospdata_analysis.csv"))
sdp_panel <- read.csv("sdp/sdp_state_panel.csv")

hosp <- hospdata_analysis %>%
  mutate(mcaid_revdis = ifelse(!is.na(mcaid_discharges) & mcaid_discharges > 0,
                                net_mcaid_rev / mcaid_discharges,
                                NA_real_)) %>%
  left_join(sdp_panel %>% select(state, sdp_adopt, sdp_year), by = "state") %>%
  filter(year >= 2008, year <= 2022)

p05 <- quantile(hosp$mcaid_revdis, 0.05, na.rm = TRUE)
p95 <- quantile(hosp$mcaid_revdis, 0.95, na.rm = TRUE)

hosp <- hosp %>%
  mutate(mcaid_revdis_wins = case_when(
    is.na(mcaid_revdis) ~ NA_real_,
    mcaid_revdis < p05  ~ p05,
    mcaid_revdis > p95  ~ p95,
    TRUE                ~ mcaid_revdis
  ))

n_excluded_unknown_year <- hosp %>% filter(sdp_adopt == 1, is.na(sdp_year)) %>% pull(state) %>% n_distinct()
cat("States excluded (adopter, unknown sdp_year):", n_excluded_unknown_year, "\n")

# ------------------------------------------------------------------
# Adopters by event time (binned at [-9, 4])
# ------------------------------------------------------------------
adopters <- hosp %>%
  filter(sdp_adopt == 1, !is.na(sdp_year)) %>%
  mutate(rel_year = year - sdp_year,
         rel_year_bin = pmin(pmax(rel_year, -9), 4))

event_data <- adopters %>%
  group_by(rel_year_bin) %>%
  summarise(
    n_obs      = n(),
    n_states   = n_distinct(state),
    mean_wins  = mean(mcaid_revdis_wins, na.rm = TRUE),
    median_val = median(mcaid_revdis, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(rel_year_bin)

print(event_data, n = Inf)

# ------------------------------------------------------------------
# Never-adopt reference levels (flat, no event time)
# ------------------------------------------------------------------
never <- hosp %>% filter(sdp_adopt == 0)
never_mean  <- mean(never$mcaid_revdis_wins, na.rm = TRUE)
never_median <- median(never$mcaid_revdis, na.rm = TRUE)
cat("\nNever-adopt reference -- mean (wins):", never_mean, " median:", never_median, "\n")

# ------------------------------------------------------------------
# Plot
# ------------------------------------------------------------------
event_long <- event_data %>%
  select(rel_year_bin, mean_wins, median_val) %>%
  tidyr::pivot_longer(cols = c(mean_wins, median_val), names_to = "stat", values_to = "value") %>%
  mutate(stat = recode(stat, mean_wins = "Mean (winsorized 5/95)", median_val = "Median"))

ref_lines <- tibble::tibble(
  stat = c("Mean (winsorized 5/95)", "Median"),
  value = c(never_mean, never_median)
)

p <- ggplot(event_long, aes(x = rel_year_bin, y = value, linetype = stat)) +
  geom_hline(data = ref_lines, aes(yintercept = value, linetype = stat),
             color = "#898781", linewidth = 0.6) +
  geom_line(color = "#2a78d6", linewidth = 0.9) +
  geom_point(color = "#2a78d6", size = 1.8) +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "#52514e") +
  scale_linetype_manual(values = c("Mean (winsorized 5/95)" = "solid", "Median" = "dashed")) +
  scale_x_continuous(breaks = seq(-9, 4, 1)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Medicaid Revenue per Medicaid Discharge Around SDP Adoption",
    subtitle = "Event time relative to sdp_year; gray lines = never-adopt reference level (flat, no event time)",
    x = "Years relative to SDP adoption",
    y = "Medicaid revenue per Medicaid discharge ($)",
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e1e0d9"),
    legend.position = "top"
  )

ggsave("sdp/mcaid_revdis_event_study.png", plot = p, width = 9, height = 5.5, dpi = 150)
