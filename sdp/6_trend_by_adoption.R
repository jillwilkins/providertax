###########################################################
# SDP preliminary analysis
# Step 6: mcaid_revdis time trend, 2008-2022, split by whether the
# hospital's state EVER adopts an SDP (sdp_adopt from
# sdp/sdp_state_panel.csv) vs never adopts. This is a raw ever/never
# split -- not yet an event-study relative to sdp_year.
#
# Same winsorization as 4_trend_mcaid_revdis_wins.R: pooled 5th/95th
# percentile across the 2008-2022 sample (computed on the full
# sample here, not separately by group, so both lines are on a
# common scale).
###########################################################

library(dplyr)
library(ggplot2)

data_output_path <- "/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/output/"

hospdata_analysis <- read.csv(paste0(data_output_path, "hospdata_analysis.csv"))
sdp_panel <- read.csv("sdp/sdp_state_panel.csv")

hospdata_analysis <- hospdata_analysis %>%
  mutate(mcaid_revdis = ifelse(!is.na(mcaid_discharges) & mcaid_discharges > 0,
                                net_mcaid_rev / mcaid_discharges,
                                NA_real_)) %>%
  left_join(sdp_panel %>% select(state, sdp_adopt), by = "state")

stopifnot(sum(is.na(hospdata_analysis$sdp_adopt)) == 0)

sample_0822 <- hospdata_analysis %>% filter(year >= 2008, year <= 2022)

p05 <- quantile(sample_0822$mcaid_revdis, 0.05, na.rm = TRUE)
p95 <- quantile(sample_0822$mcaid_revdis, 0.95, na.rm = TRUE)

sample_0822 <- sample_0822 %>%
  mutate(mcaid_revdis_wins = case_when(
    is.na(mcaid_revdis) ~ NA_real_,
    mcaid_revdis < p05  ~ p05,
    mcaid_revdis > p95  ~ p95,
    TRUE                ~ mcaid_revdis
  ))

trend_data <- sample_0822 %>%
  group_by(year, sdp_adopt) %>%
  summarise(
    n          = sum(!is.na(mcaid_revdis_wins)),
    mean_wins  = mean(mcaid_revdis_wins, na.rm = TRUE),
    median_val = median(mcaid_revdis, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(group = ifelse(sdp_adopt == 1, "Ever Adopt", "Never Adopt"))

print(trend_data %>% select(year, group, n, mean_wins, median_val), n = Inf)

trend_long <- trend_data %>%
  select(year, group, mean_wins, median_val) %>%
  tidyr::pivot_longer(cols = c(mean_wins, median_val), names_to = "stat", values_to = "value") %>%
  mutate(stat = recode(stat, mean_wins = "Mean (winsorized 5/95)", median_val = "Median"))

p <- ggplot(trend_long, aes(x = year, y = value, color = group, linetype = stat)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.6) +
  scale_color_manual(values = c("Ever Adopt" = "#2a78d6", "Never Adopt" = "#eb6834")) +
  scale_linetype_manual(values = c("Mean (winsorized 5/95)" = "solid", "Median" = "dashed")) +
  scale_x_continuous(breaks = seq(2008, 2022, 2)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Medicaid Revenue per Medicaid Discharge, 2008-2022",
    subtitle = "Ever-adopt vs never-adopt SDP states (mean winsorized at 5th/95th pct, pooled)",
    x = NULL,
    y = "Medicaid revenue per Medicaid discharge ($)",
    color = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e1e0d9"),
    legend.position = "top",
    legend.box = "vertical"
  )

ggsave("sdp/mcaid_revdis_trend_by_adoption.png", plot = p, width = 8.5, height = 5.5, dpi = 150)
