###########################################################
# SDP preliminary analysis
# Step 4: re-look at the mcaid_revdis time trend (2008-2022) after
# winsorizing at the 5th/95th percentile, pooled across the
# 2008-2022 window (same pooled-threshold approach used for
# net_mcaid_rev in diagnostics/investigate_mcaidrev.R).
#
# Median is included again for reference -- winsorizing at 5%/95%
# cannot move it (it sits well inside those bounds every year).
###########################################################

library(dplyr)
library(ggplot2)

data_output_path <- "/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/output/"

hospdata_analysis <- read.csv(paste0(data_output_path, "hospdata_analysis.csv"))

hospdata_analysis <- hospdata_analysis %>%
  mutate(mcaid_revdis = ifelse(!is.na(mcaid_discharges) & mcaid_discharges > 0,
                                net_mcaid_rev / mcaid_discharges,
                                NA_real_))

sample_0822 <- hospdata_analysis %>% filter(year >= 2008, year <= 2022)

p05 <- quantile(sample_0822$mcaid_revdis, 0.05, na.rm = TRUE)
p95 <- quantile(sample_0822$mcaid_revdis, 0.95, na.rm = TRUE)
cat("p05 bound:", p05, "\n")
cat("p95 bound:", p95, "\n")

sample_0822 <- sample_0822 %>%
  mutate(mcaid_revdis_wins = case_when(
    is.na(mcaid_revdis)      ~ NA_real_,
    mcaid_revdis < p05       ~ p05,
    mcaid_revdis > p95       ~ p95,
    TRUE                     ~ mcaid_revdis
  ))

trend_data <- sample_0822 %>%
  group_by(year) %>%
  summarise(
    n              = sum(!is.na(mcaid_revdis)),
    mean_raw       = mean(mcaid_revdis, na.rm = TRUE),
    mean_wins      = mean(mcaid_revdis_wins, na.rm = TRUE),
    median_val     = median(mcaid_revdis, na.rm = TRUE),
    .groups = "drop"
  )

print(trend_data, n = Inf)

trend_long <- trend_data %>%
  select(year, mean_raw, mean_wins, median_val) %>%
  tidyr::pivot_longer(cols = c(mean_raw, mean_wins, median_val), names_to = "stat", values_to = "value") %>%
  mutate(stat = recode(stat,
                        mean_raw   = "Mean (raw)",
                        mean_wins  = "Mean (winsorized 5/95)",
                        median_val = "Median"))
trend_long$stat <- factor(trend_long$stat, levels = c("Mean (raw)", "Mean (winsorized 5/95)", "Median"))

p <- ggplot(trend_long, aes(x = year, y = value, color = stat)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  scale_color_manual(values = c("Mean (raw)" = "#2a78d6",
                                 "Mean (winsorized 5/95)" = "#eb6834",
                                 "Median" = "#1baf7a")) +
  scale_x_continuous(breaks = seq(2008, 2022, 2)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Medicaid Revenue per Medicaid Discharge, 2008-2022",
    subtitle = "Mean winsorized at 5th/95th percentile, pooled across 2008-2022",
    x = NULL,
    y = "Medicaid revenue per Medicaid discharge ($)",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e1e0d9"),
    legend.position = "top"
  )

ggsave("sdp/mcaid_revdis_trend_wins.png", plot = p, width = 8, height = 5, dpi = 150)
