###########################################################
# SDP preliminary analysis
# Step 3: time trend of mcaid_revdis (Medicaid revenue per Medicaid
# discharge), 2008-2022 -- pre-2008 dropped per known HCRIS reporting
# gap (see 2_missing_netmcaidrev.R), 2023 dropped as an incomplete
# data-vintage year (see yr_max 2019 cohort gap note).
#
# Mean is reported (as requested) alongside the median, since the raw
# ratio is heavily right-skewed even within 2008-2022 (p99 ~ 800k,
# max ~161M vs. median ~27k) -- the mean trend alone can be pulled
# around by a handful of extreme hospital-years.
###########################################################

library(dplyr)
library(ggplot2)

data_output_path <- "/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/output/"

hospdata_analysis <- read.csv(paste0(data_output_path, "hospdata_analysis.csv"))

hospdata_analysis <- hospdata_analysis %>%
  mutate(mcaid_revdis = ifelse(!is.na(mcaid_discharges) & mcaid_discharges > 0,
                                net_mcaid_rev / mcaid_discharges,
                                NA_real_))

trend_data <- hospdata_analysis %>%
  filter(year >= 2008, year <= 2022) %>%
  group_by(year) %>%
  summarise(
    n         = sum(!is.na(mcaid_revdis)),
    mean_val  = mean(mcaid_revdis, na.rm = TRUE),
    median_val = median(mcaid_revdis, na.rm = TRUE),
    .groups = "drop"
  )

print(trend_data, n = Inf)

trend_long <- trend_data %>%
  select(year, mean_val, median_val) %>%
  tidyr::pivot_longer(cols = c(mean_val, median_val), names_to = "stat", values_to = "value") %>%
  mutate(stat = recode(stat, mean_val = "Mean", median_val = "Median"))

p <- ggplot(trend_long, aes(x = year, y = value, color = stat)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  scale_color_manual(values = c("Mean" = "#2a78d6", "Median" = "#eb6834")) +
  scale_x_continuous(breaks = seq(2008, 2022, 2)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Medicaid Revenue per Medicaid Discharge, 2008-2022",
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

ggsave("sdp/mcaid_revdis_trend.png", plot = p, width = 8, height = 5, dpi = 150)
