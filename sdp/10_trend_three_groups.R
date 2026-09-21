###########################################################
# SDP preliminary analysis
# Step 10: mcaid_revdis time trend, 2008-2022, split into THREE
# groups instead of the ever/never SDP split in 6_trend_by_adoption.R:
#   1. Managed Care + SDP   -- sdp_adopt == 1 (SDPs require Medicaid
#                              managed care by federal rule, 42 CFR
#                              438.6(c), so this is automatically a
#                              subset of "has managed care")
#   2. Managed Care, no SDP -- has MCO but never adopted an SDP
#   3. Neither               -- no Medicaid managed care at all
#
# "Has managed care" uses the SAME no_mco_states list already
# established in mco_expenditure_analysis/mco_1.R (WY, VT, ME, CT,
# AK) rather than the managed_care_programs_by_state.csv cut, since
# that file's snapshot only goes to 2016 and would misclassify
# recent adopters like NC/OK as "no MCO" -- see sdp/9's finding that
# their comprehensive MCO rollout happened after 2016.
#
# Same winsorization convention as prior scripts: pooled 5th/95th
# pct over the 2008-2022 sample.
###########################################################

library(dplyr)
library(ggplot2)

data_output_path <- "/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/output/"

hospdata_analysis <- read.csv(paste0(data_output_path, "hospdata_analysis.csv"))
sdp_panel <- read.csv("sdp/sdp_state_panel.csv")

no_mco_states <- c("WY", "VT", "ME", "CT", "AK")

hosp <- hospdata_analysis %>%
  mutate(mcaid_revdis = ifelse(!is.na(mcaid_discharges) & mcaid_discharges > 0,
                                net_mcaid_rev / mcaid_discharges,
                                NA_real_)) %>%
  left_join(sdp_panel %>% select(state, sdp_adopt), by = "state") %>%
  mutate(
    has_mco = as.integer(!state %in% no_mco_states),
    group3 = case_when(
      sdp_adopt == 1              ~ "Managed Care + SDP",
      has_mco == 1 & sdp_adopt == 0 ~ "Managed Care, No SDP",
      has_mco == 0                ~ "Neither"
    )
  ) %>%
  filter(year >= 2008, year <= 2022)

stopifnot(sum(is.na(hosp$group3)) == 0)

cat("=== States per group ===\n")
hosp %>% distinct(state, group3) %>% count(group3) %>% print()

p05 <- quantile(hosp$mcaid_revdis, 0.05, na.rm = TRUE)
p95 <- quantile(hosp$mcaid_revdis, 0.95, na.rm = TRUE)

hosp <- hosp %>%
  mutate(mcaid_revdis_wins = case_when(
    is.na(mcaid_revdis) ~ NA_real_,
    mcaid_revdis < p05  ~ p05,
    mcaid_revdis > p95  ~ p95,
    TRUE                ~ mcaid_revdis
  ))

trend_data <- hosp %>%
  group_by(year, group3) %>%
  summarise(
    n          = sum(!is.na(mcaid_revdis)),
    mean_wins  = mean(mcaid_revdis_wins, na.rm = TRUE),
    median_val = median(mcaid_revdis, na.rm = TRUE),
    .groups = "drop"
  )

print(trend_data %>% select(year, group3, n, mean_wins, median_val), n = Inf)

trend_long <- trend_data %>%
  select(year, group3, mean_wins, median_val) %>%
  tidyr::pivot_longer(cols = c(mean_wins, median_val), names_to = "stat", values_to = "value") %>%
  mutate(stat = recode(stat, mean_wins = "Mean (winsorized 5/95)", median_val = "Median"))

trend_long$group3 <- factor(trend_long$group3,
                             levels = c("Managed Care + SDP", "Managed Care, No SDP", "Neither"))

p <- ggplot(trend_long, aes(x = year, y = value, color = group3, linetype = stat)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.6) +
  scale_color_manual(values = c("Managed Care + SDP" = "#2a78d6",
                                 "Managed Care, No SDP" = "#eb6834",
                                 "Neither" = "#1baf7a")) +
  scale_linetype_manual(values = c("Mean (winsorized 5/95)" = "solid", "Median" = "dashed")) +
  scale_x_continuous(breaks = seq(2008, 2022, 2)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Medicaid Revenue per Medicaid Discharge, 2008-2022",
    subtitle = "By managed care / SDP status (mean winsorized at 5th/95th pct, pooled)",
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

ggsave("sdp/mcaid_revdis_trend_three_groups.png", plot = p, width = 9, height = 5.5, dpi = 150)
