###########################################################
# SDP preliminary analysis
# Step 8: cohort-specific event studies -- one panel per sdp_year,
# to check whether the smooth trend in 7_event_study.R (pooled
# across cohorts) is hiding a real kink at adoption in any single
# cohort.
#
# Small multiples (facet by sdp_year) instead of overlaid lines --
# 8 cohorts x 2 stats would be unreadable as one chart, and cohort
# identity is already carried by the facet title.
#
# No endpoint binning here (unlike 7_event_study.R): cohort cells
# are already small, so pooling tails per cohort would just add
# another layer of distortion on top of small samples. Points are
# shown as-is; sparse cohort-year cells will look noisy -- that's
# real, not a plotting artifact.
#
# 2024 and 2025 cohorts have NO post-adoption data at all (max
# calendar year in the panel is 2022) -- they show pre-trends only.
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

never_mean <- mean((hosp %>% filter(sdp_adopt == 0))$mcaid_revdis_wins, na.rm = TRUE)

adopters <- hosp %>%
  filter(sdp_adopt == 1, !is.na(sdp_year)) %>%
  mutate(rel_year = year - sdp_year) %>%
  filter(rel_year >= -9, rel_year <= 4)

cohort_n_states <- sdp_panel %>% filter(sdp_adopt == 1, !is.na(sdp_year)) %>%
  count(sdp_year, name = "n_states")
print(cohort_n_states)

event_by_cohort <- adopters %>%
  group_by(sdp_year, rel_year) %>%
  summarise(
    n_obs     = n(),
    n_states  = n_distinct(state),
    mean_wins = mean(mcaid_revdis_wins, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(cohort_n_states, by = "sdp_year") %>%
  mutate(facet_label = paste0(sdp_year, " cohort (n=", n_states.y, " states)"))

print(event_by_cohort %>% select(sdp_year, rel_year, n_obs, n_states = n_states.x, mean_wins), n = Inf)

p <- ggplot(event_by_cohort, aes(x = rel_year, y = mean_wins)) +
  geom_hline(yintercept = never_mean, color = "#898781", linewidth = 0.6, linetype = "dashed") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "#52514e") +
  geom_line(color = "#2a78d6", linewidth = 0.8) +
  geom_point(color = "#2a78d6", size = 1.6) +
  facet_wrap(~ facet_label, ncol = 4) +
  scale_x_continuous(breaks = seq(-9, 4, 3)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Medicaid Revenue per Medicaid Discharge Around SDP Adoption, by Cohort",
    subtitle = "Winsorized (5/95) mean; dashed gray = never-adopt reference level; dotted line = adoption",
    x = "Years relative to SDP adoption",
    y = "Medicaid revenue per Medicaid discharge ($)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e1e0d9"),
    strip.text = element_text(face = "bold")
  )

ggsave("sdp/mcaid_revdis_event_study_by_cohort.png", plot = p, width = 12, height = 7, dpi = 150)
