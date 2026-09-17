# ==============================================================================
# CALIFORNIA CASE STUDY: PRIMARY OUTCOMES VS. NATIONAL AVERAGE, 2000-2020
# CA's own provider tax cohort is 2011, but this marks a separate 2005
# treatment/policy event instead (distinct from the provider tax adoption).
# Plots CA's raw year-by-year average against the unweighted national
# average (all states, all years) for the four primary outcomes (op_bed,
# npr_bed, mcaid_dis_bed, uncomp_bed), with a reference line at 2005.
# Simple descriptive comparison -- NOT a DiD estimate (no control group,
# no fixed effects). Self-contained: reads hospdata_analysis.csv directly.
# Colors: dataviz skill's validated categorical slots 1 (blue, CA) and
# 2 (orange, National), same convention as pa_timeline.R.
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

data_output_path <- "/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/output/"

hospdata_analysis <- read.csv(paste0(data_output_path, "hospdata_analysis.csv"))

COLOR_CA       <- "#2a78d6"  # validated categorical slot 1 (blue)
COLOR_NATIONAL <- "#eb6834"  # validated categorical slot 2 (orange)

outcome_vars   <- c("op_bed", "npr_bed", "mcaid_dis_bed", "uncomp_bed")
outcome_labels <- c(
  op_bed        = "Operating Expenses / Bed",
  npr_bed       = "Net Patient Revenue / Bed",
  mcaid_dis_bed = "Medicaid Discharges / Bed",
  uncomp_bed    = "Uncompensated Care / Bed"
)

panel <- hospdata_analysis %>%
  filter(year >= 2000, year <= 2020) %>%
  select(state, year, all_of(outcome_vars))

national_avg <- panel %>%
  select(-state) %>%
  group_by(year) %>%
  summarise(across(all_of(outcome_vars), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(group = "National Average")

ca_avg <- panel %>%
  filter(state == "CA") %>%
  group_by(year) %>%
  summarise(across(all_of(outcome_vars), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(group = "California")

combined <- bind_rows(ca_avg, national_avg) %>%
  pivot_longer(cols = all_of(outcome_vars), names_to = "outcome", values_to = "value") %>%
  mutate(
    outcome = factor(outcome, levels = outcome_vars, labels = outcome_labels[outcome_vars]),
    group   = factor(group, levels = c("California", "National Average"))
  )

policy_lines <- data.frame(
  year  = c(2005),
  label = c("Treatment\n(2005)")
)

p <- ggplot(combined, aes(x = year, y = value, color = group)) +
  geom_vline(data = policy_lines, aes(xintercept = year),
             linetype = "dashed", color = "#898781", linewidth = 0.5) +
  geom_text(data = merge(policy_lines, data.frame(outcome = levels(combined$outcome))),
            aes(x = year, y = Inf, label = label),
            inherit.aes = FALSE, color = "#52514e", size = 2.8,
            vjust = 1.3, hjust = -0.05, lineheight = 0.85) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.3) +
  facet_wrap(~outcome, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c("California" = COLOR_CA, "National Average" = COLOR_NATIONAL)) +
  scale_x_continuous(breaks = seq(2000, 2020, 5)) +
  labs(
    title = "California vs. National Average: Primary Outcomes, 2000-2020",
    subtitle = "Dashed line marks the 2005 treatment (note: CA's provider tax cohort is 2011, not shown here)",
    x = NULL, y = NULL, color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

ggsave("diagnostics/ca_timeline.png", p, width = 10, height = 7, dpi = 300, bg = "white")
cat("Saved diagnostics/ca_timeline.png\n")
