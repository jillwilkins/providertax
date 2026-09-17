# ==============================================================================
# PENNSYLVANIA CASE STUDY: PRIMARY OUTCOMES VS. NATIONAL AVERAGE, 2000-2020
# PA adopted its provider tax in 2009 and repealed it in 2016 -- a rare
# adopt-then-repeal case in the sample. This plots PA's raw year-by-year
# average against the unweighted national average (all states, all years)
# for the four primary outcomes (op_bed, npr_bed, mcaid_dis_bed, uncomp_bed),
# with reference lines at adoption (2009) and repeal (2016).
# Simple descriptive comparison -- NOT a DiD estimate (no control group,
# no fixed effects). Self-contained: reads hospdata_analysis.csv directly.
# Colors: dataviz skill's validated categorical slots 1 (blue, PA) and
# 2 (orange, National), same convention as tax_line_graphs.R.
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

data_output_path <- "/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/output/"

hospdata_analysis <- read.csv(paste0(data_output_path, "hospdata_analysis.csv"))

COLOR_PA       <- "#2a78d6"  # validated categorical slot 1 (blue)
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

pa_avg <- panel %>%
  filter(state == "PA") %>%
  group_by(year) %>%
  summarise(across(all_of(outcome_vars), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(group = "Pennsylvania")

combined <- bind_rows(pa_avg, national_avg) %>%
  pivot_longer(cols = all_of(outcome_vars), names_to = "outcome", values_to = "value") %>%
  mutate(
    outcome = factor(outcome, levels = outcome_vars, labels = outcome_labels[outcome_vars]),
    group   = factor(group, levels = c("Pennsylvania", "National Average"))
  )

policy_lines <- data.frame(
  year  = c(2009, 2016),
  label = c("Adopted\n(2009)", "Repealed\n(2016)")
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
  scale_color_manual(values = c("Pennsylvania" = COLOR_PA, "National Average" = COLOR_NATIONAL)) +
  scale_x_continuous(breaks = seq(2000, 2020, 5)) +
  labs(
    title = "Pennsylvania vs. National Average: Primary Outcomes, 2000-2020",
    subtitle = "Dashed lines mark PA's provider tax adoption (2009) and repeal (2016)",
    x = NULL, y = NULL, color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

ggsave("diagnostics/pa_timeline.png", p, width = 10, height = 7, dpi = 300, bg = "white")
cat("Saved diagnostics/pa_timeline.png\n")
