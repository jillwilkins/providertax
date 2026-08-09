# ==============================================================================
# LINE GRAPHS: TAX AS % OF NPR (by years since adoption) AND
#              DOLLAR AMOUNT PAID (by calendar year)
# Mean and median plotted together on each chart. Colors are the dataviz
# skill's validated categorical slots 1 (blue) and 2 (orange) -- a different
# pair from the project's existing pre/post-treatment red/blue convention
# (run_stacked_did()), deliberately, so a reader can't confuse "mean vs
# median" with "pre- vs post-treatment" across the paper's figures.
# Requires pct_npr_by_years_since_tax (tax_pct_npr_summary.R) and
# tax_summary_by_year_full (tax_summary_by_year_full.R) already in the
# environment.
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

COLOR_MEAN   <- "#2a78d6"  # validated categorical slot 1 (blue)
COLOR_MEDIAN <- "#eb6834"  # validated categorical slot 2 (orange)

line_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title       = element_text(face = "bold", size = 16),
    axis.title       = element_text(size = 12),
    axis.text        = element_text(size = 11, color = "black"),
    panel.grid.major = element_line(color = "#ebebeb", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    plot.caption     = element_text(size = 8.5, color = "#595959", hjust = 0, margin = margin(t = 10)),
    plot.caption.position = "plot"
  )

# Wraps caption text to a fixed character width so it doesn't run off the
# figure edge -- ggplot2 doesn't auto-wrap plot.caption.
wrap_caption <- function(x, width = 100) paste(strwrap(x, width = width), collapse = "\n")

# ------------------------------------------------------------------------------
# 1. Tax as % of NPR, by years since adoption
# ------------------------------------------------------------------------------
plot_data_pct <- pct_npr_by_years_since_tax %>%
  # Truncated at 10 years since adoption -- sample coverage (n states) peaks
  # at years 9-12 (12-14 states) and declines steadily after that as states
  # either haven't been taxing that long yet or we don't have rate data that
  # far out; beyond year 10 the average is increasingly driven by whichever
  # few states happen to still be represented, not a stable cross-section.
  filter(years_since_tax <= 10) %>%
  select(years_since_tax, Mean = mean_npr_pct, Median = median_npr_pct) %>%
  pivot_longer(c(Mean, Median), names_to = "stat", values_to = "value") %>%
  mutate(stat = factor(stat, levels = c("Mean", "Median")))

end_labels_pct <- plot_data_pct %>%
  group_by(stat) %>%
  filter(years_since_tax == max(years_since_tax))

p_pct_npr <- ggplot(plot_data_pct, aes(x = years_since_tax, y = value, color = stat)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.6) +
  ggrepel::geom_text_repel(
    data = end_labels_pct,
    aes(label = sprintf("%.2f%%", value)),
    color = "black", size = 3.6, nudge_x = 1, direction = "y", segment.size = 0.3,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c(Mean = COLOR_MEAN, Median = COLOR_MEDIAN)) +
  scale_x_continuous(breaks = 0:10) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0.05, 0.12))) +
  labs(
    title    = "Provider Tax as % of Net Patient Revenue, by Years Since Adoption",
    x        = "Years Since Tax Adoption",
    y        = "Tax as % of NPR",
    caption  = wrap_caption(paste(
      "Truncated at 10 years since adoption. Sample coverage (number of states",
      "represented) peaks at years 9-12 (12-14 states) and declines steadily",
      "thereafter, as states either have not been taxing that long yet or rate",
      "data isn't available that far out -- beyond year 10 the average reflects",
      "an increasingly small, unstable set of states rather than a consistent",
      "cross-section."
    ))
  ) +
  line_theme

ggsave("tax_pct_npr_by_years_since_tax.png", p_pct_npr, width = 9, height = 6, background = "transparent", dpi = 300)
cat("Saved tax_pct_npr_by_years_since_tax.png\n")

# ------------------------------------------------------------------------------
# 2. Dollar amount paid, by calendar year (full year range, not just the
#    5-year snapshot -- more informative as a trend line). Inflation-adjusted
#    to 2023 dollars (mean_amount_real/median_amount_real, from medical_cpi.R
#    via tax_summary_by_year_full.R) so the rise reflects real growth in the
#    payment, not just nominal/inflation growth. No title -- added in LaTeX.
# ------------------------------------------------------------------------------
plot_data_amt <- tax_summary_by_year_full %>%
  select(year, Mean = mean_amount_real, Median = median_amount_real) %>%
  pivot_longer(c(Mean, Median), names_to = "stat", values_to = "value") %>%
  mutate(stat = factor(stat, levels = c("Mean", "Median")))

end_labels_amt <- plot_data_amt %>%
  group_by(stat) %>%
  filter(year == max(year))

p_amount <- ggplot(plot_data_amt, aes(x = year, y = value, color = stat)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.6) +
  ggrepel::geom_text_repel(
    data = end_labels_amt,
    aes(label = scales::dollar(value, accuracy = 1, scale = 1e-6, suffix = "M")),
    color = "black", size = 3.6, nudge_x = 1, direction = "y", segment.size = 0.3,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c(Mean = COLOR_MEAN, Median = COLOR_MEDIAN)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M"),
                      expand = expansion(mult = c(0.05, 0.12))) +
  labs(
    x = "Year",
    y = "Tax Amount Paid (2023 $, millions)"
  ) +
  line_theme

ggsave("tax_amount_by_year.png", p_amount, width = 9, height = 6, background = "transparent", dpi = 300)
cat("Saved tax_amount_by_year.png\n")
