# ==============================================================================
# TAX AS PERCENT OF NPR, BY YEARS SINCE ADOPTION
# Average tax_as_npr_best, by years since the state's provider tax was first
# in place. Deliberately labeled "Tax as % of NPR," not "hospital burden
# over time" -- this is a pooled, cross-state descriptive average, and for
# the ~40% of hospital-years in single-rate, revenue-based states,
# tax_as_npr_best is mathematically identical to that state's own statutory
# rate (tax = rate% x NPR, divided back by the same NPR). So this table
# reflects a mix of (a) real within-state rate escalation over time and (b)
# which states populate each years-since-tax bucket -- it is NOT a
# within-hospital burden trajectory. No inflation adjustment needed either
# way, since tax_as_npr_best is a same-period ratio.
# If a within-hospital burden-over-time table is needed later, that's a
# separate analysis (state fixed effects, to net out the composition
# effect) -- not this table.
# Requires tax_amounts already in the environment (calc_tax_amounts.R).
# ==============================================================================

library(dplyr)

pct_npr_by_years_since_tax <- tax_amounts %>%
  filter(year >= firsttax, state != "MI") %>%
  mutate(years_since_tax = as.numeric(year) - as.numeric(firsttax)) %>%
  group_by(years_since_tax) %>%
  summarise(
    n_obs        = sum(!is.na(tax_as_npr_best)),
    n_hospitals  = n_distinct(mcrnum[!is.na(tax_as_npr_best)]),
    n_states     = n_distinct(state[!is.na(tax_as_npr_best)]),
    mean_npr_pct = mean(tax_as_npr_best, na.rm = TRUE),
    median_npr_pct = median(tax_as_npr_best, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  arrange(years_since_tax)

cat("=== TAX AS % OF NPR, BY YEARS SINCE ADOPTION ===\n")
print(pct_npr_by_years_since_tax, n = Inf)

cat("\nStates contributing to this table:\n")
print(
  tax_amounts %>%
    filter(year >= firsttax, !is.na(tax_as_npr_best)) %>%
    distinct(state) %>%
    arrange(state)
)

# ------------------------------------------------------------------------------
# LaTeX table
# ------------------------------------------------------------------------------
library(kableExtra)

pct_npr_by_years_since_tax %>%
  transmute(
    `Years Since Tax` = years_since_tax,
    `N Hospitals`      = n_hospitals,
    `N States`         = n_states,
    `Mean (%)`         = sprintf("%.2f%%", mean_npr_pct),
    `Median (%)`       = sprintf("%.2f%%", median_npr_pct)
  ) %>%
  kbl(
    format   = "latex",
    booktabs = TRUE,
    caption  = "Tax as Percent of Net Patient Revenue, by Years Since Adoption",
    align    = c("c", "r", "r", "r", "r")
  ) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  add_footnote(
    paste(
      "Tax as % of NPR is mathematically identical to the state's own statutory",
      "rate for single-rate, revenue-based states; this table reflects a mix of",
      "real rate escalation over time and cross-state composition (which states",
      "populate each years-since-tax bucket), not a within-hospital burden",
      "trajectory. Michigan excluded (flat per-bed fee, not comparable in",
      "dollar/percent terms to revenue-based taxes)."
    ),
    notation = "none"
  ) %>%
  writeLines("tax_pct_npr_by_years_since_tax.tex")

cat("\nDone -- tax_pct_npr_by_years_since_tax.tex saved.\n")
