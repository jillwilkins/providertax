# ==============================================================================
# TAX SUMMARY: SELECTED CALENDAR YEARS
# Average dollar amount paid and tax as % of NPR, for a handful of snapshot
# years (2010, 2015, 2019, 2022, 2023), plus how many states/hospitals feed
# into each year's numbers. A cross-sectional snapshot, not a trajectory --
# see tax_pct_npr_summary.R for the years-since-adoption version and its
# composition-vs-real-escalation caveat, which applies here too.
# Dollar amounts are inflation-adjusted to 2023 dollars (medical_cpi.R) so
# the year-over-year change reflects real growth, not nominal/inflation
# growth -- % of NPR needs no adjustment, it's already a same-period ratio.
# Requires tax_amounts already in the environment (calc_tax_amounts.R).
# ==============================================================================

library(dplyr)
library(kableExtra)

source("medical_cpi.R")

snapshot_years <- c(2010, 2015, 2019, 2022, 2023)

tax_summary_by_year <- tax_amounts %>%
  filter(year %in% snapshot_years, year >= firsttax, state != "MI") %>%
  group_by(year) %>%
  summarise(
    n_obs             = sum(!is.na(tax_amount_best)),
    n_hospitals       = n_distinct(mcrnum[!is.na(tax_amount_best)]),
    n_states          = n_distinct(state[!is.na(tax_amount_best)]),
    mean_amount       = mean(tax_amount_best, na.rm = TRUE),
    median_amount     = median(tax_amount_best, na.rm = TRUE),
    mean_pct_npr      = mean(tax_as_npr_best, na.rm = TRUE),
    median_pct_npr    = median(tax_as_npr_best, na.rm = TRUE),
    .groups           = "drop"
  ) %>%
  left_join(medical_cpi %>% select(year, inflation_factor_2023), by = "year") %>%
  mutate(
    mean_amount_real   = mean_amount * inflation_factor_2023,
    median_amount_real = median_amount * inflation_factor_2023
  ) %>%
  arrange(year)

cat("=== TAX SUMMARY BY CALENDAR YEAR (2023$) ===\n")
print(tax_summary_by_year, n = Inf)

cat("\nStates contributing to each year:\n")
print(
  tax_amounts %>%
    filter(year %in% snapshot_years, year >= firsttax, !is.na(tax_amount_best)) %>%
    group_by(year) %>%
    summarise(states = paste(sort(unique(state)), collapse = ", "), .groups = "drop"),
  n = Inf, width = Inf
)

states_in_table <- tax_amounts %>%
  filter(year %in% snapshot_years, year >= firsttax, state != "MI", !is.na(tax_amount_best)) %>%
  distinct(state) %>%
  arrange(state) %>%
  pull(state) %>%
  paste(collapse = ", ")

# ------------------------------------------------------------------------------
# LaTeX table
# ------------------------------------------------------------------------------
tax_summary_by_year %>%
  transmute(
    Year          = year,
    `N Hospitals` = n_hospitals,
    `N States`    = n_states,
    `Mean ($)`    = scales::dollar(mean_amount_real, accuracy = 1),
    `Median ($)`  = scales::dollar(median_amount_real, accuracy = 1),
    `Mean (%)`    = sprintf("%.2f%%", mean_pct_npr),
    `Median (%)`  = sprintf("%.2f%%", median_pct_npr)
  ) %>%
  kbl(
    format   = "latex",
    booktabs = TRUE,
    caption  = "Provider Tax Payments and Percent of Net Patient Revenue, Selected Years",
    align    = c("c", "r", "r", "r", "r", "r", "r")
  ) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  add_footnote(
    c(
      paste(
        "Dollar amounts are inflation-adjusted to 2023 dollars using the",
        "Medical Care CPI (CUUR0000SAM, annual averages); % of NPR is a",
        "same-period ratio and needs no adjustment. Michigan excluded (flat",
        "per-bed fee, not comparable in dollar/percent terms to",
        "revenue-based taxes). See tax_summary_by_year_full.tex in the",
        "appendix for every available year."
      ),
      paste("States contributing to this table:", states_in_table)
    ),
    notation = "none"
  ) %>%
  writeLines("tax_summary_by_year.tex")

cat("\nDone -- tax_summary_by_year.tex saved.\n")
