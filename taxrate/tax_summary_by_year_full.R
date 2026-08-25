# ==============================================================================
# TAX SUMMARY: EVERY AVAILABLE YEAR (APPENDIX)
# Same as tax_summary_by_year.R but for every year with usable data instead
# of just the 5 snapshot years -- for the appendix. See tax_summary_by_year.R
# for the abbreviated version, and tax_pct_npr_summary.R for the
# composition-vs-real-escalation caveat that applies here too.
# Dollar amounts are inflation-adjusted to 2023 dollars (medical_cpi.R) so
# the year-over-year change reflects real growth, not nominal/inflation
# growth -- % of NPR needs no adjustment, it's already a same-period ratio.
# Requires tax_amounts already in the environment (calc_tax_amounts.R).
# ==============================================================================

library(dplyr)
library(kableExtra)

source("medical_cpi.R")

tax_summary_by_year_full <- tax_amounts %>%
  filter(year >= firsttax, state != "MI") %>%
  group_by(year) %>%
  summarise(
    n_obs          = sum(!is.na(tax_amount_best)),
    n_hospitals    = n_distinct(mcrnum[!is.na(tax_amount_best)]),
    n_states       = n_distinct(state[!is.na(tax_amount_best)]),
    mean_amount    = mean(tax_amount_best, na.rm = TRUE),
    median_amount  = median(tax_amount_best, na.rm = TRUE),
    mean_pct_npr   = mean(tax_as_npr_best, na.rm = TRUE),
    median_pct_npr = median(tax_as_npr_best, na.rm = TRUE),
    .groups        = "drop"
  ) %>%
  left_join(medical_cpi %>% select(year, inflation_factor_2023), by = "year") %>%
  mutate(
    mean_amount_real   = mean_amount * inflation_factor_2023,
    median_amount_real = median_amount * inflation_factor_2023
  ) %>%
  arrange(year)

cat("=== TAX SUMMARY, EVERY AVAILABLE YEAR (2023$) ===\n")
print(tax_summary_by_year_full, n = Inf)

states_in_table <- tax_amounts %>%
  filter(year >= firsttax, state != "MI", !is.na(tax_amount_best)) %>%
  distinct(state) %>%
  arrange(state) %>%
  pull(state) %>%
  paste(collapse = ", ")

# ------------------------------------------------------------------------------
# LaTeX table (longtable -- this one runs long, appendix-bound)
# ------------------------------------------------------------------------------
tax_summary_by_year_full %>%
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
    format    = "latex",
    booktabs  = TRUE,
    longtable = TRUE,
    caption   = "Provider Tax Payments and Percent of Net Patient Revenue, All Available Years",
    align     = c("c", "r", "r", "r", "r", "r", "r")
  ) %>%
  kable_styling(latex_options = c("repeat_header")) %>%
  add_footnote(
    c(
      paste(
        "Dollar amounts are inflation-adjusted to 2023 dollars using the",
        "Medical Care CPI (CUUR0000SAM, annual averages); % of NPR is a",
        "same-period ratio and needs no adjustment. Michigan excluded (flat",
        "per-bed fee, not comparable in dollar/percent terms to",
        "revenue-based taxes). Abbreviated version in tax_summary_by_year.tex."
      ),
      paste("States contributing to this table:", states_in_table)
    ),
    notation = "none"
  ) %>%
  writeLines("tax_summary_by_year_full.tex")

cat("\nDone -- tax_summary_by_year_full.tex saved.\n")
