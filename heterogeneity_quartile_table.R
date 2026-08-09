# ==============================================================================
# HETEROGENEITY: PRE-PERIOD CHARACTERISTICS BY WIN QUARTILE (LaTeX table)
# Transposed from quartile_summary (heterogeneity_win.R): quartiles as
# columns, pre-period characteristics as rows -- fits a page better than the
# original orientation. Excludes the *_win_mean "moving in tandem" columns;
# this table is pre-period characteristics only.
# Requires quartile_summary already in the environment (heterogeneity_win.R).
# ==============================================================================

library(dplyr)
library(kableExtra)

fmt_num    <- function(x, digits = 1) formatC(x, format = "f", digits = digits, big.mark = ",")
fmt_pct    <- function(x, digits = 1) paste0(formatC(x, format = "f", digits = digits), "%")
fmt_dollar <- function(x) scales::dollar(x, accuracy = 1)

quartile_table <- quartile_summary %>%
  arrange(win_quartile) %>%
  transmute(
    win_quartile,
    `N Hospitals`                 = as.character(n_hospitals),
    `Beds`                        = fmt_num(pre_beds_mean, 0),
    `Operating Margin`            = fmt_pct(pre_op_margin_mean * 100, 1),
    `Total Discharges`            = fmt_num(pre_tot_discharges_mean, 0),
    `Medicaid Share of Discharges` = fmt_pct(pre_mcaid_prop_dis_mean * 100, 1),
    `Medicaid Discharges per Bed` = fmt_num(pre_mcaid_dis_bed_mean, 2),
    `Uncompensated Care per Bed`  = fmt_dollar(pre_uncomp_bed_mean),
    `UCC-to-NPR Ratio`            = fmt_pct(pre_ucc2npr_mean * 100, 1),
    `% Rural`                     = fmt_pct(pct_rural, 1),
    `% Nonprofit`                 = fmt_pct(pct_nonprofit, 1),
    `% For-Profit`                = fmt_pct(pct_forprofit, 1)
  )

# Transpose: variables become rows, quartiles become columns
quartile_table_t <- quartile_table %>%
  select(-win_quartile) %>%
  t() %>%
  as.data.frame()
names(quartile_table_t) <- paste0("Q", quartile_table$win_quartile)
quartile_table_t <- tibble::rownames_to_column(quartile_table_t, "Variable")

cat("=== PRE-PERIOD CHARACTERISTICS BY WIN QUARTILE (transposed) ===\n")
print(quartile_table_t, row.names = FALSE)

# ------------------------------------------------------------------------------
# LaTeX table
# ------------------------------------------------------------------------------
quartile_table_t %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    caption   = "Hospital Pre-Period Characteristics by Operating Margin \"Win\" Quartile",
    col.names = c("Variable", "Q1 (Smallest Win)", "Q2", "Q3", "Q4 (Largest Win)"),
    align     = c("l", "r", "r", "r", "r")
  ) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  add_footnote(
    paste(
      "Pre-period (rel_year < 0) hospital characteristics, by quartile of the",
      "winsorized individual hospital effect on operating margin (Q4 = largest",
      "improvement). See heterogeneity_win.R and heterogeneity_analysis_notes.md",
      "for the effect definition and winsorizing rationale."
    ),
    notation = "none"
  ) %>%
  writeLines("heterogeneity_quartile_table.tex")

cat("\nDone -- heterogeneity_quartile_table.tex saved.\n")
