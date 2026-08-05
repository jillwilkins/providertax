# ==============================================================================
# TABLE: MEDICAID EXPANSION CHECKS FOR THE PROVIDER TAX PAPER
# Combines Part 1 (full timing vs. pre-2014-only) and Part 3 (triple
# interaction) from expansion_analysis.R into one table, two panels:
#   Panel A: Triple Interaction (non-expansion baseline vs. expansion
#            differential)
#   Panel B: Full Timing vs. Pre-2014-Only ("no expansion")
# Requires results_full, results_pre2014, and triple_results already in the
# environment (i.e. expansion_analysis.R already run).
# ==============================================================================

library(dplyr)
library(kableExtra)

outcome_labels <- c(
  op_bed                 = "Operating Expenses per Bed",
  npr_bed                = "Net Patient Revenue per Bed",
  op_margin              = "Operating Margin",
  mcaid_prop_discharges  = "Medicaid Share of Discharges",
  mcaid_dis_bed          = "Medicaid Discharges per Bed",
  uncomp_bed             = "Uncompensated Care per Bed",
  ucc2npr                = "UCC-to-NPR Ratio"
)

stars_for <- function(p) {
  case_when(
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE     ~ ""
  )
}

fmt_cell <- function(beta, se, p) {
  sprintf("%s%s (%s)", format(round(beta, 3), nsmall = 3), stars_for(p), format(round(se, 2), nsmall = 2))
}

# ------------------------------------------------------------------------------
# Assemble one row per outcome
# ------------------------------------------------------------------------------
expansion_table_raw <- outcome_labels %>%
  names() %>%
  lapply(function(o) {
    f  <- results_full     %>% filter(outcome == o)
    p2 <- results_pre2014  %>% filter(outcome == o)
    tr <- triple_results   %>% filter(outcome == o)

    data.frame(
      Outcome          = outcome_labels[[o]],
      `Non-Expansion`   = fmt_cell(tr$att_nonexpansion, tr$se_nonexpansion, tr$p_nonexpansion),
      `Expansion Diff.` = fmt_cell(tr$att_expansion_extra, tr$se_expansion_extra, tr$p_expansion_extra),
      `Full Timing`    = fmt_cell(f$att_full, f$se_full, f$p_full),
      `Pre-2014 Only`  = if (is.na(p2$att_pre2014)) "--" else fmt_cell(p2$att_pre2014, p2$se_pre2014, p2$p_pre2014),
      check.names = FALSE
    )
  }) %>%
  bind_rows()

cat("\n=== EXPANSION TABLE (preview) ===\n")
print(expansion_table_raw)

# ------------------------------------------------------------------------------
# Render to LaTeX
# ------------------------------------------------------------------------------
expansion_table_raw %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    align     = c("l", "r", "r", "r", "r"),
    col.names = c("Outcome", "Non-Expansion", "Expansion Diff.", "Full Timing", "Pre-2014 Only")
  ) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  add_header_above(c(
    " " = 1,
    "Panel A: Triple Interaction" = 2,
    "Panel B: Full Timing vs. Pre-2014-Only" = 2
  )) %>%
  pack_rows("Financial Outcomes", 1, 3) %>%
  pack_rows("Payer Mix / Uncompensated Care", 4, 7) %>%
  add_footnote(
    paste0(
      "Panel A reports a single triple-interaction model on the full-timing sample: ",
      "Non-Expansion is the ATT for never-expanded states (Treated x Post); ",
      "Expansion Diff. is the additional effect for ever-expanded states ",
      "(Treated x Post x EverExpanded), the coefficient of interest for testing a ",
      "differential effect. Panel B re-estimates the baseline stacked DiD (pooled treated x post ",
      "coefficient, Q-weighted, hospital x cohort and year x cohort fixed effects) on the full ",
      "sample vs. a sample restricted to year < 2014; the restricted sample uses only the ",
      "2005, 2006, and 2009 adoption cohorts. ",
      "SEs clustered by state. * p<0.10, ** p<0.05, *** p<0.01."
    ),
    notation = "none"
  ) %>%
  writeLines("expansion_table.tex")

cat("\nDone -- expansion_table.tex saved.\n")
