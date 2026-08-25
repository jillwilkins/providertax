# ==============================================================================
# TABLE: MEDICAID EXPANSION CHECKS FOR THE PROVIDER TAX PAPER
# Combines Part 1 (full timing vs. pre-2014-only) and Part 3 (triple
# interaction) from expansion_analysis.R into one table, two panels, Panel B
# stacked BENEATH Panel A (not side by side) in a single shared tabular --
# same hand-built-LaTeX convention as attrition_table.R, since kbl()'s
# add_header_above is built for side-by-side column groups, not stacked
# panels with different column meanings.
#   Panel A: Triple Interaction (non-expansion baseline vs. expansion
#            differential)
#   Panel B: Pre-2014-Only ("no expansion") -- Full Timing column dropped
#            since it just repeats the main results table; compare against
#            that table directly instead.
# Requires results_full, results_pre2014, and triple_results already in the
# environment (i.e. expansion_analysis.R already run).
# ==============================================================================

library(dplyr)

outcome_labels <- c(
  op_bed                 = "Operating Expenses per Bed",
  npr_bed                = "Net Patient Revenue per Bed",
  op_margin              = "Operating Margin",
  mcaid_prop_discharges  = "Medicaid Share of Discharges",
  mcaid_dis_bed          = "Medicaid Discharges per Bed",
  uncomp_bed             = "Uncompensated Care per Bed",
  ucc2npr                = "UCC-to-NPR Ratio"
)

financial_outcomes <- c("op_bed", "npr_bed", "op_margin")
payer_mix_outcomes  <- c("mcaid_prop_discharges", "mcaid_dis_bed", "uncomp_bed", "ucc2npr")

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

n_cols <- 3
row_line <- function(cells) {
  paste(c(cells, rep("", n_cols - length(cells))), collapse = " & ") %>% paste0(" \\\\")
}

# ------------------------------------------------------------------------------
# Panel A rows: Outcome | Non-Expansion | Expansion Diff.
# ------------------------------------------------------------------------------
panel_a_row <- function(o) {
  tr <- triple_results %>% filter(outcome == o)
  row_line(c(
    outcome_labels[[o]],
    fmt_cell(tr$att_nonexpansion, tr$se_nonexpansion, tr$p_nonexpansion),
    fmt_cell(tr$att_expansion_extra, tr$se_expansion_extra, tr$p_expansion_extra)
  ))
}

panel_a_rows <- c(
  paste0("\\multicolumn{", n_cols, "}{l}{\\textbf{Panel A: Triple Interaction}}\\\\"),
  row_line(c("Outcome", "Non-Expansion", "Expansion Diff.")),
  "\\midrule",
  paste0("\\multicolumn{", n_cols, "}{l}{\\textit{Financial Outcomes}}\\\\"),
  sapply(financial_outcomes, panel_a_row),
  paste0("\\multicolumn{", n_cols, "}{l}{\\textit{Payer Mix / Uncompensated Care}}\\\\"),
  sapply(payer_mix_outcomes, panel_a_row)
)

# ------------------------------------------------------------------------------
# Panel B rows: Outcome | Pre-2014 Only
# (Full Timing dropped -- it just repeats the main results table)
# ------------------------------------------------------------------------------
panel_b_row <- function(o) {
  p2 <- results_pre2014 %>% filter(outcome == o)
  row_line(c(
    outcome_labels[[o]],
    if (is.na(p2$att_pre2014)) "--" else fmt_cell(p2$att_pre2014, p2$se_pre2014, p2$p_pre2014)
  ))
}

panel_b_rows <- c(
  paste0("\\multicolumn{", n_cols, "}{l}{\\textbf{Panel B: Pre-2014-Only (\"No Expansion\")}}\\\\"),
  row_line(c("Outcome", "Pre-2014 Only")),
  "\\midrule",
  paste0("\\multicolumn{", n_cols, "}{l}{\\textit{Financial Outcomes}}\\\\"),
  sapply(financial_outcomes, panel_b_row),
  paste0("\\multicolumn{", n_cols, "}{l}{\\textit{Payer Mix / Uncompensated Care}}\\\\"),
  sapply(payer_mix_outcomes, panel_b_row)
)

# ------------------------------------------------------------------------------
# Combine into one table, one tabular, Panel B beneath Panel A
# ------------------------------------------------------------------------------
combined <- c(
  "\\begin{table}[!h]",
  "\\centering",
  "\\caption{Medicaid Expansion Checks}",
  paste0("\\begin{tabular}{l", paste(rep("r", n_cols - 1), collapse = ""), "}"),
  "\\toprule",
  panel_a_rows,
  "\\addlinespace",
  panel_b_rows,
  "\\bottomrule",
  "\\end{tabular}",
  paste0(
    "\\footnotesize{Panel A reports a single triple-interaction model on the full-timing sample: ",
    "Non-Expansion is the ATT for never-expanded states (Treated x Post); ",
    "Expansion Diff. is the additional effect for ever-expanded states ",
    "(Treated x Post x EverExpanded), the coefficient of interest for testing a ",
    "differential effect. Panel B re-estimates the baseline stacked DiD (pooled treated x post ",
    "coefficient, Q-weighted, hospital x cohort and year x cohort fixed effects) restricted to ",
    "year < 2014, before ACA Medicaid expansion took effect almost everywhere; this sample uses ",
    "only the 2005, 2006, and 2009 adoption cohorts. Compare against the full-timing ATT in the ",
    "main results table. SEs clustered by state. * p<0.10, ** p<0.05, *** p<0.01.}"
  ),
  "\\end{table}"
)

cat("\n=== EXPANSION TABLE (preview) ===\n")
cat(paste(combined, collapse = "\n"), "\n")

writeLines(combined, "expansion_table.tex")

cat("\nDone -- expansion_table.tex saved.\n")
