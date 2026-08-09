# ==============================================================================
# ATTRITION TABLE (LaTeX): two panels, ONE shared tabular
#   Panel A: exit timing relative to each hospital's own adoption year
#            (exit_vs_own_adoption) -- Exit Timing | N | %
#   Panel B: pre-period characteristics, cleanly exited vs. stayed
#            (leaver_summary), transposed -- Variable | Cleanly Exited |
#            Stayed Through Panel End (variables as rows, status as columns)
# Both panels are 3 columns wide now that Panel B is transposed, so they
# share one tabular with no blank-padding needed. Built by hand (not kbl())
# so both panels sit in a single tabular block, easier to paste directly
# into an existing LaTeX document.
# Requires exit_vs_own_adoption and leaver_summary already in the
# environment (attrition_analysis.R).
# ==============================================================================

library(dplyr)

fmt_pct    <- function(x, digits = 1) paste0(formatC(x, format = "f", digits = digits), "\\%")
fmt_num    <- function(x, digits = 0) formatC(x, format = "f", digits = digits, big.mark = ",")
fmt_signed <- function(x, digits = 3) formatC(x, format = "f", digits = digits, flag = "+")

n_cols <- 3

row_line <- function(cells) paste(c(cells, rep("", n_cols - length(cells))), collapse = " & ") %>% paste0(" \\\\")

# ------------------------------------------------------------------------------
# Panel A rows (3 real columns, 5 blank)
# ------------------------------------------------------------------------------
panel_a <- exit_vs_own_adoption %>%
  arrange(desc(exit_period))  # "Before own adoption" first

panel_a_rows <- c(
  paste0("\\multicolumn{", n_cols, "}{l}{\\textbf{Panel A: Exit Timing Relative to Own Adoption Year}}\\\\"),
  row_line(c("Exit Timing", "N", "\\%")),
  "\\midrule",
  sapply(seq_len(nrow(panel_a)), function(i) {
    row_line(c(panel_a$exit_period[i], as.character(panel_a$n[i]), fmt_pct(panel_a$pct[i])))
  })
)

# ------------------------------------------------------------------------------
# Panel B rows, transposed: one row per variable, one column per status
# ------------------------------------------------------------------------------
exited <- leaver_summary %>% filter(status == "Cleanly exited")
stayed <- leaver_summary %>% filter(status == "Stayed through panel end")

panel_b_vars <- list(
  list(label = "N Hospitals",    exited = as.character(exited$n_hospitals),         stayed = as.character(stayed$n_hospitals)),
  list(label = "Beds",           exited = fmt_num(exited$pre_beds_mean, 0),         stayed = fmt_num(stayed$pre_beds_mean, 0)),
  list(label = "Op. Margin",     exited = fmt_signed(exited$pre_op_margin_mean, 3), stayed = fmt_signed(stayed$pre_op_margin_mean, 3)),
  list(label = "Discharges",     exited = fmt_num(exited$pre_tot_discharges_mean, 0), stayed = fmt_num(stayed$pre_tot_discharges_mean, 0)),
  list(label = "\\% Rural",      exited = fmt_pct(exited$pct_rural),                stayed = fmt_pct(stayed$pct_rural)),
  list(label = "\\% Nonprofit",  exited = fmt_pct(exited$pct_nonprofit),            stayed = fmt_pct(stayed$pct_nonprofit)),
  list(label = "\\% For-Profit", exited = fmt_pct(exited$pct_forprofit),            stayed = fmt_pct(stayed$pct_forprofit))
)

panel_b_rows <- c(
  paste0("\\multicolumn{", n_cols, "}{l}{\\textbf{Panel B: Pre-Period Characteristics, Cleanly Exited vs. Stayed}}\\\\"),
  row_line(c("Variable", "Cleanly Exited", "Stayed Through Panel End")),
  "\\midrule",
  sapply(panel_b_vars, function(v) row_line(c(v$label, v$exited, v$stayed)))
)

# ------------------------------------------------------------------------------
# Combine into one table, one tabular
# ------------------------------------------------------------------------------
combined <- c(
  "\\begin{table}[!h]",
  "\\centering",
  "\\caption{Hospital Attrition from the Balanced Panel}",
  paste0("\\begin{tabular}{l", paste(rep("r", n_cols - 1), collapse = ""), "}"),
  "\\toprule",
  panel_a_rows,
  "\\addlinespace",
  panel_b_rows,
  "\\bottomrule",
  "\\end{tabular}",
  paste0(
    "\\footnotesize{Panel A covers treated hospitals with a clean exit (present, ",
    "then gone for good) from the balanced panel, split by whether the exit ",
    "happened before or at/after their own state's tax adoption. Panel B compares ",
    "pre-period characteristics of treated hospitals that cleanly exit vs. those ",
    "observed through the panel's final year. See attrition\\_analysis.R and ",
    "attrition\\_analysis\\_notes.md for definitions and caveats -- exiting the ",
    "balanced panel is not proof of hospital closure.}"
  ),
  "\\end{table}"
)

writeLines(combined, "attrition_table.tex")
cat("Done -- attrition_table.tex saved.\n")
