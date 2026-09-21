###########################################################
# Long-Difference Quantile Regression Results Table
# (renamed from quantreg_results_table.R)
# Builds a LaTeX table of the long-difference quantile DiD estimates
# (treated coefficient, all 5 taus, state-cluster-bootstrap SEs) for
# every outcome in longdiff.R -- coefficient + significance stars on one
# row, SE in parentheses on the row underneath, matching the style of
# results_tables/main_results_table.R.
#
# Requires longdiff.R already run in this session (ld_all_results).
############################################################

library(dplyr)
library(kableExtra)

cat("\n=== SIGNIFICANCE OF treated COEFFICIENT, ALL OUTCOMES x TAUS ===\n")
print(ld_all_results, digits = 4, row.names = FALSE)

# ------------------------------------------------------------------
# Format: coefficient + stars on one row, (SE) on the row underneath
# ------------------------------------------------------------------
fmt_num <- function(x) ifelse(abs(x) >= 1000, scales::comma(x, accuracy = 1), sprintf("%.4f", x))

outcome_labels <- c(
  op_bed                 = "Operating Expenses per Bed",
  npr_bed                = "Net Patient Revenue per Bed",
  mcaid_prop_discharges  = "Medicaid Share of Discharges",
  uncomp_bed_w95         = "Uncompensated Care per Bed (winsorized)",
  op_margin              = "Operating Margin"
)

formatted <- ld_all_results %>%
  mutate(
    Label   = outcome_labels[outcome],
    est_fmt = paste0(fmt_num(estimate), stars),
    se_fmt  = paste0("(", fmt_num(se), ")")
  )

# Interleave beta/SE rows per outcome (same convention as main_results_table.R)
long <- do.call(rbind, lapply(unique(formatted$Label), function(lab) {
  sub <- formatted %>% filter(Label == lab) %>% arrange(tau)
  rbind(
    c(Outcome = lab, setNames(sub$est_fmt, paste0("tau_", sub$tau))),
    c(Outcome = "",  setNames(sub$se_fmt,  paste0("tau_", sub$tau)))
  )
}))
long <- as.data.frame(long, stringsAsFactors = FALSE)
rownames(long) <- NULL

cat("\n=== FORMATTED (BETA / SE STACKED) ===\n")
print(long)

# ------------------------------------------------------------------
# LaTeX table
# ------------------------------------------------------------------
tex_lines <- long %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    caption   = "Quantile Difference-in-Differences: Long-Difference Estimates by Outcome and Quantile",
    col.names = c("Outcome", "$\\tau=0.10$", "$\\tau=0.25$", "$\\tau=0.50$", "$\\tau=0.75$", "$\\tau=0.90$"),
    align     = c("l", "r", "r", "r", "r", "r"),
    escape    = FALSE
  ) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  as.character() %>%
  strsplit("\n") %>%
  unlist()

bottomrule_idx <- which(tex_lines == "\\bottomrule")
footnote_lines <- c(
  "",
  "\\multicolumn{6}{l}{\\footnotesize Standard errors from a state-level cluster bootstrap (500 replicates).}\\\\",
  "\\multicolumn{6}{l}{\\footnotesize Outcome is the pre/post long difference within each hospital x cohort sub-experiment.}\\\\",
  "\\multicolumn{6}{l}{\\footnotesize * p$<$0.10, ** p$<$0.05, *** p$<$0.01.}\\\\"
)
tex_lines <- append(tex_lines, footnote_lines, after = bottomrule_idx)

writeLines(tex_lines, "longdiff_results_table.tex")
cat("\nSaved: longdiff_results_table.tex\n")
