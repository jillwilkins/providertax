###########################################################
# Canay Two-Step Quantile Regression Results Table
# Builds a LaTeX table of the Canay (2011) two-step estimates (tax_on
# coefficient, all 5 taus, state-cluster-bootstrap SEs) for the main 5
# outcomes -- coefficient + significance stars on one row, SE in
# parentheses on the row underneath, matching the style of
# longdiff_results_table.R / results_tables/main_results_table.R.
#
# Restricted to the "main 5" outcomes (op_bed, npr_bed,
# mcaid_prop_discharges, uncomp_bed_w95, op_margin) -- excludes
# mcaid_dis_bed, which canay_qr.R also estimates as a 6th outcome.
#
# Requires canay_qr.R already run in this session (canay_all_results).
############################################################

library(dplyr)
library(kableExtra)

main_five <- c("op_bed", "npr_bed", "mcaid_prop_discharges", "uncomp_bed_w95", "op_margin")

canay_main_results <- canay_all_results %>% filter(outcome %in% main_five)

cat("\n=== SIGNIFICANCE OF tax_on COEFFICIENT (CANAY), MAIN 5 OUTCOMES x TAUS ===\n")
print(canay_main_results, digits = 4, row.names = FALSE)

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

formatted <- canay_main_results %>%
  mutate(
    Label   = outcome_labels[outcome],
    est_fmt = paste0(fmt_num(estimate), stars),
    se_fmt  = paste0("(", fmt_num(se), ")")
  )

# Interleave beta/SE rows per outcome (same convention as main_results_table.R)
long <- do.call(rbind, lapply(main_five, function(oc) {
  sub <- formatted %>% filter(outcome == oc) %>% arrange(tau)
  rbind(
    c(Outcome = sub$Label[1], setNames(sub$est_fmt, paste0("tau_", sub$tau))),
    c(Outcome = "",           setNames(sub$se_fmt,  paste0("tau_", sub$tau)))
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
    caption   = "Canay (2011) Two-Step Quantile Regression: Estimates by Outcome and Quantile",
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
  "\\multicolumn{6}{l}{\\footnotesize Standard errors from a state-level cluster bootstrap (200 replicates) of the full two-step procedure.}\\\\",
  "\\multicolumn{6}{l}{\\footnotesize Step 1: panel fixed-effects regression (plm, within) to estimate each hospital's fixed effect.}\\\\",
  "\\multicolumn{6}{l}{\\footnotesize Step 2: pooled quantile regression of the fixed-effect-adjusted outcome on tax\\_on and controls.}\\\\",
  "\\multicolumn{6}{l}{\\footnotesize * p$<$0.10, ** p$<$0.05, *** p$<$0.01.}\\\\"
)
tex_lines <- append(tex_lines, footnote_lines, after = bottomrule_idx)

writeLines(tex_lines, "canay_qr_results_table.tex")
cat("\nSaved: canay_qr_results_table.tex\n")
