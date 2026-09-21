###########################################################
# Stacked DiD by Pre-Tax Op-Margin Quartile -- Results Table
# Builds a LaTeX table (outcome rows x quartile columns) of a single
# ATT per quartile x outcome cell, matching the style of
# canay_qr_results_table.R / longdiff_results_table.R.
#
# The single ATT per cell is NOT a naive average of the event-study
# coefficients (that would need the full covariance matrix to get a
# valid SE, which isn't available post-hoc). It's fixest::aggregate(),
# which does this properly -- see ?aggregate.fixest ("Sun and Abraham"
# aggregation) -- applied to the same i(rel_year, treated, ref=-1)
# model each quartile's regression already used, collapsing all
# non-negative rel_year:treated terms into one coefficient + SE.
#
# Requires stacked_data with opmargin_quartile already built
# (stacked_did_by_opmargin_quartile.R) in this session.
############################################################

library(dplyr)
library(fixest)
library(kableExtra)

quartile_outcomes <- c("op_bed", "npr_bed", "mcaid_prop_discharges",
                        "uncomp_bed_w95", "mcaid_dis_bed", "op_margin")

fit_quartile_att <- function(outcome_var, q, data = stacked_data) {
  q_data <- data %>% filter(opmargin_quartile == q)
  formula_str <- paste0(outcome_var, " ~ i(rel_year, treated, ref = -1) + exp_status + median_income_pre | mcrnum^df + year^df")
  m <- feols(as.formula(formula_str), data = q_data, weights = ~Q_weight, cluster = ~state)
  agg <- aggregate(m, agg = c("ATT" = "rel_year::[^-]+:treated"))
  data.frame(
    outcome  = outcome_var,
    quartile = q,
    estimate = agg["ATT", "Estimate"],
    se       = agg["ATT", "Std. Error"],
    p_value  = agg["ATT", "Pr(>|t|)"],
    row.names = NULL
  )
}

quartile_att_results <- bind_rows(lapply(quartile_outcomes, function(oc) {
  bind_rows(lapply(1:4, function(q) fit_quartile_att(oc, q)))
})) %>%
  mutate(stars = case_when(
    p_value < 0.01 ~ "***", p_value < 0.05 ~ "**", p_value < 0.10 ~ "*", TRUE ~ ""
  ))

cat("\n=== STACKED DiD ATT BY PRE-TAX OP-MARGIN QUARTILE (Q1 = lowest margin) ===\n")
print(quartile_att_results, digits = 4, row.names = FALSE)

# ------------------------------------------------------------------
# Format: coefficient + stars on one row, (SE) on the row underneath
# -- same convention as canay_qr_results_table.R
# ------------------------------------------------------------------
fmt_num <- function(x) ifelse(abs(x) >= 1000, scales::comma(x, accuracy = 1), sprintf("%.4f", x))

outcome_labels <- c(
  op_bed                 = "Operating Expenses per Bed",
  npr_bed                = "Net Patient Revenue per Bed",
  mcaid_prop_discharges  = "Medicaid Share of Discharges",
  uncomp_bed_w95         = "Uncompensated Care per Bed (winsorized)",
  mcaid_dis_bed          = "Medicaid Discharges per Bed",
  op_margin              = "Operating Margin"
)

formatted <- quartile_att_results %>%
  mutate(
    Label   = outcome_labels[outcome],
    est_fmt = paste0(fmt_num(estimate), stars),
    se_fmt  = paste0("(", fmt_num(se), ")")
  )

long <- do.call(rbind, lapply(quartile_outcomes, function(oc) {
  sub <- formatted %>% filter(outcome == oc) %>% arrange(quartile)
  rbind(
    c(Outcome = sub$Label[1], setNames(sub$est_fmt, paste0("q", sub$quartile))),
    c(Outcome = "",           setNames(sub$se_fmt,  paste0("q", sub$quartile)))
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
    caption   = "Stacked DiD: ATT by Pre-Tax Operating Margin Quartile",
    col.names = c("Outcome", "Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)"),
    align     = c("l", "r", "r", "r", "r"),
    escape    = FALSE
  ) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  as.character() %>%
  strsplit("\n") %>%
  unlist()

bottomrule_idx <- which(tex_lines == "\\bottomrule")
footnote_lines <- c(
  "",
  "\\multicolumn{5}{l}{\\footnotesize Quartiles of each hospital's own pre-treatment (rel\\_year$<$0) average operating margin; Q1 = lowest.}\\\\",
  "\\multicolumn{5}{l}{\\footnotesize ATT is fixest::aggregate() applied to i(rel\\_year, treated, ref=-1), collapsing all post-period}\\\\",
  "\\multicolumn{5}{l}{\\footnotesize rel\\_year:treated terms into one coefficient (Sun and Abraham 2021 aggregation).}\\\\",
  "\\multicolumn{5}{l}{\\footnotesize Standard errors clustered by state. * p$<$0.10, ** p$<$0.05, *** p$<$0.01.}\\\\"
)
tex_lines <- append(tex_lines, footnote_lines, after = bottomrule_idx)

writeLines(tex_lines, "stacked_did_quartile_results_table.tex")
cat("\nSaved: stacked_did_quartile_results_table.tex\n")
