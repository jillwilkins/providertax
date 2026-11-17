# ==============================================================================
# 7. STACKED DiD BY PRE-TAX OPERATING MARGIN QUARTILE
#
# Splits hospitals into quartiles of their own pre-treatment (rel_year < 0)
# mean operating margin and runs the stacked event study separately within
# each quartile. Cutoffs are computed on DISTINCT hospitals (treated +
# control), so a hospital's quartile is fixed across sub-experiments.
# Q1 = lowest pre-tax margin.
#
# Operating margin = margin_updated = (NPR - opex) / NPR, NA if NPR <= 0 or
# |margin| > 1. Defined ONLY in margin_updated.R (not the old op_margin in
# 1_data.R, which divides by opex).
#
# ATT per quartile x outcome: fixest::aggregate() average of the post-period
# rel_year:treated coefficients (0..4) from the same event-study model that
# makes the plot (same as the original op_margin quartile table).
#
# Run order: 5_buildstacked.R -> 6_events_paper.R (run_stacked_did()) ->
# margin_updated.R (margin_updated) -> this script.
# Outputs (quartile_stackdid/):
#   <outcome>_q1.png ... <outcome>_q4.png   event studies
#   quartile_results_table.tex              all outcomes
#   quartile_margin_table.tex               operating margin row only
# ==============================================================================

library(dplyr)
library(fixest)
library(kableExtra)

if (!exists("stacked_data"))    stop("Run 5_buildstacked.R first (needs stacked_data).")
if (!exists("run_stacked_did")) stop("Run 6_events_paper.R first (needs run_stacked_did()).")
if (!"margin_updated" %in% names(stacked_data)) stop("Run margin_updated.R first (defines margin_updated).")

out_dir <- "quartile_stackdid"
dir.create(out_dir, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. Outcomes
# ------------------------------------------------------------------------------
winsorize_var <- function(x, probs = c(0.01, 0.95)) {
  bounds <- quantile(x, probs = probs, na.rm = TRUE)
  pmin(pmax(x, bounds[1]), bounds[2])
}

stacked_data <- stacked_data %>%
  mutate(
    uncomp_bed_w95 = winsorize_var(uncomp_bed),
    mcaid_dis_bed  = mcaid_discharges / pre_beds_avg
  )

outcome_labels <- c(
  op_bed                = "Operating Expenses per Bed",
  npr_bed               = "Net Patient Revenue per Bed",
  mcaid_prop_discharges = "Medicaid Share of Discharges",
  uncomp_bed_w95        = "Uncompensated Care per Bed (winsorized)",
  mcaid_dis_bed         = "Medicaid Discharges per Bed",
  margin_updated        = "Operating Margin"
)

# ------------------------------------------------------------------------------
# 2. Quartiles of pre-tax operating margin
# ------------------------------------------------------------------------------
stacked_data <- stacked_data %>%
  mutate(pre_margin = mean(margin_updated[rel_year < 0], na.rm = TRUE), .by = mcrnum)

hosp_quartiles <- stacked_data %>%
  distinct(mcrnum, pre_margin) %>%
  filter(is.finite(pre_margin)) %>%
  mutate(margin_quartile = ntile(pre_margin, 4))

stacked_data <- stacked_data %>%
  select(-any_of("margin_quartile")) %>%
  left_join(hosp_quartiles %>% select(mcrnum, margin_quartile), by = "mcrnum")

quartile_summary <- stacked_data %>%
  filter(!is.na(margin_quartile)) %>%
  group_by(quartile = margin_quartile) %>%
  summarise(
    min_pre        = min(pre_margin),
    max_pre        = max(pre_margin),
    n_hospitals    = n_distinct(mcrnum),
    n_treated_hosp = n_distinct(mcrnum[treated == 1]),
    .groups = "drop"
  )
cat("\nPre-tax operating margin quartiles (Q1 = lowest):\n")
print(as.data.frame(quartile_summary), digits = 3)

# ------------------------------------------------------------------------------
# 3. Event study per quartile x outcome (plot) + aggregated ATT (table)
# ------------------------------------------------------------------------------
quartile_results <- list()

for (q in 1:4) {
  q_data <- stacked_data %>% filter(margin_quartile == q)
  for (oc in names(outcome_labels)) {
    res <- run_stacked_did(
      outcome_var   = oc,
      outcome_label = outcome_labels[[oc]],
      data          = q_data,
      filename      = file.path(out_dir, paste0(oc, "_q", q, ".png")),
      width         = 10,
      height        = 8
    )
    agg <- aggregate(res$model, agg = c("ATT" = "rel_year::[^-]+:treated"))
    quartile_results[[paste0(oc, "_q", q)]] <- data.frame(
      outcome  = oc, quartile = q,
      estimate = agg["ATT", "Estimate"],
      se       = agg["ATT", "Std. Error"],
      p_value  = agg["ATT", "Pr(>|t|)"]
    )
  }
}

quartile_results <- bind_rows(quartile_results) %>%
  mutate(stars = case_when(p_value < 0.01 ~ "***", p_value < 0.05 ~ "**",
                           p_value < 0.10 ~ "*", TRUE ~ ""))

cat("\n=== AGGREGATED ATT BY PRE-TAX OPERATING MARGIN QUARTILE ===\n")
print(quartile_results, digits = 4, row.names = FALSE)

# ------------------------------------------------------------------------------
# 4. LaTeX tables
# ------------------------------------------------------------------------------
fmt_num <- function(x) ifelse(abs(x) >= 1000, scales::comma(x, accuracy = 1), sprintf("%.4f", x))

formatted <- quartile_results %>%
  mutate(est_fmt = paste0(fmt_num(estimate), stars),
         se_fmt  = paste0("(", fmt_num(se), ")"))

table_rows <- function(ocs) {
  do.call(rbind, lapply(ocs, function(oc) {
    sub <- formatted %>% filter(outcome == oc) %>% arrange(quartile)
    rbind(c(outcome_labels[[oc]], sub$est_fmt),
          c("",                   sub$se_fmt))
  }))
}

# Notes kept short so no line is wider than the table (a wider
# \multicolumn line stretches the last column).
footnote_lines <- c(
  "",
  "\\multicolumn{5}{l}{\\footnotesize Quartiles of each hospital's pre-treatment average}\\\\",
  "\\multicolumn{5}{l}{\\footnotesize operating margin, (NPR $-$ opex)/NPR; Q1 = lowest.}\\\\",
  "\\multicolumn{5}{l}{\\footnotesize ATT: fixest::aggregate() average of the post-period}\\\\",
  "\\multicolumn{5}{l}{\\footnotesize rel\\_year:treated coefficients, within each quartile.}\\\\",
  "\\multicolumn{5}{l}{\\footnotesize SEs clustered by state. * p$<$0.10, ** p$<$0.05, *** p$<$0.01.}\\\\"
)

write_quartile_tex <- function(ocs, caption, file) {
  rows <- as.data.frame(table_rows(ocs), stringsAsFactors = FALSE)
  tex <- rows %>%
    kbl(format = "latex", booktabs = TRUE, escape = FALSE, caption = caption,
        col.names = c("", "Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)"),
        align = c("l", "r", "r", "r", "r"), row.names = FALSE,
        linesep = "") %>%
    kable_styling(latex_options = c("hold_position", "scale_down")) %>%
    as.character() %>% strsplit("\n") %>% unlist()
  tex <- append(tex, footnote_lines, after = which(tex == "\\bottomrule"))
  writeLines(tex, file)
  cat("Saved:", file, "\n")
}

write_quartile_tex(names(outcome_labels),
                   "Stacked DiD: ATT by Pre-Tax Operating Margin Quartile",
                   file.path(out_dir, "quartile_results_table.tex"))
write_quartile_tex("margin_updated",
                   "Stacked DiD: Operating Margin ATT by Pre-Tax Operating Margin Quartile",
                   file.path(out_dir, "quartile_margin_table.tex"))
