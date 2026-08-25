# ==============================================================================
# EVENT WINDOW ROBUSTNESS CHECK
# Re-estimates the pooled ATT for the four main outcomes (op_bed, npr_bed,
# mcaid_prop_discharges, uncomp_bed) under narrower (+/-3 year) and wider
# (+/-5 year) stacked-DiD event windows, alongside the canonical +/-4 window
# already used everywhere else (main_results_table.R), to see how sensitive
# the results are to that choice.
#
# Uncompensated care uses uncomp_bed_w95 (1%/95% winsorized), matching the
# aggressive UCC-specific winsorization already established in
# event_studies_ucc_wins95.R / uncomp_wins_att_comparison.R -- winsorized
# within EACH rebuilt stack separately (same convention as those scripts),
# not on the raw hospdata_analysis, so the caps reflect that specific
# analysis sample's own distribution.
#
# Pooled ATT is the single treated:post coefficient from a model that also
# includes the pre-period rel_year dummies + controls (same spec as
# main_results_table.R's fit_pooled()) -- NOT a post-hoc average of
# separately estimated event-study coefficients, which is a materially
# different (and less appropriate) quantity with its own, wrong SE.
#
# Does NOT modify 5_buildstacked.R or its window=4 stacked_data -- builds
# separate stacks (stacked_data_w3, stacked_data_w5) via a standalone,
# parameterized version of the same sub-experiment/balancing/Q-weight logic,
# for this robustness check only.
#
# Cohort availability changes with window width (yr_min=1999, yr_max=2022,
# same as 5_buildstacked.R): window=3 additionally includes the 2019 cohort
# (excluded at window=4/5 since 2019+4 and 2019+5 exceed yr_max=2022);
# window=5 uses the same 9 cohorts as window=4 (the largest cohort, 2017,
# sits exactly at the yr_max boundary either way).
#
# Requires hospdata_analysis already in the environment (built by
# 4_datamedicaid.R).
# ==============================================================================

library(dplyr)
library(fixest)
library(purrr)
library(kableExtra)

winsorize_var <- function(x, probs = c(0.01, 0.95)) {
  bounds <- quantile(x, probs = probs, na.rm = TRUE)
  pmin(pmax(x, bounds[1]), bounds[2])
}

yr_min <- 1999
yr_max <- 2022

build_stack <- function(window) {
  hospdata_stack <- hospdata_analysis %>%
    filter(cohort != "Always (2004)")

  cohorts <- hospdata_stack %>%
    filter(gname != 0) %>%
    distinct(gname) %>%
    pull() %>%
    sort() %>%
    keep(~ (.x - window) >= yr_min & (.x + window) <= yr_max)

  cat("  Valid cohorts (window =", window, "):", cohorts, "\n")

  getdata <- function(j) {
    n_required <- 2 * window + 1
    raw <- hospdata_stack %>%
      filter(gname == j | gname == 0 | gname > j + window) %>%
      filter(year >= j - window, year <= j + window) %>%
      mutate(df = j, rel_year = year - j, treated = as.integer(gname == j))

    keep_hosps <- raw %>%
      group_by(mcrnum) %>%
      summarise(n_yrs = n_distinct(rel_year), .groups = "drop") %>%
      filter(n_yrs == n_required) %>%
      pull(mcrnum)

    raw %>% filter(mcrnum %in% keep_hosps)
  }

  stacked <- map_df(cohorts, getdata)

  unit_counts <- stacked %>%
    distinct(mcrnum, df, treated) %>%
    group_by(df) %>%
    summarise(n_treat_a = sum(treated == 1), n_ctrl_a = sum(treated == 0), .groups = "drop")

  n_treat_total <- sum(unit_counts$n_treat_a)
  n_ctrl_total  <- sum(unit_counts$n_ctrl_a)

  stacked <- stacked %>%
    left_join(unit_counts, by = "df") %>%
    mutate(
      treat_share = n_treat_a / n_treat_total,
      ctrl_share  = n_ctrl_a  / n_ctrl_total,
      Q_weight    = if_else(treated == 1, 1, treat_share / ctrl_share),
      post        = as.integer(rel_year >= 0)
    )

  stacked <- stacked %>% mutate(uncomp_bed_w95 = winsorize_var(uncomp_bed, c(0.01, 0.95)))

  cat("  N hospitals:", n_distinct(stacked$mcrnum), " N rows:", nrow(stacked), "\n")
  stacked
}

cat("=== Building window = 3 stack ===\n")
stacked_data_w3 <- build_stack(3)
cat("\n=== Building window = 5 stack ===\n")
stacked_data_w5 <- build_stack(5)

if (!"uncomp_bed_w95" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(uncomp_bed_w95 = winsorize_var(uncomp_bed, c(0.01, 0.95)))
}

# ------------------------------------------------------------------------------
# Pooled ATT, same spec as main_results_table.R's fit_pooled(), generalized
# to the window's own pre-period keep range
# ------------------------------------------------------------------------------
outcome_info <- list(
  list(var = "op_bed",                label = "Operating Expenses per Bed"),
  list(var = "npr_bed",               label = "Net Patient Revenue per Bed"),
  list(var = "mcaid_prop_discharges", label = "Medicaid Share of Discharges"),
  list(var = "uncomp_bed_w95",        label = "Uncompensated Care per Bed (Wins. 1\\%/95\\%)")
)

fit_pooled <- function(outcome_var, data, window) {
  formula_str <- paste0(
    outcome_var,
    " ~ treated:post + i(rel_year, treated, ref = -1, keep = -", window, ":-2) + exp_status + median_income_pre",
    " | mcrnum^df + year^df"
  )
  feols(as.formula(formula_str), data = data, weights = ~Q_weight, cluster = ~state)
}

run_window <- function(data, window, window_label) {
  map_df(outcome_info, function(oi) {
    m <- fit_pooled(oi$var, data, window)
    cn <- names(coef(m))
    pt <- cn[grepl("treated.*post|post.*treated", cn)]
    data.frame(
      Outcome = oi$label,
      Window  = window_label,
      Beta    = as.numeric(coef(m)[pt]),
      SE      = as.numeric(m$se[pt]),
      P       = as.numeric(pvalue(m)[pt]),
      N_Hosp  = n_distinct(data$mcrnum[data$treated == 1])
    )
  })
}

if (!"post" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(post = as.integer(rel_year >= 0))
}

cat("\n=== FITTING WINDOW = 3 ===\n")
res_w3 <- run_window(stacked_data_w3, 3, "+/-3 years")
cat("\n=== FITTING WINDOW = 4 (canonical, from stacked_data) ===\n")
res_w4 <- run_window(stacked_data, 4, "+/-4 years (canonical)")
cat("\n=== FITTING WINDOW = 5 ===\n")
res_w5 <- run_window(stacked_data_w5, 5, "+/-5 years")

window_results <- bind_rows(res_w3, res_w4, res_w5) %>%
  mutate(
    Stars = case_when(P < 0.01 ~ "***", P < 0.05 ~ "**", P < 0.10 ~ "*", TRUE ~ ""),
    Outcome = factor(Outcome, levels = sapply(outcome_info, function(o) o$label)),
    Window  = factor(Window, levels = c("+/-3 years", "+/-4 years (canonical)", "+/-5 years"))
  ) %>%
  arrange(Outcome, Window)

cat("\n\n=== EVENT WINDOW ROBUSTNESS: POOLED ATT BY OUTCOME ===\n")
print(window_results %>% select(Outcome, Window, Beta, SE, Stars, N_Hosp), digits = 4)

# ------------------------------------------------------------------------------
# LaTeX table: outcomes as rows (beta+stars, SE below), one column per
# window, N Hospitals as the final row
# ------------------------------------------------------------------------------
fmt_num <- function(x) ifelse(abs(x) >= 1000, scales::comma(x, accuracy = 1), sprintf("%.4f", x))

window_wide <- window_results %>%
  mutate(Beta_fmt = paste0(fmt_num(Beta), Stars), SE_fmt = paste0("(", fmt_num(SE), ")"))

window_cols <- levels(window_results$Window)

beta_se_rows <- do.call(rbind, lapply(outcome_info, function(oi) {
  r <- window_wide %>% filter(Outcome == oi$label)
  beta_row <- c(Outcome = oi$label, setNames(r$Beta_fmt[match(window_cols, r$Window)], window_cols))
  se_row   <- c(Outcome = "",       setNames(r$SE_fmt[match(window_cols, r$Window)],   window_cols))
  rbind(beta_row, se_row)
}))
rownames(beta_se_rows) <- NULL

n_hosp_row <- window_results %>%
  distinct(Window, N_Hosp) %>%
  arrange(Window)
n_hosp_row <- c(Outcome = "N Hospitals", setNames(scales::comma(n_hosp_row$N_Hosp), n_hosp_row$Window))

window_table_rows <- rbind(beta_se_rows, n_hosp_row)

tbl <- window_table_rows %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    row.names = FALSE,
    caption   = "Event Window Robustness: Pooled ATT by Outcome",
    col.names = c("Outcome", window_cols),
    align     = c("l", "r", "r", "r")
  ) %>%
  kable_styling(latex_options = c("hold_position"))

tbl %>% writeLines("window_robustness.tex")

cat("\nDone -- window_results holds the comparison across +/-3, +/-4, +/-5 year windows.\n")
cat("Saved: window_robustness.tex\n")
