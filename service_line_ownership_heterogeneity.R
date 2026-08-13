# ==============================================================================
# SERVICE LINE OFFERINGS: FOR-PROFIT VS. NON-PROFIT HETEROGENEITY
# Same triple-interaction approach as expansion_analysis.R Part 3
# (treated:post + treated:post:for_profit + post:for_profit), a formal test
# of whether the ATT differs by ownership status, rather than eyeballing
# two separately-estimated split-sample ATTs. Fit TWICE with the reference
# ownership group flipped, so all three quantities get their own direct
# significance test instead of inferring one of them by addition:
#   - Non-Profit: ATT for non-profit hospitals, tested on its own
#   - For-Profit Diff.: for-profit minus non-profit; the formal test of a
#     differential effect between the two groups
#   - For-Profit Own ATT: ATT for for-profit hospitals, tested on its own
#     (previously only available as "Non-Profit + Diff.", which is the
#     right point estimate but was never actually significance-tested --
#     a differential can be significant while neither group's own effect
#     is, which turned out to matter for a couple of these outcomes)
# post:for_profit is included so the interaction isn't picking up a
# generic differential post-period trend between ownership types unrelated
# to the tax; for_profit itself and treated:for_profit are both
# time-invariant per hospital-df and absorbed by the mcrnum^df fixed
# effect, so they don't need to be added separately (same reasoning as
# expansion_analysis.R's ever_expanded).
#
# Table rows are sorted with any outcome that has at least one significant
# coefficient (p<0.10, any of the 3 columns) at the top, ranked by its
# strongest p-value; outcomes with nothing significant follow.
#
# Ownership classification: same HCRIS/AHA convention used throughout this
# project (2_dataclean.R, heterogeneity_win.R, attrition_analysis.R) --
# each hospital's OWN first non-missing typectrl/cntrl value, so ownership
# is fixed per hospital rather than time-varying (avoids mixing "hospital
# changed ownership type" with "the tax effect differs by ownership type").
#
# Does NOT rebuild the stacked sample -- uses stacked_data (with Q_weight)
# as already built by 5_buildstacked.R. Run that first.
# Requires stacked_data already in the environment.
# ==============================================================================

library(dplyr)
library(fixest)
library(purrr)
library(kableExtra)

# ------------------------------------------------------------------------------
# 1. Hospital-level ownership classification, merged onto stacked_data
# ------------------------------------------------------------------------------
hosp_ownership <- stacked_data %>%
  group_by(mcrnum) %>%
  summarise(
    typectrl = first(typectrl),
    cntrl    = first(cntrl),
    .groups  = "drop"
  ) %>%
  mutate(
    ownership = case_when(
      !is.na(typectrl) & typectrl %in% 1:2 ~ "Nonprofit",
      !is.na(typectrl) & typectrl %in% 3:6 ~ "For-profit",
      !is.na(cntrl) & cntrl %in% c(21, 23) ~ "Nonprofit",
      !is.na(cntrl) & cntrl %in% c(30, 31, 32, 33) ~ "For-profit",
      TRUE ~ "Unknown"
    ),
    for_profit = case_when(
      ownership == "For-profit" ~ 1,
      ownership == "Nonprofit"  ~ 0,
      TRUE                      ~ NA_real_
    )
  ) %>%
  select(mcrnum, for_profit)

cat("\n=== OWNERSHIP CLASSIFICATION (TREATED HOSPITALS) ===\n")
stacked_data %>%
  filter(treated == 1) %>%
  distinct(mcrnum) %>%
  left_join(hosp_ownership, by = "mcrnum") %>%
  count(for_profit) %>%
  print()

if (!"for_profit" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% left_join(hosp_ownership, by = "mcrnum")
}
if (!"post" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(post = as.integer(rel_year >= 0))
}

# ------------------------------------------------------------------------------
# 2. Outcome list (same 10 service-line outcomes)
# ------------------------------------------------------------------------------
outcome_info <- list(
  list(var = "emdephos", label = "Emergency Department",                is_binary = TRUE),
  list(var = "psyemhos", label = "Psychiatric Emergency Services",      is_binary = TRUE),
  list(var = "alchhos",  label = "Alcohol/Chemical Dependency Unit",    is_binary = TRUE),
  list(var = "traumhos", label = "Trauma Center",                      is_binary = TRUE),
  list(var = "adtchos",  label = "Adult Day Care Program",             is_binary = TRUE),
  list(var = "iclabhos", label = "Cardiac Catheterization Lab",        is_binary = TRUE),
  list(var = "eswlhos",  label = "ESWL",                               is_binary = TRUE),
  list(var = "vem",      label = "Emergency Department Visits",        is_binary = FALSE),
  list(var = "psybd",    label = "Psychiatric Beds",                   is_binary = FALSE),
  list(var = "alchbd",   label = "Alcohol/Chemical Dependency Beds",   is_binary = FALSE)
)

# ------------------------------------------------------------------------------
# 3. Triple-interaction model. Fit TWICE, flipping which ownership group is
#    the omitted/reference category, so both groups' OWN ATT (not just the
#    non-profit baseline and the relative differential) get a direct
#    significance test:
#      - for_profit as the interaction var -> treated:post = non-profit's
#        own ATT, treated:post:for_profit = the differential
#      - non_profit as the interaction var -> treated:post = for-profit's
#        own ATT directly (this is what "for_profit implied = nonprofit +
#        diff" was silently assuming was significant before -- it wasn't
#        actually tested until this refit)
# ------------------------------------------------------------------------------
if (!"non_profit" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(non_profit = 1 - for_profit)
}

fit_triple <- function(outcome_var, interaction_var) {
  formula_str <- paste0(
    outcome_var,
    " ~ treated:post + treated:post:", interaction_var, " + post:", interaction_var, " +",
    " i(rel_year, treated, ref = -1, keep = -4:-2) + exp_status + median_income_pre",
    " | mcrnum^df + year^df"
  )
  feols(as.formula(formula_str), data = stacked_data %>% filter(!is.na(for_profit)),
        weights = ~Q_weight, cluster = ~state)
}

extract_base <- function(model) {
  cn <- names(coef(model))
  base_term <- cn[grepl("^treated:post$|^post:treated$", cn)]
  list(beta = coef(model)[base_term], se = model$se[base_term], p = pvalue(model)[base_term])
}

extract_diff <- function(model, interaction_var) {
  cn <- names(coef(model))
  diff_term <- cn[grepl(paste0("treated.*post.*", interaction_var, "|", interaction_var, ".*treated.*post"), cn)]
  list(beta = coef(model)[diff_term], se = model$se[diff_term], p = pvalue(model)[diff_term])
}

triple_results <- map_df(outcome_info, function(oi) {
  m_np <- fit_triple(oi$var, "for_profit")   # non-profit is the reference group
  m_fp <- fit_triple(oi$var, "non_profit")   # for-profit is the reference group

  np   <- extract_base(m_np)
  diff <- extract_diff(m_np, "for_profit")
  fp   <- extract_base(m_fp)

  data.frame(
    outcome           = oi$var,
    att_nonprofit     = np$beta,   se_nonprofit     = np$se,   p_nonprofit     = np$p,
    att_forprofit_diff = diff$beta, se_forprofit_diff = diff$se, p_forprofit_diff = diff$p,
    att_forprofit_own = fp$beta,   se_forprofit_own = fp$se,   p_forprofit_own = fp$p
  )
})

cat("\n=== FOR-PROFIT VS. NON-PROFIT: TRIPLE-INTERACTION TEST (BOTH GROUPS TESTED DIRECTLY) ===\n")
cat("att_nonprofit = ATT for non-profit hospitals, tested on its own\n")
cat("att_forprofit_diff = for-profit minus non-profit (the differential test)\n")
cat("att_forprofit_own = ATT for for-profit hospitals, tested on its own (not just implied)\n\n")
print(triple_results, digits = 4)

# ------------------------------------------------------------------------------
# 3b. Pre-treatment means by ownership group -- context for the ATT
#    magnitudes (e.g. a given pp change means something different off an
#    ~90% base vs. an ~10% base).
# ------------------------------------------------------------------------------
pre_treat_mean_ownership <- function(outcome_var) {
  d <- stacked_data %>% filter(treated == 1, rel_year < 0, !is.na(for_profit))
  list(
    nonprofit  = mean(d[[outcome_var]][d$for_profit == 0], na.rm = TRUE),
    forprofit  = mean(d[[outcome_var]][d$for_profit == 1], na.rm = TRUE)
  )
}

triple_results <- triple_results %>%
  rowwise() %>%
  mutate(
    mean_nonprofit = pre_treat_mean_ownership(outcome)$nonprofit,
    mean_forprofit = pre_treat_mean_ownership(outcome)$forprofit
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# 4. LaTeX table: 3 columns (Non-Profit, For-Profit Diff., For-Profit Own
#    ATT). Rows sorted with any-significant outcomes first (ranked by
#    strongest p-value across the three columns), not-significant after.
# ------------------------------------------------------------------------------
stars_for <- function(p) case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")

# Beta+stars on one row, (SE) on the row underneath -- same interleaving
# convention as main_results_table.R / service_line_results_table.R. No
# "pp" unit suffix on the ATT/SE values (redundant with the table
# caption/column headers).
fmt_beta <- function(beta, p, is_binary) {
  digits <- if (is_binary) 1 else 2
  paste0(sprintf(paste0("%.", digits, "f"), beta * if (is_binary) 100 else 1), stars_for(p))
}
fmt_se <- function(se, is_binary) {
  digits <- if (is_binary) 1 else 2
  paste0("(", sprintf(paste0("%.", digits, "f"), se * if (is_binary) 100 else 1), ")")
}

fmt_mean <- function(mean_val, is_binary) {
  # escaped \% for LaTeX -- an unescaped % starts a comment and silently
  # truncates the rest of that row when compiled (same bug class caught in
  # attrition_table.R)
  if (is_binary) paste0(sprintf("%.1f", mean_val * 100), "\\%") else sprintf("%.2f", mean_val)
}

triple_results <- triple_results %>%
  mutate(min_p = pmin(p_nonprofit, p_forprofit_diff, p_forprofit_own),
         any_sig = min_p < 0.10) %>%
  arrange(desc(any_sig), min_p)

ownership_table_rows <- do.call(rbind, lapply(seq_len(nrow(triple_results)), function(i) {
  r  <- triple_results[i, ]
  oi <- outcome_info[[which(map_chr(outcome_info, "var") == r$outcome)]]
  rbind(
    data.frame(
      Outcome              = oi$label,
      `NP Mean`            = fmt_mean(r$mean_nonprofit, oi$is_binary),
      `FP Mean`            = fmt_mean(r$mean_forprofit, oi$is_binary),
      `Non-Profit`         = fmt_beta(r$att_nonprofit, r$p_nonprofit, oi$is_binary),
      `For-Profit Diff.`   = fmt_beta(r$att_forprofit_diff, r$p_forprofit_diff, oi$is_binary),
      `For-Profit Own ATT` = fmt_beta(r$att_forprofit_own, r$p_forprofit_own, oi$is_binary),
      check.names = FALSE, stringsAsFactors = FALSE
    ),
    data.frame(
      Outcome              = "",
      `NP Mean`            = "",
      `FP Mean`            = "",
      `Non-Profit`         = fmt_se(r$se_nonprofit, oi$is_binary),
      `For-Profit Diff.`   = fmt_se(r$se_forprofit_diff, oi$is_binary),
      `For-Profit Own ATT` = fmt_se(r$se_forprofit_own, oi$is_binary),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  )
}))

n_sig <- sum(triple_results$any_sig) * 2

tbl <- ownership_table_rows %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    caption   = "Service Line Offerings: For-Profit vs. Non-Profit Heterogeneity",
    col.names = c("Outcome", "NP Mean", "FP Mean", "Non-Profit", "For-Profit Diff.", "For-Profit Own ATT"),
    align     = c("l", "r", "r", "r", "r", "r")
  ) %>%
  kable_styling(latex_options = c("hold_position"))

if (n_sig > 0 && n_sig < nrow(ownership_table_rows)) {
  tbl <- tbl %>%
    pack_rows("At Least One Significant Coefficient (p < 0.10)", 1, n_sig, bold = FALSE) %>%
    pack_rows("No Significant Association", n_sig + 1, nrow(ownership_table_rows), bold = FALSE)
}

tbl %>%
  add_footnote(
    paste0(
      "NP Mean and FP Mean are pre-treatment means (treated hospitals, years ",
      "before adoption) for non-profit and for-profit hospitals respectively -- ",
      "context for the ATT magnitudes, since a given point change means ",
      "something different off a high vs. low base. Triple-interaction model, ",
      "fit twice with the reference ownership group flipped so both groups' own ",
      "ATT get a direct significance test, not just the non-profit baseline and ",
      "the relative differential: Non-Profit and For-Profit Own ATT are each ",
      "group's own treated x post effect tested on its own; For-Profit Diff. is ",
      "for-profit minus non-profit, the formal test of whether the two differ. ",
      "Rows ranked by the strongest (smallest) p-value across all three ATT ",
      "columns; vem (Emergency Department Visits) has a known pre-trend issue ",
      "documented separately and should not be read as a clean result despite ",
      "appearing here. Ownership fixed per hospital (first non-missing ",
      "typectrl/cntrl). Q-weighted, hospital x cohort and year x cohort fixed ",
      "effects, SEs clustered by state. * p<0.10, ** p<0.05, *** p<0.01."
    ),
    notation = "none"
  ) %>%
  writeLines("service_line_ownership_heterogeneity.tex")

cat("\nDone -- service_line_ownership_heterogeneity.tex saved.\n")
