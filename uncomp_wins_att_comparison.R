# ==============================================================================
# UNCOMPENSATED CARE OUTCOMES: POOLED ATT ACROSS WINSORIZATION LEVELS
# Same pooled ATT spec as main_results_table.R's fit_pooled() (single
# treated:post coefficient, Q-weighted, hospital x cohort and year x cohort
# fixed effects, controls for exp_status + median_income_pre, SEs clustered
# by state) -- run for each of the 4 uncompensated-care outcomes
# (uncomp_bed, ucc2npr, ucc_charges, log_ucc_charges) at three winsorization
# levels: raw (unwinsorized), 1%/99% (events_wins_paper), and 1%/95%
# (events_wins95_paper). Lets you see how much the ATT and its significance
# move as outliers get trimmed harder.
# Requires stacked_data already in the environment (5_buildstacked.R).
# ==============================================================================

library(dplyr)
library(fixest)
library(purrr)
library(kableExtra)

# ------------------------------------------------------------------------------
# Derived + winsorized columns needed, computed defensively
# ------------------------------------------------------------------------------
if (!"post" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(post = as.integer(rel_year >= 0))
}
if (!"ucc_charges" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(ucc_charges = tot_uncomp_care_charges / tot_charges)
}
if (!"log_ucc_charges" %in% names(stacked_data)) {
  stacked_data <- stacked_data %>% mutate(log_ucc_charges = log(tot_uncomp_care_charges))
}

winsorize_var <- function(x, probs = c(0.01, 0.99)) {
  lower <- quantile(x, probs[1], na.rm = TRUE)
  upper <- quantile(x, probs[2], na.rm = TRUE)

  x_win <- x
  x_win[x < lower] <- lower
  x_win[x > upper] <- upper

  return(x_win)
}

ucc_base_vars <- c("uncomp_bed", "ucc2npr", "ucc_charges", "log_ucc_charges")

for (v in ucc_base_vars) {
  w99_col <- paste0(v, "_w")
  w95_col <- paste0(v, "_w95")
  if (!w99_col %in% names(stacked_data)) {
    stacked_data[[w99_col]] <- winsorize_var(stacked_data[[v]], probs = c(0.01, 0.99))
  }
  if (!w95_col %in% names(stacked_data)) {
    stacked_data[[w95_col]] <- winsorize_var(stacked_data[[v]], probs = c(0.01, 0.95))
  }
}

# ------------------------------------------------------------------------------
# Pooled ATT model (identical spec to main_results_table.R)
# ------------------------------------------------------------------------------
fit_pooled <- function(outcome_var) {
  formula_str <- paste0(
    outcome_var,
    " ~ treated:post + i(rel_year, treated, ref = -1, keep = -4:-2) + exp_status + median_income_pre",
    " | mcrnum^df + year^df"
  )
  feols(as.formula(formula_str), data = stacked_data, weights = ~Q_weight, cluster = ~state)
}

pre_treat_mean <- function(outcome_var) {
  stacked_data %>%
    filter(treated == 1, rel_year < 0) %>%
    summarise(m = mean(.data[[outcome_var]], na.rm = TRUE)) %>%
    pull(m)
}

# Unique treated hospitals actually contributing to that outcome's
# regression sample (non-missing outcome + covariates) -- this varies by
# outcome/winsorization since missingness in the LHS differs (e.g. 16,551
# NAs for uncomp_bed vs. 31,283 for ucc_charges).
n_hosp_in_sample <- function(outcome_var) {
  stacked_data %>%
    filter(treated == 1, !is.na(.data[[outcome_var]]), !is.na(exp_status), !is.na(median_income_pre)) %>%
    summarise(n = n_distinct(mcrnum)) %>%
    pull(n)
}

# ------------------------------------------------------------------------------
# Run all outcome x winsorization-level combinations
# ------------------------------------------------------------------------------
ucc_labels <- c(
  uncomp_bed      = "Uncompensated Care per Bed",
  ucc2npr         = "Uncompensated Care as % of NPR",
  ucc_charges     = "Uncompensated Care as % of Total Charges",
  log_ucc_charges = "Log of Uncompensated Care Charges"
)

wins_levels <- list(
  list(suffix = "",      label = "Raw (unwinsorized)"),
  list(suffix = "_w",    label = "1% / 99% winsorized"),
  list(suffix = "_w95",  label = "1% / 95% winsorized")
)

combos <- expand.grid(base_var = ucc_base_vars, level = seq_along(wins_levels), stringsAsFactors = FALSE)

uncomp_wins_att <- map_df(seq_len(nrow(combos)), function(i) {
  base_var  <- combos$base_var[i]
  level     <- wins_levels[[combos$level[i]]]
  outcome_var <- paste0(base_var, level$suffix)

  result <- fit_pooled(outcome_var)
  cn <- names(coef(result))
  pt <- cn[grepl("treated.*post|post.*treated", cn)]

  beta <- as.numeric(coef(result)[pt])
  se   <- as.numeric(result$se[pt])
  p    <- as.numeric(pvalue(result)[pt])

  stars <- case_when(
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE     ~ ""
  )

  data.frame(
    Outcome       = ucc_labels[[base_var]],
    Winsorization = level$label,
    PreTreatMean  = pre_treat_mean(outcome_var),
    NHosp         = n_hosp_in_sample(outcome_var),
    ATT           = beta,
    SE            = se,
    Stars         = stars,
    stringsAsFactors = FALSE
  )
}) %>%
  arrange(match(Outcome, ucc_labels), match(Winsorization, sapply(wins_levels, function(x) x$label)))

cat("\n=== UNCOMPENSATED CARE OUTCOMES: POOLED ATT ACROSS WINSORIZATION LEVELS ===\n")
print(uncomp_wins_att, digits = 4)

# ------------------------------------------------------------------------------
# Format: coefficient + stars on one row, (SE) on the row underneath
# (N Hospitals shown once, on the beta row, same convention as
# main_results_table.R)
# ------------------------------------------------------------------------------
uncomp_wins_att_formatted <- uncomp_wins_att %>%
  mutate(
    Mean_fmt = ifelse(abs(PreTreatMean) >= 1000,
                       scales::comma(PreTreatMean, accuracy = 1),
                       sprintf("%.3f", PreTreatMean)),
    Beta_fmt = ifelse(abs(ATT) >= 1000,
                       scales::comma(ATT, accuracy = 1),
                       sprintf("%.3f", ATT)),
    SE_fmt   = ifelse(abs(SE) >= 1000,
                       scales::comma(SE, accuracy = 1),
                       sprintf("%.3f", SE)),
    Beta_Stars = paste0(Beta_fmt, Stars),
    SE_paren   = paste0("(", SE_fmt, ")"),
    NHosp_fmt  = scales::comma(NHosp, accuracy = 1)
  )

uncomp_wins_att_long <- do.call(rbind, lapply(seq_len(nrow(uncomp_wins_att_formatted)), function(i) {
  rbind(
    data.frame(Outcome = uncomp_wins_att_formatted$Outcome[i],
               Winsorization = uncomp_wins_att_formatted$Winsorization[i],
               Mean = uncomp_wins_att_formatted$Mean_fmt[i],
               NHosp = uncomp_wins_att_formatted$NHosp_fmt[i],
               Estimate = uncomp_wins_att_formatted$Beta_Stars[i], stringsAsFactors = FALSE),
    data.frame(Outcome = "", Winsorization = "",
               Mean = "", NHosp = "",
               Estimate = uncomp_wins_att_formatted$SE_paren[i], stringsAsFactors = FALSE)
  )
}))

cat("\n=== FORMATTED (BETA / SE STACKED) ===\n")
print(uncomp_wins_att_long)

# ------------------------------------------------------------------------------
# LaTeX table (same style as main_results_table.R)
# ------------------------------------------------------------------------------
uncomp_wins_att_long %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    caption   = "Uncompensated Care Outcomes: Pooled ATT Across Winsorization Levels",
    col.names = c("Outcome", "Winsorization", "Pre-Treatment Mean", "N Hospitals", "ATT"),
    align     = c("l", "l", "r", "r", "r")
  ) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  add_footnote(
    paste0(
      "Pooled treated x post coefficient from a stacked difference-in-differences ",
      "model (Q-weighted, hospital x cohort and year x cohort fixed effects, ",
      "controls for state Medicaid expansion status and median income, SEs ",
      "clustered by state), estimated separately for each uncompensated care ",
      "outcome at three winsorization levels (raw, 1%/99%, 1%/95%). N Hospitals ",
      "is the number of unique treated hospitals with non-missing data ",
      "contributing to that specific regression. * p<0.10, ** p<0.05, *** p<0.01."
    ),
    notation = "none"
  ) %>%
  writeLines("uncomp_wins_att_comparison.tex")

cat("\nDone -- uncomp_wins_att_comparison.tex saved.\n")
