# ==============================================================================
# EXPENDITURE SUMMARY STATISTICS BY KFF MCO PROVIDER TAX STATUS
# Mean log spending for Total Medicaid-MCO, PAHP, and PIHP, by KFF's current
# (SFY2025) classification of which states have an MCO provider tax in
# place (kff_provtax_alltype.csv, same source as mco_tax_status_comparison.R):
#   - MCO Tax:      KFF column == "Yes"
#   - No MCO Tax:   KFF column == "No" (state uses Medicaid managed care,
#                   just hasn't taxed it)
#   - No MCO Usage: WY, VT, ME, CT, AK -- no Medicaid managed care at all,
#                   so an MCO tax isn't a meaningful option; kept separate
#                   rather than folded into "No MCO Tax" (same reasoning as
#                   mco_tax_status_comparison.R)
# States coded "NR" (FL, MS) are excluded as unclassified. This is a
# cross-sectional, non-causal comparison -- not a DiD.
#
# Reuses the cached parsed FMR panel (medicaid_expenditure_panel_all.rds)
# built by mco_tax_status_comparison.R instead of re-parsing the raw Excel
# files.
# ==============================================================================

library(dplyr)
library(kableExtra)
library(tidyr)

cache_path <- file.path(data_output_path, "medicaid_expenditure_panel_all.rds")
panel_all <- readRDS(cache_path)
cat("Loaded cached parsed FMR panel:", nrow(panel_all), "state-years\n")

# ------------------------------------------------------------------------------
# 1. KFF MCO provider tax classification (SFY2025 snapshot)
# ------------------------------------------------------------------------------
kff_raw <- read.csv(file.path(data_input_path, "kff_provtax_alltype.csv"), skip = 2, nrows = 52)

no_mco_states  <- c("WY", "VT", "ME", "CT", "AK")
bad_data_states <- c("WY")  # known WY 2002-2010 data bug, see mco_tax_status_comparison.R

kff_class <- kff_raw %>%
  mutate(
    state = ifelse(Location == "District of Columbia", "DC", state.abb[match(Location, state.name)])
  ) %>%
  filter(!is.na(state), Location != "United States", !state %in% bad_data_states) %>%
  transmute(
    state,
    mco_tax_yn = States.With.an.MCO.Provider.Tax.in.Place,
    mco_group  = case_when(
      state %in% no_mco_states ~ "No MCO Usage",
      mco_tax_yn == "Yes"      ~ "MCO Tax",
      mco_tax_yn == "No"       ~ "No MCO Tax",
      TRUE                     ~ NA_character_
    )
  )

cat("\n=== GROUP SIZES (SFY2025 KFF CLASSIFICATION) ===\n")
print(kff_class %>% count(mco_group))
cat("\nExcluded (NR / unclassified):", paste(kff_class$state[is.na(kff_class$mco_group)], collapse = ", "), "\n")

group_levels <- c("MCO Tax", "No MCO Tax", "No MCO Usage")

# ------------------------------------------------------------------------------
# 2. Generic helper: build the KFF-group panel for one FMR row label
# ------------------------------------------------------------------------------
build_group_panel <- function(row_label) {
  total_col <- paste0(row_label, "_total")
  if (!total_col %in% names(panel_all)) {
    cat("!! Column not found:", total_col, "\n")
    return(NULL)
  }

  panel_all %>%
    select(state, year, all_of(total_col)) %>%
    rename(outcome_total = all_of(total_col)) %>%
    left_join(kff_class, by = "state") %>%
    filter(!is.na(mco_group), !is.na(outcome_total), outcome_total > 0) %>%
    mutate(log_outcome = log(outcome_total))
}

outcome_rows <- list(
  list(key = "Total Medicaid-MCO", label = "Medicaid - MCO"),
  list(key = "PAHP",               label = "Prepaid Ambulatory Health Plan"),
  list(key = "PIHP",               label = "Prepaid Inpatient Health Plan")
)

# ------------------------------------------------------------------------------
# 3. Group averages -- state-level means first, then averaged across states
#    within each group (so a high-volume state doesn't dominate the group
#    mean the way a raw state-year pool would)
# ------------------------------------------------------------------------------
summary_long <- bind_rows(lapply(outcome_rows, function(o) {
  panel <- build_group_panel(o$label)
  if (is.null(panel)) return(NULL)

  state_avg <- panel %>%
    group_by(state, mco_group) %>%
    summarise(mean_log_outcome = mean(log_outcome, na.rm = TRUE), n_years = n(), .groups = "drop")

  state_avg %>%
    group_by(mco_group) %>%
    summarise(
      mean_log_spending = mean(mean_log_outcome, na.rm = TRUE),
      n_states          = n(),
      n_state_years     = sum(n_years),
      .groups           = "drop"
    ) %>%
    mutate(Outcome = o$key)
}))

summary_long <- summary_long %>%
  mutate(mco_group = factor(mco_group, levels = group_levels)) %>%
  arrange(Outcome, mco_group)

cat("\n\n=== MEAN LOG SPENDING BY KFF MCO TAX STATUS (STATE-LEVEL AVERAGES) ===\n")
print(summary_long %>% select(Outcome, mco_group, mean_log_spending, n_states, n_state_years), n = 20)

# ------------------------------------------------------------------------------
# 4. Wide table + LaTeX
# ------------------------------------------------------------------------------
summary_wide <- summary_long %>%
  select(Outcome, mco_group, mean_log_spending) %>%
  pivot_wider(names_from = mco_group, values_from = mean_log_spending) %>%
  select(Outcome, all_of(group_levels))

cat("\n=== WIDE TABLE: MEAN LOG SPENDING ===\n")
print(summary_wide %>% mutate(across(where(is.numeric), ~round(., 3))))

summary_wide %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    caption   = "Mean Log Medicaid Managed Care Spending by KFF MCO Provider Tax Status (SFY2025)",
    col.names = c("Outcome", group_levels),
    align     = c("l", "r", "r", "r")
  ) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  writeLines("mco_expenditure_analysis/mco_kff_expenditure_summary.tex")

cat("\nDone -- mco_kff_expenditure_summary.tex saved.\n")