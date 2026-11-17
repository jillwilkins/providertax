# ==============================================================================
# MCO EXPENDITURE SUMMARY STATISTICS BY HOSPITAL PROVIDER TAX STATUS
# Mean log spending for Total Medicaid-MCO, PAHP, and PIHP, by four groups
# defined off the hospital provider tax's own state-level adoption timing:
#   - Always Treated:  cohort == "Always (2004)" (tax predates the sample)
#   - Never Treated:   no tax observed in the sample
#   - Eventually Treated (Pre):  years before that state's own adoption year
#   - Eventually Treated (Post): years at/after that state's own adoption year
#
# Unlike mco_provider_tax_analysis.R / expenditure_outcomes_analysis.R (which
# exclude "Always (2004)" states entirely, since they have no observable
# pre-period for a DiD), this is purely descriptive -- Always Treated is
# its own group here rather than dropped.
#
# Re-parses the raw FMR files (same parsers as the other two scripts in this
# folder) since no cached panel_all exists on disk yet.
#
# Requires hospdata_analysis already in the environment (for cohort,
# firsttax_num).
# ==============================================================================

library(readxl)
library(dplyr)
library(purrr)
library(kableExtra)

# ------------------------------------------------------------------------------
# 1. Parse FY2002-2024 FMR files (same parsers as the other MCO scripts)
# ------------------------------------------------------------------------------
SKIP_VALS <- c("Medicaid Financial Management Report",
               "Medicaid Financial Management Report  - Net Services",
               "Medicaid Finanacial Management Report",
               "Net Services", "National", "National Totals",
               "Medical Assistance Program", "Administration",
               "Service Category")

parse_old_format <- function(path, year) {
  df <- as.data.frame(read_excel(path, sheet = as.character(year), col_names = FALSE))

  is_state_row <- function(i) {
    v0 <- trimws(as.character(df[i, 1]))
    !is.na(df[i, 1]) && v0 != "" && v0 != "NA" &&
      !(v0 %in% SKIP_VALS) && !startsWith(v0, "FY") &&
      !startsWith(v0, "ADM") &&
      !grepl("Created On", v0) && is.na(df[i, 2])
  }

  state_rows  <- which(sapply(seq_len(nrow(df)), is_state_row))
  state_names <- trimws(as.character(df[state_rows, 1]))

  bind_rows(lapply(seq_along(state_rows), function(k) {
    row_i <- state_rows[k]
    state <- state_names[k]

    data_start <- row_i + 1
    while (data_start <= nrow(df)) {
      v0 <- trimws(as.character(df[data_start, 1]))
      if (!is.na(df[data_start, 1]) && v0 != "" && v0 != "NA" &&
          !(v0 %in% SKIP_VALS) && !is.na(df[data_start, 2])) break
      data_start <- data_start + 1
    }
    data_end <- if (k < length(state_rows)) state_rows[k + 1] - 1 else nrow(df)

    block <- df[data_start:data_end, ]
    rec   <- list(state = state, year = as.integer(year))

    for (j in seq_len(nrow(block))) {
      r  <- block[j, ]
      v0 <- trimws(as.character(r[[1]]))
      if (!is.na(r[[1]]) && v0 != "" && v0 != "NA" &&
          !(v0 %in% SKIP_VALS) && !is.na(r[[2]])) {
        rec[[paste0(v0, "_total")]]   <- suppressWarnings(as.numeric(r[[2]]))
        rec[[paste0(v0, "_federal")]] <- suppressWarnings(as.numeric(r[[3]]))
        rec[[paste0(v0, "_state")]]   <- suppressWarnings(as.numeric(r[[4]]))
      }
      if (ncol(block) >= 8) {
        v4 <- trimws(as.character(r[[5]]))
        if (!is.na(r[[5]]) && v4 != "" && v4 != "NA" &&
            !(v4 %in% SKIP_VALS) && !is.na(r[[6]])) {
          rec[[paste0(v4, "_total")]]   <- suppressWarnings(as.numeric(r[[6]]))
          rec[[paste0(v4, "_federal")]] <- suppressWarnings(as.numeric(r[[7]]))
          rec[[paste0(v4, "_state")]]   <- suppressWarnings(as.numeric(r[[8]]))
        }
      }
    }
    as_tibble(rec)
  }))
}

parse_new_format <- function(path, year) {
  state_sheets <- excel_sheets(path)
  state_sheets <- state_sheets[!state_sheets %in% SKIP_VALS]
  state_sheets <- state_sheets[!grepl("^ADM", state_sheets)]

  bind_rows(lapply(state_sheets, function(sheet) {
    state <- sub("^MAP - ", "", sheet)
    df <- as.data.frame(read_excel(path, sheet = sheet, col_names = FALSE))

    data_start <- 1
    while (data_start <= nrow(df)) {
      v0 <- trimws(as.character(df[data_start, 1]))
      if (!is.na(df[data_start, 1]) && v0 != "" && v0 != "NA" &&
          !(v0 %in% SKIP_VALS) && !is.na(df[data_start, 2]) &&
          !grepl("Created On", v0)) break
      data_start <- data_start + 1
    }

    block <- df[data_start:nrow(df), ]
    rec   <- list(state = state, year = as.integer(year))
    n_cols <- ncol(block)

    for (j in seq_len(nrow(block))) {
      r  <- block[j, ]
      v0 <- trimws(as.character(r[[1]]))
      if (!is.na(r[[1]]) && v0 != "" && v0 != "NA" &&
          !(v0 %in% SKIP_VALS) && !grepl("Created On", v0) && !is.na(r[[2]])) {
        rec[[paste0(v0, "_total")]]   <- suppressWarnings(as.numeric(r[[2]]))
        rec[[paste0(v0, "_federal")]] <- suppressWarnings(as.numeric(r[[3]]))
        rec[[paste0(v0, "_state")]]   <- suppressWarnings(as.numeric(r[[min(7, n_cols)]]))
      }
      if (n_cols >= 11) {
        v7 <- trimws(as.character(r[[8]]))
        if (!is.na(r[[8]]) && v7 != "" && v7 != "NA" &&
            !(v7 %in% SKIP_VALS) && !is.na(r[[9]])) {
          rec[[paste0(v7, "_total")]]   <- suppressWarnings(as.numeric(r[[9]]))
          rec[[paste0(v7, "_federal")]] <- suppressWarnings(as.numeric(r[[10]]))
          rec[[paste0(v7, "_state")]]   <- suppressWarnings(as.numeric(r[[11]]))
        }
      }
    }
    as_tibble(rec)
  }))
}

new_files <- list(
  "2012" = "FMR_Net_Expenditures_FY12.xlsx",
  "2013" = "FMR_Net_Expenditures_FY13.xlsx",
  "2014" = "FMR_Net_Expenditures_FY14.xlsx",
  "2015" = "FMR_Net_Expenditures_FY2015.xlsx",
  "2016" = "FMR_Net_Expenditures_FY2016.xlsx",
  "2017" = "FMR_Net_Expenditures_FY2017.xlsx",
  "2018" = "FMR_Net_Expenditures_FY2018.xlsx",
  "2019" = "FMR_Net_Expenditures_FY2019.xlsx",
  "2020" = "FMR_Net_Expenditures_FY2020.xlsx",
  "2021" = "FMR_Net_Expenditures_FY2021.xlsx",
  "2022" = "FMR_Net_Expenditures_FY2022.xlsx",
  "2023" = "FMR_Net_Expenditures_FY2023.xlsx",
  "2024" = "FMR_Net_Expenditures_FY2024.xlsx"
)

cat("Parsing FY2002-2011...\n")
panel_old <- bind_rows(lapply(2002:2011, function(yr) {
  cat(" FY", yr, "\n")
  parse_old_format(file.path(data_input_path, "NetExpenditure02through11.xlsx"), yr)
}))

cat("Parsing FY2012-2024...\n")
panel_new <- bind_rows(imap(new_files, function(fname, yr) {
  cat(" FY", yr, "\n")
  parse_new_format(file.path(data_input_path, fname), year = as.integer(yr))
}))

panel_all <- bind_rows(panel_old, panel_new) %>%
  filter(!state %in% c("National Totals", "National"), !grepl("Created On", state)) %>%
  mutate(state = ifelse(state == "Dist. Of Col.", "DC", state.abb[match(state, state.name)])) %>%
  filter(!is.na(state))

cat("\npanel_all:", nrow(panel_all), "state-years,", ncol(panel_all), "columns\n")

# ------------------------------------------------------------------------------
# 2. State-level tax timing -- ALL states this time, Always (2004) kept as
#    its own group instead of dropped
# ------------------------------------------------------------------------------
state_tax_info <- hospdata_analysis %>%
  distinct(state, cohort, firsttax_num) %>%
  filter(!is.na(state)) %>%
  group_by(state) %>%
  summarise(
    cohort       = first(cohort),
    firsttax_num = min(firsttax_num, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  mutate(firsttax_num = ifelse(is.infinite(firsttax_num), NA, firsttax_num))

cat("\nState-level cohort breakdown:\n")
print(state_tax_info %>% count(cohort))

# ------------------------------------------------------------------------------
# 3. Generic helper: build the 4-group panel for one FMR row label
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
    left_join(state_tax_info, by = "state") %>%
    filter(!is.na(outcome_total), outcome_total > 0) %>%
    mutate(
      group = case_when(
        cohort == "Always (2004)"                      ~ "Always Treated",
        cohort == "Never"                               ~ "Never Treated",
        !is.na(firsttax_num) & year <  firsttax_num     ~ "Eventually Treated (Pre)",
        !is.na(firsttax_num) & year >= firsttax_num     ~ "Eventually Treated (Post)",
        TRUE                                             ~ NA_character_
      ),
      log_outcome = log(outcome_total)
    ) %>%
    filter(!is.na(group))
}

group_levels <- c("Always Treated", "Never Treated", "Eventually Treated (Pre)", "Eventually Treated (Post)")

# ------------------------------------------------------------------------------
# 4. Run for MCO, PAHP, PIHP
# ------------------------------------------------------------------------------
outcome_rows <- list(
  list(key = "Total Medicaid-MCO", label = "Medicaid - MCO"),
  list(key = "PAHP",               label = "Prepaid Ambulatory Health Plan"),
  list(key = "PIHP",               label = "Prepaid Inpatient Health Plan")
)

summary_long <- bind_rows(lapply(outcome_rows, function(o) {
  panel <- build_group_panel(o$label)
  if (is.null(panel)) return(NULL)
  panel %>%
    group_by(group) %>%
    summarise(
      mean_log_spending = mean(log_outcome, na.rm = TRUE),
      n_state_years     = n(),
      n_states          = n_distinct(state),
      .groups           = "drop"
    ) %>%
    mutate(Outcome = o$key)
}))

summary_long <- summary_long %>%
  mutate(group = factor(group, levels = group_levels)) %>%
  arrange(Outcome, group)

cat("\n\n=== MEAN LOG SPENDING BY TAX STATUS GROUP ===\n")
print(summary_long %>% select(Outcome, group, mean_log_spending, n_state_years, n_states), n = 20)

# ------------------------------------------------------------------------------
# 5. Wide table: outcomes as rows, groups as columns
# ------------------------------------------------------------------------------
summary_wide <- summary_long %>%
  select(Outcome, group, mean_log_spending) %>%
  tidyr::pivot_wider(names_from = group, values_from = mean_log_spending) %>%
  select(Outcome, all_of(group_levels))

n_wide <- summary_long %>%
  select(Outcome, group, n_state_years) %>%
  tidyr::pivot_wider(names_from = group, values_from = n_state_years) %>%
  select(Outcome, all_of(group_levels))

cat("\n=== WIDE TABLE: MEAN LOG SPENDING ===\n")
print(summary_wide %>% mutate(across(where(is.numeric), ~round(., 3))))

cat("\n=== WIDE TABLE: N STATE-YEARS ===\n")
print(n_wide)

summary_wide %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    caption   = "Mean Log Medicaid Managed Care Spending by Hospital Provider Tax Status",
    col.names = c("Outcome", group_levels),
    align     = c("l", "r", "r", "r", "r")
  ) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  writeLines("mco_expenditure_analysis/mco_summary_stats.tex")

cat("\nDone -- mco_summary_stats.tex saved.\n")