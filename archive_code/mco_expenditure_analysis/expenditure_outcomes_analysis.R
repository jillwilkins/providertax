# ==============================================================================
# PROVIDER TAX AND STATE MEDICAID EXPENDITURES: MULTIPLE OUTCOMES
# Extends mco_provider_tax_analysis.R's "Medicaid - MCO" check to:
#   1. Total Net Expenditures  (the FMR grand-total row -- overall Medicaid
#      spending, not MCO-specific; the direct test of "does the tax raise
#      total Medicaid funding")
#   2. Prepaid Ambulatory Health Plan (PAHP) -- limited-benefit managed care
#      capitation (often behavioral health/dental carve-outs)
#   3. Prepaid Inpatient Health Plan (PIHP) -- limited-benefit managed care
#      capitation (often behavioral health/long-term care carve-outs)
#   4. Medicaid - MCO (comprehensive risk-based capitation, rerun here for
#      side-by-side comparison with the other three)
#
# Same sample construction as mco_provider_tax_analysis.R: "Always (2004)"
# cohort states excluded (no observable pre-period), exp_status control
# included as a second spec. Parses the raw FMR files ONCE and reuses the
# combined panel across all four outcomes instead of re-parsing per outcome.
#
# Requires hospdata_analysis already in the environment (for firsttax_num,
# cohort, exp_status).
# ==============================================================================

library(readxl)
library(dplyr)
library(purrr)
library(fixest)
library(ggplot2)

# ------------------------------------------------------------------------------
# 1. Parse FY2002-2024 FMR files ONCE (same parsers as mco_provider_tax_analysis.R)
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
# 2. Shared setup: tax adoption year (excluding "Always (2004)"), exp_status
# ------------------------------------------------------------------------------
always_2004_states <- hospdata_analysis %>%
  filter(cohort == "Always (2004)") %>%
  distinct(state) %>%
  pull(state)

state_tax_year <- hospdata_analysis %>%
  distinct(state, firsttax_num) %>%
  filter(!is.na(state), !state %in% always_2004_states) %>%
  group_by(state) %>%
  summarise(firsttax_num = min(firsttax_num, na.rm = TRUE), .groups = "drop") %>%
  mutate(firsttax_num = ifelse(is.infinite(firsttax_num), NA, firsttax_num))

state_year_exp <- hospdata_analysis %>%
  distinct(state, year, exp_status) %>%
  filter(!is.na(state)) %>%
  group_by(state, year) %>%
  summarise(exp_status = max(exp_status, na.rm = TRUE), .groups = "drop")

# ------------------------------------------------------------------------------
# 3. Generic pipeline: given a raw FMR row label, build the panel and run
#    the event-time summary + two DiD specs (with/without exp_status)
# ------------------------------------------------------------------------------
run_expenditure_outcome <- function(row_label, outcome_key) {
  total_col <- paste0(row_label, "_total")
  if (!total_col %in% names(panel_all)) {
    cat("\n!! Column not found:", total_col, "-- skipping", outcome_key, "\n")
    return(NULL)
  }

  panel <- panel_all %>%
    select(state, year, all_of(total_col)) %>%
    rename(outcome_total = all_of(total_col)) %>%
    filter(!state %in% always_2004_states) %>%
    left_join(state_tax_year, by = "state") %>%
    left_join(state_year_exp, by = c("state", "year")) %>%
    mutate(
      treated    = as.integer(!is.na(firsttax_num)),
      rel_year   = year - firsttax_num,
      post       = as.integer(!is.na(rel_year) & rel_year >= 0),
      log_outcome = log(outcome_total)
    )

  n_nonmiss <- sum(!is.na(panel$outcome_total))
  cat("\n===", outcome_key, "(", row_label, ") : ", n_nonmiss, "non-missing state-years ===\n")

  # Event-time diagnostic
  event_summary <- panel %>%
    filter(treated == 1, !is.na(outcome_total), outcome_total > 0, rel_year >= -5, rel_year <= 5) %>%
    group_by(rel_year) %>%
    summarise(n_states = n_distinct(state), mean_log = mean(log_outcome, na.rm = TRUE), .groups = "drop") %>%
    arrange(rel_year)
  print(event_summary, n = 20)

  p <- ggplot(event_summary, aes(x = rel_year, y = mean_log)) +
    geom_line(color = "#DC4B4B") +
    geom_point(color = "#DC4B4B", size = 2.5) +
    geom_text(aes(label = n_states), vjust = -1.2, size = 3, color = "gray30") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "gray40") +
    labs(title = paste("Mean Log", outcome_key, "by Years Relative to Tax Adoption"),
         subtitle = "Treated states only (labels = # states); Always-2004 cohort excluded",
         x = "Years Relative to Tax Adoption", y = paste("Mean log(", outcome_key, ")")) +
    theme_minimal()
  fname <- paste0("mco_expenditure_analysis/expenditure_event_", tolower(outcome_key), ".png")
  ggsave(fname, plot = p, width = 9, height = 6, dpi = 300)
  cat("Saved:", fname, "\n")

  # DiD, no control
  m1 <- feols(log_outcome ~ treated:post | state + year, data = panel, cluster = ~state)
  # DiD, with exp_status control
  m2 <- feols(log_outcome ~ treated:post + exp_status | state + year, data = panel, cluster = ~state)

  cn <- names(coef(m1))[grepl("treated.*post", names(coef(m1)))]
  cn2 <- names(coef(m2))[grepl("treated.*post", names(coef(m2)))]

  list(
    outcome    = outcome_key,
    row_label  = row_label,
    n_states_treated = n_distinct(panel$state[panel$treated == 1]),
    beta_no_control  = as.numeric(coef(m1)[cn]),
    se_no_control    = as.numeric(m1$se[cn]),
    p_no_control     = as.numeric(pvalue(m1)[cn]),
    beta_with_exp    = as.numeric(coef(m2)[cn2]),
    se_with_exp      = as.numeric(m2$se[cn2]),
    p_with_exp       = as.numeric(pvalue(m2)[cn2]),
    model_no_control = m1,
    model_with_exp   = m2
  )
}

# ------------------------------------------------------------------------------
# 4. Run all four outcomes
# ------------------------------------------------------------------------------
outcome_rows <- list(
  list(key = "Total_Medicaid_Expenditure", label = "Total Net Expenditures"),
  list(key = "MCO_Comprehensive",          label = "Medicaid - MCO"),
  list(key = "PAHP",                       label = "Prepaid Ambulatory Health Plan"),
  list(key = "PIHP",                       label = "Prepaid Inpatient Health Plan")
)

results_list <- lapply(outcome_rows, function(o) run_expenditure_outcome(o$label, o$key))
results_list <- Filter(Negate(is.null), results_list)

# ------------------------------------------------------------------------------
# 5. Side-by-side summary table
# ------------------------------------------------------------------------------
summary_table <- bind_rows(lapply(results_list, function(r) {
  data.frame(
    Outcome           = r$outcome,
    N_Treated_States  = r$n_states_treated,
    Beta_NoControl    = round(r$beta_no_control, 3),
    SE_NoControl      = round(r$se_no_control, 3),
    P_NoControl       = round(r$p_no_control, 3),
    Beta_WithExpStatus = round(r$beta_with_exp, 3),
    SE_WithExpStatus   = round(r$se_with_exp, 3),
    P_WithExpStatus    = round(r$p_with_exp, 3)
  )
}))

cat("\n\n=== SUMMARY: treated:post ACROSS ALL FOUR EXPENDITURE OUTCOMES ===\n")
print(summary_table, row.names = FALSE)

cat("\nDone -- expenditure_outcomes_analysis.R complete.\n")
