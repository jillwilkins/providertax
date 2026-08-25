# ==============================================================================
# PRELIMINARY: DOES ADOPTING A PROVIDER TAX CHANGE STATE MEDICAID SPENDING
# ON MCO (MANAGED CARE ORGANIZATION) CAPITATION PAYMENTS?
#
# Builds on expenditures.R (FMR parsing) and mco_1.R (MCO framing), adding
# the state-level tax adoption year (firsttax_num, from 1_data.R's cohort
# construction) to run a quick event-study/DiD check.
#
# "Medicaid - MCO" is the FMR line item for comprehensive Medicaid managed
# care capitation payments (confirmed present in the FY2012-2024 raw files;
# distinct from "MCO - National/State Sidebar Agreement", which sit right
# next to the drug rebate offset lines and are MCO-related drug rebate
# adjustments, not the capitation payment itself).
#
# Covers FY2002-2024 (both the old stacked-sheet format, 2002-2011, and the
# new one-sheet-per-state format, 2012-2024), so early cohorts (2005, 2006,
# 2009, 2010, 2011) now have real pre-period MCO spending, not just
# post-period.
#
# Requires hospdata_analysis already in the environment (for firsttax_num).
# ==============================================================================

library(readxl)
library(dplyr)
library(purrr)
library(fixest)
library(ggplot2)

# ------------------------------------------------------------------------------
# 1. Parse FY2002-2024 FMR files (same parsers as expenditures.R), keep the
#    MCO line items instead of filtering to hospital-only columns
# ------------------------------------------------------------------------------
SKIP_VALS <- c("Medicaid Financial Management Report",
               "Medicaid Financial Management Report  - Net Services",
               "Medicaid Finanacial Management Report",
               "Net Services", "National", "National Totals",
               "Medical Assistance Program", "Administration",
               "Service Category")

# FY2002-2011: all states stacked in year-named sheets
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

# FY2012-2024: each state in its own sheet
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

cat("Parsing FY2002-2011 (MCO line items)...\n")
panel_old <- bind_rows(lapply(2002:2011, function(yr) {
  cat(" FY", yr, "\n")
  parse_old_format(file.path(data_input_path, "NetExpenditure02through11.xlsx"), yr)
}))

cat("Parsing FY2012-2024 (MCO line items)...\n")
panel_new <- bind_rows(imap(new_files, function(fname, yr) {
  cat(" FY", yr, "\n")
  parse_new_format(file.path(data_input_path, fname), year = as.integer(yr))
}))

panel_all <- bind_rows(panel_old, panel_new)

# ------------------------------------------------------------------------------
# 2. Keep state, year, and the MCO capitation line
# ------------------------------------------------------------------------------
mco_panel <- panel_all %>%
  filter(!state %in% c("National Totals", "National"), !grepl("Created On", state)) %>%
  select(state, year, matches("^Medicaid - MCO_")) %>%
  rename(
    mco_total   = `Medicaid - MCO_total`,
    mco_federal = `Medicaid - MCO_federal`,
    mco_state   = `Medicaid - MCO_state`
  ) %>%
  mutate(
    state = ifelse(state == "Dist. Of Col.", "DC", state.abb[match(state, state.name)])
  ) %>%
  filter(!is.na(state))  # drops territories/unmatched

cat("\nMCO panel:", nrow(mco_panel), "state-years,",
    sum(!is.na(mco_panel$mco_total)), "with non-missing mco_total\n")

# ------------------------------------------------------------------------------
# 3. State-level tax adoption year (firsttax_num, from 1_data.R)
# "Always (2004)" cohort states already had the tax before the sample
# starts -- no observable pre-period, so they're excluded entirely here too
# (same convention as 5_buildstacked.R's `filter(cohort != "Always (2004)")`),
# not just marked untreated -- they aren't valid controls either, since
# they're not actually never-treated.
# ------------------------------------------------------------------------------
always_2004_states <- hospdata_analysis %>%
  filter(cohort == "Always (2004)") %>%
  distinct(state) %>%
  pull(state)

cat("\nExcluding 'Always (2004)' states (no observable pre-period):",
    paste(always_2004_states, collapse = ", "), "\n")

state_tax_year <- hospdata_analysis %>%
  distinct(state, firsttax_num) %>%
  filter(!is.na(state), !state %in% always_2004_states) %>%
  # a state should have one adoption year; if duplicates exist take the min
  group_by(state) %>%
  summarise(firsttax_num = min(firsttax_num, na.rm = TRUE), .groups = "drop") %>%
  mutate(firsttax_num = ifelse(is.infinite(firsttax_num), NA, firsttax_num))

mco_panel <- mco_panel %>%
  filter(!state %in% always_2004_states) %>%
  left_join(state_tax_year, by = "state") %>%
  mutate(
    treated  = as.integer(!is.na(firsttax_num)),
    rel_year = year - firsttax_num,
    log_mco  = log(mco_total)
  )

cat("\nStates with tax adoption year covered by this expenditure panel:\n")
mco_panel %>% filter(treated == 1) %>% distinct(state, firsttax_num) %>% arrange(firsttax_num) %>% print(n = 30)

# ------------------------------------------------------------------------------
# 4. Quick event-time diagnostic: mean MCO spending by rel_year
# ------------------------------------------------------------------------------
cat("\n=== MEAN MCO SPENDING BY REL_YEAR (TREATED STATES ONLY) ===\n")
mco_panel %>%
  filter(treated == 1, !is.na(mco_total), mco_total > 0, rel_year >= -5, rel_year <= 5) %>%
  group_by(rel_year) %>%
  summarise(n_states = n_distinct(state), mean_mco = mean(mco_total, na.rm = TRUE), .groups = "drop") %>%
  arrange(rel_year) %>%
  print(n = 20)

# Never-treated states have no rel_year (nothing to count relative to), so
# they can't appear on this event-time x-axis -- the DiD regression below
# is where they actually serve as the comparison group (via state + year
# FE). This plot is treated states only; n_states is annotated since the
# far pre-period (-5 to -3) is contributed by just 4-5 late-adopting
# states (LA, NV, TX, VA, WY) -- a different, much smaller set than the
# post-period, so any apparent pre-trend here is likely composition noise,
# not a real trend in the same states.
event_plot <- mco_panel %>%
  filter(treated == 1, !is.na(mco_total), mco_total > 0, rel_year >= -5, rel_year <= 5) %>%
  group_by(rel_year) %>%
  summarise(mean_log_mco = mean(log_mco, na.rm = TRUE), n_states = n_distinct(state), .groups = "drop")

p <- ggplot(event_plot, aes(x = rel_year, y = mean_log_mco)) +
  geom_line(color = "#DC4B4B") +
  geom_point(color = "#DC4B4B", size = 2.5) +
  geom_text(aes(label = n_states), vjust = -1.2, size = 3, color = "gray30") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "gray40") +
  labs(title = "Mean Log Medicaid MCO Spending by Years Relative to Hosp Tax Adoption",
       subtitle = "Treated states only (labels = # states); comparison group is in the DiD regression, not this plot",
       x = "Years Relative to Tax Adoption", y = "Mean log(MCO spending)") +
  theme_minimal()

ggsave("mco_expenditure_analysis/mco_provider_tax_event_plot.png", plot = p, width = 9, height = 6, dpi = 300)
cat("\nSaved: mco_expenditure_analysis/mco_provider_tax_event_plot.png\n")

# ------------------------------------------------------------------------------
# 5. Quick two-way FE DiD (state + year FE, cluster by state)
# ------------------------------------------------------------------------------
mco_panel <- mco_panel %>% mutate(post = as.integer(!is.na(rel_year) & rel_year >= 0))

did_model <- feols(log_mco ~ treated:post | state + year, data = mco_panel, cluster = ~state)

cat("\n=== QUICK TWO-WAY FE DiD: log(MCO spending) ~ treated:post | state + year ===\n")
print(summary(did_model))

# ------------------------------------------------------------------------------
# 5b. Same DiD, controlling for Medicaid expansion status (exp_status, same
# time-varying state-year indicator used as a control throughout the main
# hospital-level analysis -- 1 in whichever year a state's own expansion is
# actually in effect, 0 before/never). Nets out the direct, common effect of
# expansion being active on Medicaid spending, so treated:post isn't picking
# up expansion's own level shift (see the exp_status-vs-triple-interaction
# discussion from earlier: this is a confounder control, not a test of
# whether the effect DIFFERS by expansion status).
# ------------------------------------------------------------------------------
state_year_exp <- hospdata_analysis %>%
  distinct(state, year, exp_status) %>%
  filter(!is.na(state)) %>%
  group_by(state, year) %>%
  summarise(exp_status = max(exp_status, na.rm = TRUE), .groups = "drop")

mco_panel <- mco_panel %>%
  left_join(state_year_exp, by = c("state", "year"))

cat("\nMissing exp_status after merge:", sum(is.na(mco_panel$exp_status)), "of", nrow(mco_panel), "rows\n")

did_model_exp <- feols(log_mco ~ treated:post + exp_status | state + year,
                        data = mco_panel, cluster = ~state)

cat("\n=== QUICK TWO-WAY FE DiD WITH exp_status CONTROL ===\n")
cat("log(MCO spending) ~ treated:post + exp_status | state + year\n")
print(summary(did_model_exp))

cat("\n=== SIDE BY SIDE ===\n")
etable(did_model, did_model_exp, cluster = ~state)

# ------------------------------------------------------------------------------
# 6. Diagnostic: what's driving the rel_year=1 dip in the event plot?
# ------------------------------------------------------------------------------
cat("\n=== DIAGNOSTIC: STATE-LEVEL DETAIL AT REL_YEAR = 1 ===\n")
mco_panel %>%
  filter(treated == 1, rel_year == 1) %>%
  select(state, firsttax_num, year, mco_total, mco_federal, mco_state) %>%
  arrange(mco_total) %>%
  print(n = 30)

cat("\nFor comparison, same states at rel_year = 0 and rel_year = 2:\n")
rel1_states <- mco_panel %>% filter(treated == 1, rel_year == 1) %>% pull(state)
mco_panel %>%
  filter(treated == 1, state %in% rel1_states, rel_year %in% c(0, 1, 2)) %>%
  select(state, rel_year, year, mco_total) %>%
  arrange(state, rel_year) %>%
  print(n = 60)

cat("\nDone -- preliminary MCO/provider-tax analysis complete (FY2002-2024).\n")
