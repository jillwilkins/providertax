# ==============================================================================
# NON-CAUSAL COMPARISON: AVERAGE MCO EXPENDITURE BY MCO PROVIDER TAX STATUS
# Not a DiD / event study -- this is a simple cross-sectional group
# comparison using KFF's current (SFY2025) classification of which states
# have an MCO provider tax in place (kff_provtax_alltype.csv). Three groups:
#   1. "MCO Tax"        -- KFF column == "Yes"
#   2. "No MCO Tax"     -- KFF column == "No", but the state DOES use
#                          Medicaid managed care (so a tax is possible, it's
#                          just not in place)
#   3. "No MCO Usage"   -- states with no Medicaid managed care at all
#                          (WY, VT, ME, CT, AK, per mco_1.R), so an MCO tax
#                          isn't a meaningful policy option for them; kept
#                          as a separate group rather than lumped into
#                          "No MCO Tax" for that reason
# States coded "NR" (FL, MS -- did not respond to the 2025 survey) are
# excluded as unclassified.
#
# Reuses the same FMR parsing as mco_provider_tax_analysis.R /
# expenditure_outcomes_analysis.R. Caches the parsed panel to
# medicaid_expenditure_panel_all.rds so this doesn't have to be re-parsed
# from the raw Excel files every time -- delete that file to force a
# re-parse if the source Excel files change.
# ==============================================================================

library(readxl)
library(dplyr)
library(purrr)
library(ggplot2)

cache_path <- file.path(data_output_path, "medicaid_expenditure_panel_all.rds")

if (file.exists(cache_path)) {
  cat("Loading cached parsed FMR panel from", cache_path, "\n")
  panel_all <- readRDS(cache_path)
} else {
  cat("No cache found -- parsing raw FMR files (this takes a few minutes)...\n")

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
        !startsWith(v0, "ADM") && !grepl("Created On", v0) && is.na(df[i, 2])
    }
    state_rows  <- which(sapply(seq_len(nrow(df)), is_state_row))
    state_names <- trimws(as.character(df[state_rows, 1]))
    bind_rows(lapply(seq_along(state_rows), function(k) {
      row_i <- state_rows[k]; state <- state_names[k]
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
    "2012" = "FMR_Net_Expenditures_FY12.xlsx", "2013" = "FMR_Net_Expenditures_FY13.xlsx",
    "2014" = "FMR_Net_Expenditures_FY14.xlsx", "2015" = "FMR_Net_Expenditures_FY2015.xlsx",
    "2016" = "FMR_Net_Expenditures_FY2016.xlsx", "2017" = "FMR_Net_Expenditures_FY2017.xlsx",
    "2018" = "FMR_Net_Expenditures_FY2018.xlsx", "2019" = "FMR_Net_Expenditures_FY2019.xlsx",
    "2020" = "FMR_Net_Expenditures_FY2020.xlsx", "2021" = "FMR_Net_Expenditures_FY2021.xlsx",
    "2022" = "FMR_Net_Expenditures_FY2022.xlsx", "2023" = "FMR_Net_Expenditures_FY2023.xlsx",
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

  saveRDS(panel_all, cache_path)
  cat("Cached parsed panel to", cache_path, "\n")
}

# ------------------------------------------------------------------------------
# 1. KFF MCO provider tax classification (SFY2025 snapshot)
# ------------------------------------------------------------------------------
kff_raw <- read.csv(file.path(data_input_path, "kff_provtax_alltype.csv"), skip = 2, nrows = 52)

no_mco_states <- c("WY", "VT", "ME", "CT", "AK")

# KNOWN DATA BUG: Wyoming's 2002-2010 rows in the old-format FMR file are
# wildly inflated (Total Net Expenditures of $246B-$383B for a state of
# ~580K people -- physically impossible; drops to a plausible ~$500-700M
# from 2011 on). This affects WY's whole state block, not just the MCO row,
# so it looks like a column-alignment issue in parse_old_format() specific
# to Wyoming's 2002-2011 sheets. Excluding WY here rather than let it
# distort a 5-state group average. Doesn't affect the earlier DiD analyses
# (WY's own tax-adoption event window there only touches 2012+ data), but
# this bug is now baked into the cached medicaid_expenditure_panel_all.rds
# -- worth fixing parse_old_format() before trusting any pre-2011 WY
# figures elsewhere.
bad_data_states <- c("WY")

kff_class <- kff_raw %>%
  mutate(
    state = ifelse(Location == "District of Columbia", "DC", state.abb[match(Location, state.name)])
  ) %>%
  filter(!is.na(state), Location != "United States", !state %in% bad_data_states) %>%
  transmute(
    state,
    mco_tax_yn = States.With.an.MCO.Provider.Tax.in.Place,
    mco_group = case_when(
      state %in% no_mco_states  ~ "No MCO Usage",
      mco_tax_yn == "Yes"       ~ "MCO Tax",
      mco_tax_yn == "No"        ~ "No MCO Tax",
      TRUE                      ~ NA_character_   # NR -- unclassified, excluded
    )
  )

cat("\n=== GROUP SIZES (SFY2025 KFF CLASSIFICATION) ===\n")
kff_class %>% count(mco_group) %>% print()

cat("\nExcluded (NR / unclassified):\n")
kff_class %>% filter(is.na(mco_group)) %>% pull(state) %>% print()

# ------------------------------------------------------------------------------
# 2. MCO expenditure panel (Medicaid - MCO line), state-level averages
# ------------------------------------------------------------------------------
mco_panel <- panel_all %>%
  select(state, year, `Medicaid - MCO_total`) %>%
  rename(mco_total = `Medicaid - MCO_total`) %>%
  left_join(kff_class, by = "state") %>%
  filter(!is.na(mco_group))

state_avg_all_years <- mco_panel %>%
  filter(!is.na(mco_total), mco_total > 0) %>%
  group_by(state, mco_group) %>%
  summarise(mean_mco = mean(mco_total, na.rm = TRUE),
            mean_log_mco = mean(log(mco_total), na.rm = TRUE), .groups = "drop")

state_avg_recent <- mco_panel %>%
  filter(!is.na(mco_total), mco_total > 0, year >= 2020) %>%
  group_by(state, mco_group) %>%
  summarise(mean_mco = mean(mco_total, na.rm = TRUE),
            mean_log_mco = mean(log(mco_total), na.rm = TRUE), .groups = "drop")

# ------------------------------------------------------------------------------
# 3. Group averages -- all years (2002-2024) and recent years (2020-2024)
# ------------------------------------------------------------------------------
summarize_groups <- function(state_avg, label) {
  cat("\n=== GROUP AVERAGES:", label, "===\n")
  out <- state_avg %>%
    group_by(mco_group) %>%
    summarise(
      n_states       = n(),
      mean_mco       = mean(mean_mco, na.rm = TRUE),
      median_mco     = median(mean_mco, na.rm = TRUE),
      mean_log_mco   = mean(mean_log_mco, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_mco))
  print(out, n = 10)
  out
}

groups_all_years <- summarize_groups(state_avg_all_years, "ALL YEARS 2002-2024")
groups_recent    <- summarize_groups(state_avg_recent, "RECENT YEARS 2020-2024")

# ------------------------------------------------------------------------------
# 4. Pairwise t-tests (log scale, more robust to skew than raw dollars)
# ------------------------------------------------------------------------------
run_pairwise <- function(state_avg, label) {
  cat("\n=== PAIRWISE T-TESTS (log MCO spending):", label, "===\n")
  pairs <- combn(unique(state_avg$mco_group), 2, simplify = FALSE)
  for (p in pairs) {
    d <- state_avg %>% filter(mco_group %in% p)
    if (n_distinct(d$mco_group) == 2 && all(table(d$mco_group) >= 2)) {
      t <- t.test(mean_log_mco ~ mco_group, data = d)
      cat(sprintf("%s vs %s: diff = %.3f, p = %.3f\n",
                   p[1], p[2],
                   t$estimate[1] - t$estimate[2], t$p.value))
    } else {
      cat(sprintf("%s vs %s: skipped (insufficient n)\n", p[1], p[2]))
    }
  }
}

run_pairwise(state_avg_all_years, "ALL YEARS")
run_pairwise(state_avg_recent, "RECENT YEARS")

# ------------------------------------------------------------------------------
# 5. Plot: state-level mean log MCO spending by group (all years)
# ------------------------------------------------------------------------------
p <- ggplot(state_avg_all_years, aes(x = mco_group, y = mean_log_mco, color = mco_group)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +
  geom_jitter(width = 0.15, size = 2, alpha = 0.8) +
  labs(title = "State-Level Mean Log MCO Spending by MCO Provider Tax Status",
       subtitle = "Non-causal comparison -- KFF SFY2025 classification; each point is one state's own mean across 2002-2024",
       x = "", y = "Mean log(MCO spending)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("mco_expenditure_analysis/mco_tax_status_comparison.png", plot = p, width = 8, height = 6, dpi = 300)
cat("\nSaved: mco_expenditure_analysis/mco_tax_status_comparison.png\n")

cat("\nDone -- mco_tax_status_comparison.R complete.\n")
