# ==============================================================================
# This script follow download_expenditures.R and parses the downloaded Excel files into a panel dataset.
# ==============================================================================

library(readxl)
library(dplyr)
library(purrr)


SKIP_VALS <- c("Medicaid Financial Management Report",
               "Medicaid Financial Management Report  - Net Services",
               "Medicaid Finanacial Management Report",
               "Net Services", "National", "National Totals",
               "Medical Assistance Program", "Administration",
               "Service Category")

# ── FY2002–2011 parser (all states stacked in year-named sheets) ─────────────
parse_old_format <- function(path, year) {
  df <- read_excel(path, sheet = as.character(year), col_names = FALSE)
  df <- as.data.frame(df)
  
  is_state_row <- function(i) {
    v0 <- trimws(as.character(df[i, 1]))
    !is.na(df[i, 1]) && v0 != "" && v0 != "NA" &&
      !(v0 %in% SKIP_VALS) && !startsWith(v0, "FY") &&
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

# ── FY2012–2024 parser (each state in separate sheet) ───────────────────────
parse_new_format <- function(path, year) {
  state_sheets <- excel_sheets(path)
  state_sheets <- state_sheets[!state_sheets %in% SKIP_VALS]
  
  bind_rows(lapply(state_sheets, function(sheet) {
    
    # FY2013 uses "MAP - Alabama" style names — strip the prefix for the state value
    state <- sub("^MAP - ", "", sheet)
    
    df <- read_excel(path, sheet = sheet, col_names = FALSE)
    df <- as.data.frame(df)
    
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
        # State share: col 7 if 11-col layout, col 7 if 7-col layout — same either way
        rec[[paste0(v0, "_state")]]   <- suppressWarnings(as.numeric(r[[min(7, n_cols)]]))
      }
      # Right-side admin block only exists in 11-col layout (FY2012, FY2014+)
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

# ── Parse all files ───────────────────────────────────────────────────────────
cat("Parsing FY2002-2011...\n")
panel_old <- bind_rows(lapply(2002:2011, function(yr) {
  cat(" FY", yr, "\n")
  parse_old_format(file.path(data_input_path, "NetExpenditure02through11.xlsx"), yr)
}))

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

cat("Parsing FY2012-2024...\n")
panel_new <- bind_rows(imap(new_files, function(fname, yr) {
  cat(" FY", yr, "\n")
  parse_new_format(file.path(data_input_path, fname), year = as.integer(yr))
}))

# ── Combine ───────────────────────────────────────────────────────────────────
panel_all <- bind_rows(panel_old, panel_new) %>%
  filter(!state %in% c("National Totals", "National"),
         !grepl("Created On", state)) %>%
  arrange(state, year)

# ── Filter to hospital-related columns ───────────────────────────────────────
hosp_cols <- names(panel_all)[grepl(
  "Inpatient Hospital|Outpatient Hospital|Mental Health Facility|Nursing Facility|Intermediate Care|DSH|Sup\\. Payments|GME",
  names(panel_all), ignore.case = TRUE
)]

panel_hosp <- panel_all %>%
  select(state, year, all_of(hosp_cols))


# And verify Sup. Payments has data for years it should exist
panel_hosp %>%
  select(state, year, matches("Sup")) %>%
  filter(!is.na(`Inpatient Hospital - Sup. Payments_total`)) %>%
  count(year)


# ── Save ──────────────────────────────────────────────────────────────────────
write.csv(panel_all,  file.path(data_input_path, "medicaid_panel_full.csv"),  row.names = FALSE)
write.csv(panel_hosp, file.path(data_input_path, "medicaid_panel_hospital.csv"), row.names = FALSE)

cat("\nDone!\n")
cat("Full panel:    ", nrow(panel_all),  "rows,", ncol(panel_all),  "columns\n")
cat("Hospital panel:", nrow(panel_hosp), "rows,", ncol(panel_hosp), "columns\n")