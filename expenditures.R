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
      !startsWith(v0, "ADM") &&   # add this line
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
  
  # Drop non-medical sheets: ADM (administration), MAP prefix, and SKIP_VALS
  state_sheets <- state_sheets[!state_sheets %in% SKIP_VALS]
  state_sheets <- state_sheets[!grepl("^ADM", state_sheets)]  # drop administration sheets
  
  bind_rows(lapply(state_sheets, function(sheet) {
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


panel_old %>%
  distinct(state) %>%
  filter(grepl("^ADM|^MAP|^Created", state)) %>%
  print(n = 50)

panel_new %>%
  distinct(state) %>%
  filter(grepl("^ADM|^MAP|^Created", state)) %>%
  print(n = 50)

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

View(panel_hosp)

# verify supp payment data has data for years it should exist
panel_hosp %>%
  select(state, year, matches("Sup")) %>%
  filter(!is.na(`Inpatient Hospital - Sup. Payments_total`)) %>%
  count(year)

# remove T and C columns 
panel_hosp <- panel_hosp %>%
  select(-matches("^(C-|T-)"))

# remove extra dashes and spaces 
panel_hosp <- panel_hosp %>%
  rename_with(~ str_replace_all(.x, "[ .\\-–]+", "_") %>%
                str_replace_all("_+", "_") %>%       # collapse multiple underscores
                str_remove("_$"))                     # remove trailing underscore

# Save this data in case I need non hospital columns later 
write.csv(panel_hosp,  file.path(data_output_path, "medicaid_expenditures_full.csv"),  row.names = FALSE)

# keep necessary columns for hosp analysis
panel_hosp <- panel_hosp %>%
  select(state, year, matches("Hospital", ignore.case = TRUE))

colnames(panel_hosp)

# ── Save ──────────────────────────────────────────────────────────────────────
write.csv(panel_hosp, file.path(data_input_path, "medicaid_panel_hospital.csv"), row.names = FALSE)

cat("\nDone!\n")
cat("Full panel:    ", nrow(panel_all),  "rows,", ncol(panel_all),  "columns\n")
cat("Hospital panel:", nrow(panel_hosp), "rows,", ncol(panel_hosp), "columns\n")

# change names to abbreviations, drop territories
panel_hosp <- panel_hosp %>%
  mutate(
    state = ifelse(
      state == "Dist. Of Col.",
      "DC",
      state.abb[match(state, state.name)]
    )
  ) %>%
  filter(!is.na(state))  # drops territories and any unmatched names

# Check for mismatches
View(panel_hosp)
View(state_data)

state_data_merged <- state_data %>%
  left_join(panel_hosp, by = c("state", "year"))
View(state_data_merged)

colnames(state_data)

summary(state_data_merged$Inpatient_Hospital_Sup_Payments_total)
summary(hospdata_stack$npr_bed)

panel_hosp %>%
  select(Inpatient_Hospital_Reg_Payments_total,
         Inpatient_Hospital_Sup_Payments_total) %>%
  summary()

# Check negative values
panel_hosp %>%
  filter(Inpatient_Hospital_Reg_Payments_total < 0 | 
         Inpatient_Hospital_Sup_Payments_total < 0) %>%
  select(state, year, 
         Inpatient_Hospital_Reg_Payments_total,
         Inpatient_Hospital_Sup_Payments_total) %>%
  arrange(Inpatient_Hospital_Reg_Payments_total)

state_data %>%
  summarise(across(c(Inpatient_Hospital_Reg_Payments_total,
                     Inpatient_Hospital_Sup_Payments_total,
                     Outpatient_Hospital_Services_total,
                     Outpatient_Hospital_Services_Sup_Payments_total),
                   ~ sum(.x == 0, na.rm = TRUE)))



#RENAME and Handle 0s
  state_data <- state_data %>%
  mutate(
    clean_inp_tot = ifelse(Inpatient_Hospital_total < 0,    NA,
                           Inpatient_Hospital_total),
    clean_out_tot = ifelse(Outpatient_Hospital_total < 0,       NA,
                           Outpatient_Hospital_total),                       
    clean_inp_reg = ifelse(Inpatient_Hospital_Reg_Payments_total < 0,    NA,
                           Inpatient_Hospital_Reg_Payments_total),
    clean_inp_sup = ifelse(Inpatient_Hospital_Sup_Payments_total < 0,    NA,
                           Inpatient_Hospital_Sup_Payments_total),
    clean_out_reg = ifelse(Outpatient_Hospital_Services_total < 0,       NA,
                           Outpatient_Hospital_Services_total),
    clean_out_sup = ifelse(Outpatient_Hospital_Services_Sup_Payments_total < 0, NA,
                           Outpatient_Hospital_Services_Sup_Payments_total)
  )

ihs <- function(x) log(x + sqrt(x^2 + 1))

state_data <- state_data %>%
  mutate(
    inp_reg_per_bed = clean_inp_reg / pre_beds_avg,
    inp_sup_per_bed = clean_inp_sup / pre_beds_avg,
    out_reg_per_bed = clean_out_reg / pre_beds_avg,
    out_sup_per_bed = clean_out_sup / pre_beds_avg,
    
    log_inp_reg = ifelse(clean_inp_reg == 0, ihs(clean_inp_reg), log(clean_inp_reg)),
    log_inp_sup = ifelse(clean_inp_sup == 0, ihs(clean_inp_sup), log(clean_inp_sup)),
    log_out_reg = ifelse(clean_out_reg == 0, ihs(clean_out_reg), log(clean_out_reg)),
    log_out_sup = ifelse(clean_out_sup == 0, ihs(clean_out_sup), log(clean_out_sup))
  )

time_plot <- state_data %>%
  filter(!is.na(rel_year), !is.na(Inpatient_Hospital_Reg_Payments_total), state != "WY", rel_year >= -5, rel_year <= 5) %>%
  group_by(rel_year, ever_treat) %>%
  summarise(mean_inp = mean(Inpatient_Hospital_Sup_Payments_total/pre_beds_avg, na.rm = TRUE), .groups = "drop") %>%
  mutate(group = ifelse(ever_treat == 1, "Treated", "Never Treated")) %>%
  ggplot(aes(x = rel_year, y = mean_inp, color = group)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = -1, linetype = "dotted", color = "red") +
  labs(title = "Average Inpatient Regular Payments Per Bed by Treatment Status",
       x     = "Years Relative to Treatment",
       y     = "Mean Inpatient Regular Payments Per Bed",
       color  = "") +
  theme_minimal()

ggsave("time_plot.png", plot = time_plot, width = 10, height = 6, dpi = 300)

# MAJOR ISSUE with data 
p99 <- quantile(state_data$inp_reg_per_bed, 0.99, na.rm = TRUE)

state_data %>%
  filter(inp_reg_per_bed > p99) %>%
  select(state, year, rel_year, inp_reg_per_bed) %>%
  arrange(desc(inp_reg_per_bed))

View(state_data %>% filter(state == "WY"))
summary(state_data$pre_beds_avg)

# Basic summary
summary(state_data$Inpatient_Hospital_Sup_Payments_total)

# Distribution by year
state_data %>%
  filter(state != "WY") %>%  # Exclude WY if needed
  group_by(year) %>%
  summarise(
    n        = n(),
    mean     = mean(Inpatient_Hospital_Reg_Payments_total, na.rm = TRUE),
    median   = median(Inpatient_Hospital_Reg_Payments_total, na.rm = TRUE),
    sd       = sd(Inpatient_Hospital_Reg_Payments_total, na.rm = TRUE),
    min      = min(Inpatient_Hospital_Reg_Payments_total, na.rm = TRUE),
    max      = max(Inpatient_Hospital_Reg_Payments_total, na.rm = TRUE),
    p99      = quantile(Inpatient_Hospital_Reg_Payments_total, 0.99, na.rm = TRUE)
  ) %>%
  print(n = 30)

# Does panel_hosp actually have data for 2009+?
panel_hosp %>%
  group_by(year) %>%
  summarise(
    n = n(),
    non_na = sum(!is.na(Inpatient_Hospital_total))
  ) %>%
  print(n = 30)


# Top 10 highest values
state_data %>%
  select(state, year, clean_inp_reg) %>%
  arrange(desc(clean_inp_reg)) %>%
  head(10)

# Bottom 10 non-zero values  
state_data %>%
  filter(clean_inp_reg > 0) %>%
  select(state, year, clean_inp_reg) %>%
  arrange(clean_inp_reg) %>%
  head(10)

  # Column names in panel_old
names(panel_old)[grepl("Inpatient|Outpatient", names(panel_old), ignore.case = TRUE)]
summary(panel_hosp$Inpatient_Hospital_total)
summary(panel_hosp$Inpatient_Hospital_Reg_Payments_total)

# Column names in panel_new
names(panel_new)[grepl("Inpatient|Outpatient", names(panel_new), ignore.case = TRUE)]

state_year_inp <- panel_all %>%
  group_by(state, year) %>%
  summarise(
    mean_inp_reg = mean(`Inpatient Hospital - Reg. Payments_total`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(state, year)

rel_year_inp <- state_data %>%
  group_by(state, rel_year) %>%
  summarise(
    mean_inp_reg = mean(Inpatient_Hospital_Reg_Payments_total, na.rm = TRUE),
    maen_inp_sup = mean(Inpatient_Hospital_Sup_Payments_total, na.rm = TRUE),
    mean_out_reg = mean(Outpatient_Hospital_Services_Reg_Payments_total, na.rm = TRUE),
    mean_out_sup = mean(Outpatient_Hospital_Services_Sup_Payments_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(state, rel_year)

View(rel_year_inp)

colnames(panel_hosp)
