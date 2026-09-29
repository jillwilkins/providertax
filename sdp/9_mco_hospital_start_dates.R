###########################################################
# SDP preliminary analysis
# Step 9: pull program start dates for every Medicaid managed care
# program that covers inpatient hospital care (Inpatient.hospital.
# physical.health == "X"), from managed_care_programs_by_state.csv.
#
# NOTE on the source file: its "Year" column only takes two values
# (2014, 2016) -- it's two snapshot pulls of a Medicaid.gov tracker,
# not an annual panel. Program.start.date is the real timeline
# variable and goes back to the 1980s in some states, but the
# snapshot can't see any program that started fresh after 2016.
###########################################################

library(dplyr)

data_input_path <- "/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/input/"

mco_raw <- read.csv(paste0(data_input_path, "managed_care_programs_by_state.csv"))

hosp_progs <- mco_raw %>%
  filter(Inpatient.hospital.physical.health == "X") %>%
  select(state_name = State, program_name = Program.Name, program_type = Program.type,
         start_date_raw = Program.start.date) %>%
  mutate(start_date = as.Date(start_date_raw, format = "%m/%d/%Y")) %>%
  distinct(state_name, program_name, program_type, start_date_raw, .keep_all = TRUE) %>%
  arrange(state_name, start_date)

cat("n programs with hospital inpatient benefit:", nrow(hosp_progs), "\n")
cat("n distinct states represented:", n_distinct(hosp_progs$state_name), "(of 50 -- no DC in this file)\n\n")

write.csv(hosp_progs %>% select(-start_date_raw), "sdp/mco_hospital_programs_tmp.csv", row.names = FALSE)
file.rename("sdp/mco_hospital_programs_tmp.csv", "sdp/mco_hospital_programs.csv")

# ------------------------------------------------------------------
# State-level summary: earliest hospital-inclusive COMPREHENSIVE MCO
# start date (excludes PACE/PCCM/other narrower arrangements, which
# also carry the hospital flag but aren't full-risk managed care in
# the SDP-relevant sense)
# ------------------------------------------------------------------
comprehensive_only <- hosp_progs %>%
  filter(program_type %in% c("Comprehensive MCO", "Comprehensive MCO + MLTSS"))

state_summary <- comprehensive_only %>%
  group_by(state_name) %>%
  summarise(
    earliest_comprehensive_mco_hosp = min(start_date, na.rm = TRUE),
    n_comprehensive_programs = n(),
    .groups = "drop"
  ) %>%
  arrange(earliest_comprehensive_mco_hosp)

cat("=== States with NO comprehensive MCO covering hospitals (any type, per this file) ===\n")
missing_states <- setdiff(unique(mco_raw$State), state_summary$state_name)
print(sort(missing_states))

cat("\n=== Earliest hospital-inclusive Comprehensive MCO start date, by state ===\n")
print(state_summary, n = Inf)

write.csv(state_summary, "sdp/mco_hospital_state_summary_tmp.csv", row.names = FALSE)
file.rename("sdp/mco_hospital_state_summary_tmp.csv", "sdp/mco_hospital_state_summary.csv")

View(hosp_progs)
