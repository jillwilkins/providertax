# ==============================================================================
# ADD COUNTY-LEVEL INCOME DATA
# ==============================================================================

library(censusapi)
library(dplyr)
library(purrr)

# Set API key
Sys.setenv(CENSUS_KEY = "3d68cc260a0373b59509aed3e418df2d057eb664")

# Fetch data for each year (compact version)
cat("Fetching Census SAIPE data...\n")

saipe_raw <- map_dfr(2003:2022, function(yr) {
  getCensus(
    name = "timeseries/poverty/saipe",
    vintage = NULL,
    vars = c("NAME", "SAEMHI_PT", "STATE"),
    region = "state:*",
    time = yr
  ) %>% mutate(year = yr)
})

# Clean and prepare
saipe_clean <- saipe_raw %>%
  mutate(
    # Convert FIPS to state abbreviation  
    state_fips = sprintf("%02d", as.numeric(STATE)),
    state = c(state.abb, "DC")[match(state_fips, c(sprintf("%02d", c(1:56)[-c(3,7,14,43,52)]), "11"))],
    state = toupper(state),
    # Convert income
    median_income = as.numeric(SAEMHI_PT)
  ) %>%
  filter(!is.na(state), !is.na(median_income)) %>%
  select(state, year, median_income)

# Check a few examples
cat("\n=== SAMPLE STATE NAMES ===\n")
saipe_clean %>% filter(year == 2010) %>%
  select(state, median_income) %>%
  head(10) %>%
  print()

# CORRECT:
hospdata_clean <- hospdata_clean %>%
  mutate(state = toupper(state)) %>%
  left_join(saipe_clean, by = c("state", "year"))

# Check merge success
cat("\n=== MERGE CHECK ===\n")
cat(paste("Observations with income data:", 
          sum(!is.na(hospdata_clean$median_income)), "\n"))
cat(paste("Percent merged:", 
          round(mean(!is.na(hospdata_clean$median_income)) * 100, 1), "%\n"))
# drop the original median_income column if it exists (to avoid confusion) 

# pre treatment median income
hospdata_clean <- hospdata_clean %>%
  group_by(mcrnum) %>%
  mutate(median_income_pre = ifelse(post_treat == 0, median_income, NA_real_)) %>%
  fill(median_income_pre, .direction = "downup") %>%
  ungroup()

# ==============================================================================
# SAVE ANALYTICAL DATASET
# ==============================================================================

write.csv(
  hospdata_clean,
  paste0(data_output_path, "hospdata_analysis.csv"),
  row.names = FALSE
)

cat("\n=== DATA SAVED SUCCESSFULLY ===\n")
cat("Output file: hospdata_analysis.csv\n")
cat("Location:", data_output_path, "\n")

cat("\n", rep("=", 70), "\n", sep = "")
cat("CLEANING COMPLETE!\n")
cat(rep("=", 70), "\n\n", sep = "")