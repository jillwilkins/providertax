# ==============================================================================
# ADD STATE MEDICAID ENROLLMENT DATA
# ==============================================================================

library(tidycensus)
library(tidyverse)
install.packages("tabulapdf")
library(tabulapdf)
library(tigris)
install.packages('ipumsr')
library('ipumsr')

# obtain IPUMS ACS data on Medicaid Enrollment 
ddi <- read_ipums_ddi(paste0(data_input_path, "usa_00002.xml"))
data_enroll <- read_ipums_micro(ddi)


# updated approach
data_enroll <- data_enroll %>%
  rename(
    year                = YEAR,
    medicaid_enrollment = HINSCAID
  ) %>%
  mutate(
    statefip   = sprintf("%02d", as.numeric(STATEFIP)),
    state_name = fips_codes$state_name[match(statefip, fips_codes$state_code)],
    state      = state.abb[match(state_name, state.name)]
  ) %>%
  filter(!is.na(state)) %>%   # drops Puerto Rico, territories, unknowns explicitly
  group_by(state, statefip, year) %>%
  summarise(
    medicaid_enrollment = sum(medicaid_enrollment, na.rm = TRUE),
    .groups = "drop"
  )

# rename variables, convert the fips code
data_enroll <- data_enroll %>%
  rename(
    year               = YEAR,
    medicaid_enrollment = HINSCAID
  ) %>%
  mutate(
    state = c(state.abb, "DC")[match(sprintf("%02d", as.numeric(STATEFIP)), 
                                     c(sprintf("%02d", c(1:56)[-c(3,7,14,43,52)]), "11"))]
  ) %>%
# aggregate to the state year level
  group_by(state, year) %>%
  summarise(
    medicaid_enrollment = sum(medicaid_enrollment, na.rm = TRUE),
    .groups = "drop"
  )

#save ipums data
write.csv(
  data_enroll,
  paste0(data_output_path, "ipums_data_enroll.csv"),
  row.names = FALSE
)

###################################################
# After running ipums once, I can start here. 
data_enroll <- read.csv(paste0(data_output_path, "ipums_data_enroll.csv"))

data_enroll <- data_enroll %>% filter(year > 2007)

# Load data if not already in environment
 hospdata_analysis <- read.csv(paste0(data_output_path, "hospdata_analysis.csv"))

hospdata_analysis <- hospdata_analysis %>%
  left_join(
    data_enroll %>% select(state, year, medicaid_enrollment),
    by = c("state", "year")
  )

summary(hospdata_analysis$medicaid_enrollment)

# ==============================================================================
# ADD STATE MEDICAID ELIGIBILITY DATA
# =========================================================================

elig <- read.csv(
  paste0(data_input_path, "elig_kff.csv"), 
  skip = 2  # Skip header rows
)

elig <- elig %>%
  select(-Footnotes) %>%           # Remove footnote column
  slice(2:52) %>%                  # Keep state rows only (remove title rows)
  rename(statename = Location) %>%
  mutate(
    # Convert state names to abbreviations
    state = ifelse(
      statename == "District of Columbia", 
      "DC",
      state.abb[match(statename, state.name)]
    )
  )
  

# rename columns to the closet year they represent
elig <- elig %>%
  rename("2003" = "April.2003", "2004" = "July.2004", "2005" = "July.2005", "2006" = "July.2006", "2010" = "December.2009", "2024" = "May.2024") %>%
  rename_with(~ str_remove(.x, "^January\\."))

# keep only state name and eligibility columns
elig <- elig %>%
  select(state, starts_with("20"))

# convert to numeric
elig <- elig %>%
  mutate(across(starts_with("20"), as.numeric))

# Then pivot to long format
elig <- elig %>%
  pivot_longer(
    cols = -state,
    names_to = "year",
    values_to = "eligibility"
  ) %>%
  mutate(year = as.integer(year))

View(elig)

# Merge with hospdata_analysis
hospdata_analysis <- hospdata_analysis %>%
  left_join(elig, by = c("state", "year"))

# SAVE 
write.csv(
  hospdata_analysis,
  paste0(data_output_path, "hospdata_analysis.csv"),
  row.names = FALSE
)
