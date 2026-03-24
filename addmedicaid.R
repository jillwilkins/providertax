# ==============================================================================
# ADD STATE MEDICAID ENROLLMENT DATA
# ==============================================================================

library(tidycensus)
library(tidyverse)
install.packages("tabulapdf")
library(tabulapdf)


# ==============================================================================
# EARLY YEARS (not consistent with the ACS data)
# ==============================================================================

# Data from The Kaiser Commision on Medicaid and the Uninsured
# Extract from page 5 only (this table is on page 5)
tables <- extract_tables(paste0(data_input_path, "/Kaiser2008.pdf"), pages = 8, output = "tibble")
length(tables)

medicaid_early <- tables[[1]] %>%
  pivot_longer(
    cols = -State,
    names_to = "Year",
    values_to = "Enrollment"
  ) %>%
  filter(State != "TOTAL") %>%
  mutate(
    Enrollment = Enrollment * 1000,
    State = state.abb[match(State, state.name)],
    State = ifelse(is.na(State), "DC", State),
    Year = as.integer(paste0(ifelse(as.integer(sub("Jun-", "", Year)) >= 97, "19", "20"),
                             sub("Jun-", "", Year))), 
  ) %>%
  rename_with(tolower)

View(medicaid_early)

# ==============================================================================
# 2010+ YEARS (ACS, consistent)
# ==============================================================================
# this medicaid enrollment variable began in 2010; 2020 was not released due to COVID

vmed <- load_variables(2010, "acs5", cache = TRUE)
View(vmed)

medicaid_var <- "B992707_002"

years <- c(2010:2019, 2021:2022)  
 
# Pull data for all years
medicaid_late <- map_dfr(years, function(yr) {
  message("Fetching ", yr, "...")
  
  get_acs(
    geography = "state",
    variables = medicaid_var,
    year = yr,
    survey = "acs1",       # 1-year ACS for annual state-level data
    output = "wide"
  ) %>%
    mutate(year = yr)
})
View(medicaid_late)
#
medicaid_late <- medicaid_late %>%
  rename(
    state = NAME,
    geoid = GEOID,
    enrollment = B992707_002E  
  ) %>%
  select(geoid, state, year, enrollment) %>%
  arrange(state, year)


medicaid_late <- medicaid_late %>%
  mutate(
    state = c(state.abb, "DC")[match(state, c(state.name, "District of Columbia"))], 
  )


#combine early and late data
#medicaid_data <- bind_rows(
#  medicaid_early,
#  medicaid_late %>% select(state, year, enrollment)
#)


#View(medicaid_data)


hospdata_analysis <- hospdata_analysis %>%
  left_join(medicaid_late, by = c("state" = "state", "year" = "year")) %>%
  left_join(medicaid_early, by = c("state" = "state", "year" = "year")) 



