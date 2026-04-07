# ==============================================================================
# ADD STATE MEDICAID ENROLLMENT DATA
# ==============================================================================

library(tidycensus)
library(tidyverse)
install.packages("tabulapdf")
library(tabulapdf)


# IPUMS 
install.packages('ipumsr')
library('ipumsr')

ddi <- read_ipums_ddi(paste0(data_input_path, "usa_00002.xml"))
data_enroll <- read_ipums_micro(ddi)
colnames(data_enroll)

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
# Old code but dont want to delete it yet 

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

