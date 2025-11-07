install.packages("censusapi")
library(censusapi)

# If you have an API key, set it:
censusapi::censusapikey("3d68cc260a0373b59509aed3e418df2d057eb664", install = TRUE)
Sys.setenv(CENSUS_KEY = "3d68cc260a0373b59509aed3e418df2d057eb664")

years <- 2004:2023
results_list <- list()

for (yr in years) {
  dat <- getCensus(
    name = "timeseries/poverty/saipe",
    vintage = NULL,                # time‐series endpoint uses YEAR parameter
    vars = c("NAME", "SAEMHI_PT", "STATE", "COUNTY"),
    region = "county:*",
    YEAR = yr
  )
  dat$YEAR <- yr
  results_list[[as.character(yr)]] <- dat
  Sys.sleep(0.5)  # be polite to API
}

# Bind into one data.frame
library(dplyr)
full_df <- bind_rows(results_list)
# View the first few rows

library(dplyr)
library(stringr)

saipe_clean <- full_df %>%
  mutate(
    county_name = str_to_title(str_remove(NAME, "\\s+County$")),  # remove "County", proper case
    state_name = case_when(
      STATE == "01" ~ "Alabama",
      STATE == "02" ~ "Alaska",
      STATE == "04" ~ "Arizona",
      STATE == "05" ~ "Arkansas",
      STATE == "06" ~ "California",
      STATE == "08" ~ "Colorado",
      STATE == "09" ~ "Connecticut",
      STATE == "10" ~ "Delaware",
      STATE == "11" ~ "District Of Columbia",
      STATE == "12" ~ "Florida",
      STATE == "13" ~ "Georgia",
      STATE == "15" ~ "Hawaii",
      STATE == "16" ~ "Idaho",
      STATE == "17" ~ "Illinois",
      STATE == "18" ~ "Indiana",
      STATE == "19" ~ "Iowa",
      STATE == "20" ~ "Kansas",
      STATE == "21" ~ "Kentucky",
      STATE == "22" ~ "Louisiana",
      STATE == "23" ~ "Maine",
      STATE == "24" ~ "Maryland",
      STATE == "25" ~ "Massachusetts",
      STATE == "26" ~ "Michigan",
      STATE == "27" ~ "Minnesota",
      STATE == "28" ~ "Mississippi",
      STATE == "29" ~ "Missouri",
      STATE == "30" ~ "Montana",
      STATE == "31" ~ "Nebraska",
      STATE == "32" ~ "Nevada",
      STATE == "33" ~ "New Hampshire",
      STATE == "34" ~ "New Jersey",
      STATE == "35" ~ "New Mexico",
      STATE == "36" ~ "New York",
      STATE == "37" ~ "North Carolina",
      STATE == "38" ~ "North Dakota",
      STATE == "39" ~ "Ohio",
      STATE == "40" ~ "Oklahoma",
      STATE == "41" ~ "Oregon",
      STATE == "42" ~ "Pennsylvania",
      STATE == "44" ~ "Rhode Island",
      STATE == "45" ~ "South Carolina",
      STATE == "46" ~ "South Dakota",
      STATE == "47" ~ "Tennessee",
      STATE == "48" ~ "Texas",
      STATE == "49" ~ "Utah",
      STATE == "50" ~ "Vermont",
      STATE == "51" ~ "Virginia",
      STATE == "53" ~ "Washington",
      STATE == "54" ~ "West Virginia",
      STATE == "55" ~ "Wisconsin",
      STATE == "56" ~ "Wyoming",
      TRUE ~ NA_character_
    )
  ) %>%
  select(state_name, county_name, YEAR, median_income = SAEMHI_PT)

saipe_clean <- saipe_clean %>%
  mutate(
    state_name = str_to_lower(state_name),
    county_name = toupper(county_name)
  )

state_lookup <- tibble(
  state_name = tolower(state.name),
  state = tolower(state.abb)
)

saipe_clean <- saipe_clean %>%
  left_join(state_lookup, by = "state_name")

saipe_clean <- saipe_clean %>%
  select(state, county = county_name, year = YEAR, median_income)

saipe_clean <- saipe_clean %>%
    mutate(state = toupper(state))

hospdata_clean <- hospdata_clean %>%
  mutate(
    state = toupper(state))

hospdata_clean <- hospdata_clean %>%
  left_join(saipe_clean, by = c("state", "county", "year"))

colnames(hospdata_clean)

nainc <- hospdata_clean %>%
  filter(is.na(median_income)) 
View(nainc)

summary(hospdata_clean$median_income)

