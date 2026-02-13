# Meta --------------------------------------------------------------------
## Author:        Jill Wilkins
## Date Created:  11/18/2025
## Date Edited:   12/9/2025
## Goal:          Clean data
## 

# load cleaned data 
hospdata <- read.csv("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/output/hospdata_hcris.csv")

# keep years 2020 and before
hospdata_clean <- hospdata %>%
  filter(year <= 2020)

# drop hospitals that have beds = NA 
hospdata_clean <- hospdata_clean %>%
  group_by(mcrnum) %>%                       
  filter(!all(is.na(beds))) %>%            
  ungroup()

# keep hospitals with 3 or fewer years of missing beds 
hospdata_clean <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(sum(is.na(beds)) <= 3) %>%  
  ungroup()

# drop if hospitals have beds = 0 ever 
hospdata_clean <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(!any(beds == 0, na.rm = TRUE)) %>%  
  ungroup()

# drop if hospitals ever have beds less than 30
hospdata_clean <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(!any(beds < 30, na.rm = TRUE)) %>%  
  ungroup()

# drop hospitals that have NA for an outcome in all years 
hospdata_clean <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(!all(is.na(private_prop_discharges))) %>%   
  ungroup()

hospdata_clean <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(!all(is.na(mcaid_prop_discharges))) %>%   
  ungroup()

# drop if state is empty for all years; if missing fill that in 
hospdata_clean <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(!all(is.na(state))) %>%   # keep hospitals with at least one non-NA state
  ungroup()

# define pre treatment level covariates (beds, county income)
# pre treatment beds 
hospdata_clean <- hospdata_clean %>%
  group_by(mcrnum) %>%
  mutate(prebeds = mean(beds[yes_tax == 0], na.rm = TRUE)) %>%
  ungroup()

# insert county level income data 
library(censusapi)
censusapi::censusapikey("3d68cc260a0373b59509aed3e418df2d057eb664", install = TRUE)
Sys.setenv(CENSUS_KEY = "3d68cc260a0373b59509aed3e418df2d057eb664")

years <- 2004:2023
results_list <- list()

for (yr in years) {
  dat <- getCensus(
    name = "timeseries/poverty/saipe",
    vintage = NULL,                
    vars = c("NAME", "SAEMHI_PT", "STATE", "COUNTY"),
    region = "county:*",
    YEAR = yr
  )
  dat$YEAR <- yr
  results_list[[as.character(yr)]] <- dat
  Sys.sleep(0.5)  # be polite to API
}

full_df <- bind_rows(results_list)

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
  select(state, county = county_name, year = YEAR, median_income) %>%
    mutate(state = toupper(state))

hospdata_clean <- hospdata_clean %>%
  mutate(state = toupper(state))%>%
  left_join(saipe_clean, by = c("state", "county", "year"))

hospdata_clean <- hospdata_clean %>%
  group_by(county) %>%
  mutate(ct_pre_income = mean(median_income[year < firsttax], na.rm = TRUE)) %>%  
  ungroup()

summary(hospdata_clean$ct_pre_income)

# save cleaned and income data 
write.csv(hospdata_clean, "/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/output/hospdata_clean.csv", row.names = FALSE)

# filter for only general acute hospitals 
hospdata_clean <- hospdata_clean %>%
  filter(
    !mcrnum %in% multi_state_mcrnums,
    !typectrl %in% 7:13
  )

View(hospdata_clean)

# NOW dec 7 attempt to add in serv from AHA 
aha$MCRNUM <- as.integer(aha$MCRNUM)

aha_serv <- aha %>%
  group_by(MCRNUM) %>%
  summarize(SERV = first(SERV), .groups = "drop")

hospdata_aha <- hospdata_clean%>%
  left_join(aha_serv, by = c("mcrnum" = "MCRNUM"))

View(hospdata_aha)

# georgia calc
georgia_ratio <- hospdata_clean %>%
  filter(state == "GA", hrrp_payment > 0, net_pat_rev > 0, year == 2013 | year == 2012) %>%                      # keep Georgia hospitals
  mutate(hrrp_ratio = hrrp_payment / net_pat_rev)

summary(georgia_ratio$hrrp_ratio)
hospdata %>%
  group_by(state) %>%
  filter(firsttax != 2004) %>%
  summarise(count = n())


