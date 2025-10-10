# Meta --------------------------------------------------------------------
## Author:        Jill Wilkins
## Date Created:  9/22/2025
## Date Edited:   10/7/2025
## Goal:         Clean data and create the working data set for analysis       
##  

library(dplyr)
library(stringr)

#load in hcris data  
hcris <- read.delim("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/output/HCRIS_data.txt", stringsAsFactors = FALSE)

# clean state names
unique(hcris$state)
hcris <- hcris %>%
  mutate(
    state = str_trim(toupper(state)),
    state = recode(state,
      "TENNESSEE" = "TN",
      "TE" = "TN",
      "MICHIGAN" = "MI",
      "ILLINOIS" = "IL",
      "CALIFORNIA" = "CA",
      "ARIZONA" = "AZ",
      "MONTANA" = "MT", 
      "NORTH CAROLINA" = "NC",
      "WISCONSIN" = "WI", 
      "KA" = "KS",
      "P." = "PR",
      "AX" = "AZ"))

#remove states with NA name 
hcris <- hcris %>%
  filter(!is.na(state))


# load in tax data
tax <- read.csv("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/input/statetaxadopt_kff.csv", skip = 2)

# remove footnotes column and notes in extra rows 
tax <- tax %>% select(-Footnotes)
tax <- tax[2:52, ]

# convert full state names (statename) to abbreviations (state)
tax <- tax %>% rename(statename = Location)
tax$state <- state.abb[match(tax$statename, state.name)]
tax$state[tax$statename == "District of Columbia"] <- "DC"

# change column names 
tax <- tax %>%
  rename_with(~ str_remove(.x, "^SFY\\.")) 

#find first year with tax 
tax <- tax %>%
  select(state, starts_with("20"))

tax <- tax %>%
  mutate(firsttax = apply(select(., starts_with("20")), 1, function(row) {
    years <- names(row)[str_detect(as.character(row), regex("yes|^Y$", ignore_case = TRUE))]
    if (length(years) == 0 || any(is.na(as.integer(years)))) {"never"} 
    else {min(as.integer(years))}}))


#load in FMAP data 
fmap <- read.csv("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/input/fmaps_kff.csv", skip = 2)
fmap <- fmap %>% select(-Footnotes)
fmap <- fmap[2:52, ]
fmap <- fmap %>% rename(statename = Location)
fmap$state <- state.abb[match(fmap$statename, state.name)]
fmap$state[fmap$statename == "District of Columbia"] <- "DC"

# change column names 
fmap <- fmap %>%
  rename_with(~ case_when(
    str_detect(.x, "FMAP") ~ paste0("fmap_", str_extract(.x, "\\d{4}")),
    str_detect(.x, "Multiplier") ~ paste0("m_", str_extract(.x, "\\d{4}")),
    TRUE ~ .x
  ))

# pivot longer
fmap <- fmap %>%
  mutate(across(matches("^(fmap|m)_\\d{4}$"), ~ as.numeric(as.character(.))))

fmap <- fmap %>%
  pivot_longer(
    cols = -state,  
    names_to = c(".value", "year"),
    names_pattern = "(fmap|m)_(\\d{4})"
  ) %>%
  rename(multiplier = m) %>%
  mutate(year = as.integer(year))

#remove NA rows from titles 
fmap <- fmap %>%
  filter(!is.na(fmap) & !is.na(multiplier))

# create cumulative count of states with a tax each year 
tax_totals <- tax %>%
  pivot_longer(
    cols = matches("^20\\d{2}$"), # all year columns like 2004, 2005, ...
    names_to = "year",
    values_to = "has_tax"
  ) %>%
  mutate(
    year = as.integer(year),
    has_tax = tolower(has_tax) %in% c("yes", "y") # convert to logical TRUE/FALSE
  ) %>%
  group_by(year) %>%
  summarise(totaltax = sum(has_tax, na.rm = TRUE), .groups = "drop")


# join tax and fmap
fmap_tax <- fmap %>%
  left_join(tax %>% select(state, firsttax),
    by = c("state" = "state")) %>%
  left_join(tax_totals,
    by = c("year" = "year"))

# join fmap_tax into hcris
hcris <- hcris %>%
  left_join(
    fmap_tax %>% select(state, year, fmap, multiplier, firsttax, totaltax),
    by = c("state" = "state", "year" = "year")
  )

# load in AHA data 
aha <- read_csv("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/input/AHAdata_20052023.csv")

# convert year columns to numeric
hcris <- hcris %>% mutate(year = as.numeric(year))
aha   <- aha %>% mutate(YEAR = as.numeric(YEAR))

# join in aha variables 
hospdatafull <- hcris %>%
  mutate(provider_number = as.character(provider_number)) %>%
  left_join(
    aha %>% select(MCRNUM, YEAR, SERV, TRAUMHOS, TRAUMSYS, PSYEMHOS, PSYEMSYS, ALCHHOS, ALCHSYS),
    by = c("provider_number" = "MCRNUM", "year" = "YEAR")
  )

# identify treatment groups 
always_st <- hospdatafull %>%
  filter(firsttax < 2008) %>%
  distinct(state) %>%
  pull(state)
print(always_st)

never_st <- hospdatafull %>%
  filter(firsttax == "never") %>%
  distinct(state) %>%
  pull(state)
never_st

hospdatafull <- hospdatafull %>%
  mutate(
    always = case_when(
      state %in% always_st ~ 1,    # 1 if state is in always_st
      state %in% never_st  ~ NA,   # NA if state is in never_st
      TRUE                 ~ 0     # 0 otherwise
      )
  ) %>%
  mutate(
    never = case_when(
      state %in% never_st  ~ 1,    # 1 if state is in never_st
      state %in% always_st ~ NA,   # NA if state is in always_st
      TRUE                 ~ 0     # 0 otherwise
    )
  ) %>%
  mutate(
    treated = case_when(
      state %in% never_st  ~ 0,    # 0 if state is in never_st
      state %in% always_st ~ 0,    # 0 if state is in always_st
      TRUE                 ~ 1     # 1 otherwise
    )
  )

View(hospdatafull)

hospdatafull %>%
  group_by(provider_number, state) %>%
  summarise(
    always = first(always),
    never  = first(never),
    treated = first(treated),
    .groups = "drop"
  ) %>%
  summarise(
    always_states = n_distinct(state[always == 1]),
    never_states  = n_distinct(state[never == 1]),
    treated_states = n_distinct(state[treated == 1])
  )

unique(hospdatafull$state[hospdatafull$always == 1])
unique(hospdatafull$state[hospdatafull$never == 1])
unique(hospdatafull$state[hospdatafull$treated == 1])

# remove territories and correct error 
hospdatafull <- hospdatafull %>%
  mutate(state = case_when(
    state == "UTAH" ~ "UT",   
    TRUE ~ state              
  )) %>%
  filter(!state %in% c("AS", "GU", "MP", "PR", "VI"))   

hospdatafull <- hospdatafull %>%
  mutate(treated_group = case_when(
    treated == 1 ~ "Treated",
    always   == 1 ~ "Always",
    never    == 1 ~ "Never",
    TRUE          ~ "Other"
  ))

# when i do this, i only get years after 2008. 
hospdata <- hospdatafull %>%
  filter(SERV == 10)

# add variables
hospdata <- hospdata %>%
  mutate(yes_tax = ifelse(!is.na(firsttax) & year >= firsttax, 1, 0))

# add dependent variable calculations 
hospdata <- hospdata %>%
  mutate(
    ucc_prop = tot_uncomp_care_charges / tot_charges, 
    cost_per_discharge = tot_charges / tot_discharges, 
    mcaid_ccr = mcaid_cost / mcaid_charges,
    mcaid_prop = mcaid_charges / tot_charges, 
    mcaid_prop_discharges = mcaid_discharges / tot_discharges)
