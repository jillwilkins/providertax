# Meta --------------------------------------------------------------------
## Author:        Jill Wilkins
## Date Created:  9/22/2025
## Date Edited:   12/9/2025
## Goal:         Clean data and create the working data set for analysis       
##  

library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)

#load in hcris data  
hcris <- read.delim("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/output/HCRIS_data.txt", stringsAsFactors = FALSE)
colnames(hcris)
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
      "UTAH" = "UT",
      "KA" = "KS",
      "P." = "PR",
      "AX" = "AZ"))

#remove states with NA name 
hcris <- hcris %>%
  filter(!is.na(state)) %>%
  filter(!state %in% c("AS", "GU", "MP", "PR", "VI")) %>% 
  filter(!is.na(provider_number))

# load in tax data
tax <- read.csv("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/input/statetaxadopt_kff.csv", skip = 2)

# remove footnotes column, notes in extra rows; convert the state names to abbreviations
tax <- tax %>%
  select(-Footnotes) %>%
  slice(2:52) %>%
  rename(statename = Location) %>%
  mutate(
    state = ifelse(
      statename == "District of Columbia", "DC",
      state.abb[match(statename, state.name)]
    )
  )

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

# clean and rename states 
fmap <- fmap %>%
  select(-Footnotes) %>%
  slice(2:52) %>%
  rename(statename = Location) %>%
  mutate(
    state = ifelse(
      statename == "District of Columbia", "DC",
      state.abb[match(statename, state.name)]
    )
  )

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
    cols = matches("^20\\d{2}$"), # all year columns like 2004, 2005, 
    names_to = "year",
    values_to = "has_tax"
  ) %>%
  mutate(
    year = as.integer(year),
    has_tax = tolower(has_tax) %in% c("yes", "y") # convert to logical TRUE/FALSE
  ) %>%
  group_by(year) %>%
  summarise(totaltax = sum(has_tax, na.rm = TRUE), .groups = "drop")

# load in state expansion status 
# expansion <- read.csv("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/input/expansion.csv", skip = 2)

#drop excess rows and rename states
# expansion <- expansion %>% select(-Footnotes)
#expansion <- expansion[2:52, ]
#expansion <- expansion %>% rename(statename = Location) 
#expansion <- expansion %>% rename(expdate = Expansion.Implementation.Date) 
#expansion$state <- state.abb[match(expansion$statename, state.name)]
#expansion$state[expansion$statename == "District of Columbia"] <- "DC"
#expansion <- expansion %>%
#  mutate(
 #   expyear = case_when(
  #    expdate %in% c("N/A", "", NA) ~ NA_real_,   # keep NAs for non-expansion states
   #   TRUE ~ year(mdy(expdate))                   # extract the year (month/day/year)
   # )) %>% select(state, expdate, expyear)

# join data
fmap_tax <- fmap %>%
  left_join(tax %>% select(state, firsttax),
    by = c("state" = "state")) %>%
  left_join(tax_totals,
    by = c("year" = "year")) 
    #%>%
  #left_join(expansion, 
   # by = c("state" = "state"))

# join fmap_tax and hcris
hcris_tax <- hcris %>%
  left_join(
    fmap_tax %>% select(state, year, fmap, multiplier, firsttax, totaltax),
    by = c("state" = "state", "year" = "year")
  )

# verify provider_number class
class(hcris_tax$provider_number)

# SAVE here 12/6. 
write.csv(hcris_tax, "/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/output/hcris_prog.csv", row.names = FALSE)

# plan is to load treatment data and then merge with aha later when needed to avoid pre 2010 issues. 
hospdata <- hcris_tax

# GROUPS: 
# yes_tax: hospital with tax in that year (1 if year >= firsttax, 0 otherwise)
hospdata <- hospdata %>%
  mutate(yes_tax = ifelse(!is.na(firsttax) & year >= firsttax, 1, 0))

# identify always states, never states, treated states
# always states: firsttax <= 2005
always_state <- tax %>% filter(firsttax < 2005) %>% pull(state)
never_state <- tax %>% filter(firsttax == "never") %>% pull(state)

hospdata <- hospdata %>%
  mutate(
    always = case_when(
      state %in% always_state ~ 1,    # 1 if state is in always_state
      TRUE                 ~ 0     # 0 otherwise
      )
  ) %>%
  mutate(
    never = case_when(
      state %in% never_state  ~ 1,    # 1 if state is in never_st
      TRUE                 ~ 0     # 0 otherwise
    )
  ) %>%
  mutate(
    treated = case_when(
      state %in% never_state  ~ 0,    # 0 if state is in never_st
      state %in% always_state ~ 0,    # 0 if state is in always_st
      TRUE                 ~ 1     # 1 otherwise
    )
  )

#rename provider_number to mcrnum for merging later
hospdata <- hospdata %>%
  rename(mcrnum = provider_number)

# verify number of states in each group 
hospdata %>%
  group_by(mcrnum, state) %>%
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

# for my purposes, created treatment by 2020
hospdata <- hospdata %>%
  mutate(
    treated_by_2020 = case_when(
      !is.na(firsttax) & firsttax >= 2005 & firsttax <= 2019 ~ 1,
      TRUE ~ 0  
    ))

unique(hospdata$state[hospdata$always == 1])
unique(hospdata$state[hospdata$never == 1])
unique(hospdata$state[hospdata$treated == 1])
unique(hospdata$state[hospdata$treated_by_2020 == 1])

# treatment groups variable 
hospdata <- hospdata %>%
  mutate(
    treatment_group = case_when(
      always == 1 ~ "always",
      treated_by_2020 == 1 ~ "treated",
      TRUE ~ "not yet by 2020"
    )
  )

# make firsttax numeric for event study
hospdata <- hospdata %>%
  mutate(firsttax = as.numeric(firsttax))

# treatment group numerically for event study
hospdata <- hospdata %>%
  mutate(
    treatment_num = case_when(
      treatment_group == "always" ~ NA_real_,
      treatment_group == "treated" ~ firsttax,
      treatment_group == "not yet by 2020" ~ 0
    )
  )

# add outcome variable calculations 
hospdata <- hospdata %>%
  mutate(
    ucc_prop = tot_uncomp_care_charges / tot_charges, 
    cost_per_discharge = (cost_to_charge * tot_charges) / tot_discharges, 
    mcaid_ccr = mcaid_cost / mcaid_charges,
    mcaid_prop = mcaid_charges / tot_charges, 
    mcaid_prop_discharges = mcaid_discharges / tot_discharges, 
    mcare_prop_discharges = mcare_discharges / tot_discharges, 
    mm_prop_discharges = (mcaid_discharges + mcare_discharges)/ tot_discharges, 
    private_prop_discharges = 1 - mm_prop_discharges) 
    # obbd_prop = obbd / beds,
    #psychbed_prop = psybd / beds,
    #alcbed_prop = alchbd / beds)

# save working data
write.csv(hospdata, "/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/output/hospdata_hcris.csv", row.names = FALSE)

View(hospdata)
