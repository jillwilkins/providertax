# Meta --------------------------------------------------------------------
## Author:        Jill Wilkins
## Date Created:  9/22/2025
## Date Edited:   10/7/2025
## Goal:         Clean data and create the working data set for analysis       
##  

library(dplyr)
library(stringr)

#load in  hcris data for net patient revenue  
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

#remove states NA 
hcris <- hcris %>%
  filter(!is.na(state))


# load state provider tax data
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
hospdata <- hcris %>%
  mutate(provider_number = as.character(provider_number)) %>%
  left_join(
    aha %>% select(MCRNUM, YEAR, SERV),
    by = c("provider_number" = "MCRNUM", "year" = "YEAR")
  )

# when i do this, i only get years after 2008. 
hospdata_10 <- hospdata %>%
  filter(SERV == 10)


View(hospdata)
