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
View(hcris)

# load state provider tax data
tax <- read.csv("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/input/statetaxadopt_kff.csv", skip = 2)
# remove footnotes column and notes in extra rows 
tax <- tax %>% select(-Footnotes)
tax <- tax[1:52, ]


# convert full state names (statename) to abbreviations (state)
tax <- tax %>% rename(statename = Location)
tax$state <- state.abb[match(tax$statename, state.name)]
tax$state[tax$statename == "District of Columbia"] <- "DC"

# change column names 
tax <- tax %>%
  rename_with(~ str_remove(.x, "^SFY\\.")) 

#find first year with tax 
tax <- tax %>%
  select(state, starts_with("20")) %>%
  filter(state != "NA")

tax <- tax %>%
  mutate(firsttax = apply(select(., starts_with("20")), 1, function(row) {
    years <- names(row)[str_detect(as.character(row), regex("yes|^Y$", ignore_case = TRUE))]
    if (length(years) == 0) NA_integer_ else min(as.integer(years))
  }))

#load in FMAP data 
fmap <- read.csv("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/input/fmaps_kff.csv", skip = 2)
fmap <- fmap %>% select(-Footnotes)
fmap <- fmap[1:52, ]
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

#rename USA
fmap <- fmap %>%
  mutate(state = if_else(is.na(state) | state == "NA", "USA", state))

# join tax, fmap, and hcris data by state and year 
fmap_tax <- fmap %>%
  left_join(
    tax %>% select(state, firsttax),
    by = c("state" = "state")
  )

hcris <- hcris %>%
  left_join(
    fmap_tax %>% select(state, year, fmap, multiplier, firsttax),
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


