# Meta --------------------------------------------------------------------

## Author:        Jill Wilkins
## Date Created:  9/22/2025
## Date Edited:   9/22/2025
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

View(hcris)



# summarize net pat rev for state, year
state_year_avg <- hcris %>%
  group_by(state, year) %>%
  summarise(avg_net_pat_rev = mean(net_pat_rev, na.rm = TRUE)) %>%
  ungroup()

state_year_avg <- state_year_avg %>%
  left_join(tax %>% select(state_abbrev, first_tax_year), by = c("state" = "state_abbrev"))


library(dplyr)
library(ggplot2)

# 1. Count how many states have each first_tax_year
state_cum <- tax %>%
  filter(!is.na(first_tax_year)) %>%          # remove any NAs
  group_by(first_tax_year) %>%
  summarise(states_this_year = n()) %>%       # number of states with tax starting this year
  arrange(first_tax_year) %>%                 # make sure years are in order
  mutate(first_tax_year = as.integer(first_tax_year),   # convert to numeric
         cumulative_states = cumsum(states_this_year))  # cumulative total

# 2. Plot
ggplot(state_cum, aes(x = first_tax_year, y = cumulative_states)) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  labs(x = "Year", y = "Cumulative Number of States with Tax",
       title = "Cumulative Adoption of State Provider Taxes") +
  theme_minimal()

# adding net patient revenue 
avg_rev <- state_year_avg %>%
  group_by(year) %>%
  summarise(avg_net_pat_rev = mean(avg_net_pat_rev, na.rm = TRUE)) %>%
  ungroup()

ggplot() +
  # cumulative states line
  geom_line(data = state_cum, aes(x = first_tax_year, y = cumulative_states), 
            color = "steelblue", size = 1.2) +
  geom_point(data = state_cum, aes(x = first_tax_year, y = cumulative_states),
             color = "steelblue", size = 2) +
  
  # average net patient revenue line (scaled to fit)
  geom_line(data = avg_rev, aes(x = year, y = avg_net_pat_rev), 
            color = "darkred", size = 1.2) +
  geom_point(data = avg_rev, aes(x = year, y = avg_net_pat_rev),
             color = "darkred", size = 2) +

  # labels
  labs(x = "Year",
       y = "Cumulative States with Tax / Avg Net Patient Revenue",
       title = "Cumulative State Provider Tax Adoption and Avg Net Patient Revenue") +
  theme_minimal()

state_cum <- state_cum %>%
  filter(first_tax_year >= 2004)

avg_rev <- avg_rev %>%
  filter(year >= 2004)

# Find max values
max_states <- max(state_cum$cumulative_states)
max_revenue <- max(avg_rev$avg_net_pat_rev)

# Scaling factor
scale_factor <- max_revenue / max_states
library(ggplot2)

ggplot() +
  # cumulative states line (left axis)
  geom_line(data = state_cum, aes(x = first_tax_year, y = cumulative_states), 
            color = "steelblue", size = 1.2) +
  geom_point(data = state_cum, aes(x = first_tax_year, y = cumulative_states),
             color = "steelblue", size = 2) +

  # avg net patient revenue line, scaled down for plotting (right axis)
  geom_line(data = avg_rev, aes(x = year, y = avg_net_pat_rev / scale_factor), 
            color = "darkred", size = 1.2) +
  geom_point(data = avg_rev, aes(x = year, y = avg_net_pat_rev / scale_factor),
             color = "darkred", size = 2) +

  # labels
  scale_y_continuous(
    name = "Cumulative States with Tax",
    sec.axis = sec_axis(~ . * scale_factor, name = "Avg Net Patient Revenue ($)")
  ) +
  labs(x = "Year", title = "Cumulative State Provider Tax Adoption and Avg Net Patient Revenue") +
  theme_minimal()

# end original plot, this was good but scale was off and kinda funky looking. 

# Convert avg_net_patient_rev to millions
avg_rev <- avg_rev %>%
  mutate(avg_net_pat_rev_million = avg_net_pat_rev / 1e6)

# scaling factor for dual axis
scale_factor <- max(avg_rev$avg_net_pat_rev_million) / max(state_cum$cumulative_states)

ggplot() +
  # cumulative states line (left axis)
  geom_line(data = state_cum, aes(x = first_tax_year, y = cumulative_states), 
            color = "steelblue", size = 1.2) +
  geom_point(data = state_cum, aes(x = first_tax_year, y = cumulative_states),
             color = "steelblue", size = 2) +

  # avg net patient revenue line (scaled for plotting)
  geom_line(data = avg_rev, aes(x = year, y = avg_net_pat_rev_million / scale_factor), 
            color = "darkred", size = 1.2) +
  geom_point(data = avg_rev, aes(x = year, y = avg_net_pat_rev_million / scale_factor),
             color = "darkred", size = 2) +

  # dual Y axis
  scale_y_continuous(
    name = "Cumulative States with Tax",
    sec.axis = sec_axis(~ . * scale_factor, name = "Avg Net Patient Revenue (Millions $)")
  ) +
  # X-axis every 2 years
  scale_x_continuous(breaks = seq(2004, max(avg_rev$year), by = 2)) +
  
  labs(x = "Year", title = "Cumulative State Provider Tax Adoption and Avg Net Patient Revenue") +
  # White background with grid lines
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "grey95"),  # major grid lines
    panel.grid.minor = element_line(color = "grey90"),   # minor grid lines
  )

ggsave("tax_rev_plot.png", width = 12, height = 12, dpi = 300)
