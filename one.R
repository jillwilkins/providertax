# Meta --------------------------------------------------------------------

## Author:        Jill Wilkins
## Date Created:  9/22/2025
## Date Edited:   9/22/2025
## Notes:          
##  

#load in  hcris data for net patient revenue  
hcris <- read.delim("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/output/HCRIS_data.txt", stringsAsFactors = FALSE)

# Example: convert full state name to abbreviation
library(tools)
hcris$state <- trimws(hcris$state) 

hcris$state_abbrev <- state.abb[match(hcris$state, state.name)]
hcris$state_abbrev[hcris$state == "District of Columbia"] <- "DC"

# load state provider tax data 
tax <- read.csv("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/input/statetaxadopt_kff.csv", skip = 2)
tax <- tax %>% select(-Footnotes)
# keep only the first 52 rows
tax <- tax[1:52, ]
tax <- tax %>% rename(state = Location)

# convert full state names to abbreviations
tax$state_abbrev <- state.abb[match(tax$state, state.name)]
tax$state_abbrev[tax$state == "District of Columbia"] <- "DC"

View(state_year_avg)
# take out hospital types not relevant for tax (only keep serv == 10) 
# hosp <- hcris %>% filter(serv == 10)


# generate first year with tax 
# select all SFY columns (assuming they start with "SFY")
year_cols <- grep("^SFY\\.", colnames(tax), value = TRUE)

# create first_tax_year column
tax$first_tax_year <- apply(tax[, year_cols], 1, function(x) {
  first_yes_col <- year_cols[which(x == "Yes")[1]]  # find first "Yes" column
  if (is.na(first_yes_col)) return(NA)             # no "Yes" found
  # strip "SFY" prefix to keep only the year number
  sub("^SFY\\.", "", first_yes_col)
})

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
