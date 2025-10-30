# Meta --------------------------------------------------------------------
## Author:        Jill Wilkins
## Date Created:  10/30/2025
## Date Edited:   10/30/2025
## Goal:         Clean a map figure to show policy adoption over time      
##  

library(ggplot2)
library(dplyr)
install.packages("maps")
library(maps)

# Example data
# tax_adopt <- data.frame(
#   state_abbr = c("CA", "TX", "FL", "NY", "MA", "AL"),
#   first_tax_year = c(2010, 2013, 2012, 2018, NA, 2011)
# )


tax_adopt_clean <- fmap_tax %>%
  group_by(state) %>%
  summarize(first_tax_year = min(firsttax, na.rm = TRUE)) %>%
  mutate(first_tax_year = ifelse(is.infinite(first_tax_year), NA, first_tax_year))

state_lookup <- data.frame(
  state_full = tolower(state.name),
  state = state.abb
)

tax_adopt_clean <- tax_adopt_clean %>%
  left_join(state_lookup, by = "state") %>%
  mutate(state_full = tolower(state_full))

us_states <- map_data("state")

map_merged <- us_states %>%
  left_join(tax_adopt_clean, by = c("region" = "state_full"), relationship = "many-to-one") %>%
  mutate(first_tax_year = as.numeric(first_tax_year))

install.packages("viridis")
library(viridis)
scale_fill_viridis(
  name = "Year Adopted",
  option = "Blues",
  direction = -1,    # darker = earlier, lighter = recent
  na.value = "grey90"
)

ggplot(map_merged, aes(long, lat, group = group, fill = first_tax_year)) +
  geom_polygon(color = "white", size = 0.3) +
  coord_fixed(1.3) +
  scale_fill_gradient(
    name = "Year Adopted",
   low  = "#bfe7fd",  # light pastel blue
  high = "#1f78b4",    # dark blue
    na.value = "grey90"
  ) +
  theme_void() +
  labs(
    title = "Year of Hospital Tax Adoption by State",
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "right"
  )


