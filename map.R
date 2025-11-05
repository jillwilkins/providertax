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

adopt_map <- ggplot(map_merged, aes(long, lat, group = group, fill = first_tax_year)) +
  geom_polygon(color = "white", size = 0.3) +
  coord_fixed(1.3) +
  scale_fill_gradient(
    name = "Year Adopted",
   low  = "#bfe7fd",  # light pastel blue
  high = "#1f78b4",    # dark blue
    na.value = "#e5e5e5"
  ) +
  theme_void() +
  labs(
    title = "Year of Hospital Tax Adoption by State",
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "right"
  )
ggsave("adopt_map.png", plot = adopt_map, width = 10, height = 6, dpi = 300)

# RATE specific map 
rate <- read.csv("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/input/rate.csv", skip = 1)
View(rate)
rate <- rate %>%
  rename(state = State, hosprate = Hospital.taxes) %>%
  mutate(state = tolower(state))

# Get map data
us_states <- map_data("state")

# Join directly since 'state' matches 'region'
map_rate <- us_states %>%
  left_join(rate, by = c("region" = "state"))
unique(rate$hosprate)

# Make sure legend order matches color order
map_rate$hosprate <- factor(map_rate$hosprate,
  levels = c(
    "No Response",
    "No Provider Tax",
    "No Provider Tax Over 3.5%",
    "Provider Tax Over 3.5% But Not Over 5.5%",
    "Provider Tax Over 5.5%"
  )
)

ratemap <- ggplot(map_rate, aes(long, lat, group = group, fill = hosprate)) +
  geom_polygon(color = "white", size = 0.3) +
  coord_fixed(1.3) +
  scale_fill_manual(
    name = "Tax Rate",
    values = c(
      "No Response" = "#9a9a9a",       # light gray
      "No Provider Tax" = "#e5e5e5",   # very light gray
      "No Provider Tax Over 3.5%" = "#bfe7fd",      # blue
      "Provider Tax Over 3.5% But Not Over 5.5%" = "#5caee0", 
      "Provider Tax Over 5.5%" = "#1f78b4"
    ),
    labels = c("No Response", "No Tax", "< 3.5%", "3.5 - 5.5%", " > 5.5%")
  ) +
  labs(
    title = "Hospital Provider Tax Status by State",
    caption = "Data: SFY 2024, KFF Survey of Medicaid Officials"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )

ggsave("ratemap.png", plot = ratemap, width = 10, height = 6, dpi = 300)

# histogram version 
ratehist <- ggplot(rate %>% filter(hosprate != "No Response"), aes(x = hosprate, fill = hosprate)) +
  geom_bar(color = "white", width = 0.8) +
  scale_fill_manual(
    name = "Tax Status",
    values = c(
      "No Provider Tax" = "#9a9a9a",
      "No Provider Tax Over 3.5%" = "#bfe7fd",
      "Provider Tax Over 3.5% But Not Over 5.5%" = "#5caee0",
      "Provider Tax Over 5.5%" = "#1f78b4"
    )
  ) +
  scale_x_discrete(
    labels = c(
      "No Provider Tax" = "No Tax",
      "No Provider Tax Over 3.5%" = "< 3.5%",
      "Provider Tax Over 3.5% But Not Over 5.5%" = "3.5–5.5%",
      "Provider Tax Over 5.5%" = "> 5.5%"
    )
  ) +
  labs(
    title = " ",
    x = "Tax Rate (SFY 2024)",
    y = "Number of States",
    caption = "Data: SFY 2024, KFF Survey of Medicaid Officials"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_text(color = "black", hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 15)),
    panel.grid = element_blank(),  # removes all grid lines
    panel.background = element_rect(fill = "transparent", color = NA),  # make panel transparent
    plot.background = element_rect(fill = "transparent", color = NA),    # make background transparent
    plot.caption = element_text(color = "#888888", size = 8, vjust = -1, hjust = 1)  # smaller caption
  ) 

ggsave("ratehist.png", plot = ratehist, width = 7, height = 5, dpi = 300, bg = "transparent")

