# Meta --------------------------------------------------------------------
## Author:        Jill Wilkins
## Date Created:  10/2/2025
## Date Edited:   10/7/2025
## Goal:         fmap vs number of states with tax        
##  

library(ggplot2)
fmap_tax <- fmap_tax %>% filter(!is.na(year)) 

# figure of tax adoption over time
sum_data <- fmap_tax %>%
  group_by(year) %>%
  summarise(
    states_with_tax = sum(year >= firsttax, na.rm = TRUE),  # states with tax in that year
    avg_fmap = mean(fmap, na.rm = TRUE),                   # average FMAP across states
    .groups = "drop"
  )

scale_factor <- max(sum_data$states_with_tax) / max(sum_data$avg_fmap)

taxfmapcorr <- ggplot(sum_data, aes(x = year)) +
  # Left axis: number of states with tax
  geom_line(aes(y = states_with_tax), color = "blue", size = 1.2) +
  geom_point(aes(y = states_with_tax), color = "blue", size = 2) +
  # Right axis: average FMAP (scaled)
  geom_line(aes(y = avg_fmap * scale_factor), color = "red", size = 1.2) +
  geom_point(aes(y = avg_fmap * scale_factor), color = "red", size = 2) +
  scale_y_continuous(
    name = "States with Tax",
    sec.axis = sec_axis(~ . / scale_factor, name = "Average FMAP")
  ) +
  labs(
    x = "Year",
    title = "States with Tax vs Average FMAP Over Time"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = "blue", size = 12),
    axis.text.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red", size = 12),
    axis.text.y.right = element_text(color = "red"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
  )

setwd("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/providertax")
ggsave("sumplots/taxfmapcorr.png", plot = taxfmapcorr, width = 8, height = 8, dpi = 300)


# correlation test 
fmap_tax <- fmap_tax %>%
  mutate(firsttax = ifelse(is.na(firsttax), "never", firsttax)) %>%
  mutate(tax_adopted_num = ifelse(firsttax != "never", 1, 0))

cor(fmap_tax$fmap, fmap_tax$tax_adopted_num, use = "complete.obs")

state_corr <- fmap_tax %>%
  group_by(year) %>%
  summarise(correlation = cor(fmap, tax_adopted_num, use = "complete.obs"))
View(state_corr)
# conclusion: correlation is low suggesting that the reason a state adopted a tax is not explained by FMAP
