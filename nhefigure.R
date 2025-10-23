# Meta --------------------------------------------------------------------
## Author:        Jill Wilkins
## Date Created:  10/20/2025
## Date Edited:   10/20/2025
## Goal:         Create motivational figure of total health expenditure     
##  

# National Spending Chart
nhe <- read.csv("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/input/nhe_table2.csv")

 nhe <- nhe %>% 
 mutate(total = nhe2023dol / 1000000000) %>%
 mutate(hospbil = percent_hosp * total)

library(ggplot2)
library(ggrepel)   # for smart label placement

nhe_plot <- ggplot(nhe, aes(x = year)) +
  # Lines
  geom_line(aes(y = total), color = "#1f77b4", size = 1.2) +
  geom_line(aes(y = hospbil), color = "#d62728", linetype = "dashed", size = 1.2) +
  
  # End-of-line labels
  geom_text_repel(
    data = nhe |> dplyr::filter(year == max(year)),
    aes(y = total, label = "Total Spending"),
    color = "#1f77b4", hjust = 0, nudge_x = 0.3, nudge_y = 20, size = 4
  ) +
  geom_text_repel(
    data = nhe |> dplyr::filter(year == max(year)),
    aes(y = hospbil, label = "Hospital Spending"),
    color = "#d62728", hjust = 0, nudge_x = 0.3, nudge_y = 20, size = 4
  ) +
  
  # Titles & labels
  labs(
    x = "Year",
    y = "Spending (Billions)"
  ) +
  
  # Theme tweaks
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 40, 10, 10)  # make room for labels
  ) +
  # Extend x-axis slightly to make room for labels
  expand_limits(x = max(nhe$year) + 1)

nhe_plot
ggsave("nhe_plot.png", plot = nhe_plot, width = 7, height = 5, dpi = 300)

# PANEL VIEW 
# if not already installed
install.packages('devtools', repos = 'http://cran.us.r-project.org') 

# note: "V" is capitalized
devtools::install_github('xuyiqing/panelView') 
library(panelView)

# for simple viewing add yes_tax to fmap_tax 
fmap_tax <- fmap_tax %>%
  mutate(
    yes_tax = case_when(
      !is.na(firsttax) & year >= firsttax ~ 1,  # taxed from firsttax onward
      TRUE ~ 0                                # never taxed or before firsttax
    )
  )
View(fmap_tax)
panelview <- panelview(
  firsttax ~ yes_tax,
  data = fmap_tax %>% filter(year < 2020),
  index = c("state", "year"),
  type = "treat",
  main = "Hospital Provider Tax Adoption Across States",
  legend.labs = c("Not Taxed", "Taxed"),
  xlab = "Year",
  ylab = "State"
)
ggsave("panelview.png", plot = panelview, width = 10, height = 8, dpi = 300)


View(hospdata)
