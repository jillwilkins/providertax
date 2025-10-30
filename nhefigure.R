# Meta --------------------------------------------------------------------
## Author:        Jill Wilkins
## Date Created:  10/20/2025
## Date Edited:   10/20/2025
## Goal:         Create motivational figure of total health expenditure     
##  

# National Spending Chart
nhe <- read.csv("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/input/nhe_table2.csv")

 nhe <- nhe %>% 
 mutate(total = nhe2023dol / 1000000000000) %>%
 mutate(hospbil = percent_hosp * total)
View(nhe)
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
    color = "#1f77b4", hjust = -.1, nudge_x = 0.3, nudge_y = 0.2, size = 4
  ) +
  geom_text_repel(
    data = nhe |> dplyr::filter(year == max(year)),
    aes(y = hospbil, label = "Hospital Spending"),
    color = "#d62728", hjust = -.1, nudge_x = 0.3, nudge_y = 0.2, size = 4
  ) +
  # Titles & labels
  labs(
    x = "Year",
    y = "Spending (Trillions)"
  ) +
  # Theme tweaks
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 40, 10, 10)  # make room for labels
  ) +
  # X-axis limits
  scale_x_continuous(limits = c(min(nhe$year), 2023)) +
  # Fix Y-axis limits
  coord_cartesian(ylim = c(0, 5))  # sets Y-axis from 0 to 5

library(ggrepel)

nhe_plot <- ggplot(nhe, aes(x = year)) +
  geom_line(aes(y = total), color = "#1f77b4", size = 1.2) +
  geom_line(aes(y = hospbil), color = "#d62728", size = 1.2) +
  
  geom_text_repel(
    data = nhe |> dplyr::filter(year == 2020),
    aes(y = total, label = "Total Spending"),
    color = "#1f77b4", size = 3, nudge_y = 0.15
  ) +
  geom_text_repel(
    data = nhe |> dplyr::filter(year == 2020),
    aes(y = hospbil, label = "Hospital Spending"),
    color = "#d62728", size = 3, nudge_y = 0.1
  ) +
  
  labs(x = "Year", y = "Spending (Trillions)") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 40, 10, 10)
  ) +
  scale_x_continuous(limits = c(min(nhe$year), 2023)) +
  coord_cartesian(ylim = c(0, 5))

  # Extend x-axis slightly to make room for labels
   # expand_limits(x = max(nhe$year) + 1)

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


# Hospital Share of Expenditures Donut
library(ggplot2)
library(dplyr)
install.packages("ggtext")
library(ggtext)

hospshare<- read.csv("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/input/hospshare.csv") 
View(hospshare)

hospshare <- hospshare %>%
mutate(category = case_when(
  category == "Hospitals" ~ "Hospitals",
  category == "Physicians & clinics" ~ "Physicians & Clinics",
  category == "Retail prescription drugs" ~ "Retail Prescription Drugs",
  category == "Nursing care" ~ "Nursing Care",
  category == "Dental" ~ "Dental",
  category == "Other professional services" ~ "Other Professional Services",
  category == "Home health care" ~ "Home Health",
  category == "Other health" ~ "Other Health"
))

hospshare <- hospshare %>%
  arrange(desc(category)) %>%
  mutate(
    fraction = total / sum(total),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n = -1)),
    label_pos = (ymax + ymin) / 2,
    label_text = paste0(category, "\n", round(fraction * 100, 1), "%"),
    outside = category %in% c("Home Health", "Dental", "Nursing Care", "Other Professional Services", "Retail Prescription Drugs")
  )
 
# Set fill colors (Hospitals = bright blue, others = lighter gradient)
n_non_hosp <- sum(hospshare$category != "Hospitals")
non_hosp_colors <- scales::seq_gradient_pal("#c3cbd0", "#b0dcf2e7")(seq(0, 1, length.out = n_non_hosp))
donut_colors <- hospshare %>%
  mutate(
    fill_color = ifelse(category == "Hospitals", "#308bc8", non_hosp_colors),
    # make Hospitals label larger
    label_size = ifelse(category == "Hospitals", 5, 3)
  )

# Plot
donut <- ggplot(donut_colors, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.5, fill = fill_color)) +
  geom_rect(color = "white") +
  coord_polar(theta = "y") +
  xlim(c(2, 5)) +  # extend x-axis to give space for outside labels
  # Labels inside the donut
  geom_text(
    data = filter(donut_colors, !outside),
    aes(x = 3.25, y = label_pos, label = label_text),
    size = 2,
    color = "black"
  ) +
  # Labels outside the donut
  geom_text(
    data = filter(donut_colors, outside),
    aes(x = 4.5, y = label_pos, label = label_text),
    size = 2,
    hjust = 0.5
  ) +
  # Lines connecting outside labels to slices
  geom_segment(
    data = filter(donut_colors, outside),
    aes(x = 4, xend = 4.4, y = label_pos, yend = label_pos),
    color = "#7f7f7f5b",
    size = 0.5
  ) +
  theme_void() +
  scale_fill_identity() +
  labs(title = "Share of Total Health Spending by Category") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "plain"),
    legend.position = "none"
  )

# Save the plot
ggsave("hospshare_donut.png", plot = donut, width = 7, height = 5, dpi = 300)


