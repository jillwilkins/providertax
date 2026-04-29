#######################################################
# This script will contain all the code used to generate figures in my paper and presentation. 
#########################################################

# ============================================================
# PANEL VIEW 
# Last Updated: 4/27/26 for 3YP draft.

library(panelview)

# Filter data to 2004-2022 first
state_data_filtered <- state_data %>%
  filter(year >= 2004 & year <= 2022)

state_data_panelview <- state_data_filtered %>%
  select(state, year, ever_treat, post_treat,
         median_income_pre, eligibility, exp_status, firsttax)

# Get the 2021 rows for those states and copy them as 2022
rows_to_add <- state_data_panelview %>%
  filter(state %in% c("CT", "VT", "RI"), year == 2021) %>%
  mutate(year = 2022)

# Bind them in
state_data_panelview <- bind_rows(state_data_panelview, rows_to_add)

# Verify
state_data_panelview %>%
  filter(state %in% c("CT", "VT", "RI"), year %in% c(2021, 2022)) %>%
  select(state, year, post_treat) %>%
  arrange(state, year)

panelview <- panelview(
    data        = state_data_panelview,
    D           = "post_treat",
    index       = c("state", "year"),
    type        = "treat",
    main        = "",
    xlab        = "Year",
    ylab        = "State",
    legend.labs = c("Control", "Treated", "Missing"),
    color       = c("#BFCDE0", "#1B3A6B"),
    axis.lab.gap = c(1, 0),
    by.timing   = TRUE,
    display.all = TRUE        # <-- forces never-treated units to show
  )

panelview <- panelview + labs(fill = NULL) + 
  scale_fill_manual(
    values = c("#BFCDE0", "#1B3A6B" ),
    labels = c("No Provider Tax", "Provider Tax")  # change these to whatever you want
  )

ggsave("panelview_treatment_status.png", panelview, width = 14, height = 10, dpi = 300)
# ============================================================


# ============================================================
# Cumulative Tax Adoption 
# Last Updated: 4/27/26 for 3YP poster.

adoption_plot <- ggplot(adoption_over_time, 
                        aes(x = firsttax, y = cumulative_states)) +
  geom_col(fill = "#b8deff", color = "#17527f", alpha = 0.75, width = 0.95) +
  annotate(
    "label",
    x          = min(adoption_over_time$firsttax) + 0.5,
    y          = max(adoption_over_time$cumulative_states) * 0.95,
    label      = "33 states have implemented\na provider tax since 2004.",
    hjust      = 0.075,
    vjust      = .80,
    size       = 8,
    fontface   = "italic",
    fill       = "#ffd16659",
    color      = "#17527f",
    label.size = 0.8,
    lineheight = 1.5
  ) +
  labs(
   # title = "Cumulative State Adoption of Provider Tax Over Time",
    x     = "Year",
    y     = "Number of States"
  ) +
  scale_x_continuous(breaks = seq(min(adoption_over_time$firsttax), 
                                  max(adoption_over_time$firsttax), by = 2)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title         = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(adoption_plot)

ggsave("figures/cumulative_adoption.png",
       plot  = adoption_plot,
       width = 16, height = 10, dpi = 300)


# ============================================================
# GEORGIA TAX_HRRP Comparison 
# Last Updated: 4/27/26 for 3YP draft.
# Note: This code following script georgia.R creating the data 


# Separate the data
ga_tax_only <- ga_comparison_share_long %>%
  filter(type %in% c("Provider Tax (Mean)", "Provider Tax (Median)"))

ga_hrrp_lines <- ga_comparison_share_long %>%
  filter(type %in% c("HRRP Penalty (Mean)", "HRRP Penalty (Median)"))

# Plot
comparison_share_plot <- ggplot(ga_tax_only,
                                aes(x = factor(year), y = share, fill = type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c(
    "Provider Tax (Mean)"   = "#2484d2",
    "Provider Tax (Median)" = "#abd9e9"
  )) +
  # HRRP horizontal lines per year
  geom_segment(
    data = ga_hrrp_lines,
    aes(
      x     = as.numeric(factor(year)) - 0.35,
      xend  = as.numeric(factor(year)) + 0.35,
      y     = share,
      yend  = share,
      color = type
    ),
    linewidth   = 3.25,
    inherit.aes = FALSE
  ) +
geom_text(
  data = ga_hrrp_lines %>% filter(year == 2022, type == "HRRP Penalty (Mean)"),
  aes(
    x     = 2 - 0.38,
    y     = share,
    label = "HRRP Mean"
  ),
  hjust       = 1,
  vjust       = -.5,     # push up
  size        = 4.25,
  color       = "#DC4B4B",
  inherit.aes = FALSE
) +
# HRRP Median label on the RIGHT
geom_text(
  data = ga_hrrp_lines %>% filter(year == 2022, type == "HRRP Penalty (Median)"),
  aes(
    x     = 2 + 0.38,
    y     = share,
    label = "HRRP Median"
  ),
  hjust       = 0,
  vjust       = 0,    # push down
  size        = 4.25,
  color       = "#f4a261",
  inherit.aes = FALSE
) +
  # Provider tax bar top labels
  geom_text(
    data = ga_tax_only,
    aes(
      x     = factor(year),
      y     = share,
      label = case_when(
        type == "Provider Tax (Mean)"   ~ "Provider Tax \nMean",
        type == "Provider Tax (Median)" ~ "Provider Tax \nMedian"),
      group = type
    ),
    position = position_dodge(width = 0.7),
    vjust    = -0.05,  # slightly above the bar
    size     = 4.25,
    color    = "black"
  ) +
  scale_color_manual(values = c(
    "HRRP Penalty (Mean)"   = "#DC4B4B", "HRRP Penalty (Median)" = "#f4a261"), guide = "none"
  ) +
  labs(
    x     = "Year",
    y     = "Payment as \nShare of Pre-Tax Operating Expenses",
    fill  = NULL,
    color = NULL
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 0.01),
    expand = expansion(mult = c(0, 0.15))  # extra space at top for bar labels
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title         = element_text(face = "bold", hjust = 0.5),
    plot.subtitle      = element_text(hjust = 0.5, size = 10),
    legend.position    = "none",
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  guides(
    fill  = guide_legend(nrow = 1),
    color = guide_legend(nrow = 1)
  ) + 
  annotate(
    "label",
    x          = 1,        # positions over the 2012 bars
    y          = 0.035,    # adjust height as needed
    label      = "Provider taxes represent a much larger\nfinancial obligation than HRRP penalties.",
    hjust      = 0.5,
    vjust      = 0.5,
    size       = 8,
    fontface   = "italic",
    fill       = "#ffd16659",
    color      = "#17527f",
    label.size = 0.8,
    lineheight = 1.5
  ) 
  
ggsave("figures/georgia_share_comparison.png",
       plot  = comparison_share_plot,
       width = 16, height = 10, dpi = 300)




# ============================================================
# DONUT of NHE Expenditures by Category
# Last Updated: 4/10/26 for 3YP poster presentation.

library(ggplot2)
library(dplyr)
library(ggtext)

hospshare<- read.csv("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/input/hospshare.csv") 

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
  arrange(desc(category)) %>%  # <-- change this line
  mutate(
    fraction = total / sum(total),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n = -1)),
    label_pos = (ymax + ymin) / 2,
    label_text = paste0(category, "\n", round(fraction * 100, 1), "%"),
    outside = category %in% c("Home Health", "Dental", "Nursing Care", "Other Professional Services", "Retail Prescription Drugs")
  )

# Set fill colors (Hospitals = bright blue, others = lighter gradient)
n_non_hosp <- sum(hospshare$category != "Hospitals", na.rm = TRUE)
non_hosp_colors <- scales::seq_gradient_pal("#c6dce7", "#738e9ce7")(seq(0, 1, length.out = n_non_hosp))
donut_colors <- hospshare %>%
  mutate(
    fill_color = ifelse(category == "Hospitals", "#53baff", non_hosp_colors),
    # make Hospitals label larger
    label_size = ifelse(category == "Hospitals", 5, 3)
  )

# create donut
donut <- ggplot(donut_colors, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.5, fill = fill_color)) +
  geom_rect(color = "white") +
  coord_polar(theta = "y") +
  xlim(c(2, 4.5)) +
  geom_text(
    data = filter(donut_colors, !outside),
    aes(
      x = 3.25,
      y = label_pos,
      label = label_text,
      fontface = ifelse(category == "Hospitals", "bold", "plain")
    ),
    size = 3,
    color = "black"
  ) +
  geom_text(
    data = filter(donut_colors, outside),
    aes(x = 4.5, y = label_pos, label = label_text),
    size = 2.5,
    hjust = 0.5
  ) +
  geom_segment(
    data = filter(donut_colors, outside),
    aes(x = 4, xend = 4.4, y = label_pos, yend = label_pos),
    color = "#7f7f7f5b",
    size = 0.5
  ) +
  theme_void() +
  scale_fill_identity() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "transparent", color = NA),  # ← added
    panel.background = element_rect(fill = "transparent", color = NA)  # ← added
  )

# Save with transparent background
ggsave("hospshare_donut.png", plot = donut, width = 7, height = 5, dpi = 300, bg = "transparent")
# ============================================================


# ============================================================
# NHE Over Time 
# Last Updated: 10/20/25 for practice presentation. 

library(ggrepel)

nhe <- read.csv("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/input/nhe_table2.csv")

 nhe <- nhe %>% 
 mutate(total = nhe2023dol / 1000000000000) %>%
 mutate(hospbil = percent_hosp * total)


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


ggsave("nhe_plot.png", plot = nhe_plot, width = 7, height = 5, dpi = 300)
# ============================================================

