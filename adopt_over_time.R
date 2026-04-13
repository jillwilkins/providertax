# ==================================================================
# Goal: Create a figure to show the adoption of provider taxes over time across states
# ==================================================================

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
    size       = 4.5,
    fontface   = "italic",
    fill       = "#ffd16659",
    color      = "#17527f",
    label.size = 0.6,
    lineheight = 1.3
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
       width = 7, height = 7, dpi = 300)
