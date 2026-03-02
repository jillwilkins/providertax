# ==============================================================================
# VISUALIZE OUTCOME DISTRIBUTION
# ==============================================================================

library(ggplot2)

# Which outcome do you want to visualize?
outcome_var <- "net_pat_rev"  # Change as needed
outcome_label <- "Net Patient Revenue"  # Change as needed

# Remove missing values
plot_data <- hospdata_analysis %>%
  filter(!is.na(.data[[outcome_var]]) & year <= 2020)

# Summary statistics
cat("\n=== DISTRIBUTION SUMMARY ===\n")
summary(plot_data[[outcome_var]])

# 1. Histogram
p1 <- ggplot(plot_data, aes(x = .data[[outcome_var]])) +
  geom_histogram(bins = 50, fill = "#4575B4", alpha = 0.7, color = "white") +
  geom_vline(xintercept = mean(plot_data[[outcome_var]], na.rm = TRUE), 
             linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = median(plot_data[[outcome_var]], na.rm = TRUE), 
             linetype = "dotted", color = "darkgreen", size = 1) +
  labs(
    title = paste("Distribution of", outcome_label),
    x = outcome_label,
    y = "Count",
    caption = "Red line = mean, Green line = median"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

print(p1)
#ggsave(paste0("figures/dist_", outcome_var, "_histogram.png"), 
       #width = 10, height = 6, dpi = 300)

# 2. Density plot
p2 <- ggplot(plot_data, aes(x = .data[[outcome_var]])) +
  geom_density(fill = "#4575B4", alpha = 0.5, color = "#4575B4", size = 1) +
  geom_vline(xintercept = mean(plot_data[[outcome_var]], na.rm = TRUE), 
             linetype = "dashed", color = "red", size = 1) +
  labs(
    title = paste("Density of", outcome_label),
    x = outcome_label,
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

print(p2)
#ggsave(paste0("figures/dist_", outcome_var, "_density.png"), 
       #width = 10, height = 6, dpi = 300)

