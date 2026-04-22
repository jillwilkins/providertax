# ===================================================
# This script is to compare the Georgia provider tax amount to the HRRP tax amount 
# ===================================================
# Calculate change for each hospital
ga_opex_change <- hospdata_analysis %>%
  filter(state == "GA", year %in% c(2010, 2011)) %>%
  arrange(mcrnum, year) %>%
  group_by(mcrnum) %>%
  filter(n() == 2) %>%  # Only keep hospitals with both years
  summarise(
    opex_2010 = tot_operating_exp[year == 2010],
    opex_2011 = tot_operating_exp[year == 2011],
    change = opex_2011 - opex_2010,
    pct_change = (opex_2011 - opex_2010) / opex_2010 * 100,
    net_pat_rev_leg = net_pat_rev[year == 2010],
    tax_pay = case_when (
      state == "GA" & year == 2011 ~ 0.0145 * net_pat_rev_leg,  # 1.45% of 2010 OPEX
      TRUE ~ NA_real_
    ),
    .groups = "drop"
  )

summary(ga_opex_change$change)
summary(ga_opex_change$tax_pay)
summary(ga_opex_change$pct_change)
summary(hospdata_stack$op_margin)

# calculate tax amount: 1.45% of the previous years npr since 2011
georgia_tax <- hospdata_analysis %>%
  arrange(mcrnum, year) %>%
  group_by(mcrnum) %>%
  mutate(
    net_pat_rev_lag = lag(net_pat_rev, n = 1),
    
    taxamount = case_when(
      state == "GA" & year >= 2011 ~ 0.0145 * net_pat_rev_lag,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

summary(georgia_tax$taxamount)

# compare to HRRP penalty amount
georgia_tax <- georgia_tax %>%
  mutate(
hrrp_tax_diff = case_when(
      state == "GA" & year >= 2012 ~ taxamount - hrrp_payment,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

summary(georgia_tax$hrrp_tax_diff)

# pre tax opearting expesnses for comparison 
georgia_tax <- georgia_tax %>%
  group_by(mcrnum) %>%
  mutate(
    pre_tax_op_exp = mean(tot_operating_exp[year < 2011], na.rm = TRUE), 
    hrrp_as_per_op_exp = hrrp_payment / pre_tax_op_exp, 
    tax_as_per_op_exp = taxamount / pre_tax_op_exp 
  ) %>%
  ungroup()


# Plot the comparison

# ==============================================================================
# AVERAGE BY HOSPITAL-YEAR AND CREATE BAR CHART
# ==============================================================================

library(ggplot2)
library(tidyr)

# Calculate averages by year
ga_comparison <- georgia_tax %>%
  filter(state == "GA", year %in% c(2012, 2022)) %>%
  group_by(year) %>%
  summarise(
    avg_taxamount = mean(taxamount, na.rm = TRUE),
    avg_hrrp = mean(hrrp_payment, na.rm = TRUE),
    n_hospitals = n(),
    .groups = "drop"
  )

# View the summary
cat("\n=== GEORGIA: TAX vs HRRP COMPARISON ===\n")
print(ga_comparison)

# Reshape for plotting
ga_comparison_long <- ga_comparison %>%
  select(year, avg_taxamount, avg_hrrp) %>%
  pivot_longer(
    cols = c(avg_taxamount, avg_hrrp),
    names_to = "type",
    values_to = "amount"
  ) %>%
  mutate(
    type = case_when(
      type == "avg_taxamount" ~ "Provider Tax",
      type == "avg_hrrp" ~ "HRRP Penalty"
    )
  )

# Create bar chart
comparison_plot <- ggplot(ga_comparison_long, 
                          aes(x = factor(year), y = amount, fill = type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Provider Tax" = "#2C7BB6", 
                                "HRRP Penalty" = "#D73027")) +
  labs(
    title = "Georgia Hospitals: Provider Tax vs HRRP Penalty",
    subtitle = "Average per Hospital",
    x = "Year",
    y = "Amount ($)",
    fill = NULL
  ) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(comparison_plot)

# Save
ggsave("figures/georgia_raw_comparison.png", 
       plot = comparison_plot, 
       width = 8, height = 6, dpi = 300)

# Print actual values
cat("\n=== VALUES IN CHART ===\n")
ga_comparison_long %>%
  pivot_wider(names_from = type, values_from = amount) %>%
  mutate(
    difference = `Provider Tax` - `HRRP Penalty`,
    ratio = `Provider Tax` / `HRRP Penalty`
  ) %>%
  print()

# ==============================================================================
# PERCENT of pre period op exp: AVERAGE BY HOSPITAL-YEAR AND CREATE BAR CHART
# ==============================================================================

# Calculate mean and median by year (shares)
ga_comparison_share <- georgia_tax %>%
  filter(state == "GA", year %in% c(2012, 2022)) %>%
  group_by(year) %>%
  summarise(
    `Provider Tax (Mean)`    = mean(tax_as_per_op_exp,   na.rm = TRUE),
    `Provider Tax (Median)`  = median(tax_as_per_op_exp, na.rm = TRUE),
    `HRRP Penalty (Mean)`    = mean(hrrp_as_per_op_exp[hrrp_as_per_op_exp > 0],   na.rm = TRUE),
    `HRRP Penalty (Median)`  = median(hrrp_as_per_op_exp[hrrp_as_per_op_exp > 0], na.rm = TRUE),
    .groups = "drop"
  )

# Reshape for plotting
ga_comparison_share_long <- ga_comparison_share %>%
  pivot_longer(
    cols      = -year,
    names_to  = "type",
    values_to = "share"
  )

# Plot
comparison_share_plot <- ggplot(ga_comparison_share_long,
                                aes(x = factor(year), y = share, fill = type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c(
    "Provider Tax (Mean)"   = "#2484d2",
    "Provider Tax (Median)" = "#abd9e9",
    "HRRP Penalty (Mean)"   = "#DC4B4B",
    "HRRP Penalty (Median)" = "#fdae61"
  )) +
  labs(
    x    = "Year",
    y    = "Share of Pre-Tax Operating Expenses",
    fill = NULL
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title         = element_text(face = "bold", hjust = 0.5),
    plot.subtitle      = element_text(hjust = 0.5, size = 10),
    legend.position    = "bottom",
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1))

print(comparison_share_plot)

ggsave("figures/georgia_share_comparison.png",
       plot = comparison_share_plot,
       width = 8, height = 6, dpi = 300)


# TRY WITH THE LINE 
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
  # HRRP line labels (right side of each line)
 # HRRP Mean label on the LEFT
# HRRP Mean label on the LEFT
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
    "HRRP Penalty (Mean)"   = "#DC4B4B",
    "HRRP Penalty (Median)" = "#f4a261"),
  guide = "none"
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
  
print(comparison_share_plot)

ggsave("figures/georgia_share_comparison.png",
       plot  = comparison_share_plot,
       width = 16, height = 10, dpi = 300)



# ==============================================================================
# AVERAGE BY HOSPITAL-YEAR WITH MEDIAN AND EXCLUDING ZEROS
# ==============================================================================

library(ggplot2)
library(tidyr)

# Calculate averages AND medians by year, excluding HRRP = 0
ga_comparison <- georgia_tax %>%
  filter(state == "GA", year %in% c(2012, 2022)) %>%
  group_by(year) %>%
  summarise(
    # Tax statistics (all hospitals)
    avg_taxamount = mean(taxamount, na.rm = TRUE),
    median_taxamount = median(taxamount, na.rm = TRUE),
    n_hospitals_tax = sum(!is.na(taxamount)),
    
    # HRRP statistics (excluding zeros)
    avg_hrrp = mean(hrrp_payment[hrrp_payment > 0], na.rm = TRUE),
    median_hrrp = median(hrrp_payment[hrrp_payment > 0], na.rm = TRUE),
    n_hospitals_hrrp = sum(hrrp_payment > 0, na.rm = TRUE),
    n_hospitals_zero_hrrp = sum(hrrp_payment == 0, na.rm = TRUE),
    
    .groups = "drop"
  )

# View the summary
cat("\n=== GEORGIA: TAX vs HRRP COMPARISON ===\n")
print(ga_comparison)

# Reshape for plotting - AVERAGES
ga_comparison_avg <- ga_comparison %>%
  select(year, avg_taxamount, avg_hrrp) %>%
  pivot_longer(
    cols = c(avg_taxamount, avg_hrrp),
    names_to = "type",
    values_to = "amount"
  ) %>%
  mutate(
    type = case_when(
      type == "avg_taxamount" ~ "Provider Tax (Mean)",
      type == "avg_hrrp" ~ "HRRP Penalty (Mean)"
    ),
    stat = "Mean"
  )

# Reshape for plotting - MEDIANS
ga_comparison_med <- ga_comparison %>%
  select(year, median_taxamount, median_hrrp) %>%
  pivot_longer(
    cols = c(median_taxamount, median_hrrp),
    names_to = "type",
    values_to = "amount"
  ) %>%
  mutate(
    type = case_when(
      type == "median_taxamount" ~ "Provider Tax (Median)",
      type == "median_hrrp" ~ "HRRP Penalty (Median)"
    ),
    stat = "Median"
  )

# Combine
ga_comparison_long <- bind_rows(ga_comparison_avg, ga_comparison_med)

# Create bar chart with both mean and median
comparison_plot <- ggplot(ga_comparison_long, 
                          aes(x = factor(year), y = amount, fill = type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c(
    "Provider Tax (Mean)" = "#2484d2",
    "Provider Tax (Median)" = "#abd9e9",
    "HRRP Penalty (Mean)" = "#DC4B4B",
    "HRRP Penalty (Median)" = "#fdae61"
  )) +
  labs(
    # title = "Georgia Hospitals: Provider Tax vs HRRP Penalty",
    # subtitle = "Mean and Median per Hospital (HRRP excludes hospitals with $0 penalty)",
    x = "Year",
    y = "Amount per Hospital ($)",
    fill = NULL
  ) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1))

print(comparison_plot)

ggsave("georgia_tax_hrrp_mean_median.png", 
       plot = comparison_plot, 
       width = 10, height = 7, dpi = 300)

# Alternative: Separate charts for Mean vs Median
comparison_plot_facet <- ggplot(ga_comparison_long, 
                                aes(x = factor(year), y = amount, fill = type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~stat) +
  scale_fill_manual(values = c(
    "Provider Tax (Mean)" = "#2C7BB6",
    "Provider Tax (Median)" = "#2C7BB6",
    "HRRP Penalty (Mean)" = "#D73027",
    "HRRP Penalty (Median)" = "#D73027"
  )) +
  labs(
    #title = "Georgia Hospitals: Provider Tax vs HRRP Penalty",
    #subtitle = "HRRP excludes hospitals with $0 penalty",
    x = "Year",
    y = "Amount per Hospital ($)",
    fill = NULL
  ) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(comparison_plot_facet)

ggsave("figures/georgia_tax_hrrp_facet.png", 
       plot = comparison_plot_facet, 
       width = 10, height = 6, dpi = 300)

# Print detailed statistics
cat("\n=== DETAILED STATISTICS ===\n\n")

for (yr in c(2012, 2019, 2022)) {
  cat(paste("===", yr, "===\n"))
  
  ga_yr <- georgia_tax %>% filter(state == "GA", year == yr)
  
  cat("\nProvider Tax:\n")
  cat(paste("  N hospitals:", sum(!is.na(ga_yr$taxamount)), "\n"))
  cat(paste("  Mean: $", format(round(mean(ga_yr$taxamount, na.rm = TRUE)), big.mark = ","), "\n", sep = ""))
  cat(paste("  Median: $", format(round(median(ga_yr$taxamount, na.rm = TRUE)), big.mark = ","), "\n", sep = ""))
  
  cat("\nHRRP Penalty (excluding $0):\n")
  hrrp_nonzero <- ga_yr$hrrp_payment[ga_yr$hrrp_payment > 0]
  cat(paste("  N hospitals with penalty > $0:", length(hrrp_nonzero), "\n"))
  cat(paste("  N hospitals with $0 penalty:", sum(ga_yr$hrrp_payment == 0, na.rm = TRUE), "\n"))
  cat(paste("  Mean: $", format(round(mean(hrrp_nonzero, na.rm = TRUE)), big.mark = ","), "\n", sep = ""))
  cat(paste("  Median: $", format(round(median(hrrp_nonzero, na.rm = TRUE)), big.mark = ","), "\n", sep = ""))
  
  cat("\n")
}
