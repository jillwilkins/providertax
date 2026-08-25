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


# compare to HRRP penalty amount
georgia_tax <- georgia_tax %>%
  mutate(
hrrp_tax_diff = case_when(
      state == "GA" & year >= 2012 ~ taxamount - hrrp_payment,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()


# pre tax opearting expesnses for comparison 
georgia_tax <- georgia_tax %>%
  group_by(mcrnum) %>%
  mutate(
    pre_tax_op_exp = mean(tot_operating_exp[year < 2011], na.rm = TRUE), 
    hrrp_as_per_op_exp = hrrp_payment / pre_tax_op_exp, 
    tax_as_per_op_exp = taxamount / pre_tax_op_exp 
  ) %>%
  ungroup()

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

# Separate the data
ga_tax_only <- ga_comparison_share_long %>%
  filter(type %in% c("Provider Tax (Mean)", "Provider Tax (Median)"))

ga_hrrp_lines <- ga_comparison_share_long %>%
  filter(type %in% c("HRRP Penalty (Mean)", "HRRP Penalty (Median)"))

# Plot (as seen in figures_used.R)
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

