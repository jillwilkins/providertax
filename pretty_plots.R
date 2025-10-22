
# Medicaid Discharges Proportion 
group_mcaid_dis_prop2 <- ggplot(
  filter(
    hospdata,
    !is.na(mcaid_prop_discharges),
    year < 2020,
    mcaid_prop_discharges >= quantile(mcaid_prop_discharges, 0.01, na.rm = TRUE),
    mcaid_prop_discharges <= quantile(mcaid_prop_discharges, 0.99, na.rm = TRUE)
  ),
  aes(x = year, y = mcaid_prop_discharges, color = factor(treatment_group))
) +
  stat_summary(fun = mean, geom = "line", size = 1.2, aes(group = treatment_group)) +
  stat_summary(fun = mean, geom = "point", size = 2, aes(group = treatment_group)) +
  labs(
    x = "Year",
    y = "Average Medicaid Discharges Proportion",
    color = "Group",
    title = "Average Medicaid Discharges Proportion Over Time by Group"
  ) +
  scale_x_continuous(breaks = seq(2005, 2019, 2), limits = c(2005, 2019)) +
  theme_minimal()

print(group_mcaid_dis_prop2)
ggsave("sumplots/group_mcaid_dis_prop2.png", plot = group_mcaid_dis_prop2, width = 8, height = 8, dpi = 300)

# Privae Payer Proportion of Discharges
group_payermix_2 <- ggplot(
  filter(
    hospdata,
    !is.na(mcaid_prop_discharges),
    year < 2020,
    private_prop_discharges >= quantile(private_prop_discharges, 0.01, na.rm = TRUE),
    private_prop_discharges <= quantile(private_prop_discharges, 0.99, na.rm = TRUE)
  ),
  aes(x = year, y = private_prop_discharges, color = factor(treatment_group))
) +
  stat_summary(fun = mean, geom = "line", size = 1.2, aes(group = treatment_group)) +
  stat_summary(fun = mean, geom = "point", size = 2, aes(group = treatment_group)) +
  labs(
    x = "Year",
    y = "Average Non Public Payer Proportion",
    color = "Group",
    title = "Average Payer Mix Over Time"
  ) +
  scale_x_continuous(breaks = seq(2005, 2019, 2), limits = c(2005, 2019)) +
  theme_minimal()
  print(group_payermix_2)
ggsave("sumplots/group_payermix2.png", plot = group_payermix_2, width = 8, height = 8, dpi = 300)

# Uncompensated Care Charges 
group_ucc_prop2 <- ggplot(
  filter(
    hospdata,
    !is.na(ucc_prop),
    year < 2020,
    ucc_prop >= quantile(ucc_prop, 0.01, na.rm = TRUE),
    ucc_prop <= quantile(ucc_prop, 0.99, na.rm = TRUE)
  ),
  aes(x = year, y = ucc_prop, color = factor(treatment_group))
) +
  stat_summary(fun = mean, geom = "line", size = 1.2, aes(group = treatment_group)) +
  stat_summary(fun = mean, geom = "point", size = 2, aes(group = treatment_group)) +
  labs(
    x = "Year",
    y = "Average Uncompensated Care Charges Proportion",
    color = "Group",
    title = "Average Uncompensated Care Charges Proportion Over Time"
  ) +
  scale_x_continuous(breaks = seq(2011, 2019, 2), limits = c(2011, 2019)) +
  theme_minimal()

ggsave("sumplots/group_ucc_prop2.png", plot = group_ucc_prop2, width = 8, height = 8, dpi = 300)


hospdata_clean %>%
  filter(treatment_group == "not yet by 2020", !is.na(mcaid_prop_discharges), year < 2020, year > 2005) %>%
  summarise(mean_mcaid_prop = mean(mcaid_prop_discharges, na.rm = TRUE))
