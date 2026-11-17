# Meta --------------------------------------------------------------------
## Author:        Jill Wilkins
## Date Created:  10/2/2025 (11/13/25)
## Date Edited:   11/13/2025
## Goal:          Here is the code for the plots used in my third year paper.        
##  



# Single Treatment Cohort 
# Medicaid Discharges Proportion 
group_mcaid_dis_2011 <- ggplot(
  hospdata_clean %>%
    filter(
      !is.na(mcaid_prop_discharges),
      year < 2020,
      mcaid_prop_discharges >= quantile(mcaid_prop_discharges, 0.01, na.rm = TRUE),
      mcaid_prop_discharges <= quantile(mcaid_prop_discharges, 0.99, na.rm = TRUE),
      (treatment_num == 2011 | treatment_group == "not yet by 2020")
    ) %>%
    mutate(group_label = case_when(
      treatment_num == 2011 ~ "Treated in 2011",
      treatment_group == "not yet by 2020" ~ "Not Treated by 2020"
    )), 
    aes(x = year, y = mcaid_prop_discharges, color = group_label, group = group_label)
) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  labs(
    x = "Year",
    y = "Share of Medicaid Discharges",
    color = NULL,  
    title = "Average Share of Medicaid Discharges Over Time"
  ) +
  scale_x_continuous(breaks = seq(2005, 2019, 2), limits = c(2005, 2019)) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(group_mcaid_dis_2011)
ggsave("sumplots/plots2011/mcaid_disclean.png", plot = group_mcaid_dis_2011, width = 10, height = 8, dpi = 300)

# Commercial Proportion 
group_payermix_2011 <- ggplot(
  hospdata_clean %>%
    filter(
      !is.na(private_prop_discharges),
      year < 2020,
      private_prop_discharges >= quantile(private_prop_discharges, 0.01, na.rm = TRUE),
      private_prop_discharges <= quantile(private_prop_discharges, 0.99, na.rm = TRUE),
      (treatment_num == 2011 | treatment_group == "not yet by 2020")
    ) %>%
    mutate(group_label = case_when(
      treatment_num == 2011 ~ "Treated in 2011",
      treatment_group == "not yet by 2020" ~ "Not Treated by 2020"
    )), 
    aes(x = year, y = private_prop_discharges, color = group_label, group = group_label)
) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  labs(
    x = "Year",
    y = "Share of Commercial Discharges",
    color = NULL,  
    title = "Average Share of Commercial Discharges Over Time"
  ) +
  scale_x_continuous(breaks = seq(2005, 2019, 2), limits = c(2005, 2019)) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(group_payermix_2011)
ggsave("sumplots/plots2011/payermixclean.png", plot = group_payermix_2011, width = 10, height = 8, dpi = 300)

# Uncompensated Care (2012)
group_ucc_2012 <- ggplot(
  hospdata_clean %>%
    filter(
      !is.na(ucc_prop),
      year < 2020,
      ucc_prop >= quantile(ucc_prop, 0.01, na.rm = TRUE),
      ucc_prop <= quantile(ucc_prop, 0.99, na.rm = TRUE),
      treatment_num == 2012 | treatment_group == "not yet by 2020"
    ) %>%
    mutate(group_label = case_when(
      treatment_num == 2012 ~ "Treated in 2012",
      treatment_group == "not yet by 2020" ~ "Not Treated by 2020"
    )),
  aes(x = year, y = ucc_prop, color = group_label, group = group_label)
) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  labs(
    x = "Year",
    y = "Share of Uncompensated Care",
    color = NULL,  
    title = "Average Share of Uncompensated Care Over Time"
  ) +
  scale_x_continuous(breaks = seq(2011, 2019, 2), limits = c(2011, 2019)) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(group_ucc_2012)
ggsave("sumplots/plots2011/uccclean.png", plot = group_ucc_2012, width = 10, height = 8, dpi = 300)



# All Treatment Cohorts 
# Medicaid Discharges Proportion 
group_mcaid_dis <- ggplot(
  filter(
    hospdata_clean,
    !is.na(mcaid_prop_discharges),
    year < 2020,
    treatment_group != "always",
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

print(group_mcaid_dis)
ggsave("sumplots/group_mcaid_dis.png", plot = group_mcaid_dis, width = 10, height = 8, dpi = 300)

# Commercial Proportion
group_payermix <- ggplot(
  filter(
    hospdata_clean,
    treatment_group != "always",
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
    y = "Average Commercial Proportion",
    color = "Group",
    title = "Average Payer Mix Over Time"
  ) +
  scale_x_continuous(breaks = seq(2005, 2019, 2), limits = c(2005, 2019)) +
  theme_minimal()
  print(group_payermix)
ggsave("sumplots/group_payermix.png", plot = group_payermix, width = 80, height = 8, dpi = 300)

# Uncompensated Care Proportion 
group_ucc_prop <- ggplot(
  hospdata_clean %>%
    filter(
      !is.na(ucc_prop),
      year < 2020,
      ucc_prop >= quantile(ucc_prop, 0.01, na.rm = TRUE),
      ucc_prop <= quantile(ucc_prop, 0.99, na.rm = TRUE),
      !(treatment_group == "always")
    ),
  aes(x = year, y = ucc_prop, color = treatment_group, group = treatment_group)
) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  labs(
    x = "Year",
    y = "Average Uncompensated Care Charges ",
    color = "Group",
    title = "Average Uncompensated Care Charges Over Time"
  ) +
  scale_x_continuous(breaks = seq(2011, 2019, 2), limits = c(2011, 2019)) +
  theme_minimal()

print(group_ucc_prop)
ggsave("sumplots/group_ucc_prop.png", plot = group_ucc_prop, width = 10, height = 8, dpi = 300)

# net patient revenue 
group_npr <- ggplot(
  filter(
    hospdata_clean,
    !is.na(net_pat_rev),
    year < 2020,
    treatment_group != "always",
   net_pat_rev >= quantile(net_pat_rev, 0.01, na.rm = TRUE),
    net_pat_rev <= quantile(net_pat_rev, 0.99, na.rm = TRUE)
  ),
  aes(x = year, y = net_pat_rev, color = factor(treatment_group))
) +
  stat_summary(fun = mean, geom = "line", size = 1.2, aes(group = treatment_group)) +
  stat_summary(fun = mean, geom = "point", size = 2, aes(group = treatment_group)) +
  labs(
    x = "Year",
    y = "Average Net Patient Revenue",
    color = "Group",
    title = "Average Net Patient Over Time by Group"
  ) +
  scale_x_continuous(breaks = seq(2005, 2019, 2), limits = c(2005, 2019)) +
  theme_minimal()

print(group_npr)

pre_96 <- hospdata_clean %>%
  filter(year < 2010) %>%
  select(net_pat_rev, mcrnum, year)
View(pre_96)
