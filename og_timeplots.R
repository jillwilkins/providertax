# Meta --------------------------------------------------------------------
## Author:        Jill Wilkins
## Date Created:  10/2/2025
## Date Edited:   11/13/2025
## Goal:          Figures that show relationship of tax adoption and outcome variable        
##  

install.packages("panelView")
library(panelView)
library(ggplot2)


# FMAP and TAX ----------------------------------------------------------------
# figure of tax adoption over time
plot1data <- hospdata %>%
  filter(!is.na(totaltax)) %>%
  group_by(year) %>%
  summarise(
    avg_fmap = mean(fmap, na.rm = TRUE),
    totaltax = first(totaltax),  # same value for all hospitals that year
    .groups = "drop"
  )

scale <- max(plot1data$totaltax) / max(plot1data$avg_fmap)
 
taxfmapcorr <- ggplot(plot1data, aes(x = year)) +
  # Left axis: number of states with tax
  geom_line(aes(y = totaltax), color = "red", size = 1.2) +
  geom_point(aes(y = totaltax), color = "red", size = 2) +
  # Right axis: average FMAP (scaled)
  geom_line(aes(y = avg_fmap * scale), color = "blue", size = 1.2) +
  geom_point(aes(y = avg_fmap * scale), color = "blue", size = 2) +
  scale_y_continuous(
    name = "Number of States with Tax",
    sec.axis = sec_axis(~ . / scale, name = "Average FMAP")
  ) +
  labs(
    x = "Year",
    title = "States with Tax vs Average FMAP Over Time"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = "red", size = 12),
    axis.text.y.left = element_text(color = "red"),
    axis.title.y.right = element_text(color = "blue", size = 12),
    axis.text.y.right = element_text(color = "blue"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
  )

setwd("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/providertax")
ggsave("sumplots/taxfmapcorr.png", plot = taxfmapcorr, width = 8, height = 8, dpi = 300)

# correlations 
fmap_tax <- fmap_tax %>%
  mutate(yes_adopt = ifelse(!is.na(firsttax) & year >= firsttax, 1, 0))
state_corr <- fmap_tax %>%
  group_by(state) %>%
  summarise(
    correlation = cor(fmap, yes_adopt, use = "complete.obs")
  )
# confirm that fmap does not fully explain tax adoption
# ----------------------------------------------------------------
summary(hospdata$mcaid_prop_discharges)

# payer mix over time plot
#note: ill wnat to figure out how to remove outliers here
ggplot(filter(hospdata),aes(x = year, y = private_prop_discharges)) +
  stat_summary(
    fun = mean, geom = "line", color = "blue", size = 1.2) +
  stat_summary(
    fun = mean, geom = "point", color = "blue", size = 2) +
  labs(
    x = "Year",
    y = "Average Non Public Payer Proportion",
    title = "Average Non Public Payer % Over Time"
  ) +
  theme_minimal()


# dual axis: Mcaid Charges prop and total states with tax
plot2 <- hospdata %>%
  filter(!is.na(mcaid_prop)) %>%
  group_by(year) %>%
  summarise(
    avg_mcaid_prop = mean(mcaid_prop, na.rm = TRUE),
    totaltax = first(totaltax[!is.na(totaltax)]),
    .groups = "drop"
  )

scale_factor <- max(plot2$avg_mcaid_prop, na.rm = TRUE) / max(plot2$totaltax, na.rm = TRUE)

mcaidpropplot <- ggplot(plot2, aes(x = year)) +
  geom_line(aes(y = avg_mcaid_prop), color = "blue", size = 1) +
  geom_line(aes(y = totaltax * scale_factor), color = "red", size = 1) +
  scale_y_continuous(
    name = "Average Proportion of Medicaid Charges",
    sec.axis = sec_axis(~ . / scale_factor, name = "Number of States Adopted Tax")
  ) +
   labs(title = "Average Medicaid Charges and State Tax Adoption Over Time") +
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red")
  )

ggsave("sumplots/mcaidpropplot.png", plot = mcaidpropplot, width = 8, height = 8, dpi = 300)


# dual axis: Mcaid Discharges prop and total states with tax
plot3 <- hospdata %>%
  filter(!is.na(mcaid_prop_discharges)) %>%
  group_by(year) %>%
  summarise(
    avg_mcaid_dis = mean(mcaid_prop_discharges, na.rm = TRUE),
    totaltax = first(totaltax[!is.na(totaltax)]),
    .groups = "drop"
  )

scale3 <- max(plot3$avg_mcaid_dis, na.rm = TRUE) / max(plot3$totaltax, na.rm = TRUE)

mcaiddispropplot <- ggplot(plot3, aes(x = year)) +
  geom_line(aes(y = avg_mcaid_dis), color = "blue", size = 1) +
  geom_line(aes(y = totaltax * scale3), color = "red", size = 1) +
  scale_y_continuous(
    name = "Average Medicaid Discharges Proportion",
    sec.axis = sec_axis(~ . / scale3, name = "Number of States Adopted Tax")
  ) +
  labs(title = "Average Medicaid Discharges and State Tax Adoption Over Time") +
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red")
  )

ggsave("sumplots/mcaiddispropplot.png", plot = mcaiddispropplot, width = 8, height = 8, dpi = 300)

# dual axis: uncompensated care charges and total states with tax
plot4 <- hospdata %>%
  filter(!is.na(tot_uncomp_care_charges) & year < 2025 & year >= 2011) %>%
  group_by(year) %>%
  summarise(
    avg_uc_charges = mean(tot_uncomp_care_charges, na.rm = TRUE),
    totaltax = first(totaltax[!is.na(totaltax)]),
    .groups = "drop"
  )

scale4 <- max(plot4$avg_uc_charges, na.rm = TRUE) / max(plot4$totaltax, na.rm = TRUE)

post_uncompplot <- ggplot(plot4, aes(x = year)) +
  geom_line(aes(y = avg_uc_charges), color = "blue", size = 1) +
  geom_line(aes(y = totaltax * scale4), color = "red", size = 1) +
  scale_y_continuous(
    name = "Average Uncompensated Care Charges ",
    sec.axis = sec_axis(~ . / scale4, name = "Number of States Adopted Tax")
  ) +
  theme_minimal() +
  labs(title = "Post 2011: Average Uncompensated Care Charges and State Tax Adoption Over Time") +
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red")
  )

ggsave("sumplots/post_uncompplot.png", plot = post_uncompplot, width = 8, height = 8, dpi = 300)

# dual axis: cost to charge ratio and total states with tax
plot5 <- hospdata %>%
  filter(!is.na(cost_to_charge)) %>%
  group_by(year) %>%
  summarise(
    avg_ccr = mean(cost_to_charge, na.rm = TRUE),
    totaltax = first(totaltax[!is.na(totaltax)]),
    .groups = "drop"
  )

scale5 <- max(plot5$avg_ccr, na.rm = TRUE) / max(plot5$totaltax, na.rm = TRUE)

ccrplot <- ggplot(plot5, aes(x = year)) +
  geom_line(aes(y = avg_ccr), color = "blue", size = 1) +
  geom_line(aes(y = totaltax * scale5), color = "red", size = 1) +
  scale_y_continuous(
    name = "Average Cost to Charge Ratio",
    sec.axis = sec_axis(~ . / scale5, name = "Number of States Adopted Tax")
  ) +
  labs(title = "Average Cost to Charge Ratio and State Tax Adoption Over Time") +
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red")
  )
ggsave("sumplots/ccrplot.png", plot = ccrplot, width = 8, height = 8, dpi = 300)

# dual axis: uncompensated care charges to total charges and total states w tax
plot6 <- hospdata %>%
  filter(!is.na(tot_uncomp_care_charges) & tot_uncomp_care_charges > 0) %>%
  group_by(year) %>%
  summarise(
    avg_ucc_tc = mean((tot_uncomp_care_charges / tot_charges), na.rm = TRUE),
    totaltax = first(totaltax[!is.na(totaltax)]),
    .groups = "drop"
  )

scale6 <- max(plot6$avg_ucc_tc, na.rm = TRUE) / max(plot6$totaltax, na.rm = TRUE)

ucctcplot <- ggplot(plot6, aes(x = year)) +
  geom_line(aes(y = avg_ucc_tc), color = "blue", size = 1) +
  geom_line(aes(y = totaltax * scale6), color = "red", size = 1) +
  scale_y_continuous(
    name = "Average UCC to Total Charges",
    sec.axis = sec_axis(~ . / scale6, name = "Number of States Adopted Tax")
  ) +
  labs(title = "Average UCC to Total Charges and State Tax Adoption Over Time") +
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red")
  )
ggsave("sumplots/ucctcplot.png", plot = ucctcplot, width = 8, height = 8, dpi = 300)


# ----------------------------------------------------------------
# Plots but by treatment status
# ----------------------------------------------------------------
# mcaid charge proportion 
group_mcaid_prop <- ggplot(
  filter(hospdata, !is.na(mcaid_prop)),
  aes(x = year, y = mcaid_prop, color = factor(treatment_group))
) +
  stat_summary(fun = mean, geom = "line", size = 1.2, aes(group = treatment_group)) +
  stat_summary(fun = mean, geom = "point", size = 2, aes(group = treatment_group)) +
  labs(
    x = "Year",
    y = "Average Medicaid Charges Proportion",
    color = "Group",
    title = "Average Medicaid Charges Proportion Over Time by Group"
  ) +
  theme_minimal()

ggsave("sumplots/group_mcaid_prop.png", plot = group_mcaid_prop, width = 8, height = 8, dpi = 300)

# mcaid discharges proportion
group_mcaid_dis_prop <- ggplot(
  filter(hospdata, !is.na(mcaid_prop_discharges), year < 2020),
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
  theme_minimal()

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

# uncompensated care charges proportion
group_ucc_prop <- ggplot(
  filter(hospdata, !is.na(ucc_prop) & ucc_prop < 1, year < 2020),
  aes(x = year, y = ucc_prop, color = factor(treatment_group))
) +
  stat_summary(fun = mean, geom = "line", size = 1.2, aes(group = treatment_group)) +
  stat_summary(fun = mean, geom = "point", size = 2, aes(group = treatment_group)) +
  labs(
    x = "Year",
    y = "Average Uncompensated Care Charges Proportion",
    color = "Group",
    title = "Average Uncompensated Care to Total Charges Over Time by Group"
  ) +
  theme_minimal()
print(group_ucc_prop2)

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

# cost to charge ratio
group_ccr <- ggplot(
  filter(hospdata, !is.na(cost_to_charge) & cost_to_charge < 10),
  aes(x = year, y = cost_to_charge, color = factor(treatment_group))
) +
  stat_summary(fun = mean, geom = "line", size = 1.2, aes(group = treatment_group)) +
  stat_summary(fun = mean, geom = "point", size = 2, aes(group = treatment_group)) +
  labs(
    x = "Year",
    y = "Average Cost to Charge Ratio",
    color = "Group",
    title = "Average Cost to Charge Ratio Over Time by Group"
  ) +
  theme_minimal()

ggsave("sumplots/group_ccr.png", plot = group_ccr, width = 8, height = 8, dpi = 300)


# cost per discharge 
tmnt_cost_per_discharge <- ggplot(
  filter(hospdata, !is.na(cost_per_discharge) & cost_per_discharge < 100000),
  aes(x = year, y = cost_per_discharge, color = factor(treatment_group))
) +
  stat_summary(fun = mean, geom = "line", size = 1.2, aes(group = treatment_group)) +
  stat_summary(fun = mean, geom = "point", size = 2, aes(group = treatment_group)) +
  labs(
    x = "Year",
    y = "Average Cost per Discharge",
    color = "Group",
    title = "Average Cost per Discharge Over Time by Group"
  ) +
  theme_minimal()

ggsave("sumplots/tmnt_cost_per_discharge.png", plot = tmnt_cost_per_discharge, width = 8, height = 8, dpi = 300)

unique(hospdata$treatment_group)

# private (proxy) 
group_payermix <- ggplot(
  filter(hospdata, !is.na(mm_prop_discharges), year < 2020),
  aes(x = year, y = private_prop_discharges, color = factor(treatment_group))
) +
  stat_summary(fun = mean, geom = "line", size = 1.2, aes(group = treatment_group)) +
  stat_summary(fun = mean, geom = "point", size = 2, aes(group = treatment_group)) +
  labs(
    x = "Year",
    y = "Average Proportion of Non Public Payers",
    color = "Group",
    title = "Average Case Mix Over Tim"
  ) +
  theme_minimal()
print(group_payermix)

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

summary(hospdata$private_prop_discharges)
unique(hospdata$treatment_group)

# Private for EMORY system only 
library(stringr)
group_payermix_emory <- ggplot(
  filter(
    hospdata,
    str_detect(name, regex("Emory", ignore_case = TRUE)),  # only hospitals with "EMORY"
    !is.na(private_prop_discharges),
    year < 2020
  ),
  aes(x = year, y = private_prop_discharges, color = factor(treatment_group))
) +
  stat_summary(fun = mean, geom = "line", size = 1.2, aes(group = treatment_group)) +
  stat_summary(fun = mean, geom = "point", size = 2, aes(group = treatment_group)) +
  labs(
    x = "Year",
    y = "Average Proportion of Non-Public Payers",
    color = "Group",
    title = "Average Case Mix Over Time: EMORY Hospitals"
  ) +
  scale_x_continuous(breaks = seq(2005, 2019, 2), limits = c(2005, 2019)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.text = element_text(size = 12, color = "black"),
    legend.position = "bottom"
  )

print(group_payermix_emory)

# ACROSS AND WITHIN VARIATION 
# DENSITY PLOTS
dens_npr <- ggplot(filter(hospdata_clean, treatment_group == "treated", year < 2020, net_pat_rev > 0), aes(x = net_pat_rev)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  labs(title = "Density of Net Patient Revenue",
       x = "Net Patient Revenue", y = "Density") +
  theme_minimal()
print(dens_npr)

dens_mcaid_dis <- ggplot(filter(hospdata_clean, treatment_group == "treated", year < 2020), aes(x = mcaid_prop_discharges)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  labs(title = "Density of Medicaid Proportion of Discharges",
       x = "Medicaid Proportion of Discharges", y = "Density") +
  theme_minimal()
print(dens_mcaid_dis)

dens_casemix <- ggplot(filter(hospdata_clean, treatment_group == "not yet by 2020", year < 2020), aes(x = private_prop_discharges)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  labs(title = "Density of Non Public Proportion of Discharges",
       x = "Non Public Proportion of Discharges", y = "Density") +
  theme_minimal()
print(dens_casemix)

dens_uccprop <- ggplot(filter(hospdata, treatment_group == "not yet by 2020", year < 2020, ucc_prop >= 0), aes(x = ucc_prop)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  labs(title = "Density of Uncompensated Care Proportion",
       x = "Uncompensated Care Proportion of Charges", y = "Density") +
  theme_minimal()
print(dens_uccprop)

dens_taxyr <- ggplot(filter(hospdata, treatment_group == "treated", year < 2020), aes(x = event_time)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  labs(title = "Density of Years Since Tax Adoption",
       x = "Years Since Tax Adoption", y = "Density") +
  theme_minimal()
print(dens_taxyr)

dens_ccr <- ggplot(filter(hospdata, treatment_group == "not yet by 2020", year < 2020, cost_to_charge <= 2), aes(x = cost_to_charge)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  labs(title = "Density of Cost to Charge Ratio",
       x = "Cost to Charge Ratio", y = "Density") +
  theme_minimal()
print(dens_ccr)

dens_beds <- ggplot(filter(hospdata, treatment_group == "not yet by 2020", year < 2020), aes(x = beds)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  labs(title = "Density of Number of Beds",
       x = "Number of Beds", y = "Density") +
  theme_minimal()
print(dens_beds)

# hist version 
dens_taxyr_hist <- ggplot(
  filter(hospdata, treatment_group == "treated", year < 2020), 
  aes(x = event_time)
) +
  geom_histogram(
    binwidth = 1,             # width of each bin; adjust as needed
    fill = "steelblue", 
    color = "black",          # adds a border to each bar
    alpha = 0.6
  ) +
  labs(
    title = "Histogram of Years Since Tax Adoption",
    x = "Years Since Tax Adoption", 
    y = "Count"
  ) +
  theme_minimal()
dens_taxyr_hist

 cor(
  hospdata$rural,
  hospdata$yes_tax,
  use = "complete.obs"   # ignores NAs
)
# Count of rural vs non-rural hospitals in 2013
hospdata %>%
  filter(year == 2013) %>%
  count(rural)

payer_did %>%
  group_by(treatment_num) %>%
  summarise(
    rural_var = var(rural, na.rm = TRUE))

hospdata %>%
  filter(treatment_num == 2013, rural == 1) %>%
  summarise(n_rural0 = n())

hospdata %>%
  filter(firsttax == 2006) %>%
  pull(state) %>%
  unique()


# prob of having alchhos 
group_psyemhos <- ggplot(
  filter(hospdata, !is.na(psyemhos), year < 2020, rural == 1),
  aes(x = year, y = psyemhos, color = factor(treatment_group), 
  (treatment_num == 2011 | treatment_group == "not yet by 2020"))
) +
  stat_summary(fun = mean, geom = "line", size = 1.2, aes(group = treatment_group)) +
  stat_summary(fun = mean, geom = "point", size = 2, aes(group = treatment_group)) +
  labs(
    x = "Year",
    y = "Average Probability of Having Psychiatric Emergency Services",
    color = "Group",
    title = "Average Probability of Having Psychiatric Emergency Services Over Time by Group"
  ) +
  theme_minimal()
print(group_psyemhos)

group_psyemhos_2011 <- ggplot(
  hospdata_st %>%
    filter(
      !is.na(psyemhos),
      year < 2020,
      (treatment_num == 2011 | treatment_group == "not yet by 2020")
    ),
  aes(x = year, y = psyemhos, color = treatment_group, group = treatment_group)
) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  labs(
    x = "Year",
    y = "Share of Psychiatric Emergency Services",
    color = "Group",
    title = "Average Share of Psychiatric Emergency Services Over Time"
  ) +
  scale_x_continuous(breaks = seq(2005, 2019, 2), limits = c(2005, 2019)) +
  theme_minimal()
print(group_psyemhos_2011)
ggsave("sumplots/group_psyemhos_2011.png", plot = group_psyemhos_2011, width = 8, height = 8, dpi = 300)

# prob of having alchhosp 
group_alchhos <- ggplot(
  filter(hospdata, !is.na(alchhos), year < 2020),
  aes(x = year, y = alchhos, color = factor(treatment_group))
) +
  stat_summary(fun = mean, geom = "line", size = 1.2, aes(group = treatment_group)) +
  stat_summary(fun = mean, geom = "point", size = 2, aes(group = treatment_group)) +
  labs(
    x = "Year",
    y = "Average Probability of Having Alcohol and Drug Services",
    color = "Group",
    title = "Average Probability of Having Alcohol and Drug Services Over Time by Group"
  ) +
  theme_minimal()
print(group_alchhos)

group_alchhos_2011 <- ggplot(
  hospdata %>%
    filter(
      !is.na(alchhos),
      year < 2020,
      (treatment_num == 2011 | treatment_group == "not yet by 2020")
    ),
  aes(x = year, y = alchhos, color = treatment_group, group = treatment_group)
) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  labs(
    x = "Year",
    y = "Share of Alcohol and Drug Services",
    color = "Group",
    title = "Average Share of Alcohol and Drug Services Over Time"
  ) +
  scale_x_continuous(breaks = seq(2005, 2019, 2), limits = c(2005, 2019)) +
  theme_minimal()
print(group_alchhos_2011)
ggsave("sumplots/group_alchhos_2011.png", plot = group_alchhos_2011, width = 8, height = 8, dpi = 300)

# bar style 
# Summarize shares by year, service, and treatment group
bar_data <- hospdata %>%
  filter(year %in% c(2005, 2010, 2015, 2019)) %>%  # pick key years
  group_by(year, treatment_group) %>%
  summarise(
    alc_drug_share = mean(alchhos == 1, na.rm = TRUE),
    psyem_share = mean(psyemhos == 1, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(alc_drug_share, psyem_share),
               names_to = "service", values_to = "share")

# Plot
ggplot(bar_data, aes(x = factor(year), y = share, fill = treatment_group)) +
  geom_col(position = "dodge") +
  facet_wrap(~ service, ncol = 1) +  # separate panels for each service
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Year",
    y = "Share of Hospitals",
    fill = "Treatment Group",
    title = "Share of Hospitals Offering ALC/Drug or PSYEM Services by Group"
  ) +
  theme_minimal(base_size = 12)


unique(hospdata$state[hospdata$firsttax == 2019])

# number of ob beds
group_ob_beds <- ggplot(
  filter(hospdata, !is.na(obbd), year < 2020, treatment_group != "always"),
  aes(x = year, y = obbd / hospbd, color = factor(treatment_group))
) +
  stat_summary(fun = mean, geom = "line", size = 1.2, aes(group = treatment_group)) +
  stat_summary(fun = mean, geom = "point", size = 2, aes(group = treatment_group)) +
  labs(
    x = "Year",
    y = "Average Number of OB Beds",
    color = "Group",
    title = "Average Number of OB Beds Over Time by Group"
  ) +
  theme_minimal()
print(group_ob_beds)

group_ob_2011 <- ggplot(
  hospdata %>%
    filter(
      !is.na(obbd),
      year < 2020,
      (treatment_num == 2011 | treatment_group == "not yet by 2020")
    ),
  aes(x = year, y = obbd / hospbd, color = treatment_group, group = treatment_group)
) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  labs(
    x = "Year",
    y = "Number of OB Beds",
    color = "Group",
    title = "Average Number of OB Beds Over Time"
  ) +
  scale_x_continuous(breaks = seq(2005, 2019, 2), limits = c(2005, 2019)) +
  theme_minimal()
print(group_ob_2011)
