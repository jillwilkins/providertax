# Meta --------------------------------------------------------------------
## Author:        Jill Wilkins
## Date Created:  10/2/2025
## Date Edited:   10/8/2025
## Goal:          Figures that show relationship of tax adoption and outcome variable        
##  

library(ggplot2)

# FMAP and TAX ----------------------------------------------------------------
# figure of tax adoption over time
plot1data <- hospdata %>%
  filter(year > 2003 & year < 2024, !is.na(totaltax)) %>%
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


# Tax adoption and payer mix 
hospdata <- hospdata %>%
  mutate(mcaid_prop = mcaid_charges / tot_charges) %>%
  mutate(mcaid_prop_discharges = mcaid_discharges / tot_discharges)

summary(hospdata$mcaid_prop_discharges)

# payer mix over time plot
#note: ill wnat to figure out how to remove outliers here
ggplot(filter(hospdata, !is.na(mcaid_prop) & mcaid_prop < 1),aes(x = year, y = mcaid_prop)) +
  stat_summary(
    fun = mean, geom = "line", color = "blue", size = 1.2) +
  stat_summary(
    fun = mean, geom = "point", color = "blue", size = 2) +
  labs(
    x = "Year",
    y = "Average Medicaid Proportion",
    title = "Average Medicaid Proportion Over Time"
  ) +
  theme_minimal()


# dual axis: Mcaid Charges prop and total states with tax
plot2 <- hospdata %>%
  filter(!is.na(mcaid_prop) & mcaid_prop < 1 & year < 2025 & year > 2003 ) %>%
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
  filter(!is.na(mcaid_prop_discharges) & mcaid_prop_discharges < 1 & year < 2025 & year > 2003) %>%
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
  filter(!is.na(tot_uncomp_care_charges) & year < 2025 & year > 2011) %>%
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
  filter(!is.na(cost_to_charge) & cost_to_charge < 10 & year < 2025 & year > 2003) %>%
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

# dual axis: uncompensated care charges to gross patient revenue and total states w tax 
colnames(hospdata)
plot6 <- hospdata %>%
  filter(!is.na(tot_uncomp_care_charges) & tot_uncomp_care_charges > 0 & !is.na(tot_charges) & year < 2025 & year > 2011) %>%
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

# Pre Post Adoption Averages --------------------------------------------------------------