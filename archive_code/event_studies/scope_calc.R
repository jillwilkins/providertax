# SCOPE & SIZE: Year-1 ATT for op_bed → Total Impact at Median Hospital

# Step 1: Year-1 ATT
year1_att <- coef(results[["op_bed"]]$model)["rel_year::1:treated"]
year1_se  <- results[["op_bed"]]$model$se["rel_year::1:treated"]

# Step 2: Median beds from treated hospitals in stacked_data (pre-treatment)
median_beds <- stacked_data %>%
  filter(treated == 1, rel_year < 0) %>%
  group_by(mcrnum) %>%
  summarise(avg_beds = mean(beds, na.rm = TRUE), .groups = "drop") %>%
  summarise(median_beds = median(avg_beds, na.rm = TRUE)) %>%
  pull(median_beds)

# Step 3: Scale up and print
cat(sprintf(
  "Year-1 ATT: $%.2f/bed | Median beds: %.1f | Total impact: $%s (95%% CI: $%s to $%s)",
  year1_att,
  median_beds,
  formatC(year1_att * median_beds,                        format = "f", digits = 0, big.mark = ","),
  formatC((year1_att - 1.96 * year1_se) * median_beds,   format = "f", digits = 0, big.mark = ","),
  formatC((year1_att + 1.96 * year1_se) * median_beds,   format = "f", digits = 0, big.mark = ",")
), "\n")


# SCOPE & SIZE: Year-1 ATT for npr_bed → Total Impact at Median Hospital

# Step 1: Year-1 ATT
year1_att <- coef(results[["npr_bed"]]$model)["rel_year::2:treated"]
year1_se  <- results[["npr_bed"]]$model$se["rel_year::2:treated"]

# Step 2: Median beds from treated hospitals in stacked_data (pre-treatment)

# Step 3: Scale up and print
cat(sprintf(
  "Year-1 ATT: $%.2f/bed | Median beds: %.1f | Total impact: $%s (95%% CI: $%s to $%s)",
  year1_att,
  median_beds,
  formatC(year1_att * median_beds,                        format = "f", digits = 0, big.mark = ","),
  formatC((year1_att - 1.96 * year1_se) * median_beds,   format = "f", digits = 0, big.mark = ","),
  formatC((year1_att + 1.96 * year1_se) * median_beds,   format = "f", digits = 0, big.mark = ",")
), "\n")

# SCOPE & SIZE: Year-1 ATT for mcaid_prop_discharges → Total Impact at Median Hospital

# Step 1: Year-1 ATT
year1_att <- coef(results[["mcaid_prop_discharges"]]$model)["rel_year::0:treated"]
year1_se  <- results[["mcaid_prop_discharges"]]$model$se["rel_year::0:treated"]

# Step 2: Median beds from treated hospitals in stacked_data (pre-treatment)

# Step 3: Scale up and print
cat(sprintf(
  "Year-1 ATT: %.4f/bed | Median beds: %.1f | Total impact: $%s (95%% CI: $%s to $%s)",
  year1_att,
  median_beds,
  formatC(year1_att * median_beds,                        format = "f", digits = 0, big.mark = ","),
  formatC((year1_att - 1.96 * year1_se) * median_beds,   format = "f", digits = 0, big.mark = ","),
  formatC((year1_att + 1.96 * year1_se) * median_beds,   format = "f", digits = 0, big.mark = ",")
), "\n")

# SCOPE & SIZE: Year-1 ATT for ucc2npr → Total Impact at Median Hospital

# Step 1: Year-1 ATT
year1_att <- coef(results[["ucc2npr"]]$model)["rel_year::3:treated"]
year1_se  <- results[["ucc2npr"]]$model$se["rel_year::3:treated"]

# Step 2: Median beds from treated hospitals in stacked_data (pre-treatment)
results[["ucc2npr"]]$coef_data %>%
  filter(rel_year >= 0) %>%
  summarise(att = mean(coef, na.rm = TRUE))

results[["ucc2npr"]]$coef_data %>%
  filter(rel_year >= 0) %>%
  summarise(
    att  = mean(coef, na.rm = TRUE),
    se   = mean(se, na.rm = TRUE),
    tstat = att / se,
    pval  = 2 * pt(-abs(tstat), df = Inf)
  )

stacked_data %>%
  filter(treated == 1, rel_year < 0) %>%
  summarise(mean_ucc2npr = mean(ucc2npr, na.rm = TRUE))

 results[["uncomp_bed"]]$coef_data %>%
  filter(rel_year >= 0) %>%
  summarise(att = mean(coef, na.rm = TRUE)) 

# mean ucc2npr among treated hospitals in pre-treatment period
mean_ucc2npr <- stacked_data %>%
  filter(treated == 1, rel_year < 0) %>%
  summarise(mean_ucc2npr = mean(ucc2npr, na.rm = TRUE)) %>%
  pull(mean_ucc2npr)

# Step 3: Scale up and print
cat(sprintf(
  "Year-1 ATT: %.4f/npr | Median beds: %.1f, Mean ucc2npr: %.4f | Total impact: %s (95%% CI: $%s to $%s)",
  year1_att,
  median_beds,
  mean_ucc2npr,
  formatC(year1_att * median_beds,                        format = "f", digits = 0, big.mark = ","),
  formatC((year1_att - 1.96 * year1_se) * median_beds,   format = "f", digits = 0, big.mark = ","),
  formatC((year1_att + 1.96 * year1_se) * median_beds,   format = "f", digits = 0, big.mark = ",")
), "\n")

# SCOPE & SIZE: Year-1 ATT for uncomp_bed → Total Impact at Median Hospital
# aggregate ATT ucc2npr 


# Step 1: Year-1 ATT
year1_att <- coef(results[["uncomp_bed"]]$model)["rel_year::2:treated"]
year1_se  <- results[["uncomp_bed"]]$model$se["rel_year::2:treated"]

# Step 2: Median beds from treated hospitals in stacked_data (pre-treatment)

# mean uncomp_bed among treated hospitals in pre-treatment period
mean_uncomp_bed <- stacked_data %>%
  filter(treated == 1, rel_year < 0) %>%
  summarise(mean_uncomp_bed = mean(uncomp_bed, na.rm = TRUE)) %>%
  pull(mean_uncomp_bed)

# median 
median_ucc <- stacked_data %>%
  filter(treated == 1, rel_year < 0) %>%
  summarise(median_ucc = median(ucc2npr, na.rm = TRUE)) %>%
  pull(median_ucc)

median_ucc

# Step 3: Scale up and print
cat(sprintf(
  "Year-1 ATT: %.4f/bed | Median beds: %.1f, Mean uncomp_bed: %.4f | Total impact: $%s (95%% CI: $%s to $%s)",
  year1_att,
  median_beds,
  mean_uncomp_bed,
  formatC(year1_att * median_beds,                        format = "f", digits = 0, big.mark = ","),
  formatC((year1_att - 1.96 * year1_se) * median_beds,   format = "f", digits = 0, big.mark = ","),
  formatC((year1_att + 1.96 * year1_se) * median_beds,   format = "f", digits = 0, big.mark = ",")
), "\n")

# medicaid patients 
# Pull ATT coefficients for each post-treatment year from event study
att_by_year <- results[["mcaid_prop_discharges"]]$coef_data %>%
  filter(rel_year >= 0)

# Average total discharges for treated hospitals by rel_year
avg_discharges <- stacked_data %>%
  filter(treated == 1, rel_year >= 0) %>%
  group_by(rel_year) %>%
  summarise(avg_tot_discharges = mean(tot_discharges, na.rm = TRUE),
            .groups = "drop")

# Join and calculate additional Medicaid patients
att_by_year %>%
  left_join(avg_discharges, by = "rel_year") %>%
  mutate(additional_medicaid_patients = coef * avg_tot_discharges) %>%
  select(rel_year, coef, avg_tot_discharges, additional_medicaid_patients) %>%
  print()

stacked_data %>%
  filter(treated == 1, rel_year < 0) %>%
  mutate(medicaid_share = mcaid_prop_discharges) %>%
  summarise(avg_medicaid_share = mean(medicaid_share, na.rm = TRUE))

stacked_data %>%
  filter(treated == 1, rel_year < 0) %>%
  mutate(medicaid_patients_per_bed = (mcaid_prop_discharges * tot_discharges) / beds) %>%
  summarise(avg_medicaid_patients_per_bed = mean(medicaid_patients_per_bed, na.rm = TRUE))


stacked_data %>%
  filter(treated == 1) %>%
  distinct(mcrnum, in_sys) %>%
  summarise(pct_in_sys = mean(in_sys == 1, na.rm = TRUE) * 100)


# summary for ucc2npr 
results[["ucc2npr"]]$coef_data %>%
  filter(rel_year >= 0) %>%
  summarise(att = mean(coef, na.rm = TRUE))

summary(stacked_data$ucc2npr)
