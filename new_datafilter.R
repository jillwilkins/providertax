# ==============================================================================
# Check outcome variables: ucc_prop, mcaid_prop_discharges, net_pat_rev, tot_operating_exp
# ==============================================================================

#load in hopsdata_analysis
hospdata_analysis <- read.csv(paste0(data_output_path, "hospdata_analysis.csv"))

# 1. Summary statistics
summary(hospdata_analysis[, c("ucc_prop", "mcaid_prop_discharges", "net_pat_rev", "tot_operating_exp")])


# 2. Check for impossible/problematic values
hospdata_analysis %>%
  summarise(
    # ucc_prop
    ucc_over_1 = sum(ucc_prop > 1, na.rm = TRUE),
    ucc_negative = sum(ucc_prop < 0, na.rm = TRUE),
    ucc_inf = sum(is.infinite(ucc_prop), na.rm = TRUE),
    ucc_na = sum(is.na(ucc_prop)),
    
    # mcaid_prop_discharges
    mcaid_over_1 = sum(mcaid_prop_discharges > 1, na.rm = TRUE),
    mcaid_negative = sum(mcaid_prop_discharges < 0, na.rm = TRUE),
    mcaid_inf = sum(is.infinite(mcaid_prop_discharges), na.rm = TRUE),
    mcaid_na = sum(is.na(mcaid_prop_discharges)),
    
    # tot_operating_exp
    tot_operating_exp_negative = sum(tot_operating_exp < 0, na.rm = TRUE),
    tot_operating_exp_inf = sum(is.infinite(tot_operating_exp), na.rm = TRUE),
    tot_operating_exp_na = sum(is.na(tot_operating_exp)),

    # net_pat_rev
    netrev_negative = sum(net_pat_rev < 0, na.rm = TRUE),
    netrev_inf = sum(is.infinite(net_pat_rev), na.rm = TRUE),
    netrev_na = sum(is.na(net_pat_rev))
  )

# 3. Top 10 extreme values for each
hospdata_analysis %>%
  arrange(desc(ucc_prop)) %>%
  select(mcrnum, state, year, ucc_prop) %>%
  head(10)

hospdata_analysis %>%
  arrange(desc(mcaid_prop_discharges)) %>%
  select(mcrnum, state, year, mcaid_prop_discharges) %>%
  head(50)

hospdata_analysis %>%
  arrange(net_pat_rev) %>%  # Lowest (most negative)
  select(mcrnum, state, year, net_pat_rev) %>%
  head(10)

hospdata_analysis %>%
  arrange(desc(tot_operating_exp)) %>%  # Highest
  select(mcrnum, state, year, tot_operating_exp) %>%
  head(10)

# 4. Missing data by treatment status
missing_pct <- hospdata_analysis %>%
  group_by(post_treat) %>%
  summarise(
    n = n(),
    pct_missing_ucc = round(mean(is.na(ucc_prop)) * 100, 1),
    pct_missing_mcaid = round(mean(is.na(mcaid_prop_discharges)) * 100, 1),
    pct_missing_netrev = round(mean(is.na(net_pat_rev)) * 100, 1),
    pct_missing_tot_operating_exp = round(mean(is.na(tot_operating_exp)) * 100, 1)
  )
View(missing_pct)

# ==============================================================================
# ucc_prop investigation 
# ==============================================================================
# How many negative ucc_prop values?
sum(hospdata_analysis$ucc_prop < 0, na.rm = TRUE)

# Look at the negative ucc_prop observations
hospdata_analysis %>%
  filter(ucc_prop < 0) %>%
  select(mcrnum, state, year, ucc_prop, tot_uncomp_care_charges, tot_charges) %>%
  arrange(ucc_prop) %>%
  head(20)

# Set ucc_prop to NA if either component is negative or zero
hospdata_analysis <- hospdata_analysis %>%
  mutate(
    ucc_prop = case_when(
      tot_charges <= 0 ~ NA_real_,
      tot_uncomp_care_charges < 0 ~ NA_real_,
      TRUE ~ ucc_prop
    )
  )

# ==============================================================================
# net_pat_rev investigation 
# ==============================================================================
# Look at extremely negative net_pat_rev
hospdata_analysis %>%
  filter(net_pat_rev < 100000) %>%  # Less than -100k
  select(mcrnum, state, year, net_pat_rev, beds, tot_discharges) %>%
  arrange(net_pat_rev)

# drop unrealistic values 
hospdata_analysis <- hospdata_analysis %>%
  filter(net_pat_rev >= 100000)  

# Look at extremely high net_pat_rev
# 2/23 didnt do anything here, not sure if i should? 
hospdata_analysis %>%
  filter(net_pat_rev > 5e9) %>%  # More than $5 billion
  select(mcrnum, state, year, net_pat_rev, beds, tot_discharges) %>%
  arrange(desc(net_pat_rev))

# Calculate revenue per discharge for the high-revenue hospitals
hospdata_analysis %>%
  filter(net_pat_rev > 5e9) %>%
  mutate(rev_per_discharge = net_pat_rev / tot_discharges) %>%
  select(mcrnum, state, year, net_pat_rev, beds, tot_discharges, rev_per_discharge) %>%
  arrange(desc(rev_per_discharge))

# Look at all years for the suspicious hospitals (CA and OH especially)
suspicious_mcrnums <- c("50327", "50441", "50454", "360180", "100007", "390164", "100062")

hospdata_analysis %>%
  filter(mcrnum %in% suspicious_mcrnums) %>%
  mutate(rev_per_discharge = net_pat_rev / tot_discharges) %>%
  select(mcrnum, state, year, name, net_pat_rev, beds, tot_discharges, rev_per_discharge) %>%
  arrange(mcrnum, year) 

# The large positive values are consistent across the years for a large medical center. Leave them for now, 2/16
# Check treatment status for the three problematic observations
hospdata_analysis %>%
  filter(
    (mcrnum == "50327" & year == 2014) |
    (mcrnum == "390164" & year == 2010) |
    (mcrnum == "100062" & year == 2013)
  ) %>%
  select(mcrnum, state, year, name, post_treat, cohort, gname, time_to_treat)

# Look at extremely high tot_operating_exp
hospdata_analysis %>%
  filter(tot_operating_exp > 5e9) %>%  # More than $5 billion
  select(mcrnum, state, year, tot_operating_exp, beds, tot_discharges) %>%
  arrange(desc(tot_operating_exp))

# cost per discharge investigation 
summary(hospdata_analysis$tot_discharges)

quantile(hospdata_analysis$tot_discharges, 
         c(0.01, 0.05, 0.10, 0.50, 0.90, 0.95, 0.99, 0.995, 0.999), 
         na.rm = TRUE)

hospdata_analysis %>%
  filter(cost_per_discharge > 1e6) %>%  # More than $1 million per discharge
  select(mcrnum, state, year, cost_per_discharge, beds, tot_discharges) %>%
  arrange(desc(cost_per_discharge))

# charges investigation 
summary(hospdata_analysis$mcaid_charges)
summary(hospdata_analysis$tot_charges)
summary(hospdata_analysis$cost_to_charge)
quantile(hospdata_analysis$cost_to_charge, 
         c(0.01, 0.05, 0.10, 0.50, 0.90, 0.95, 0.99, 0.995, 0.999), 
         na.rm = TRUE)
summary(hospdata_analysis$net_pat_rev)

