# This code is an improvement of hosp_taxamounts.R, which was the original code to analyze tax amounts.
# The goal is to create a table that shows the extent of the tax as a financial burden 
 
# HOSPITAL PROVIDER TAX AMOUNT 
# Step 0: Run hosp_taxamounts.R 
# Step 0a: Patch Ohio's tax_amount_total using tot_operating_exp
hosp_taxamounts <- hosp_taxamounts %>%
  mutate(
    tax_amount_total = case_when(
      state == "OH" ~ tot_operating_exp * (3.1965 / 100),
      TRUE          ~ tax_amount_total
    )
  )

# Step 1: Add tax_as_npr_hy to hosp_taxamounts
hosp_taxamounts <- hosp_taxamounts %>%
  mutate(
    effective_rate = case_when(
      state %in% c("CT", "MA", "NV") ~ (tax_rate + tax_rate2) / 2,
      TRUE                            ~ tax_rate
    ),
    tax_as_npr_hy = case_when(
      # For states where we compute from actual payments / NPR
      state %in% c("AZ", "MI", "OH", "WI") & net_pat_rev > 0 
                  ~ (tax_amount_total / net_pat_rev) * 100,
      # For all others use effective rate directly
      net_pat_rev > 0 ~ effective_rate,
      TRUE            ~ NA_real_
    )
  )

# Step 2: Build filtered base (all tax bases now)
base_filtered2 <- hosp_taxamounts %>%
  filter(year >= firsttax,
         tax_amount_total > 0,
         !is.na(tax_as_npr_hy),
         state != "NV") %>%         # exclude NV — tax only starts 2023
  mutate(years_since_tax = as.numeric(year) - as.numeric(firsttax))

# Summary stats for footer
# STARTING SPOT 6/6
footer_stats2 <- base_filtered2 %>%
  summarise(n_hospitals = n_distinct(mcrnum),
            n_states    = n_distinct(state))

# Step 3: Build table
bind_rows(
  base_filtered2 %>%
    filter(years_since_tax <= 7, state != "MI") %>%
    group_by(years_since_tax) %>%
    summarise(
      avg_tax_amount  = mean(tax_amount_total, na.rm = TRUE),
      avg_tax_as_npr  = mean(tax_as_npr_hy,    na.rm = TRUE),
      avg_beds        = mean(beds,             na.rm = TRUE),
      .groups         = "drop"
    ) %>%
    mutate(years_since_tax = as.character(years_since_tax)),

  base_filtered2 %>%
    filter(years_since_tax >= 8, state != "MI") %>%
    summarise(
      years_since_tax = "8+",
      avg_tax_amount  = mean(tax_amount_total, na.rm = TRUE),
      avg_tax_as_npr  = mean(tax_as_npr_hy,    na.rm = TRUE),
      avg_beds        = mean(beds,             na.rm = TRUE)
    )
) %>%
  mutate(
    avg_tax_amount = scales::dollar(avg_tax_amount),
    avg_tax_as_npr = sprintf("%.2f%%", avg_tax_as_npr),
    avg_beds       = sprintf("%.0f",   avg_beds)
  ) %>%
  rename(
    `Years Since Tax`      = years_since_tax,
    `Avg. Tax Amount`      = avg_tax_amount,
    `Avg. Tax as % of NPR` = avg_tax_as_npr,
    `Avg. Beds`            = avg_beds
  ) %>%
  kable(format   = "latex",
        booktabs = TRUE,
        caption  = "Average Hospital Provider Tax Payments by Years Since Adoption",
        align    = c("c", "r", "r", "c")) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  row_spec(8, hline_after = TRUE) %>%
  add_footnote(c(paste0("Hospitals: ", footer_stats2$n_hospitals),
                 paste0("States: ",    footer_stats2$n_states)),
               notation = "none")


# check 6 to 7 
base_filtered2 %>%
  filter(years_since_tax %in% c(6, 7)) %>%
  group_by(years_since_tax, state) %>%
  summarise(
    avg_tax_amount = mean(tax_amount_total[tax_amount_total > 0], na.rm = TRUE),
    avg_tax_as_npr = mean(tax_as_npr_hy,                          na.rm = TRUE),
    actual_rate    = first(tax_rate),
    tax_base       = first(tax_base),
    n_hospitals    = n_distinct(mcrnum),
    calendar_years = paste(sort(unique(year)), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(years_since_tax, state) %>%
  print(n = Inf)


# States in hosp_taxamounts vs base_filtered2
cat("States in hosp_taxamounts:", sort(unique(hosp_taxamounts$state)), "\n")
cat("States in base_filtered2: ", sort(unique(base_filtered2$state)),  "\n")

# Which states are getting dropped and why
hosp_taxamounts %>%
  group_by(state) %>%
  summarise(
    n_obs              = n(),
    n_after_firsttax   = sum(year >= firsttax,                           na.rm = TRUE),
    n_tax_amount_pos   = sum(tax_amount_total > 0,                       na.rm = TRUE),
    n_tax_as_npr_valid = sum(!is.na(tax_as_npr_hy)),
    n_all_filters      = sum(year >= firsttax & 
                             tax_amount_total > 0 & 
                             !is.na(tax_as_npr_hy) &
                             state != "NV",                              na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(n_all_filters) %>%
  print(n = Inf)

# investigate CT 
hosp_taxamounts %>%
  filter(state == "AR") %>%
  select(mcrnum, year, tax_base, tax_base2, tax_rate, tax_rate2,
         base1_value, base2_value, tax_amount1, tax_amount2,
         tax_amount_total, net_pat_rev, tax_as_npr_hy) %>%
  head(20)

hosp_taxamounts %>%
  filter(state %in% c("CT", "MA")) %>%
  distinct(state, tax_base, tax_base2)

# check pa
hosp_taxamounts %>%
  filter(state == "PA", year >= firsttax) %>%
  summarise(
    n_total            = n(),
    n_na_net_pat_rev   = sum(is.na(net_pat_rev)),
    n_zero_net_pat_rev = sum(net_pat_rev == 0,  na.rm = TRUE),
    n_na_tax_as_npr    = sum(is.na(tax_as_npr_hy)),
    n_na_effective_rate = sum(is.na(effective_rate))
  )

hosp_taxamounts %>%
  filter(state == "PA") %>%
  summarise(
    n_na_tax_rate  = sum(is.na(tax_rate)),
    n_na_tax_rate2 = sum(is.na(tax_rate2))
  )

# Verify PA is fixed
hosp_taxamounts %>%
  filter(state == "PA") %>%
  summarise(
    n_na_effective_rate = sum(is.na(effective_rate)),
    n_na_tax_as_npr     = sum(is.na(tax_as_npr_hy)),
    avg_tax_as_npr      = mean(tax_as_npr_hy, na.rm = TRUE)
  )

hosp_taxamounts %>%
  group_by(state) %>%
  summarise(
    n_obs              = n(),
    n_zero_tax_amount  = sum(tax_amount_total == 0,    na.rm = TRUE),
    n_na_tax_as_npr    = sum(is.na(tax_as_npr_hy)),
    avg_tax_amount     = mean(tax_amount_total[tax_amount_total > 0], na.rm = TRUE),
    avg_tax_as_npr     = mean(tax_as_npr_hy,            na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(state) %>%
  print(n = Inf)

# check 0 
# Check for zeros and NAs across key columns
hosp_taxamounts %>%
  group_by(state) %>%
  summarise(
    n_obs              = n(),
    n_zero_tax_amount  = sum(tax_amount_total == 0,    na.rm = TRUE),
    n_na_tax_amount    = sum(is.na(tax_amount_total)),
    n_zero_tax_as_npr  = sum(tax_as_npr_hy == 0,       na.rm = TRUE),
    n_na_tax_as_npr    = sum(is.na(tax_as_npr_hy)),
    avg_tax_amount     = mean(tax_amount_total,         na.rm = TRUE),
    avg_tax_as_npr     = mean(tax_as_npr_hy,            na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_zero_tax_amount)) %>%
  print(n = Inf)

# investigate 
# Check if the lookup is finding prior year values
hosp_taxamounts %>%
  filter(state %in% c("FL", "PA")) %>%
  summarise(
    n_na_base1      = sum(is.na(base1_value)),
    n_na_npr_lookup = sum(is.na(net_pat_rev_lookup)),
    n_na_lookup_yr  = sum(is.na(lookup_year)),
    avg_lookup_year = mean(lookup_year, na.rm = TRUE),
    .groups = "drop"
  )

# Check if the mcrnum + lookup_year combinations actually exist in hospdata_analysis
hosp_taxamounts %>%
  filter(state == "FL") %>%
  select(mcrnum, year, lookup_year, base_year, net_pat_rev_lookup, base1_value) %>%
  head(20)

hosp_taxamounts %>%
  filter(state %in% c("FL", "PA")) %>%
  distinct(state, tax_base, tax_base2)

# state level 
statesum <-hosp_taxamounts %>%
  filter(year == 2019) %>%
  group_by(state) %>%
  summarise(
    tax_base        = first(tax_base),
    tax_base2       = first(tax_base2),
    actual_rate     = first(tax_rate),
    actual_rate2    = first(tax_rate2),
    avg_tax_amount  = mean(tax_amount_total[tax_amount_total > 0], na.rm = TRUE),
    avg_tax_as_npr  = mean(tax_as_npr_hy,                          na.rm = TRUE),
    n_hospitals     = n_distinct(mcrnum),
    .groups = "drop"
  ) %>%
  arrange(state) %>%
  print(n = Inf)

View(statesum)

