# GOAL: Join taxrates_clean to hospdata_analysis to get tax amounts for each hospital-year
# run rates_table.R 
# See what state looks like BEFORE the mutation

#taxrates_clean <- taxrates_clean %>%
#  mutate(state = state.abb[match(state, state.name)])

hosp_taxamounts <- hospdata_analysis %>%
  inner_join(taxrates_clean, by = c("state", "year"))

# PRELIMINARY TAX CALCULATIONS (only net patient revenue, or clear base)
# Using base amounts and base years (using base years did not drastically change things)
# June 1, 2026 

# Step 1: Build lookup table from hospdata_analysis
lookup <- hospdata_analysis %>%
  select(mcrnum, year, net_pat_rev, tot_discharges, beds, tot_charges) %>%
  mutate(year = as.numeric(year))

# Step 2: Determine lookup year in hosp_taxamounts
hosp_taxamounts <- hosp_taxamounts %>%
  mutate(
    lookup_year = case_when(
      !is.na(base_year) & base_year != 0 ~ as.numeric(base_year),
      TRUE                                ~ as.numeric(year) - 1
    )
  )

# Step 3: Join lookup values in on mcrnum + lookup_year
hosp_taxamounts <- hosp_taxamounts %>%
  left_join(lookup, by = c("mcrnum", "lookup_year" = "year"),
            suffix = c("", "_lookup"))


# Step 4: Recalculate tax amounts using looked-up base values
flat_bases <- c("per discharge", "per bed")

hosp_taxamounts <- hosp_taxamounts %>%
  mutate(
    base1_value = case_when(
      tax_base %in% c("net patient revenue",
                      "net operating revenue",
                      "net operating revenue for inpatient services",
                      "inpatient net revenue",
                      "inpatient hospital services",                   # CT
                      "inpatient services",                            # MA
                      "gross revenue",
                      "gross receipts (care provided to patients)",
                      "gross patient revenue",
                      "inpatient and outpatient gross revenues") ~ net_pat_rev_lookup,
      tax_base == "per discharge"          ~ tot_discharges_lookup,
      tax_base == "per bed"                ~ beds_lookup,
      tax_base == "total hospital charges" ~ tot_charges_lookup,
      TRUE                                 ~ NA_real_
    ),

    base2_value = case_when(
      tax_base2 == "per facility"                              ~ 1,
      tax_base2 %in% c("net operating revenue for outpatient services",
                       "outpatient net revenue",
                       "outpatient hospital services",                 # CT
                       "outpatient services")                ~ net_pat_rev_lookup, # MA
      TRUE                                                     ~ NA_real_
    ),

    tax_amount1 = case_when(
      tax_base %in% flat_bases ~ tax_rate  * base1_value,
      TRUE                     ~ (tax_rate  / 100) * base1_value
    ),

    tax_amount2 = case_when(
      tax_base2 == "per facility" ~ tax_rate2 * base2_value,
      TRUE                        ~ (tax_rate2 / 100) * base2_value
    ),

    tax_amount_total = rowSums(cbind(tax_amount1, tax_amount2), na.rm = TRUE)
  )

# Step 5: Patch Ohio
hosp_taxamounts <- hosp_taxamounts %>%
  mutate(
    tax_amount_total = case_when(
      state == "OH" ~ tot_operating_exp * (3.1965 / 100),
      TRUE          ~ tax_amount_total
    )
  )

# Step 6: Add effective_rate and tax_as_npr_hy
hosp_taxamounts <- hosp_taxamounts %>%
  mutate(
    effective_rate = case_when(
      state %in% c("CT", "MA", "FL", "PA") & !is.na(tax_rate2) 
                  ~ (tax_rate + tax_rate2) / 2,
      state %in% c("CT", "MA", "FL", "PA") & is.na(tax_rate2)  
                  ~ tax_rate,
      TRUE        ~ tax_rate
    ),
    tax_as_npr_hy = case_when(
      state %in% c("AZ", "MI", "OH", "WI") & net_pat_rev > 0
                  ~ (tax_amount_total / net_pat_rev) * 100,
      net_pat_rev > 0 ~ effective_rate,
      TRUE            ~ NA_real_
    )
  )

hosp_taxamounts %>%
  group_by(state) %>%
  summarise(
    n_obs              = n(),
    n_zero_tax_amount  = sum(tax_amount_total == 0,    na.rm = TRUE),
    n_na_tax_as_npr    = sum(is.na(tax_as_npr_hy)),
    avg_tax_amount     = mean(tax_amount_total,         na.rm = TRUE),
    avg_tax_as_npr     = mean(tax_as_npr_hy,            na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(state) %>%
  print(n = Inf)


# Now Build Descriptives and the La Tex table from this data 
hosp_taxamounts %>%
  group_by(tax_base) %>%
  summarise(
    n_hospitals    = n(),
    avg_rate       = mean(tax_rate, na.rm = TRUE),
    avg_base_value = mean(base1_value, na.rm = TRUE),
    avg_tax_amount = mean(tax_amount1, na.rm = TRUE),
    n_missing_base = sum(is.na(base1_value)),
    .groups = "drop"
  ) %>%
  print()

# DESCRIPTIVES 
# Average rate among net_pat_rev based taxes
hosp_taxamounts %>%
  filter(tax_base %in% c("net patient revenue",
                         "net operating revenue",
                         "gross revenue",
                         "gross receipts (care provided to patients)",
                         "gross patient revenue",
                         "inpatient and outpatient gross revenues"), tax_amount_total > 0) %>%
  summarise(avg_rate = mean(tax_rate, na.rm = TRUE))

# Average tax amount in year 1 of the tax (rel_year == 0)
hosp_taxamounts %>%
  filter(year == firsttax, tax_amount_total > 0) %>%
  summarise(avg_tax_amount = mean(tax_amount_total, na.rm = TRUE))

# Average rate among net_pat_rev based taxes by years since tax
hosp_taxamounts %>%
  filter(tax_base %in% c("net patient revenue",
                         "net operating revenue",
                         "gross revenue",
                         "gross receipts (care provided to patients)",
                         "gross patient revenue",
                         "inpatient and outpatient gross revenues"),
         year >= firsttax,
         tax_amount_total > 0) %>%
  mutate(years_since_tax = as.numeric(year) - as.numeric(firsttax)) %>%
  group_by(years_since_tax) %>%
  summarise(avg_rate       = mean(tax_rate, na.rm = TRUE),
            avg_tax_amount = mean(tax_amount_total, na.rm = TRUE),
            avg_beds       = mean(beds, na.rm = TRUE),
            n_hospitals    = n(),
            .groups        = "drop")

hosp_taxamounts %>%
  filter(tax_base %in% c("net patient revenue",
                         "net operating revenue",
                         "gross revenue",
                         "gross receipts (care provided to patients)",
                         "gross patient revenue",
                         "inpatient and outpatient gross revenues"),
         year >= firsttax,
         tax_amount_total > 0) %>%
  summarise(n_hospitals = n_distinct(mcrnum),
            n_states    = n_distinct(state),
            states      = paste(unique(state), collapse = ", "))


# la tex table 
library(kableExtra)

# Base filtered data
base_filtered <- hosp_taxamounts %>%
  filter(tax_base %in% c("net patient revenue",
                         "net operating revenue",
                         "gross revenue",
                         "gross receipts (care provided to patients)",
                         "gross patient revenue",
                         "inpatient and outpatient gross revenues"),
         year >= firsttax,
         tax_amount_total > 0) %>%
  mutate(years_since_tax = as.numeric(year) - as.numeric(firsttax))

# Summary stats for footer
footer_stats <- base_filtered %>%
  summarise(n_hospitals = n_distinct(mcrnum),
            n_states    = n_distinct(state))

# Build table
bind_rows(
  base_filtered %>%
    filter(years_since_tax <= 7) %>%
    group_by(years_since_tax) %>%
    summarise(avg_rate       = mean(tax_rate, na.rm = TRUE),
              avg_tax_amount = mean(tax_amount_total, na.rm = TRUE),
              avg_beds       = mean(beds, na.rm = TRUE),
              .groups        = "drop") %>%
    mutate(years_since_tax = as.character(years_since_tax)),

  base_filtered %>%
    filter(years_since_tax >= 8) %>%
    summarise(years_since_tax = "8+",
              avg_rate        = mean(tax_rate, na.rm = TRUE),
              avg_tax_amount  = mean(tax_amount_total, na.rm = TRUE),
              avg_beds        = mean(beds, na.rm = TRUE))
) %>%
  mutate(avg_tax_amount = scales::dollar(avg_tax_amount),
         avg_rate       = sprintf("%.2f%%", avg_rate),
         avg_beds       = sprintf("%.0f", avg_beds)) %>%
  rename(`Years Since Tax` = years_since_tax,
         `Avg. Rate`       = avg_rate,
         `Avg. Tax Amount` = avg_tax_amount,
         `Avg. Beds`       = avg_beds) %>%
  kable(format  = "latex",
        booktabs = TRUE,
        caption  = "Average Hospital Provider Tax Payments by Years Since Adoption",
        align    = c("c", "c", "r", "c")) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  row_spec(8, hline_after = TRUE) %>%
  add_footnote(c(paste0("Hospitals: ", footer_stats$n_hospitals),
                 paste0("States: ",    footer_stats$n_states)),
               notation = "none")
