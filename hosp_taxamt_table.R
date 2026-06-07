# ==============================================================================
# Hospital Provider Tax Amount Analysis
# Goal: Table of tax burden by years since adoption, using tax as % of NPR
# ==============================================================================

# ------------------------------------------------------------------------------
# Step 1: Build hosp_taxamounts via join
# ------------------------------------------------------------------------------
hosp_taxamounts <- hospdata_analysis %>%
  inner_join(taxrates_clean, by = c("state", "year"))

# ------------------------------------------------------------------------------
# Step 2: Build lookup table
# ------------------------------------------------------------------------------
lookup <- hospdata_analysis %>%
  select(mcrnum, year, net_pat_rev, tot_discharges, beds, tot_charges) %>%
  mutate(year = as.numeric(year))

# ------------------------------------------------------------------------------
# Step 3: Determine lookup year
# ------------------------------------------------------------------------------
hosp_taxamounts <- hosp_taxamounts %>%
  mutate(
    lookup_year = case_when(
      !is.na(base_year) & base_year != 0 ~ as.numeric(base_year),
      TRUE                                ~ as.numeric(year) - 1
    )
  )

# ------------------------------------------------------------------------------
# Step 4: Join lookup values
# ------------------------------------------------------------------------------
hosp_taxamounts <- hosp_taxamounts %>%
  left_join(lookup, by = c("mcrnum", "lookup_year" = "year"),
            suffix = c("", "_lookup"))

# ------------------------------------------------------------------------------
# Step 5: Calculate tax amounts
# ------------------------------------------------------------------------------
flat_bases <- c("per discharge", "per bed")

hosp_taxamounts <- hosp_taxamounts %>%
  mutate(
    base1_value = case_when(
      tax_base %in% c("net patient revenue",
                      "net operating revenue",
                      "net operating revenue for inpatient services",
                      "inpatient net revenue",
                      "inpatient hospital services",
                      "inpatient services",
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
                       "outpatient hospital services",
                       "outpatient services")                  ~ net_pat_rev_lookup,
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

# ------------------------------------------------------------------------------
# Step 6: Patch Ohio — tax base is total operating expenses
# ------------------------------------------------------------------------------
hosp_taxamounts <- hosp_taxamounts %>%
  mutate(
    tax_amount_total = case_when(
      state == "OH" ~ tot_operating_exp * (3.1965 / 100),
      TRUE          ~ tax_amount_total
    )
  )

# ------------------------------------------------------------------------------
# Step 7: Add effective_rate and tax_as_npr_hy
# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
# Step 8: Build filtered base
# ------------------------------------------------------------------------------
base_filtered2 <- hosp_taxamounts %>%
  filter(
    year >= firsttax,
    !is.na(tax_as_npr_hy),
    state != "NV",
    (tax_amount_total > 0 | state %in% c("CT", "MA"))
  ) %>%
  mutate(years_since_tax = as.numeric(year) - as.numeric(firsttax))

# ------------------------------------------------------------------------------
# Step 9: Footer stats
# ------------------------------------------------------------------------------
footer_stats2 <- base_filtered2 %>%
  summarise(n_hospitals = n_distinct(mcrnum),
            n_states    = n_distinct(state))

# ------------------------------------------------------------------------------
# Step 10: Build LaTeX table
# ------------------------------------------------------------------------------
library(kableExtra)

# Compute average beds for footer
avg_beds_overall <- base_filtered2 %>%
  summarise(avg_beds = mean(beds, na.rm = TRUE)) %>%
  pull(avg_beds) %>%
  round(0)

bind_rows(
  base_filtered2 %>%
    filter(years_since_tax <= 7) %>%
    group_by(years_since_tax) %>%
    summarise(
      avg_tax_amount = mean(tax_amount_total[tax_amount_total > 0], na.rm = TRUE),
      .groups        = "drop"
    ) %>%
    left_join(
      base_filtered2 %>%
        filter(years_since_tax <= 7) %>%
        group_by(years_since_tax, state) %>%
        summarise(state_avg_npr = mean(tax_as_npr_hy, na.rm = TRUE), .groups = "drop") %>%
        group_by(years_since_tax) %>%
        summarise(avg_tax_as_npr = mean(state_avg_npr, na.rm = TRUE), .groups = "drop"),
      by = "years_since_tax"
    ) %>%
    mutate(years_since_tax = as.character(years_since_tax)),

  tibble(
    years_since_tax = "8+",
    avg_tax_amount  = base_filtered2 %>%
      filter(years_since_tax >= 8) %>%
      pull(tax_amount_total) %>%
      .[. > 0] %>% mean(na.rm = TRUE),
    avg_tax_as_npr  = base_filtered2 %>%
      filter(years_since_tax >= 8) %>%
      group_by(state) %>%
      summarise(s = mean(tax_as_npr_hy, na.rm = TRUE), .groups = "drop") %>%
      pull(s) %>% mean(na.rm = TRUE)
  )
) %>%
  mutate(
    avg_tax_amount = scales::dollar(avg_tax_amount),
    avg_tax_as_npr = sprintf("%.2f%%", avg_tax_as_npr)
  ) %>%
  rename(
    `Years Since Tax`      = years_since_tax,
    `Avg. Tax Amount`      = avg_tax_amount,
    `Avg. Tax as % of NPR` = avg_tax_as_npr
  ) %>%
  kable(format    = "latex",
        booktabs  = TRUE,
        caption   = "Average Hospital Provider Tax Payments by Years Since Adoption",
        align     = c("c", "r", "r")) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  row_spec(8, hline_after = TRUE) %>%
  add_footnote(c(paste0("Hospitals: ", footer_stats2$n_hospitals),
                 paste0("States: ",    footer_stats2$n_states),
                 paste0("Avg. Beds: ", avg_beds_overall)),
               notation = "none") %>%
  save_kable("tax_burden_table.tex")
