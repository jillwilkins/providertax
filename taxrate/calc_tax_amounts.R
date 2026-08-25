# ==============================================================================
# CALCULATE HOSPITAL PROVIDER TAX AMOUNTS
# Replaces hosp_taxamounts.R / hosp_taxamounts_impr.R / hosp_taxamt_table.R.
# Joins clean_taxrates.R's state-level rate/base data onto hospdata_analysis
# and estimates each hospital-year's tax payment. Built for a "record what
# you have, estimate the rest" schema (see expansion_analysis_notes.md-style
# design discussion): tax_amount_recorded is a placeholder for real hospital-
# level dollar records (to be merged in once available), tax_amount_estimated
# is rate x base, and tax_amount_best coalesces the two with a source flag so
# the two are never silently blended without being able to tell which is
# which.
# Requires hospdata_analysis and taxrates_clean (source clean_taxrates.R)
# already in the environment.
# ==============================================================================

library(dplyr)

# ------------------------------------------------------------------------------
# 1. Join tax rates onto the hospital panel. hospdata_analysis has ~18
#    duplicate mcrnum-year rows (pre-existing, noted separately) that overlap
#    several states we have rate data for (MI, MO, PA, WI) -- dedupe before
#    joining so they don't get carried through as literal duplicate rows.
# ------------------------------------------------------------------------------
hospdata_analysis_dedup <- hospdata_analysis %>%
  distinct(mcrnum, year, .keep_all = TRUE)

tax_amounts <- hospdata_analysis_dedup %>%
  inner_join(taxrates_clean, by = c("state", "year"))

# ------------------------------------------------------------------------------
# 2. Determine the lookup year for each hospital-year's tax base: the
#    specified base_year if given (and not the "not specified" sentinel 0),
#    else the prior year. Same convention as the old scripts.
# ------------------------------------------------------------------------------
lookup <- hospdata_analysis %>%
  select(mcrnum, year, net_pat_rev, tot_discharges, beds, tot_charges, tot_operating_exp) %>%
  mutate(year = as.numeric(year)) %>%
  # hospdata_analysis has ~18 duplicate mcrnum-year rows (pre-existing, noted
  # separately) -- without deduping here, joining against them would multiply
  # some hospital-tax-years via an unintended many-to-many match.
  distinct(mcrnum, year, .keep_all = TRUE)

tax_amounts <- tax_amounts %>%
  mutate(
    lookup_year = case_when(
      !is.na(base_year) & base_year != 0 ~ base_year,
      TRUE                                ~ as.numeric(year) - 1
    )
  ) %>%
  left_join(lookup, by = c("mcrnum", "lookup_year" = "year"), suffix = c("", "_lookup"))

# ------------------------------------------------------------------------------
# 3. Map each tax_base string to the hospital-level column it represents,
#    and compute the dollar amount for each of up to two rate/base
#    components. "per bed", "per discharge", and "per facility" are flat
#    dollar amounts (multiply directly); everything else is a percentage
#    (divide by 100 first).
# ------------------------------------------------------------------------------
revenue_bases <- c(
  "net patient revenue", "net operating revenue", "gross revenue",
  "gross receipts (care provided to patients)", "gross patient revenue",
  "inpatient and outpatient gross revenues", "inpatient net revenue",
  "inpatient hospital services", "inpatient services"
)
outpatient_revenue_bases <- c(
  "outpatient net revenue", "outpatient hospital services", "outpatient services"
)
flat_bases <- c("per discharge", "per bed", "per facility")

tax_amounts <- tax_amounts %>%
  mutate(
    base1_value = case_when(
      tax_base %in% revenue_bases                 ~ net_pat_rev_lookup,
      tax_base == "per discharge"                  ~ tot_discharges_lookup,
      tax_base == "per bed"                        ~ beds_lookup,
      tax_base == "total hospital charges"         ~ tot_charges_lookup,
      tax_base == "total facility costs (non medicare)" ~ tot_operating_exp_lookup,
      TRUE                                          ~ NA_real_
    ),
    base2_value = case_when(
      tax_base2 == "per facility"                  ~ 1,
      tax_base2 %in% outpatient_revenue_bases       ~ net_pat_rev_lookup,
      TRUE                                          ~ NA_real_
    ),
    # tax_amount1 uses tax_rate (never NA here, guaranteed by the earlier
    # filter in clean_taxrates.R) but base1_value can be NA when tax_base
    # didn't match any known category -- that must produce NA (unknown),
    # not silently drop out of a rowSums() as if it were a zero contribution.
    tax_amount1 = if_else(
      tax_base %in% flat_bases,
      tax_rate * base1_value,
      (tax_rate / 100) * base1_value
    ),
    # tax_amount2: NA tax_rate2 means "this state genuinely has no second
    # component" -> contributes 0. A present tax_rate2 with an unresolved
    # base2_value means "second component exists but its base is unknown"
    # -> must stay NA, not become 0.
    tax_amount2 = case_when(
      is.na(tax_rate2)              ~ 0,
      tax_base2 %in% flat_bases     ~ tax_rate2 * base2_value,
      TRUE                          ~ (tax_rate2 / 100) * base2_value
    ),
    tax_amount_estimated = if_else(
      is.na(tax_amount1) | is.na(tax_amount2),
      NA_real_,
      tax_amount1 + tax_amount2
    ),
    # Flag hospital-years with a genuine two-part tax structure, so the
    # split shows up in any average without needing to be excluded from it
    n_rate_components = 1L + as.integer(!is.na(tax_rate2))
  )

# ------------------------------------------------------------------------------
# 4. Coalesce recorded vs. estimated dollar amounts. tax_amount_recorded is a
#    placeholder column (all NA) until real hospital-level dollar records are
#    merged in -- keeping the column and the source flag now means that merge
#    won't require touching this calculation logic later.
# ------------------------------------------------------------------------------
tax_amounts <- tax_amounts %>%
  mutate(
    tax_amount_recorded = NA_real_,
    tax_amount_best      = coalesce(tax_amount_recorded, tax_amount_estimated),
    amount_source         = case_when(
      !is.na(tax_amount_recorded)  ~ "recorded",
      !is.na(tax_amount_estimated) ~ "estimated",
      TRUE                          ~ NA_character_
    ),
    # Tax as % of NPR, using the SAME lookup-year revenue the dollar amount
    # itself was calculated from (not the current year's revenue) -- keeps
    # the numerator and denominator on the same base period. This is a
    # deliberate change from the old scripts, which mixed current-year NPR
    # as the denominator with prior-year NPR as the tax base.
    tax_as_npr_best = if_else(
      !is.na(net_pat_rev_lookup) & net_pat_rev_lookup > 0,
      (tax_amount_best / net_pat_rev_lookup) * 100,
      NA_real_
    )
  )

# ------------------------------------------------------------------------------
# Sanity checks
# ------------------------------------------------------------------------------
cat("Hospital-year rows with a tax rate on file:", nrow(tax_amounts), "\n")
cat("Rows with a computable tax_amount_best:", sum(!is.na(tax_amounts$tax_amount_best)), "\n")
cat("Rows with 2 rate components:", sum(tax_amounts$n_rate_components == 2, na.rm = TRUE), "\n")

cat("\nRows with a rate on file but NO computable base1_value. Every tax_base\n")
cat("string IS mapped to a hospital-level column, so this is NOT a mapping\n")
cat("typo -- it means the hospital has no financial data in hospdata_analysis\n")
cat("for the specific lookup year (common for states with a FIXED base_year,\n")
cat("where a hospital that entered the panel after that year has nothing to\n")
cat("look up). Genuine missing data, not a bug:\n")
print(
  tax_amounts %>%
    filter(is.na(base1_value)) %>%
    count(state, tax_base, base_year) %>%
    arrange(desc(n))
)

cat("\nSummary of tax_amount_best by state:\n")
print(
  tax_amounts %>%
    group_by(state) %>%
    summarise(
      n_obs               = n(),
      n_components_2      = sum(n_rate_components == 2, na.rm = TRUE),
      avg_tax_amount_best = mean(tax_amount_best, na.rm = TRUE),
      avg_tax_as_npr      = mean(tax_as_npr_best, na.rm = TRUE),
      .groups             = "drop"
    ) %>%
    arrange(state),
  n = Inf
)
