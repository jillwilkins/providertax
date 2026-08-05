# ==============================================================================
# ATTRITION FROM THE HOSPITAL PANEL (NON-STACKED)
# Earlier version of this script rebuilt the stacked sub-experiments to check
# attrition, but that double-counts control hospitals (a not-yet-treated
# hospital can be a "control" in several cohorts' windows) and has no real
# "post-adoption" anchor for hospitals that are never treated. This version
# works directly off the raw panel -- one row per hospital, no replication.
# Requires hospdata_analysis already in the environment.
# ==============================================================================

library(dplyr)

attr_panel <- hospdata_analysis %>%
  filter(cohort != "Always (2004)")  # same exclusion as 5_buildstacked.R -- no identifiable pre-period

panel_end_year <- max(attr_panel$year, na.rm = TRUE)
cat("Panel end year:", panel_end_year, "\n")

# ------------------------------------------------------------------------------
# 1. One row per hospital: first/last year observed, whether the run is
#    contiguous (no gaps), and treatment status. No sub-experiment windows,
#    so no hospital is counted more than once.
# ------------------------------------------------------------------------------
hosp_panel_presence <- attr_panel %>%
  group_by(mcrnum) %>%
  summarise(
    ever_treat = first(ever_treat),
    firsttax   = suppressWarnings(as.numeric(first(firsttax))),
    n_years    = n_distinct(year),
    first_year = min(year),
    last_year  = max(year),
    .groups    = "drop"
  ) %>%
  mutate(
    contiguous  = n_years == (last_year - first_year + 1),
    exits_early = last_year < panel_end_year,
    group       = ifelse(ever_treat == 1, "Ever Treated", "Never Treated")
  )

cat("Hospitals checked:", nrow(hosp_panel_presence), "\n")

# ------------------------------------------------------------------------------
# 2. Overall exit rate: ever-treated vs. never-treated
# ------------------------------------------------------------------------------
exit_rate_by_group <- hosp_panel_presence %>%
  group_by(group) %>%
  summarise(
    n_total        = n(),
    n_exit_clean   = sum(contiguous & exits_early),
    n_exit_any     = sum(exits_early),   # includes hospitals with internal gaps too
    pct_exit_clean = round(100 * n_exit_clean / n_total, 1),
    pct_exit_any   = round(100 * n_exit_any / n_total, 1),
    .groups        = "drop"
  )

cat("\n=== EXIT RATE: EVER-TREATED VS NEVER-TREATED (one row per hospital) ===\n")
print(exit_rate_by_group)

prop_test <- prop.test(
  x = exit_rate_by_group$n_exit_clean,
  n = exit_rate_by_group$n_total
)
cat("\n=== TWO-PROPORTION TEST: CLEAN EXIT RATE ===\n")
print(prop_test)

# ------------------------------------------------------------------------------
# 3. Timing, treated hospitals only: exit relative to THEIR OWN adoption
#    year (firsttax), not a borrowed cohort clock. Never-treated hospitals
#    have no adoption year, so this question doesn't apply to them.
# ------------------------------------------------------------------------------
treated_exit_timing <- hosp_panel_presence %>%
  filter(ever_treat == 1, contiguous, exits_early, !is.na(firsttax)) %>%
  mutate(years_since_adoption_at_exit = last_year - firsttax)

cat("\n=== TREATED HOSPITALS THAT CLEANLY EXIT: TIMING RELATIVE TO OWN ADOPTION YEAR ===\n")
print(summary(treated_exit_timing$years_since_adoption_at_exit))

exit_vs_own_adoption <- treated_exit_timing %>%
  mutate(exit_period = ifelse(years_since_adoption_at_exit < 0, "Before own adoption", "At/after own adoption")) %>%
  count(exit_period) %>%
  mutate(pct = round(100 * n / sum(n), 1))

cat("\n=== DO TREATED HOSPITALS EXIT BEFORE OR AFTER THEIR OWN ADOPTION? ===\n")
print(exit_vs_own_adoption)

# ------------------------------------------------------------------------------
# 4. Who leaves? Pre-period characteristics of treated hospitals that
#    cleanly exit vs. treated hospitals that stay through the panel end.
#    "Pre" is defined per hospital as year < its own firsttax.
# ------------------------------------------------------------------------------
treated_pre_chars <- attr_panel %>%
  select(-firsttax) %>%  # hospdata_analysis's own firsttax is character-typed; use the numeric one from hosp_panel_presence
  semi_join(hosp_panel_presence %>% filter(ever_treat == 1), by = "mcrnum") %>%
  inner_join(hosp_panel_presence %>% select(mcrnum, firsttax), by = "mcrnum") %>%
  filter(year < firsttax) %>%
  group_by(mcrnum) %>%
  summarise(
    state              = first(state),
    pre_beds           = mean(beds, na.rm = TRUE),
    pre_op_margin      = mean(op_margin, na.rm = TRUE),
    pre_tot_discharges = mean(tot_discharges, na.rm = TRUE),
    pct_rural          = mean(rurlhos, na.rm = TRUE),
    typectrl           = first(typectrl),
    cntrl              = first(cntrl),
    .groups            = "drop"
  ) %>%
  mutate(
    # same HCRIS/AHA ownership classification used in 2_dataclean.R
    ownership = case_when(
      !is.na(typectrl) & typectrl %in% 1:2 ~ "Nonprofit",
      !is.na(typectrl) & typectrl %in% 3:6 ~ "For-profit",
      !is.na(cntrl) & cntrl %in% c(21, 23) ~ "Nonprofit",
      !is.na(cntrl) & cntrl %in% c(30, 31, 32, 33) ~ "For-profit",
      TRUE ~ "Unknown"
    )
  )

leaver_summary <- hosp_panel_presence %>%
  filter(ever_treat == 1, contiguous) %>%
  filter(exits_early | last_year == panel_end_year) %>%
  mutate(status = ifelse(exits_early, "Cleanly exited", "Stayed through panel end")) %>%
  left_join(treated_pre_chars, by = "mcrnum") %>%
  group_by(status) %>%
  summarise(
    n_hospitals             = n(),
    pre_beds_mean           = mean(pre_beds, na.rm = TRUE),
    pre_op_margin_mean      = mean(pre_op_margin, na.rm = TRUE),
    pre_tot_discharges_mean = mean(pre_tot_discharges, na.rm = TRUE),
    pct_rural               = mean(pct_rural, na.rm = TRUE) * 100,
    pct_nonprofit           = mean(ownership == "Nonprofit", na.rm = TRUE) * 100,
    pct_forprofit           = mean(ownership == "For-profit", na.rm = TRUE) * 100,
    .groups                 = "drop"
  )

cat("\n=== TREATED HOSPITALS: CLEANLY EXITED VS STAYED (pre-period characteristics) ===\n")
print(leaver_summary)
View(leaver_summary)
View(hosp_panel_presence)
