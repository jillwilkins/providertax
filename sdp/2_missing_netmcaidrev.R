###########################################################
# SDP preliminary analysis
# Step 2: investigate WHERE net_mcaid_rev is missing -- since
# mcaid_revdis's missingness (see 1_build_sdp.R) is driven almost
# entirely by missing net_mcaid_rev (22,619 of 23,370 NAs), not
# missing mcaid_discharges (1,297).
###########################################################

library(dplyr)
library(tidyr)

data_output_path <- "/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/output/"

hospdata_analysis <- read.csv(paste0(data_output_path, "hospdata_analysis.csv"))

hospdata_analysis <- hospdata_analysis %>%
  mutate(rev_missing = is.na(net_mcaid_rev))

# ------------------------------------------------------------------
# By year
# ------------------------------------------------------------------
cat("\n=== Missing net_mcaid_rev BY YEAR ===\n")
by_year <- hospdata_analysis %>%
  group_by(year) %>%
  summarise(n = n(), n_missing = sum(rev_missing), pct_missing = round(100 * mean(rev_missing), 1)) %>%
  arrange(year)
print(by_year, n = Inf)

# ------------------------------------------------------------------
# By state
# ------------------------------------------------------------------
cat("\n=== Missing net_mcaid_rev BY STATE ===\n")
by_state <- hospdata_analysis %>%
  group_by(state) %>%
  summarise(n = n(), n_missing = sum(rev_missing), pct_missing = round(100 * mean(rev_missing), 1)) %>%
  arrange(desc(pct_missing))
print(by_state, n = Inf)

# ------------------------------------------------------------------
# State x year: is missingness concentrated in specific state-year
# cells (e.g., a state not reporting for a stretch of years) or
# spread evenly (a few hospitals always missing)?
# ------------------------------------------------------------------
cat("\n=== State-years with >50% missing (n>=5 hospitals) ===\n")
by_state_year <- hospdata_analysis %>%
  group_by(state, year) %>%
  summarise(n = n(), n_missing = sum(rev_missing), pct_missing = round(100 * mean(rev_missing), 1), .groups = "drop") %>%
  filter(n >= 5, pct_missing > 50) %>%
  arrange(state, year)
print(by_state_year, n = Inf)

# ------------------------------------------------------------------
# Is missingness concentrated in a small set of hospitals (always
# missing) vs. spread thin across many hospitals (sporadic gaps)?
# ------------------------------------------------------------------
cat("\n=== Hospital-level missingness ===\n")
by_hosp <- hospdata_analysis %>%
  group_by(mcrnum, state) %>%
  summarise(n_years = n(), n_missing = sum(rev_missing), pct_missing = round(100 * mean(rev_missing), 1), .groups = "drop")

cat("Hospitals never missing:      ", sum(by_hosp$n_missing == 0), "\n")
cat("Hospitals always missing:     ", sum(by_hosp$n_missing == by_hosp$n_years), "\n")
cat("Hospitals sometimes missing:  ", sum(by_hosp$n_missing > 0 & by_hosp$n_missing < by_hosp$n_years), "\n")

cat("\n=== States with hospitals ALWAYS missing net_mcaid_rev ===\n")
always_missing_by_state <- by_hosp %>%
  filter(n_missing == n_years) %>%
  count(state, sort = TRUE)
print(always_missing_by_state, n = Inf)

# ------------------------------------------------------------------
# Does missingness relate to treatment status (tax_on / ever_treat)?
# ------------------------------------------------------------------
cat("\n=== Missing net_mcaid_rev BY ever_treat ===\n")
by_treat <- hospdata_analysis %>%
  group_by(ever_treat) %>%
  summarise(n = n(), n_missing = sum(rev_missing), pct_missing = round(100 * mean(rev_missing), 1))
print(by_treat, n = Inf)
