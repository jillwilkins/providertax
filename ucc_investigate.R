# ==============================================================================
# DIAGNOSTIC: Uncompensated Care Missingness by Year
# ==============================================================================

# 1. Overall missingness by year
ucc_missing_by_year <- hospdata_analysis %>%
  group_by(year) %>%
  summarise(
    n_obs = n(),
    n_hospitals = n_distinct(mcrnum),
    n_missing_ucc = sum(is.na(ucc_prop)),
    pct_missing_ucc = round(n_missing_ucc / n_obs * 100, 1)
  ) %>%
  arrange(year)

cat("\n=== UNCOMPENSATED CARE MISSINGNESS BY YEAR ===\n")
print(ucc_missing_by_year, n = Inf)

# 2. Visualize the pattern
library(ggplot2)

ggplot(ucc_missing_by_year, aes(x = year, y = pct_missing_ucc)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2011, linetype = "dashed", color = "red") +
  labs(
    title = "Uncompensated Care Data Missingness Over Time",
    x = "Year",
    y = "Percent Missing (%)",
    caption = "Red line = 2011 (when reporting became more reliable)"
  ) +
  theme_minimal()

# 3. Check if missingness differs by treatment status
ucc_missing_by_treatment <- hospdata_analysis %>%
  filter(year < 2011) %>%  # Focus on pre-2011
  group_by(ever_treat) %>%
  summarise(
    n_obs = n(),
    n_hospitals = n_distinct(mcrnum),
    n_missing_ucc = sum(is.na(ucc_prop)),
    pct_missing_ucc = round(n_missing_ucc / n_obs * 100, 1)
  ) %>%
  mutate(group = ifelse(ever_treat == 1, "Ever Treated", "Never Treated"))

cat("\n=== MISSINGNESS PRE-2011 BY TREATMENT STATUS ===\n")
print(ucc_missing_by_treatment)

# 4. Check missingness by state (pre-2011)
ucc_missing_by_state_pre2011 <- hospdata_analysis %>%
  filter(year < 2011) %>%
  group_by(state) %>%
  summarise(
    n_obs = n(),
    n_hospitals = n_distinct(mcrnum),
    n_missing_ucc = sum(is.na(ucc_prop)),
    pct_missing_ucc = round(n_missing_ucc / n_obs * 100, 1)
  ) %>%
  arrange(desc(pct_missing_ucc))

cat("\n=== TOP 10 STATES WITH MOST MISSING UCC DATA (PRE-2011) ===\n")
print(head(ucc_missing_by_state_pre2011, 10))

# 5. Summary: Pre-2011 vs Post-2011
ucc_missing_summary <- hospdata_analysis %>%
  mutate(period = ifelse(year < 2011, "Pre-2011", "2011+")) %>%
  group_by(period) %>%
  summarise(
    n_obs = n(),
    n_missing_ucc = sum(is.na(ucc_prop)),
    pct_missing_ucc = round(n_missing_ucc / n_obs * 100, 1)
  )

cat("\n=== OVERALL SUMMARY: PRE-2011 vs 2011+ ===\n")
print(ucc_missing_summary)

# 6. Check how many hospitals have NO ucc_prop data before 2011
hospitals_no_ucc_pre2011 <- hospdata_analysis %>%
  filter(year < 2011) %>%
  group_by(mcrnum) %>%
  summarise(
    n_years = n(),
    n_missing = sum(is.na(ucc_prop)),
    pct_missing = n_missing / n_years * 100
  ) %>%
  filter(pct_missing == 100)  # Always missing pre-2011

cat("\n=== HOSPITALS WITH 100% MISSING UCC PRE-2011 ===\n")
cat(paste("Number of hospitals with NO ucc_prop data before 2011:", 
          nrow(hospitals_no_ucc_pre2011), "\n"))