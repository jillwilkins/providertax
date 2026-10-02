#=============================================
# This script is to estimate the Quantile Treatment Effect on the Treated (QTT) 
# as described in Callaway and Li 2019. 
#=============================================

install.packages("remotes")  # if not already installed
remotes::install_github("bcallaway11/qte")
library(qte)
library(qte)
packageVersion("qte")

remotes::install_github("bcallaway11/BMisc")
library(BMisc)

hospdata_analysis <- hospdata_analysis %>%
    mutate(
        qtt_gname = case_when(
    is.na(firsttax_num) ~ 0,              # Never treated = 0
    firsttax_num == 2004 ~ NA_real_,      # Always treated: no valid pre-period, exclude
  TRUE ~ firsttax_num
))

hospdata_analysis_qtt <- hospdata_analysis %>%
  filter(!is.na(qtt_gname))

dup_hospital_years <- hospdata_analysis_qtt %>%
  count(mcrnum, year) %>%
  filter(n > 1) %>%
  select(mcrnum, year)

hospdata_analysis_qtt <- hospdata_analysis_qtt %>%
  anti_join(dup_hospital_years, by = c("mcrnum", "year"))

# panel_qtt_gt subsets treated units via G == g and sorts/positions rows by
# id without merging -- that silently breaks if a hospital's qtt_gname isn't
# constant across its own panel years (produces mismatched pre1/pre2 vector
# lengths deep inside the package). Drop any hospital where it isn't constant.
unstable_gname_ids <- hospdata_analysis_qtt %>%
  group_by(mcrnum) %>%
  summarise(n_distinct_gname = n_distinct(qtt_gname), .groups = "drop") %>%
  filter(n_distinct_gname > 1) %>%
  pull(mcrnum)

hospdata_analysis_qtt <- hospdata_analysis_qtt %>%
  filter(!mcrnum %in% unstable_gname_ids)

# confirm it's clean
hospdata_analysis_qtt %>% count(mcrnum, year) %>% filter(n > 1)

hospdata_analysis_qtt %>%
  count(mcrnum) %>%
  count(n)

# balance strictly for now 
balanced_ids <- hospdata_analysis_qtt %>%
  count(mcrnum) %>%
  filter(n == 25) %>%
  pull(mcrnum)

hospdata_analysis_qtt <- hospdata_analysis_qtt %>%
  filter(mcrnum %in% balanced_ids)

# confirm
n_distinct(hospdata_analysis_qtt$mcrnum)
hospdata_analysis_qtt %>% count(mcrnum) %>% count(n)

# also check the outcome itself has no gaps now
sum(is.na(hospdata_analysis_qtt$op_margin))

# remove hospitals with na op margin
hospitals_with_na_margin <- hospdata_analysis_qtt %>%
  filter(is.na(op_margin)) %>%
  pull(mcrnum) %>%
  unique()

length(hospitals_with_na_margin)  # how many hospitals this affects

hospdata_analysis_qtt <- hospdata_analysis_qtt %>%
  filter(!mcrnum %in% hospitals_with_na_margin)

# confirm clean
sum(is.na(hospdata_analysis_qtt$op_margin))
n_distinct(hospdata_analysis_qtt$mcrnum)
table(hospdata_analysis_qtt$qtt_gname)

# drop cohorts with no observed post-treatment period (e.g. gname = 2024 when
# the panel only runs through 2023) -- panel_qtt can't evaluate a group that
# never reaches year >= gname, and leaving it in breaks the internal (g,t)
# grid, producing a length mismatch between Y_pre1_trt and Y_pre2_trt.
max_year <- max(hospdata_analysis_qtt$year)
hospdata_analysis_qtt <- hospdata_analysis_qtt %>%
  filter(qtt_gname == 0 | qtt_gname <= max_year)

table(hospdata_analysis_qtt$qtt_gname)

res <- panel_qtt(
  yname = "op_margin",
  gname = "qtt_gname",
  tname = "year",
  idname = "mcrnum",
  data = hospdata_analysis_qtt,
  xformula = ~1,
  weightsname = NULL,
  control_group = "notyettreated",
  anticipation = 0,
  alp = 0.05,
  cband = TRUE,
  biters = 100,
  cl = 1,
  gt_type = "qtt",
  probs = seq(0.1, 0.9, 0.1),
  pre_copula = "long"
)
summary(res)


min_year <- min(hospdata_analysis_qtt$year)
hospdata_analysis_qtt %>%
  filter(qtt_gname != 0) %>%
  distinct(qtt_gname) %>%
  mutate(years_before_adoption = qtt_gname - min_year) %>%
  arrange(years_before_adoption)

range(hospdata_analysis_qtt$year)
hospdata_analysis_qtt %>% filter(qtt_gname == 2024) %>% summarise(n_hosp = n_distinct(mcrnum), max_yr = max(year))

hospdata_analysis_qtt %>%
  group_by(mcrnum) %>%
  summarise(n_distinct_gname = n_distinct(qtt_gname), .groups = "drop") %>%
  filter(n_distinct_gname > 1)
