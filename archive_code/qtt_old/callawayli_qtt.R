#=============================================================
# Quantile Treatment Effect on the Treated (QTT)
# Callaway & Li (2019), via the qte::panel_qtt() estimator
#=============================================================


# ---- 0. One-time setup (run once, then comment out) ----------
# ---- libraries ----
library(dplyr)
library(qte)
library(BMisc)
library(ptetools)

#=============================================================
# 1. Build the group variable (gname)
#    0 = never treated; "Always treated" (2004) hospitals have
#    no valid pre-period, so they're excluded entirely (NA).
#=============================================================
hospdata_analysis <- hospdata_analysis %>%
  mutate(
    qtt_gname = case_when(
      is.na(firsttax_num)      ~ 0,
      firsttax_num == 2004     ~ NA_real_,
      TRUE                     ~ firsttax_num
    )
  )

hospdata_analysis_qtt <- hospdata_analysis %>%
  filter(!is.na(qtt_gname))

#=============================================================
# 2. Data cleaning
#    Each step below fixes a specific way panel_qtt() breaks
#    internally (mismatched pre1/pre2 vector lengths, or empty
#    ECDF inputs) when the panel isn't perfectly rectangular.
#=============================================================

## 2a. Drop duplicate hospital-year rows (likely subunit filings)
dup_hospital_years <- hospdata_analysis_qtt %>%
  count(mcrnum, year) %>%
  filter(n > 1) %>%
  select(mcrnum, year)

hospdata_analysis_qtt <- hospdata_analysis_qtt %>%
  anti_join(dup_hospital_years, by = c("mcrnum", "year"))

## 2b. Drop hospitals whose qtt_gname isn't constant over their own panel
##     (panel_qtt_gt sorts/positions rows by id without merging on gname --
##     an inconsistent gname silently breaks that alignment)
unstable_gname_ids <- hospdata_analysis_qtt %>%
  group_by(mcrnum) %>%
  summarise(n_distinct_gname = n_distinct(qtt_gname), .groups = "drop") %>%
  filter(n_distinct_gname > 1) %>%
  pull(mcrnum)

hospdata_analysis_qtt <- hospdata_analysis_qtt %>%
  filter(!mcrnum %in% unstable_gname_ids)

## 2c. Restrict to hospitals present in every year (strict balance for now --
##     panel_qtt() needs matched ids across the periods each (g,t) cell uses,
##     and full balance is the simplest way to guarantee that)
balanced_ids <- hospdata_analysis_qtt %>%
  count(mcrnum) %>%
  filter(n == 25) %>%
  pull(mcrnum)

hospdata_analysis_qtt <- hospdata_analysis_qtt %>%
  filter(mcrnum %in% balanced_ids)

## 2d. Drop hospitals with any missing op_margin (a present row with a
##     missing outcome breaks a cell the same way an absent row does)
hospitals_with_na_margin <- hospdata_analysis_qtt %>%
  filter(is.na(op_margin)) %>%
  pull(mcrnum) %>%
  unique()

hospdata_analysis_qtt <- hospdata_analysis_qtt %>%
  filter(!mcrnum %in% hospitals_with_na_margin)

## 2e. Drop cohorts with no observed post-treatment period (e.g. a 2024
##     adoption cohort when the panel only runs through 2023)
max_year <- max(hospdata_analysis_qtt$year)
hospdata_analysis_qtt <- hospdata_analysis_qtt %>%
  filter(qtt_gname == 0 | qtt_gname <= max_year)

#=============================================================
# 3. Confirm the panel is clean before estimating
#=============================================================
stopifnot(
  nrow(hospdata_analysis_qtt %>% count(mcrnum, year) %>% filter(n > 1)) == 0,
  all((hospdata_analysis_qtt %>% count(mcrnum))$n == 25),
  sum(is.na(hospdata_analysis_qtt$op_margin)) == 0
)

n_distinct(hospdata_analysis_qtt$mcrnum)
table(hospdata_analysis_qtt$qtt_gname)

#=============================================================
# 4. Estimate the QTT
#=============================================================

res <- panel_qtt(
  yname         = "op_margin",
  gname         = "qtt_gname",
  tname         = "year",
  idname        = "mcrnum",
  data          = hospdata_analysis_qtt,
  xformula      = ~1,
  weightsname   = NULL,
  control_group = "notyettreated",
  anticipation  = 0,
  alp           = 0.05,
  cband         = TRUE,
  biters        = 100,
  cl            = 1,
  gt_type       = "qtt",
  probs         = seq(0.1, 0.9, 0.1),
  pre_copula    = "short"
)

summary(res)

str(res, max.level = 1)
names(res)

library(ggplot2)

plot_df <- res_cov$overall

# Choose a y-range that comfortably covers everything except the blown-out tail
y_lim <- c(-0.09, 0.09)

# Flag which bands get clipped, so we can mark them instead of silently cutting them
plot_df$clipped <- plot_df$lower_ub < y_lim[1] | plot_df$upper_ub > y_lim[2]

ggplot(plot_df, aes(x = probs, y = qtt)) +
  geom_ribbon(aes(ymin = pmax(lower_ub, y_lim[1]), ymax = pmin(upper_ub, y_lim[2])),
              fill = "grey80", alpha = 0.4) +
  geom_errorbar(aes(ymin = pmax(lower_pw, y_lim[1]), ymax = pmin(upper_pw, y_lim[2])),
                width = 0.01, color = "grey30") +
  geom_point(size = 2.5, color = "#2484d2") +
  geom_line(color = "#2484d2") +
  # little arrow glyphs marking where a band actually extends past the frame
  geom_point(data = subset(plot_df, clipped), aes(y = y_lim[2] * 0.97),
             shape = 24, size = 2, color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_cartesian(ylim = y_lim) +
  scale_x_continuous(breaks = plot_df$probs) +
  labs(
    title    = "Quantile Treatment Effect on the Treated (QTT)",
    subtitle = "Operating Margin — Provider Tax Adoption",
    x        = "Quantile (\u03c4)",
    y        = "QTT",
    caption  = paste0(
      "Shaded band: 95% uniform confidence band. Error bars: pointwise 95% CI.\n",
      "Triangle markers indicate quantiles where the uniform band extends beyond the plotted range\n",
      "(\u03c4 = 0.9 band: [", round(plot_df$lower_ub[plot_df$probs == 0.9], 2), ", ",
      round(plot_df$upper_ub[plot_df$probs == 0.9], 2), "])."
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, size = 9, color = "grey30"))

ggsave("opmargin_qtt_plot.png", width = 10, height = 10, dpi = 300)


hospdata_analysis %>%
  filter(firsttax_num %in% c(2019, 2021)) %>%
  distinct(mcrnum) %>%
  nrow()

# # Restrict to the same cohorts your ATT actually uses (2003-2018 validity window)
hospdata_analysis_qtt_restricted <- hospdata_analysis_qtt %>%
  filter(qtt_gname == 0 | (qtt_gname >= 2003 & qtt_gname <= 2018))

table(hospdata_analysis_qtt_restricted$qtt_gname)

res_cov <- panel_qtt(
  yname         = "op_margin",
  gname         = "qtt_gname",
  tname         = "year",
  idname        = "mcrnum",
  data          = hospdata_analysis_qtt_restricted,
  xformula      = ~ 1,
  weightsname   = NULL,
  control_group = "notyettreated",
  anticipation  = 0,
  alp           = 0.05,
  cband         = TRUE,
  biters        = 100,
  cl            = 1,
  gt_type       = "qtt",
  probs         = seq(0.1, 0.9, 0.1),
  pre_copula    = "short"
)

summary(res_cov)


# for MCAID share 
## Start from the post-2c (balanced, dedup'd, stable-gname) panel.
## If you no longer have that intermediate object, rerun 1, 2a-2c first.

## 2d (medicaid share version). Drop hospitals with any missing mcaid_prop_discharges
hospitals_with_na_mcaid <- hospdata_analysis_qtt %>%
  filter(is.na(mcaid_prop_discharges)) %>%
  pull(mcrnum) %>%
  unique()

hospdata_analysis_qtt_mcaid <- hospdata_analysis_qtt %>%
  filter(!mcrnum %in% hospitals_with_na_mcaid)

## 2e. Drop cohorts with no observed post-treatment period
max_year <- max(hospdata_analysis_qtt_mcaid$year)
hospdata_analysis_qtt_mcaid <- hospdata_analysis_qtt_mcaid %>%
  filter(qtt_gname == 0 | qtt_gname <= max_year)

## 3. Confirm clean
stopifnot(
  nrow(hospdata_analysis_qtt_mcaid %>% count(mcrnum, year) %>% filter(n > 1)) == 0,
  all((hospdata_analysis_qtt_mcaid %>% count(mcrnum))$n == 25),
  sum(is.na(hospdata_analysis_qtt_mcaid$mcaid_prop_discharges)) == 0
)

n_distinct(hospdata_analysis_qtt_mcaid$mcrnum)
table(hospdata_analysis_qtt_mcaid$qtt_gname)

# # Restrict to the same cohorts your ATT actually uses (2003-2018 validity window)
hospdata_analysis_qtt_restricted <- hospdata_analysis_qtt %>%
  filter(qtt_gname == 0 | (qtt_gname >= 2003 & qtt_gname <= 2018))

table(hospdata_analysis_qtt_restricted$qtt_gname)

## 4. Estimate
res_mcaid <- panel_qtt(
  yname         = "mcaid_prop_discharges",
  gname         = "qtt_gname",
  tname         = "year",
  idname        = "mcrnum",
  data          = hospdata_analysis_qtt_mcaid,
  xformula      = ~1,
  weightsname   = NULL,
  control_group = "notyettreated",
  anticipation  = 0,
  alp           = 0.05,
  cband         = TRUE,
  biters        = 100,
  cl            = 1,
  gt_type       = "qtt",
  probs         = seq(0.1, 0.9, 0.1),
  pre_copula    = "short"
)



summary(res_mcaid)

library(ggplot2)

plot_df_mcaid <- res_mcaid$overall

# Set y_lim after looking at the printed table above -- start with a guess
# and tighten/loosen based on where most of the bands actually sit.
y_lim <- range(c(plot_df_mcaid$lower_pw, plot_df_mcaid$upper_pw)) * 1.3

plot_df_mcaid$clipped <- plot_df_mcaid$lower_ub < y_lim[1] | plot_df_mcaid$upper_ub > y_lim[2]

ggplot(plot_df_mcaid, aes(x = probs, y = qtt)) +
  geom_ribbon(aes(ymin = pmax(lower_ub, y_lim[1]), ymax = pmin(upper_ub, y_lim[2])),
              fill = "grey70", alpha = 0.4) +
  geom_errorbar(aes(ymin = pmax(lower_pw, y_lim[1]), ymax = pmin(upper_pw, y_lim[2])),
                width = 0.01, color = "grey30") +
  geom_point(size = 2.5, color = "#1f3864") +
  geom_line(color = "#1f3864") +
  geom_point(data = subset(plot_df_mcaid, clipped), aes(y = y_lim[2] * 0.97),
             shape = 24, size = 2, color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_cartesian(ylim = y_lim) +
  scale_x_continuous(breaks = plot_df_mcaid$probs) +
  labs(
    title    = "Quantile Treatment Effect on the Treated (QTT)",
    subtitle = "Medicaid Share of Discharges — Provider Tax Adoption",
    x        = "Quantile (\u03c4)",
    y        = "QTT",
    caption  = "Shaded band: 95% uniform confidence band. Error bars: pointwise 95% CI.\nTriangle markers indicate quantiles where the uniform band extends beyond the plotted range."
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, size = 9, color = "grey30"))

ggsave("qtt_mcaid_plot.png", width = 7.5, height = 5.5, dpi = 300)
