#=============================================================
# Quantile Treatment Effect on the Treated (QTT) -- margin_updated
# Callaway & Li (2019), via the qte::panel_qtt() estimator
# Same as callawayli_qtt.R, with margin_updated in place of op_margin
#=============================================================

library(qte)
library(BMisc)

#=============================================================
# 0. margin_updated (same definition as margin_updated.R)
#=============================================================
hospdata_analysis <- hospdata_analysis %>%
  mutate(
    margin_updated = if_else(net_pat_rev > 0,
                             (net_pat_rev - tot_operating_exp) / net_pat_rev,
                             NA_real_),
    margin_updated = if_else(abs(margin_updated) <= 1, margin_updated, NA_real_)
  )

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
#=============================================================

## 2a. Drop duplicate hospital-year rows (likely subunit filings)
dup_hospital_years <- hospdata_analysis_qtt %>%
  count(mcrnum, year) %>%
  filter(n > 1) %>%
  select(mcrnum, year)

hospdata_analysis_qtt <- hospdata_analysis_qtt %>%
  anti_join(dup_hospital_years, by = c("mcrnum", "year"))

## 2b. Drop hospitals whose qtt_gname isn't constant over their own panel
unstable_gname_ids <- hospdata_analysis_qtt %>%
  group_by(mcrnum) %>%
  summarise(n_distinct_gname = n_distinct(qtt_gname), .groups = "drop") %>%
  filter(n_distinct_gname > 1) %>%
  pull(mcrnum)

hospdata_analysis_qtt <- hospdata_analysis_qtt %>%
  filter(!mcrnum %in% unstable_gname_ids)

## 2c. Restrict to hospitals present in every year
balanced_ids <- hospdata_analysis_qtt %>%
  count(mcrnum) %>%
  filter(n == 25) %>%
  pull(mcrnum)

hospdata_analysis_qtt <- hospdata_analysis_qtt %>%
  filter(mcrnum %in% balanced_ids)

## 2d. Drop hospitals with any missing margin_updated
hospitals_with_na_margin <- hospdata_analysis_qtt %>%
  filter(is.na(margin_updated)) %>%
  pull(mcrnum) %>%
  unique()

hospdata_analysis_qtt <- hospdata_analysis_qtt %>%
  filter(!mcrnum %in% hospitals_with_na_margin)

## 2e. Drop cohorts with no observed post-treatment period
max_year <- max(hospdata_analysis_qtt$year)
hospdata_analysis_qtt <- hospdata_analysis_qtt %>%
  filter(qtt_gname == 0 | qtt_gname <= max_year)

#=============================================================
# 3. Confirm the panel is clean before estimating
#=============================================================
stopifnot(
  nrow(hospdata_analysis_qtt %>% count(mcrnum, year) %>% filter(n > 1)) == 0,
  all((hospdata_analysis_qtt %>% count(mcrnum))$n == 25),
  sum(is.na(hospdata_analysis_qtt$margin_updated)) == 0
)

n_distinct(hospdata_analysis_qtt$mcrnum)
table(hospdata_analysis_qtt$qtt_gname)

#=============================================================
# 4. Estimate the QTT
#=============================================================
res_margin_updated <- panel_qtt(
  yname         = "margin_updated",
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

summary(res_margin_updated)

#=============================================================
# 5. Plot (same as the op_margin QTT plot, no title/caption)
#=============================================================
plot_margin <- res_margin_updated$overall

# Choose a y-range that comfortably covers everything except the blown-out tail
y_lim <- c(-0.09, 0.09)

# Flag which bands get clipped, so we can mark them instead of silently cutting them
plot_margin$clipped <- plot_margin$lower_ub < y_lim[1] | plot_margin$upper_ub > y_lim[2]

ggplot(plot_margin, aes(x = probs, y = qtt)) +
  geom_ribbon(aes(ymin = pmax(lower_ub, y_lim[1]), ymax = pmin(upper_ub, y_lim[2])),
              fill = "grey80", alpha = 0.4) +
  geom_errorbar(aes(ymin = pmax(lower_pw, y_lim[1]), ymax = pmin(upper_pw, y_lim[2])),
                width = 0.01, color = "grey30") +
  geom_point(size = 2.5, color = "#2484d2") +
  geom_line(color = "#2484d2") +
  # little arrow glyphs marking where a band actually extends past the frame
  geom_point(data = subset(plot_margin, clipped), aes(y = y_lim[2] * 0.97),
             shape = 24, size = 2, color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_cartesian(ylim = y_lim) +
  scale_x_continuous(breaks = plot_margin$probs) +
  labs(
    x = "Quantile (τ)",
    y = "QTT"
  ) +
  theme_minimal(base_size = 13)

ggsave("margin_updated_qtt_plot.png", width = 10, height = 8, dpi = 300)
