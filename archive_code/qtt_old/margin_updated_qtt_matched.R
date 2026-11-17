#---------------------------------------------------------------------
# Matched QTT for margin_updated
# Same as callaway_li_match.R (op_margin section), with margin_updated
# in place of op_margin. Requires hospdata_analysis and stacked_data.
#---------------------------------------------------------------------

library(qte)
library(dplyr)
library(ggplot2)

#---------------------------------------------------------------------
# 0. margin_updated (same definition as margin_updated.R)
#---------------------------------------------------------------------
hospdata_analysis <- hospdata_analysis %>%
  mutate(
    margin_updated = if_else(net_pat_rev > 0,
                             (net_pat_rev - tot_operating_exp) / net_pat_rev,
                             NA_real_),
    margin_updated = if_else(abs(margin_updated) <= 1, margin_updated, NA_real_)
  )

#---------------------------------------------------------------------
# 1. Build the matched base sample
#---------------------------------------------------------------------
hospdata_matched_base <- hospdata_analysis %>%
  filter(mcrnum %in% unique(stacked_data$mcrnum)) %>%
  filter(year >= 2003, year <= 2022) %>%
  mutate(
    qtt_gname = case_when(
      is.na(firsttax_num)  ~ 0,
      firsttax_num == 2004 ~ NA_real_,
      TRUE                 ~ firsttax_num
    )
  ) %>%
  filter(!is.na(qtt_gname)) %>%
  filter(qtt_gname == 0 | (qtt_gname >= 2003 & qtt_gname <= 2018))

#---------------------------------------------------------------------
# 2. Structural cleaning
#---------------------------------------------------------------------
dup_hospital_years <- hospdata_matched_base %>%
  count(mcrnum, year) %>%
  filter(n > 1) %>%
  select(mcrnum, year)

hospdata_matched_base <- hospdata_matched_base %>%
  anti_join(dup_hospital_years, by = c("mcrnum", "year"))

unstable_gname_ids <- hospdata_matched_base %>%
  group_by(mcrnum) %>%
  summarise(n_distinct_gname = n_distinct(qtt_gname), .groups = "drop") %>%
  filter(n_distinct_gname > 1) %>%
  pull(mcrnum)

hospdata_matched_base <- hospdata_matched_base %>%
  filter(!mcrnum %in% unstable_gname_ids)

n_required <- length(2003:2022)

balanced_ids <- hospdata_matched_base %>%
  count(mcrnum) %>%
  filter(n == n_required) %>%
  pull(mcrnum)

hospdata_matched_base <- hospdata_matched_base %>%
  filter(mcrnum %in% balanced_ids)

cat("Matched base sample -- hospitals:", n_distinct(hospdata_matched_base$mcrnum), "\n")
print(table(hospdata_matched_base$qtt_gname))

#---------------------------------------------------------------------
# 3. margin_updated
#---------------------------------------------------------------------
hospitals_with_na_mu <- hospdata_matched_base %>%
  filter(is.na(margin_updated)) %>%
  pull(mcrnum) %>%
  unique()

hospdata_matched_mu <- hospdata_matched_base %>%
  filter(!mcrnum %in% hospitals_with_na_mu)

cat("margin_updated sample -- hospitals:", n_distinct(hospdata_matched_mu$mcrnum), "\n")
print(table(hospdata_matched_mu$qtt_gname))

# ---------------------------------------------------------------------
# 4. estimation margin_updated
# ---------------------------------------------------------------------
res_mu_matched <- panel_qtt(
  yname         = "margin_updated",
  gname         = "qtt_gname",
  tname         = "year",
  idname        = "mcrnum",
  data          = hospdata_matched_mu,
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

summary(res_mu_matched)

# ---------------------------------------------------------------------
# 5. Plot margin_updated
# ---------------------------------------------------------------------
plot_mu <- res_mu_matched$overall

# Choose a y-range that comfortably covers everything except the blown-out tail
y_lim <- c(-0.09, 0.09)

# Flag which bands get clipped, so we can mark them instead of silently cutting them
plot_mu$clipped <- plot_mu$lower_ub < y_lim[1] | plot_mu$upper_ub > y_lim[2]

ggplot(plot_mu, aes(x = probs, y = qtt)) +
  # pale orange reference lines at the quartile cutpoints (compare to ATT quartile split)
  geom_vline(xintercept = c(0.25, 0.5, 0.75), linetype = "dashed",
             color = "#f5b97a", linewidth = 0.6) +
  geom_ribbon(aes(ymin = pmax(lower_ub, y_lim[1]), ymax = pmin(upper_ub, y_lim[2])),
              fill = "grey80", alpha = 0.4) +
  geom_errorbar(aes(ymin = pmax(lower_pw, y_lim[1]), ymax = pmin(upper_pw, y_lim[2])),
                width = 0.02, linewidth = 0.8, color = "#2484d2") +
  geom_point(size = 2.5, color = "#2484d2") +
  geom_line(color = "#2484d2") +
  # little arrow glyphs marking where a band actually extends past the frame
  geom_point(data = subset(plot_mu, clipped), aes(y = y_lim[2] * 0.97),
             shape = 24, size = 2, color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_cartesian(ylim = y_lim) +
  scale_x_continuous(breaks = plot_mu$probs) +
  labs(
    x        = "Quantile (τ)",
    y        = "QTT"
  ) +
  theme_minimal(base_size = 13) +
 theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, size = 9, color = "grey30"),
        panel.grid = element_line(color = "grey95"))

ggsave("qtt_margin_updated_matched_plot.png", width = 10, height = 8, dpi = 300, background = "transparent")

#---------------------------------------------------------------------
# margin_updated restricted window (e = 0..4, matching the stack's window)
#---------------------------------------------------------------------
qtt_restricted_mu <- res_mu_matched$dynamic %>%
  filter(e >= 0, e <= 4) %>%
  group_by(probs) %>%
  summarise(qtt_avg = mean(qtt), .groups = "drop")

print(qtt_restricted_mu)

plot_df_mu_restricted <- res_mu_matched$dynamic %>%
  filter(e >= 0, e <= 4) %>%
  group_by(probs) %>%
  summarise(
    qtt      = mean(qtt),
    lower_pw = mean(lower_pw),
    upper_pw = mean(upper_pw),
    lower_ub = mean(lower_ub),
    upper_ub = mean(upper_ub),
    .groups  = "drop"
  )

ggplot(plot_df_mu_restricted, aes(x = probs, y = qtt)) +
  geom_ribbon(aes(ymin = lower_ub, ymax = upper_ub), fill = "grey70", alpha = 0.4) +
  geom_errorbar(aes(ymin = lower_pw, ymax = upper_pw), width = 0.01, color = "grey30") +
  geom_point(size = 2.5, color = "#1f3864") +
  geom_line(color = "#1f3864") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = plot_df_mu_restricted$probs) +
  labs(
    x        = "Quantile (τ)",
    y        = "QTT"
  ) +
  theme_minimal(base_size = 13)

ggsave("qtt_margin_updated_restricted_plot.png", width = 7.5, height = 5.5, dpi = 300)

#---------------------------------------------------------------------
# 6. Re-estimate at quartiles (tau = 0.25, 0.50, 0.75)
#---------------------------------------------------------------------
res_mu_matched_q <- panel_qtt(
  yname         = "margin_updated",
  gname         = "qtt_gname",
  tname         = "year",
  idname        = "mcrnum",
  data          = hospdata_matched_mu,
  xformula      = ~1,
  weightsname   = NULL,
  control_group = "notyettreated",
  anticipation  = 0,
  alp           = 0.05,
  cband         = TRUE,
  biters        = 100,
  cl            = 1,
  gt_type       = "qtt",
  probs         = c(0.25, 0.5, 0.75),
  pre_copula    = "short"
)

summary(res_mu_matched_q)

# Plot (same style as section 5)
plot_mu_q <- res_mu_matched_q$overall

y_lim <- c(-0.09, 0.09)

plot_mu_q$clipped <- plot_mu_q$lower_ub < y_lim[1] | plot_mu_q$upper_ub > y_lim[2]

ggplot(plot_mu_q, aes(x = probs, y = qtt)) +
  geom_ribbon(aes(ymin = pmax(lower_ub, y_lim[1]), ymax = pmin(upper_ub, y_lim[2])),
              fill = "grey80", alpha = 0.4) +
  geom_errorbar(aes(ymin = pmax(lower_pw, y_lim[1]), ymax = pmin(upper_pw, y_lim[2])),
                width = 0.02, linewidth = 0.8, color = "#2484d2") +
  geom_point(size = 2.5, color = "#2484d2") +
  geom_line(color = "#2484d2") +
  geom_point(data = subset(plot_mu_q, clipped), aes(y = y_lim[2] * 0.97),
             shape = 24, size = 2, color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_cartesian(ylim = y_lim) +
  scale_x_continuous(breaks = plot_mu_q$probs) +
  labs(
    x        = "Quantile (τ)",
    y        = "QTT"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid = element_line(color = "grey95"))

ggsave("qtt_margin_updated_matched_quartiles_plot.png", width = 10, height = 8, dpi = 300, background = "transparent")

# Restricted window (e = 0..4), quartiles
qtt_restricted_mu_q <- res_mu_matched_q$dynamic %>%
  filter(e >= 0, e <= 4) %>%
  group_by(probs) %>%
  summarise(qtt_avg = mean(qtt), .groups = "drop")

print(qtt_restricted_mu_q)
