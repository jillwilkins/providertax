
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
# 3a. op_margin
#---------------------------------------------------------------------
hospitals_with_na_margin <- hospdata_matched_base %>%
  filter(is.na(op_margin)) %>%
  pull(mcrnum) %>%
  unique()

hospdata_matched_margin <- hospdata_matched_base %>%
  filter(!mcrnum %in% hospitals_with_na_margin)

cat("op_margin sample -- hospitals:", n_distinct(hospdata_matched_margin$mcrnum), "\n")
print(table(hospdata_matched_margin$qtt_gname))

# ---------------------------------------------------------------------
# 4a. estimation op_margin 
# ---------------------------------------------------------------------
res_margin_matched <- panel_qtt(
  yname         = "op_margin",
  gname         = "qtt_gname",
  tname         = "year",
  idname        = "mcrnum",
  data          = hospdata_matched_margin,
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

summary(res_margin_matched)

# ---------------------------------------------------------------------
# 5a. Plot op_margin
# ---------------------------------------------------------------------

plot_margin <- res_margin_matched$overall

# Choose a y-range that comfortably covers everything except the blown-out tail
y_lim <- c(-0.09, 0.09)

# Flag which bands get clipped, so we can mark them instead of silently cutting them
plot_margin$clipped <- plot_margin$lower_ub < y_lim[1] | plot_margin$upper_ub > y_lim[2]

ggplot(plot_margin, aes(x = probs, y = qtt)) +
  geom_ribbon(aes(ymin = pmax(lower_ub, y_lim[1]), ymax = pmin(upper_ub, y_lim[2])),
              fill = "grey80", alpha = 0.4) +
  geom_errorbar(aes(ymin = pmax(lower_pw, y_lim[1]), ymax = pmin(upper_pw, y_lim[2])),
                width = 0.02, linewidth = 0.8, color = "#2484d2") +
  geom_point(size = 2.5, color = "#2484d2") +
  geom_line(color = "#2484d2") +
  # little arrow glyphs marking where a band actually extends past the frame
  geom_point(data = subset(plot_margin, clipped), aes(y = y_lim[2] * 0.97),
             shape = 24, size = 2, color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_cartesian(ylim = y_lim) +
  scale_x_continuous(breaks = plot_margin$probs) +
  labs(
    x        = "Quantile (\u03c4)",
    y        = "QTT"
  ) +
  theme_minimal(base_size = 13) +
 theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, size = 9, color = "grey30"),
        panel.grid = element_line(color = "grey95"))


ggsave("qtt_margin_matched_plot.png", width = 10, height = 8, dpi = 300, background = "transparent")

#---------------------------------------------------------------------
# op margin restricted window 
#---------------------------------------------------------------------
qtt_restricted_margin <- res_margin_matched$dynamic %>%
  filter(e >= 0, e <= 4) %>%
  group_by(probs) %>%
  summarise(qtt_avg = mean(qtt), .groups = "drop")

print(qtt_restricted_margin)

#---------------------------------------------------------------------
# 3b. mcaid_prop_discharges
#---------------------------------------------------------------------
hospitals_with_na_mcaid <- hospdata_matched_base %>%
  filter(is.na(mcaid_prop_discharges)) %>%
  pull(mcrnum) %>%
  unique()

hospdata_matched_mcaid <- hospdata_matched_base %>%
  filter(!mcrnum %in% hospitals_with_na_mcaid)

cat("mcaid_prop_discharges sample -- hospitals:", n_distinct(hospdata_matched_mcaid$mcrnum), "\n")
print(table(hospdata_matched_mcaid$qtt_gname))

# ---------------------------------------------------------------------
# 4b. estimation mcaid_prop_discharges
# ---------------------------------------------------------------------

res_mcaid_matched <- panel_qtt(
  yname         = "mcaid_prop_discharges",
  gname         = "qtt_gname",
  tname         = "year",
  idname        = "mcrnum",
  data          = hospdata_matched_mcaid,
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

summary(res_mcaid_matched)

#---------------------------------------------------------------------
# 5b. Plot mcaid_prop_discharges    
#---------------------------------------------------------------------

plot_df_mcaid <- res_mcaid_matched$overall

# Check magnitudes first to pick a sensible y_lim
print(plot_df_mcaid)

y_lim <- range(c(plot_df_mcaid$lower_pw, plot_df_mcaid$upper_pw)) * 1.3

plot_df_mcaid$clipped <- plot_df_mcaid$lower_ub < y_lim[1] | plot_df_mcaid$upper_ub > y_lim[2]

ggplot(plot_df_mcaid, aes(x = probs, y = qtt)) +
  geom_ribbon(aes(ymin = pmax(lower_ub, y_lim[1]), ymax = pmin(upper_ub, y_lim[2])),
              fill = "grey80", alpha = 0.4) +
  geom_errorbar(aes(ymin = pmax(lower_pw, y_lim[1]), ymax = pmin(upper_pw, y_lim[2])),
                width = 0.01, color = "grey30") +
  geom_point(size = 2.5, color = "#2484d2") +
  geom_line(color = "#2484d2") +
  geom_point(data = subset(plot_df_mcaid, clipped), aes(y = y_lim[2] * 0.97),
             shape = 24, size = 2, color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_cartesian(ylim = y_lim) +
  scale_x_continuous(breaks = plot_df_mcaid$probs) +
  labs(
    title    = "Quantile Treatment Effect on the Treated (QTT)",
    subtitle = "Medicaid Share of Discharges — Provider Tax Adoption (Matched Sample)",
    x        = "Quantile (\u03c4)",
    y        = "QTT",
    caption  = "Shaded band: 95% uniform confidence band. Error bars: pointwise 95% CI.\nTriangle markers indicate quantiles where the uniform band extends beyond the plotted range."
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, size = 9, color = "grey30"),
        panel.grid = element_line(color = "grey95"))

ggsave("qtt_mcaid_matched_plot.png", width = 10, height = 10, dpi = 300, background = "transparent")

#---------------------------------------------------------------------
# 4b. Restricted-horizon comparison (e = 0..4, matching the stack's window)
#---------------------------------------------------------------------
qtt_restricted_mcaid <- res_mcaid_matched$dynamic %>%
  filter(e >= 0, e <= 4) %>%
  group_by(probs) %>%
  summarise(qtt_avg = mean(qtt), .groups = "drop")

print(qtt_restricted_mcaid)

# -----------------------------------------------------------------------
# restrcicted-horizon comparison (e = 0..4, matching the stack's window)
#-----------------------------------------------------------------------

library(ggplot2)
library(dplyr)


# --- op_margin ---
plot_df_margin_restricted <- res_margin_matched$dynamic %>%
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

ggplot(plot_df_margin_restricted, aes(x = probs, y = qtt)) +
  geom_ribbon(aes(ymin = lower_ub, ymax = upper_ub), fill = "grey70", alpha = 0.4) +
  geom_errorbar(aes(ymin = lower_pw, ymax = upper_pw), width = 0.01, color = "grey30") +
  geom_point(size = 2.5, color = "#1f3864") +
  geom_line(color = "#1f3864") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = plot_df_margin_restricted$probs) +
  labs(
    title    = "Quantile Treatment Effect on the Treated (QTT)",
    subtitle = "Operating Margin — Restricted to e = 0..4 (Matched Sample)",
    x        = "Quantile (\u03c4)",
    y        = "QTT"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("qtt_margin_restricted_plot.png", width = 7.5, height = 5.5, dpi = 300)

# --- mcaid_prop_discharges ---
plot_df_mcaid_restricted <- res_mcaid_matched$dynamic %>%
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

ggplot(plot_df_mcaid_restricted, aes(x = probs, y = qtt)) +
  geom_ribbon(aes(ymin = lower_ub, ymax = upper_ub), fill = "grey70", alpha = 0.4) +
  geom_errorbar(aes(ymin = lower_pw, ymax = upper_pw), width = 0.01, color = "grey30") +
  geom_point(size = 2.5, color = "#2dbfb0") +
  geom_line(color = "#2dbfb0") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = plot_df_mcaid_restricted$probs) +
  labs(
    title    = "Quantile Treatment Effect on the Treated (QTT)",
    subtitle = "Medicaid Share of Discharges — Restricted to e = 0..4 (Matched Sample)",
    x        = "Quantile (\u03c4)",
    y        = "QTT"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("qtt_mcaid_restricted_plot.png", width = 7.5, height = 5.5, dpi = 300)