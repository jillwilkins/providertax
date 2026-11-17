#=====================================================================
# QTT FOR OPERATING MARGIN -- MATCHED SAMPLE (the paper figure)
# Callaway & Li (2019) via qte::panel_qtt()
#
# Matched sample = hospitals in the stacked DiD sample, years 2003-2022,
# cohorts adopting by 2018, balanced over 2003-2022 (from
# callaway_li_match.R).
#
# Run order: 5_buildstacked.R (for stacked_data), then this script.
# Output: events_for_paper/qtt_opmargin_matched.png
#         qtt_opmargin_matched.rds (saved results; replot without re-estimating)
#=====================================================================

library(dplyr)
library(ggplot2)
library(qte)

# ---- Settings -------------------------------------------------------
outcome  <- "op_margin"   # (NPR - opex) / NPR, defined in 1_data.R
probs    <- seq(0.1, 0.9, 0.1)
y_lim    <- c(-0.09, 0.09)
fig_file <- "events_for_paper/qtt_opmargin_matched.png"
rds_file <- "qtt_opmargin_matched.rds"

if (!exists("stacked_data")) stop("Run 5_buildstacked.R first (needs stacked_data).")
if (!exists("hospdata_analysis")) {
  hospdata_analysis <- read.csv(paste0(data_output_path, "hospdata_analysis.csv"))
}

#---------------------------------------------------------------------
# 1. Matched sample
#---------------------------------------------------------------------
qtt_data <- hospdata_analysis %>%
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

# Drop duplicate hospital-years
dup_hospital_years <- qtt_data %>% count(mcrnum, year) %>% filter(n > 1)
qtt_data <- qtt_data %>% anti_join(dup_hospital_years, by = c("mcrnum", "year"))

# Drop hospitals whose cohort changes across their own years
unstable_ids <- qtt_data %>%
  group_by(mcrnum) %>%
  summarise(k = n_distinct(qtt_gname), .groups = "drop") %>%
  filter(k > 1) %>%
  pull(mcrnum)
qtt_data <- qtt_data %>% filter(!mcrnum %in% unstable_ids)

# Balanced over 2003-2022
balanced_ids <- qtt_data %>% count(mcrnum) %>% filter(n == length(2003:2022)) %>% pull(mcrnum)
qtt_data <- qtt_data %>% filter(mcrnum %in% balanced_ids)

# Drop hospitals with any missing outcome
na_ids <- qtt_data %>% filter(is.na(.data[[outcome]])) %>% pull(mcrnum) %>% unique()
qtt_data <- qtt_data %>% filter(!mcrnum %in% na_ids)

cat("QTT sample --", outcome, "-- hospitals:", n_distinct(qtt_data$mcrnum), "\n")
print(table(qtt_data %>% distinct(mcrnum, qtt_gname) %>% pull(qtt_gname)))

#---------------------------------------------------------------------
# 2. Estimate and save
#---------------------------------------------------------------------
res_qtt <- panel_qtt(
  yname         = outcome,
  gname         = "qtt_gname",
  tname         = "year",
  idname        = "mcrnum",
  data          = qtt_data,
  xformula      = ~1,
  weightsname   = NULL,
  control_group = "notyettreated",
  anticipation  = 0,
  alp           = 0.05,
  cband         = TRUE,
  biters        = 100,
  cl            = 1,
  gt_type       = "qtt",
  probs         = probs,
  pre_copula    = "short"
)

summary(res_qtt)
saveRDS(res_qtt, rds_file)
# To replot later without re-estimating: res_qtt <- readRDS(rds_file)

#---------------------------------------------------------------------
# 3. Figure
#---------------------------------------------------------------------
plot_df <- res_qtt$overall
plot_df$clipped <- plot_df$lower_ub < y_lim[1] | plot_df$upper_ub > y_lim[2]

ggplot(plot_df, aes(x = probs, y = qtt)) +
  # pale orange reference lines at the quartile cutpoints
  geom_vline(xintercept = c(0.25, 0.5, 0.75), linetype = "dashed",
             color = "#f5b97a", linewidth = 0.6) +
  # uniform band (grey), pointwise CI (blue bars)
  geom_ribbon(aes(ymin = pmax(lower_ub, y_lim[1]), ymax = pmin(upper_ub, y_lim[2])),
              fill = "grey80", alpha = 0.4) +
  geom_errorbar(aes(ymin = pmax(lower_pw, y_lim[1]), ymax = pmin(upper_pw, y_lim[2])),
                width = 0.02, linewidth = 0.8, color = "#2484d2") +
  geom_point(size = 2.5, color = "#2484d2") +
  geom_line(color = "#2484d2") +
  # triangles where the uniform band runs past the plotted range
  geom_point(data = subset(plot_df, clipped), aes(y = y_lim[2] * 0.97),
             shape = 24, size = 2, color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_cartesian(ylim = y_lim) +
  scale_x_continuous(breaks = plot_df$probs) +
  labs(x = "Quantile (τ)", y = "QTT") +
  theme_minimal(base_size = 13) +
  theme(panel.grid = element_line(color = "grey95"))

ggsave(fig_file, width = 10, height = 8, dpi = 300, background = "transparent")

# Values for the LaTeX caption (uniform band at any clipped quantile)
print(plot_df[, c("probs", "qtt", "lower_pw", "upper_pw", "lower_ub", "upper_ub")], digits = 3)
