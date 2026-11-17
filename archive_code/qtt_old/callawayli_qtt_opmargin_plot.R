#=============================================================
# Operating Margin QTT -- estimate, save, and plot
# Callaway & Li (2019) via qte::panel_qtt(), same sample construction
# and call as callawayli_qtt.R. Results are saved so the figure can be
# restyled without re-estimating.
#
# Plot is styled to match the event studies from run_stacked_did()
# (6_events_paper.R): post-treatment blue, width-0.3 error bars, size-3
# points, solid black zero line, theme_minimal(14), no title/legend/
# caption (caption goes in the LaTeX).
#
# Outputs:
#   qtt_opmargin_results.rds   panel_qtt() result object
#   qtt_opmargin_results.csv   tau, QTT, SE, pointwise + uniform CIs
#   opmargin_qtt_plot.png
#=============================================================

library(dplyr)
library(ggplot2)
library(qte)

hospdata_analysis <- read.csv(paste0(data_output_path, "hospdata_analysis.csv"))

#-------------------------------------------------------------
# Sample (identical to callawayli_qtt.R steps 1-2e)
#-------------------------------------------------------------
hospdata_analysis_qtt <- hospdata_analysis %>%
  mutate(qtt_gname = case_when(
    is.na(firsttax_num)  ~ 0,
    firsttax_num == 2004 ~ NA_real_,
    TRUE                 ~ firsttax_num
  )) %>%
  filter(!is.na(qtt_gname))

dup_hospital_years <- hospdata_analysis_qtt %>% count(mcrnum, year) %>% filter(n > 1)
hospdata_analysis_qtt <- hospdata_analysis_qtt %>% anti_join(dup_hospital_years, by = c("mcrnum", "year"))

unstable_gname_ids <- hospdata_analysis_qtt %>%
  group_by(mcrnum) %>%
  summarise(k = n_distinct(qtt_gname), .groups = "drop") %>%
  filter(k > 1) %>%
  pull(mcrnum)

balanced_ids <- hospdata_analysis_qtt %>%
  filter(!mcrnum %in% unstable_gname_ids) %>%
  count(mcrnum) %>%
  filter(n == 25) %>%
  pull(mcrnum)

hospitals_with_na_margin <- hospdata_analysis_qtt %>% filter(is.na(op_margin)) %>% pull(mcrnum) %>% unique()

hospdata_analysis_qtt <- hospdata_analysis_qtt %>%
  filter(mcrnum %in% balanced_ids, !mcrnum %in% hospitals_with_na_margin) %>%
  filter(qtt_gname == 0 | qtt_gname <= max(year))

cat("QTT sample:", n_distinct(hospdata_analysis_qtt$mcrnum), "hospitals\n")

#-------------------------------------------------------------
# Estimate
#-------------------------------------------------------------
set.seed(20261008)
res <- panel_qtt(
  yname = "op_margin", gname = "qtt_gname", tname = "year", idname = "mcrnum",
  data = hospdata_analysis_qtt, xformula = ~1, weightsname = NULL,
  control_group = "notyettreated", anticipation = 0, alp = 0.05, cband = TRUE,
  biters = 100, cl = 1, gt_type = "qtt", probs = seq(0.1, 0.9, 0.1),
  pre_copula = "short"
)
saveRDS(res, "qtt_opmargin_results.rds")

#-------------------------------------------------------------
# Tidy results
#-------------------------------------------------------------
qtt_df <- data.frame(tau = res$probs, qtt = res$qte, se = res$qte.se) %>%
  mutate(
    lb = qtt - 1.96 * se,
    ub = qtt + 1.96 * se,
    ucb_lb = if (!is.null(res$qte.lower)) res$qte.lower else NA_real_,
    ucb_ub = if (!is.null(res$qte.upper)) res$qte.upper else NA_real_
  )
print(qtt_df, digits = 4)
write.csv(qtt_df, "qtt_opmargin_results.csv", row.names = FALSE)

#-------------------------------------------------------------
# Plot -- event-study styling
#-------------------------------------------------------------
p <- ggplot(qtt_df, aes(x = tau, y = qtt)) +
  geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.03, linewidth = 0.8, color = "#2484d2") +
  geom_point(size = 3, color = "#2484d2") +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 0.4) +
  labs(x = "Quantile", y = " ") +
  scale_x_continuous(breaks = seq(0.1, 0.9, 0.1)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 11, color = "black"),
    panel.grid.major = element_line(color = "#f2f2f2", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.position = "none"
  )

print(p)
ggsave("opmargin_qtt_plot.png", plot = p, width = 10, height = 8, dpi = 300)
cat("Saved: opmargin_qtt_plot.png\n")
