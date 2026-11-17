# ==============================================================================
# STACKED DiD: RAW vs WINSORIZED op_bed AND npr_bed
# Event-study coefficients (rel_year -4..4, ref = -1) from the main stacked
# spec (same as run_stacked_did() in 6_events_paper.R), for each outcome raw,
# winsorized at the 1st/95th percentile (the same convention as
# uncomp_bed_w95 in the quartile/decile tables and alt_estimators_comparison.R),
# and winsorized at the 1st/99th percentile.
# Cutoffs are computed on stacked_data.
#
# Requires stacked_data (5_buildstacked.R). Outputs (in rob_alt/):
#   stacked_winsorized_opbed_nprbed.png
#   stacked_winsorized_opbed_nprbed.csv
# ==============================================================================

library(dplyr)
library(fixest)
library(purrr)
library(ggplot2)

winsorize_var <- function(x, probs = c(0.01, 0.95)) {
  bounds <- quantile(x, probs = probs, na.rm = TRUE)
  pmin(pmax(x, bounds[1]), bounds[2])
}

stacked_data <- stacked_data %>%
  mutate(op_bed_w95  = winsorize_var(op_bed),
         npr_bed_w95 = winsorize_var(npr_bed),
         op_bed_w99  = winsorize_var(op_bed,  c(0.01, 0.99)),
         npr_bed_w99 = winsorize_var(npr_bed, c(0.01, 0.99)))

v_raw <- "Raw"
v_99  <- "Winsorized (1st/99th pctile)"
v_95  <- "Winsorized (1st/95th pctile)"


spec_info <- list(
  list(var = "op_bed",      outcome = "Operating Expenses per Bed ($)",  version = v_raw),
  list(var = "op_bed_w99",  outcome = "Operating Expenses per Bed ($)",  version = v_99),
  list(var = "op_bed_w95",  outcome = "Operating Expenses per Bed ($)",  version = v_95),
  list(var = "npr_bed",     outcome = "Net Patient Revenue per Bed ($)", version = v_raw),
  list(var = "npr_bed_w99", outcome = "Net Patient Revenue per Bed ($)", version = v_99),
  list(var = "npr_bed_w95", outcome = "Net Patient Revenue per Bed ($)", version = v_95)
)

es_stacked <- function(outcome_var) {
  m <- feols(
    as.formula(paste0(outcome_var, " ~ i(rel_year, treated, ref = -1) + exp_status + median_income_pre | mcrnum^df + year^df")),
    data = stacked_data, weights = ~Q_weight, cluster = ~state
  )
  agg <- aggregate(m, agg = c("ATT" = "rel_year::[^-]+:treated"))
  cat(sprintf("%-12s pooled ATT = %s (SE %s)\n", outcome_var,
              scales::comma(agg["ATT", "Estimate"]), scales::comma(agg["ATT", "Std. Error"])))
  ct <- coeftable(m)
  ct <- ct[grepl("^rel_year::", rownames(ct)), , drop = FALSE]
  bind_rows(
    data.frame(rel_year = as.numeric(sub("rel_year::(-?[0-9]+):treated", "\\1", rownames(ct))),
               Estimate = ct[, "Estimate"], SE = ct[, "Std. Error"]),
    data.frame(rel_year = -1, Estimate = 0, SE = 0)
  )
}

wins_results <- map_df(spec_info, function(si) {
  es_stacked(si$var) %>% mutate(outcome_var = si$var, Outcome = si$outcome, Version = si$version)
}) %>%
  mutate(
    lb = Estimate - 1.96 * SE,
    ub = Estimate + 1.96 * SE,
    Outcome = factor(Outcome, levels = unique(sapply(spec_info, function(s) s$outcome))),
    Version = factor(Version, levels = c(v_raw, v_99, v_95))
  ) %>%
  arrange(Outcome, Version, rel_year)

write.csv(wins_results, "rob_alt/stacked_winsorized_opbed_nprbed.csv", row.names = FALSE)

ver_colors <- setNames(c("#2a78d6", "#1baf7a", "#eb6834"), c(v_raw, v_99, v_95))
ver_shapes <- setNames(c(16, 15, 17), c(v_raw, v_99, v_95))
dodge <- position_dodge(width = 0.55)

p <- ggplot(wins_results,
            aes(x = rel_year, y = Estimate, ymin = lb, ymax = ub,
                color = Version, shape = Version)) +
  geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.4) +
  geom_errorbar(position = dodge, width = 0, linewidth = 0.6) +
  geom_point(position = dodge, size = 2.4) +
  facet_wrap(~ Outcome, scales = "free_y", ncol = 2) +
  scale_color_manual(values = ver_colors) +
  scale_shape_manual(values = ver_shapes) +
  scale_x_continuous(breaks = -4:4) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(x = "Years Relative to Provider Tax Adoption",
       y = "Estimate and 95% CI", color = NULL, shape = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position    = "top",
    legend.text        = element_text(size = 12),
    strip.text         = element_text(face = "bold", size = 12, hjust = 0),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing      = unit(1.2, "lines"),
    axis.title         = element_text(color = "gray25")
  )

print(p)
ggsave("rob_alt/stacked_winsorized_opbed_nprbed.png", plot = p, width = 11, height = 5, dpi = 300, bg = "white")
cat("\nSaved: rob_alt/stacked_winsorized_opbed_nprbed.png\n")
