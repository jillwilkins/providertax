library(tidyverse)
library(broom)

# preliminary event study for mcaid revenue per bed
results_mr <- feols(
   net_mcaid_rev_wins / pre_beds_avg ~ i(rel_year, treated, ref = -1) +
                      median_income_pre + exp_status | mcrnum^df + year^df,
  data    = stacked_data_real %>%  filter(rev_per_dis_clean > 0, rev_per_dis_clean < 100000),  
  weights = ~Q_weight,
  cluster = ~state^df
)


# preliminary event study for mcaid revenue per bed
results_mr_log <- feols(
   log(net_mcaid_rev_wins) ~ i(rel_year, treated, ref = -1) +
                      median_income_pre + exp_status | mcrnum^df + year^df,
  data    = stacked_data_real %>%  filter(rev_per_dis_clean > 0, rev_per_dis_clean < 100000, beds > 350),  
  weights = ~Q_weight,
  cluster = ~state^df
)

summary(results_mr_log)

stacked_data_real <- stacked_data_real %>%
  group_by(mcrnum) %>%
  mutate(pre_mcaid_dis = mean(mcaid_discharges[rel_year < 0], na.rm = TRUE)) %>%
  ungroup()

# preliminary event study for mcaid revenue per discharge
results_mr_dis <- feols(
   net_mcaid_rev_wins/ mcaid_discharges ~ i(rel_year, treated, ref = -1) +
                      median_income_pre + exp_status | mcrnum^df + year^df,
  data    = stacked_data_real %>%  filter(rev_per_dis_clean > 0, rev_per_dis_clean < 100000, beds < 200),  
  weights = ~Q_weight,
  cluster = ~state^df
)

summary(results_mr_dis)

  # Extract coefficients
  coef_data_mr <- data.frame(
    rel_year = as.numeric(gsub(".*::(-?[0-9]+):.*", "\\1", 
                               names(coef(results_mr_dis))[grepl("rel_year", names(coef(results_mr_dis)))])),
    coef = coef(results_mr_dis)[grepl("rel_year", names(coef(results_mr_dis)))],
    se = results_mr_dis$se[grepl("rel_year", names(coef(results_mr_dis)))]
  ) %>%
    mutate(
      ci_lower = coef - 1.96 * se,
      ci_upper = coef + 1.96 * se
    ) %>%
    bind_rows(data.frame(rel_year = -1, coef = 0, se = 0, ci_lower = 0, ci_upper = 0)) %>%
    arrange(rel_year) %>%
    mutate(
      period_color = ifelse(rel_year < 0, "Pre-Treatment", "Post-Treatment")
    )
  
  # Create plot
  p_mcaid_rev <- ggplot(coef_data_mr, aes(x = rel_year, y = coef, color = period_color)) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                  width = 0.3, size = 0.8) +
    geom_point(size = 3) +
    scale_color_manual(values = c("Pre-Treatment" = "#DC4B4B", "Post-Treatment" = "#2484d2")) +
    geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.4) +
    labs(
      x = "Years Relative to Treatment",
      y = "Net Medicaid Revenue Per Medicaid Discharge"
    ) +
    scale_x_continuous(
                breaks = seq(min(coef_data_mr$rel_year),
               max(coef_data_mr$rel_year), 1)
) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
      axis.title.x = element_text(size = 12, margin = margin(t = 10)),
      axis.title.y = element_text(size = 12, margin = margin(r = 10)),
      axis.text = element_text(size = 11, color = "black"),
      panel.grid.major = element_line(color = "grey90", size = 0.3),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "none"
    )
  
  print(p_mcaid_rev)

ggsave("es_mcaid_rev_predis.png",
       plot  = p_mcaid_rev,
       width = 10, height = 8, dpi = 300)



# overall ATT — average of all post-period coefficients
att_overall <- results_mr %>%
  tidy() %>%
  filter(str_detect(term, "rel_year::") & 
         as.numeric(str_extract(term, "-?\\d+")) > 0) %>%
  summarise(
    att     = mean(estimate),
    se      = sd(estimate) / sqrt(n()),
    n_coefs = n()
  ) %>%
  mutate(
    ci_low  = att - 1.96 * se,
    ci_high = att + 1.96 * se
  )

print(att_overall)

stacked_data_real %>%
  filter(treated == 1, rel_year < 0) %>%
  summarise(pre_mean = mean(net_mcaid_rev_wins / pre_beds_avg, na.rm = TRUE), 
  pre_mean_pre_dis = mean(net_mcaid_rev_wins / pre_mcaid_dis, na.rm = TRUE), 
  pre_mean_dis = mean(net_mcaid_rev_wins / mcaid_discharges, na.rm = TRUE))

stacked_data_real %>%
  filter(treated == 1, rel_year < 0) %>%
  summarise(
    min    = min(net_mcaid_rev_wins / pre_beds_avg, na.rm = TRUE),
    p25    = quantile(net_mcaid_rev_wins / pre_beds_avg, 0.25, na.rm = TRUE),
    median = median(net_mcaid_rev_wins / pre_beds_avg, na.rm = TRUE),
    mean   = mean(net_mcaid_rev_wins / pre_beds_avg, na.rm = TRUE),
    p75    = quantile(net_mcaid_rev_wins / pre_beds_avg, 0.75, na.rm = TRUE),
    p95    = quantile(net_mcaid_rev_wins / pre_beds_avg, 0.95, na.rm = TRUE),
    max    = max(net_mcaid_rev_wins / pre_beds_avg, na.rm = TRUE)
  )

# ROUGHLY A 20% Increase in Medicaid Revenue Per Bed after tax 
# Overall ATT = 25201
# Pre Treatment Mean = 122,894; Pre Treatment Median = 74,783