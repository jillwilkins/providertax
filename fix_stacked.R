# ------------------------------------------------------------------------------
# 0. Parameters
# ------------------------------------------------------------------------------
window = 4      # ±5 event window
yr_min   <- 1999     # first year in data
yr_max   <- 2022     # last year in data

# ------------------------------------------------------------------------------
# 1. Base data — exclude always-treated, keep never-treated
# ------------------------------------------------------------------------------
hospdata_stack <- hospdata_analysis %>%
  filter(cohort != "Always (2004)")   # never-treated (cohort == "Never (0)") stays

# ------------------------------------------------------------------------------
# 2. Valid cohorts — IC1 enforced here, not inside getdata()
#    Cohort j is valid if data covers j - window through j + window
#    With 1999-2022 and window=5: valid cohorts are 2004 through 2017
# ------------------------------------------------------------------------------
cohorts <- hospdata_stack %>%
  filter(gname != 0) %>%                        # treated units only
  distinct(gname) %>%
  pull() %>%
  sort() %>%
  keep(~ (.x - window) >= yr_min &              # IC1: enough pre-period data
          (.x + window) <= yr_max)              # IC1: enough post-period data

cat("\n=== TREATMENT COHORTS AFTER IC1 ===\n")
cat("Valid cohorts:", cohorts, "\n")

# ------------------------------------------------------------------------------
# 3. Sub-experiment builder — single clean function
# ------------------------------------------------------------------------------
getdata <- function(j, pre_window, post_window) {
  
  n_required <- 2 * window + 1    # hospitals must appear at all 11 event times
  
  # Step 1: pull treated + clean controls for this sub-experiment
  raw <- hospdata_stack %>%
    filter(
      gname == j              |    # treated cohort j
      gname == 0              |    # never-treated controls (gname == 0)
      gname > j + window           # not-yet-treated clean controls
    ) %>%
    filter(
      year >= j - window,          # pre-period: pre_window years before adoption
      year <= j + window           # post-period: post_window years after adoption
    ) %>%
    mutate(
      df       = j,
      rel_year = year - j,                   # event time; 0 = adoption year
      treated  = as.integer(gname == j)      # 1 if treated in THIS sub-experiment
    )
  
  # Step 2: enforce strongly balanced panel
  # Keep only hospitals present at ALL 2*pre_window+1 event times
  keep_hosps <- raw %>%
    group_by(mcrnum) %>%
    summarise(n_yrs = n_distinct(rel_year), .groups = "drop") %>%
    filter(n_yrs == n_required) %>%
    pull(mcrnum)
  
  out <- raw %>% filter(mcrnum %in% keep_hosps)
  
  # Reporting
  cat(sprintf(
    "Sub-exp %d | raw hosps: %4d | balanced: %4d | treated: %3d | control: %4d\n",
    j,
    n_distinct(raw$mcrnum),
    length(keep_hosps),
    out %>% filter(treated == 1) %>% distinct(mcrnum) %>% nrow(),
    out %>% filter(treated == 0) %>% distinct(mcrnum) %>% nrow()
  ))
  
  out
}

# ------------------------------------------------------------------------------
# 4. Build stacked data — single map_df call
# ------------------------------------------------------------------------------
cat("\n=== BUILDING SUB-EXPERIMENTS ===\n")

stacked_data <- map_df(cohorts, ~ getdata(., ))

cat("\n=== STACK SUMMARY ===\n")
cat("Total rows     :", nrow(stacked_data),              "\n")
cat("Total hospitals:", n_distinct(stacked_data$mcrnum), "\n")
cat("Sub-experiments:", n_distinct(stacked_data$df),     "\n")

# ------------------------------------------------------------------------------
# 5. Balance check
# ------------------------------------------------------------------------------
cat("\n=== STACKED DATA BALANCE ===\n")
stacked_data %>%
  group_by(df) %>%
  summarise(
    n_obs       = n(),
    n_hospitals = n_distinct(mcrnum),
    n_treated   = n_distinct(mcrnum[treated == 1]),
    n_control   = n_distinct(mcrnum[treated == 0]),
    .groups     = "drop"
  ) %>%
  print()

cat("\n=== TOTAL SAMPLE ===\n")
hospdata_analysis %>%
  summarise(
    n_obs            = n(),
    n_hospitals      = n_distinct(mcrnum),
    #n_treated        = n_distinct(mcrnum[treate == 1]),
    #n_control        = n_distinct(mcrnum[treated == 0]),
    n_states         = n_distinct(state),
    year_min         = min(year),
    year_max         = max(year)
  ) %>%
  print()

# Hospitals in hospdata_analysis but not in stacked_data
missing_hosps <- hospdata_analysis %>%
  filter(!mcrnum %in% unique(stacked_data$mcrnum)) %>%
  distinct(mcrnum, state, year) %>%
  group_by(mcrnum) %>%
  summarise(
    state    = first(state),
    min_year = min(year),
    max_year = max(year),
    n_years  = n_distinct(year),
    .groups  = "drop"
  )

cat("N missing hospitals:", nrow(missing_hosps), "\n")
summary(missing_hosps$n_years)  # are they short-lived?
table(missing_hosps$state)      # concentrated in certain states?

# ------------------------------------------------------------------------------
# 6. Q-weights — computed on the same stacked_data, not a rebuilt version
# ------------------------------------------------------------------------------
unit_counts <- stacked_data %>%
  distinct(mcrnum, df, treated) %>%
  group_by(df) %>%
  summarise(
    n_treat_a = sum(treated == 1),
    n_ctrl_a  = sum(treated == 0),
    .groups   = "drop"
  )

n_treat_total <- sum(unit_counts$n_treat_a)
n_ctrl_total  <- sum(unit_counts$n_ctrl_a)

stacked_data <- stacked_data %>%
  left_join(unit_counts, by = "df") %>%
  mutate(
    treat_share = n_treat_a / n_treat_total,
    ctrl_share  = n_ctrl_a  / n_ctrl_total,
    Q_weight    = if_else(treated == 1, 1, treat_share / ctrl_share)
  )

cat("\n=== Q-WEIGHT INPUTS ===\n")
unit_counts %>%
  mutate(
    treat_share = n_treat_a / n_treat_total,
    ctrl_share  = n_ctrl_a  / n_ctrl_total,
    Q           = treat_share / ctrl_share
  ) %>%
  print()

# ------------------------------------------------------------------------------
# 7. Q-weight balance diagnostic
# ------------------------------------------------------------------------------
cat("\n=== Q-WEIGHT BALANCE DIAGNOSTIC ===\n")
diag <- stacked_data %>%
  filter(treated == 1) %>%
  group_by(rel_year) %>%
  summarise(
    n_cohorts       = n_distinct(df),
    n_treated_hosps = n_distinct(mcrnum),
    total_Q_weight  = sum(Q_weight),
    .groups         = "drop"
  ) %>%
  arrange(rel_year)

print(diag)

q_range <- range(diag$total_Q_weight)
cat(sprintf(
  "\ntotal_Q_weight range: %.4f to %.4f (diff = %.6f)\n",
  q_range[1], q_range[2], diff(q_range)
))

if (diff(q_range) < 0.001) {
  cat("✓ Balanced — composition stable across all event times\n")
} else {
  cat("✗ Still unbalanced — check which sub-experiments have partial windows\n")
}

# ------------------------------------------------------------------------------
# 8. Regression
# ------------------------------------------------------------------------------
stacked_result <- feols(
  mcaid_discharges/ pre_beds_avg ~ i(rel_year, treated, ref = -1) +
                      median_income_pre + exp_status |
    mcrnum^df + year^df,
  data    = stacked_data %>% filter( state != "HI"), # %>% filter(mcrnum %in% hospitals_with_high_npr_bed == FALSE, mcrnum %in% hosp_w_neg_npr_bed == FALSE), # state != "TX", state != "NJ",
  weights = ~Q_weight,
  cluster = ~state^df
)

post_coefs_count <- coef(stacked_result)[grepl("rel_year::[0-9]+:treated", 
                                                names(coef(stacked_result)))]
att_patients <- mean(post_coefs_count)
att_patients

summary(hospdata_stack$mcaid_dis_bed)

stacked_single_result <- feols(
  private_discharges ~ i(rel_year, treated, ref = -1) +
                      median_income_pre + exp_status |
    mcrnum^df + year^df,
data = stacked_data %>% 
  mutate(post = ifelse(rel_year >= 0, 1, 0)),
  weights = ~Q_weight,
  cluster = ~state^df
)

summary(stacked_result)

# ------------------------------------------------------------------------------
# 9. Event study plot
# ------------------------------------------------------------------------------
iplot(
  stacked_result,
  main = "Weighted stacked DiD — total operating expenditure",
  xlab = "Event time (years since adoption)",
  ylab = "Coefficient estimate (95% CI)"
)
 abline(h = 0, lty = 2, col = "gray50")
abline(v = -0.5, lty = 3, col = "gray70")

# ------------------------------------------------------------------------------
# Diagnose: how many sub-experiments contribute at each event time?
# ------------------------------------------------------------------------------

# Count contributing sub-experiments and their combined weight at each event time
event_time_info <- stacked_data %>%
  filter(treated == 1) %>%
  group_by(rel_year) %>%
  summarise(
    n_cohorts         = n_distinct(df),
    n_treated_hosps   = n_distinct(mcrnum),
    total_Q_weight    = sum(Q_weight),
    cohorts_present   = paste(sort(unique(df)), collapse = ", "),
    .groups           = "drop"
  ) %>%
  arrange(rel_year)

cat("=== CONTRIBUTING SUB-EXPERIMENTS BY EVENT TIME ===\n")
print(event_time_info, width = 120)

# ------------------------------------------------------------------------------
# Extract event study coefficients and SEs to see the pattern directly
# ------------------------------------------------------------------------------
es_df <- broom::tidy(stacked_result) %>%
  filter(str_detect(term, "rel_year")) %>%
  mutate(
    rel_year = as.numeric(str_extract(term, "-?[0-9]+")),
    ci_lo    = estimate - 1.96 * std.error,
    ci_hi    = estimate + 1.96 * std.error,
    ci_width = ci_hi - ci_lo
  ) %>%
  arrange(rel_year)

cat("\n=== SE AND CI WIDTH BY EVENT TIME ===\n")
es_df %>%
  select(rel_year, estimate, std.error, ci_width) %>%
  print(n = 20)

# Is the SE pattern symmetric around 0?
# If SEs at e=-5 ≈ SEs at e=+5, widening is due to fewer cohorts (expected)
# If SEs at e=+5 >> SEs at e=-5, post-period variance is growing (investigate)
cat("\n=== PRE vs POST SE COMPARISON ===\n")
es_df %>%
  mutate(period = if_else(rel_year < 0, "pre", "post")) %>%
  group_by(period) %>%
  summarise(
    mean_se  = mean(std.error),
    max_se   = max(std.error),
    min_se   = min(std.error),
    .groups  = "drop"
  ) %>%
  print()


# ------------------------------------------------------------------------------
# SE DIAGNOSTIC
# 1. Distribution of expenditure — are there very small hospitals?
#    Log is sensitive near zero; check the lower tail
# ------------------------------------------------------------------------------
stacked_data %>%
  filter(treated == 1, rel_year == -1) %>%   # baseline year only
  summarise(
    p1   = quantile(tot_operating_exp, 0.01, na.rm = TRUE),
    p5   = quantile(tot_operating_exp, 0.05, na.rm = TRUE),
    p10  = quantile(tot_operating_exp, 0.10, na.rm = TRUE),
    p25  = quantile(tot_operating_exp, 0.25, na.rm = TRUE),
    p50  = quantile(tot_operating_exp, 0.50, na.rm = TRUE),
    p75  = quantile(tot_operating_exp, 0.75, na.rm = TRUE),
    p90  = quantile(tot_operating_exp, 0.90, na.rm = TRUE),
    p99  = quantile(tot_operating_exp, 0.99, na.rm = TRUE),
    mean = mean(tot_operating_exp,     na.rm = TRUE),
    sd   = sd(tot_operating_exp,       na.rm = TRUE),
    cv   = sd / mean    # coefficient of variation — high CV = log problems
  )

# CV > 1 is a strong signal that log SEs will explode
# because residual variance is dominated by the size distribution

# ------------------------------------------------------------------------------
# 2. Is the treatment effect heterogeneous by hospital size?
#    This is the most substantively interesting diagnostic
# ------------------------------------------------------------------------------

# Create size terciles based on pre-period expenditure
size_terciles <- hospdata_stack %>%
  filter(rel_year == -1) %>%
  distinct(mcrnum, pre_beds_avg) %>%
  mutate(
    size_tercile = ntile(pre_beds_avg, 3),
    size_label   = case_when(
      size_tercile == 1 ~ "Small (bottom third)",
      size_tercile == 2 ~ "Medium (middle third)",
      size_tercile == 3 ~ "Large (top third)"
    )
  ) %>%
  select(mcrnum, size_tercile, size_label)

stacked_data <- stacked_data %>%
  left_join(size_terciles, by = "mcrnum")

# Run separate regressions by size tercile
size_results <- map_df(1:3, function(s) {
  
  df_s <- stacked_data %>% filter(size_tercile == s)
  
  res <- feols(
    op_bed ~ i(rel_year, treated, ref = -1) +
                        median_income_pre + exp_status |
      mcrnum^df + year^df,
    data    = df_s,
    weights = ~Q_weight,
    cluster = ~state
  )
  
  broom::tidy(res) %>%
    filter(str_detect(term, "rel_year")) %>%
    mutate(
      size_label = unique(df_s$size_label),
      rel_year   = as.numeric(str_extract(term, "-?[0-9]+")),
      ci_lo      = estimate - 1.96 * std.error,
      ci_hi      = estimate + 1.96 * std.error
    )
})

# Plot by size tercile — do large hospitals drive the result?
size_results %>%
  filter(rel_year != -1) %>%
  ggplot(aes(x = rel_year, y = estimate)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi),
              fill = "steelblue", alpha = 0.2) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 1.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "gray50") +
  facet_wrap(~ size_label, scales = "free_y") +
  labs(
    title    = "Treatment effect heterogeneity by hospital size",
    subtitle = paste0("If large hospitals drive the result in levels, ",
                      "log SEs explode due to size heterogeneity"),
    x        = "Event time",
    y        = "Effect on total operating expenditure ($)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 9, face = "bold"))


# ------------------------------------------------------------------------------
# Treatment effect by size tercile — LOG outcome
# This directly shows whether the proportional effect is heterogeneous
# which is the exact mechanism causing log SE explosion
# ------------------------------------------------------------------------------

log_size_results <- map_df(1:3, function(s) {
  
  df_s <- stacked_data %>% 
    filter(size_tercile == s)
  
  res <- tryCatch(
    feols(
      log(tot_operating_exp) ~ i(rel_year, treated, ref = -1) +
                                median_income_pre + exp_status |
        mcrnum^df + year^df,
      data    = df_s,
      weights = ~Q_weight,
      cluster = ~state
    ),
    error = function(e) NULL
  )
  
  if (is.null(res)) return(NULL)
  
  broom::tidy(res) %>%
    filter(str_detect(term, "rel_year")) %>%
    mutate(
      size_label = unique(df_s$size_label),
      size_n     = s,
      rel_year   = as.numeric(str_extract(term, "-?[0-9]+")),
      ci_lo      = estimate - 1.96 * std.error,
      ci_hi      = estimate + 1.96 * std.error
    )
})

# ------------------------------------------------------------------------------
# Plot — compare log effect across terciles
# ------------------------------------------------------------------------------
log_size_results %>%
  filter(rel_year != -1) %>%
  ggplot(aes(x = rel_year, y = estimate)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi),
              fill = "steelblue", alpha = 0.2) +
  geom_line(color = "steelblue",  linewidth = 0.8) +
  geom_point(color = "steelblue", size = 1.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5,   linetype = "dotted", color = "gray50") +
  facet_wrap(~ size_label, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = "Log treatment effect by hospital size tercile",
    subtitle = paste0("If proportional effect varies across terciles, ",
                      "pooling all hospitals inflates log residual variance"),
    x        = "Event time (years since adoption)",
    y        = "Effect on log operating expenditure (approx % change)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 9, face = "bold"))

# ------------------------------------------------------------------------------
# Summary table — post-period average log effect by tercile
# This is the key number: what % increase did each size group experience?
# ------------------------------------------------------------------------------
cat("=== POST-PERIOD AVERAGE LOG EFFECT BY SIZE TERCILE ===\n")
log_size_results %>%
  filter(rel_year >= 0) %>%
  group_by(size_label, size_n) %>%
  summarise(
    avg_log_effect = mean(estimate),
    avg_se         = mean(std.error),
    approx_pct     = scales::percent(avg_log_effect, accuracy = 0.1),
    t_stat         = avg_log_effect / avg_se,
    significant    = abs(t_stat) > 1.96,
    .groups        = "drop"
  ) %>%
  arrange(size_n) %>%
  print()

# ------------------------------------------------------------------------------
# Side by side: levels vs log by tercile
# The contrast between these two plots is your key diagnostic figure
# ------------------------------------------------------------------------------

# Combine levels and log results
levels_size_results <- map_df(1:3, function(s) {
  
  df_s <- stacked_data %>% filter(size_tercile == s)
  
  res <- tryCatch(
    feols(
      tot_operating_exp ~ i(rel_year, treated, ref = -1) +
                          median_income_pre + exp_status |
        mcrnum^df + year^df,
      data    = df_s,
      weights = ~Q_weight,
      cluster = ~state
    ),
    error = function(e) NULL
  )
  
  if (is.null(res)) return(NULL)
  
  broom::tidy(res) %>%
    filter(str_detect(term, "rel_year")) %>%
    mutate(
      size_label = unique(df_s$size_label),
      size_n     = s,
      spec       = "Levels ($)",
      rel_year   = as.numeric(str_extract(term, "-?[0-9]+")),
      ci_lo      = estimate - 1.96 * std.error,
      ci_hi      = estimate + 1.96 * std.error
    )
})

# Add spec label to log results
log_size_labelled <- log_size_results %>%
  mutate(spec = "Log (approx %)")

# Bind and plot side by side
bind_rows(levels_size_results, log_size_labelled) %>%
  filter(rel_year != -1) %>%
  ggplot(aes(x = rel_year, y = estimate)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi),
              fill = "steelblue", alpha = 0.15) +
  geom_line(color  = "steelblue", linewidth = 0.7) +
  geom_point(color = "steelblue", size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "gray50") +
  facet_grid(spec ~ size_label, scales = "free_y") +
  labs(
    title    = "Levels vs log treatment effect by hospital size tercile",
    subtitle = paste0("Top row: absolute $ effect. ",
                      "Bottom row: proportional % effect. ",
                      "Heterogeneity in % effect explains log SE explosion."),
    x        = "Event time (years since adoption)",
    y        = "Estimate"
  ) +
  theme_minimal() +
  theme(
    strip.text   = element_text(size = 9, face = "bold"),
    axis.text.y  = element_text(size = 8)
  )

# ------------------------------------------------------------------------------
# 3. Compare cluster-level log residual variance pre vs post
#    If log residuals are much more dispersed at the state level post-treatment,
#    that confirms treatment effect heterogeneity in proportional terms
# ------------------------------------------------------------------------------

# Get residuals from log regression
stacked_result_log <- feols(
  log_op ~ i(rel_year, treated, ref = -1) +
                      median_income_pre + exp_status |
    mcrnum^df + year^df,
  data    = stacked_data, # %>% filter(mcrnum %in% hospitals_with_high_npr_bed == FALSE, mcrnum %in% hosp_w_neg_npr_bed == FALSE), # state != "TX", state != "NJ",
  weights = ~Q_weight,
  cluster = ~state^df
)

log_resids <- residuals(stacked_result_log)

stacked_data %>%
  filter(!is.na(log(tot_operating_exp))) %>%
  mutate(resid_log = log_resids) %>%
  group_by(state, rel_year) %>%
  summarise(
    mean_resid = mean(resid_log,  na.rm = TRUE),
    sd_resid   = sd(resid_log,    na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  group_by(rel_year) %>%
  summarise(
    # Variance across states at each event time
    # High variance = states responding very differently = big cluster SEs
    between_state_sd = sd(mean_resid, na.rm = TRUE),
    .groups          = "drop"
  ) %>%
  arrange(rel_year) %>%
  print()

# If between_state_sd is much larger post-treatment than pre-treatment,
# states are diverging in their proportional cost response —
# which is exactly why log SEs are large

# ------------------------------------------------------------------------------
# Confirm the absolute dollar effect is similar across terciles
# by computing the implied dollar effect from the log coefficient
# ------------------------------------------------------------------------------

# Get baseline mean expenditure by tercile
baseline_by_tercile <- stacked_data %>%
  filter(rel_year == -1, treated == 1) %>%
  group_by(size_tercile, size_label) %>%
  summarise(
    mean_baseline = mean(tot_operating_exp, na.rm = TRUE),
    med_baseline  = median(tot_operating_exp, na.rm = TRUE),
    n_hosps       = n_distinct(mcrnum),
    .groups       = "drop"
  )

# Combine with log effect estimates
log_size_results %>%
  filter(rel_year >= 0) %>%
  group_by(size_label, size_n) %>%
  summarise(
    avg_log_effect = mean(estimate),
    .groups        = "drop"
  ) %>%
  left_join(baseline_by_tercile, by = "size_label") %>%
  mutate(
    # Implied absolute dollar effect = % change x baseline mean
    implied_dollar_effect = avg_log_effect * mean_baseline,
    implied_dollar_effect_fmt = scales::dollar(
      implied_dollar_effect, 
      scale = 1e-6, 
      suffix = "M"
    ),
    mean_baseline_fmt = scales::dollar(
      mean_baseline,
      scale = 1e-6,
      suffix = "M"
    )
  ) %>%
  select(size_label, n_hosps, mean_baseline_fmt, 
         avg_log_effect, implied_dollar_effect_fmt) %>%
  print()

# ------------------------------------------------------------------------------
# This is your key table for the paper
# Shows that similar absolute effects look very different proportionally
# ------------------------------------------------------------------------------
