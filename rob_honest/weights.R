# ------------------------------------------------------------------------------
# 1. Clean base data — exclude always treated
hospdata_stack <- hospdata_analysis %>%
  filter(gname != 2004)  # remove always-treated by gname, not cohort label

# Valid cohorts: treated states only, never-treated flagged as gname == 0
cohorts <- hospdata_stack %>%
  filter(gname != 0) %>%
  distinct(gname) %>%
  pull() %>%
  sort()

cat("\n=== TREATMENT COHORTS ===\n")
cat("Valid cohorts:", cohorts, "\n")

# ------------------------------------------------------------------------------
# 2. Sub-experiment builder — single clean version
window <- 5  # ± window years; pre = window+1 periods, post = window periods

getdata <- function(j, window) {
  hospdata_stack %>%
    filter(
      gname == j          |   # treated cohort j
      gname == 0          |   # never treated
      gname > j + window      # not-yet-treated clean controls
    ) %>%
    filter(
      year >= j - (window),   # pre-periods: window+1 years before adoption
      year <= j + window           # post-periods: window years after adoption
    ) %>%
    mutate(
      df      = j,                          # sub-experiment identifier
      rel_year = year - j,                  # event time (0 = adoption year)
      treated  = as.integer(gname == j)     # 1 if treated in THIS sub-experiment
    )
}

# ------------------------------------------------------------------------------
# 3. Build stacked data — single pipeline
stacked_data <- map_df(cohorts, ~ getdata(., window = window))

# Verify balance
cat("\n=== STACKED DATA BALANCE ===\n")
stacked_data %>%
  group_by(df) %>%
  summarise(
    n_obs        = n(),
    n_hospitals  = n_distinct(mcrnum),
    n_treated    = n_distinct(mcrnum[treated == 1]),
    n_control    = n_distinct(mcrnum[treated == 0]),
    .groups = "drop"
  ) %>%
  print()

# ------------------------------------------------------------------------------
# 4. Compute Q-weights
#    - Treated observations: weight = 1
#    - Control observations: weight = (treated share in sub-exp a) /
#                                     (control share in sub-exp a)

# Count treated and control hospitals per sub-experiment
# Use distinct hospital x sub-experiment to avoid double-counting by year
unit_counts <- stacked_data %>%
  distinct(mcrnum, df, treated) %>%
  group_by(df) %>%
  summarise(
    n_treat_a = sum(treated == 1),
    n_ctrl_a  = sum(treated == 0),
    .groups = "drop"
  )

# Total treated and control across all sub-experiments
n_treat_total <- sum(unit_counts$n_treat_a)
n_ctrl_total  <- sum(unit_counts$n_ctrl_a)

cat("\n=== WEIGHT INPUTS ===\n")
cat("Total treated hospital-subexp units:", n_treat_total, "\n")
cat("Total control hospital-subexp units:", n_ctrl_total,  "\n")
print(unit_counts)

# Merge counts back and compute Q
stacked_data <- stacked_data %>%
  left_join(unit_counts, by = "df") %>%
  mutate(
    treat_share = n_treat_a / n_treat_total,
    ctrl_share  = n_ctrl_a  / n_ctrl_total,
    Q_weight    = if_else(treated == 1, 1, treat_share / ctrl_share)
  )

# Sanity check: weighted control shares should now equal treated shares
cat("\n=== WEIGHT SANITY CHECK ===\n")
stacked_data %>%
  group_by(df) %>%
  summarise(
    treat_share     = first(treat_share),
    ctrl_share_raw  = first(ctrl_share),
    Q               = first(Q_weight[treated == 0]),
    .groups = "drop"
  ) %>%
  print()


stacked_data %>%
  distinct(mcrnum, df, treated, Q_weight, 
           treat_share, ctrl_share) %>%
  group_by(df) %>%
  summarise(
    treat_share       = first(treat_share),
    # raw unweighted control share
    ctrl_share_raw    = first(ctrl_share),
    # weighted control share: Q * n_ctrl_a / sum(Q * n_ctrl across all subexps)
    # simplifies to: treat_share/ctrl_share * ctrl_share = treat_share
    Q                 = first(Q_weight[treated == 0]),
    ctrl_share_wtd    = first(Q_weight[treated == 0]) * 
                        first(ctrl_share),
    .groups = "drop"
  ) %>%
  mutate(
    balanced = round(ctrl_share_wtd, 4) == round(treat_share, 4)
  )
# ------------------------------------------------------------------------------
# 5. Weighted stacked event study via feols

stacked_result <- feols(
  npr_bed ~ i(rel_year, treated, ref = -1) + median_income_pre + exp_status |
    mcrnum^df + year^df,
  data    = stacked_data %>% filter(year <= 2022),
  weights = ~Q_weight,          # <-- corrective Q-weights here
  cluster = ~state
)

summary(stacked_result)

# ------------------------------------------------------------------------------
# 6. Event study plot
iplot(stacked_result,
      main  = "Weighted stacked DiD — effect on op_bed",
      xlab  = "Event time (years since adoption)",
      ylab  = "Estimate (95% CI)")
abline(h = 0, lty = 2, col = "gray50")




# install.packages("HonestDiD")  # if not already installed
library(HonestDiD)
library(ggplot2)

# install.packages("HonestDiD")  # if not already installed
library(HonestDiD)
library(ggplot2)

# ------------------------------------------------------------------------------
# Step 1: Extract event study coefficients and covariance matrix from feols
# ------------------------------------------------------------------------------

# Get all event study coefficients (these are the i(rel_year, treated) terms)
# feols stores them with names like "rel_year::-5:treated", etc.
coef_names <- names(coef(stacked_result))
es_coefs   <- grep("^rel_year::", coef_names, value = TRUE)

cat("Event study coefficients found:\n")
print(es_coefs)

# Extract point estimates — full vector including pre and post
betahat <- coef(stacked_result)[es_coefs]

# Extract the corresponding block of the variance-covariance matrix
# This needs to be the CLUSTERED vcov (matching your cluster = ~state)
sigma <- vcov(stacked_result, type = "cluster")[es_coefs, es_coefs]

cat("\nDimensions of betahat:", length(betahat), "\n")
cat("Dimensions of sigma:  ", dim(sigma), "\n")

# ------------------------------------------------------------------------------
# Step 2: Identify which coefficients are pre vs post
# HonestDiD needs to know how many pre-periods you have
# Your ref = -1, so pre-periods are rel_year = -6, -5, -4, -3, -2
# and post-periods are rel_year = 0, 1, 2, 3, 4, 5
# ------------------------------------------------------------------------------

# Extract the numeric event times from coefficient names
event_times <- as.numeric(
  gsub("rel_year::(-?[0-9]+):treated", "\\1", es_coefs)
)
cat("\nEvent times in order:", sort(event_times), "\n")

# Sort everything by event time (feols may not return them in order)
ord      <- order(event_times)
betahat  <- betahat[ord]
sigma    <- sigma[ord, ord]
event_times <- event_times[ord]

# Number of pre-periods (excluding ref period -1, so e = -6,-5,-4,-3,-2 = 5 pre)
n_pre  <- sum(event_times < -1)   # periods before ref
n_post <- sum(event_times >= 0)   # periods at and after treatment

cat("Pre-periods: ", n_pre,  "\n")
cat("Post-periods:", n_post, "\n")

# ------------------------------------------------------------------------------
# Step 3: Run HonestDiD sensitivity analysis
# Using the "relative magnitudes" restriction (Mbar)
# ------------------------------------------------------------------------------

# Range of Mbar values to test
# Start at 0 (exact parallel trends) and go up to 2
mbar_grid <- seq(0, 2, by = 0.5)

honest_results <- createSensitivityResults_relativeMagnitudes(
  betahat        = betahat,
  sigma          = sigma,
  numPrePeriods  = n_pre,
  numPostPeriods = n_post,
  Mbarvec        = mbar_grid,
  alpha          = 0.05         # 95% confidence sets
)

cat("\n=== HONESTDID SENSITIVITY RESULTS ===\n")
print(honest_results)

# ------------------------------------------------------------------------------
# Step 4: Also run the smoothness restriction (Delta_SD)
# This assumes violations of parallel trends evolve smoothly over time
# M bounds the second difference (how fast the trend can accelerate)
# ------------------------------------------------------------------------------

honest_smooth <- createSensitivityResults(
  betahat        = betahat,
  sigma          = sigma,
  numPrePeriods  = n_pre,
  numPostPeriods = n_post,
  method         = "FLCI",       # fixed-length confidence intervals
  alpha          = 0.05
)

cat("\n=== SMOOTHNESS RESTRICTION RESULTS ===\n")
print(honest_smooth)

# ------------------------------------------------------------------------------
# Step 5: Plot — sensitivity of post-period average effect
# ------------------------------------------------------------------------------

# Original (standard) confidence interval at Mbar = 0
original_ci <- data.frame(
  lb     = betahat[event_times == 0] - 
            1.96 * sqrt(sigma[event_times == 0, event_times == 0]),
  ub     = betahat[event_times == 0] + 
            1.96 * sqrt(sigma[event_times == 0, event_times == 0]),
  method = "Standard (Mbar=0)",
  Mbar   = 0
)

# HonestDiD sensitivity plot using their built-in plotter
sensitivity_plot <- createSensitivityPlot_relativeMagnitudes(
  robustResults   = honest_results,
  originalResults = original_ci    # overlays your standard CI at Mbar=0
)

sensitivity_plot +
  labs(
    title    = "HonestDiD sensitivity: effect on total operating expenditure",
    subtitle = "Mbar = max post-period violation relative to pre-period deviations",
    x        = "Mbar (relative magnitude of permitted trend violation)",
    y        = "95% confidence set for ATT at e = 0"
  ) +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50")

# ------------------------------------------------------------------------------
# Step 6: Event-study version — sensitivity at each post period
# ------------------------------------------------------------------------------

# This shows how the CI widens at each post-period event time
# as you relax the parallel trends assumption

honest_by_period <- createSensitivityResults_relativeMagnitudes(
  betahat        = betahat,
  sigma          = sigma,
  numPrePeriods  = n_pre,
  numPostPeriods = n_post,
  Mbarvec        = c(0, 0.5, 1, 1.5, 2),
  alpha          = 0.05,
  l_vec          = basisVector(1, n_post)  # effect at e=0 only
  # change index to look at e=1,2,... by changing basisVector argument
)

# To look at the average post-period effect instead of a single period:
# l_vec = rep(1/n_post, n_post)   <-- uniform average over all post periods