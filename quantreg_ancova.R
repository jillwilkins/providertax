###########################################################
# Quantile Regression: ANCOVA (levels, controlling for pre-period)
# Same hospital x cohort long-difference sample as quant_reg.R, but
# instead of differencing (delta = post - pre), the post-period LEVEL is
# the outcome and the pre-period level enters as a control. This answers
# a "levels" question -- does the tax shift the spread of post-period
# op_bed? -- instead of a "spread of changes" question.
# stacked_data (5_buildstacked.R) is only read here, never modified.
############################################################

options(scipen = 999)

library(dplyr)
library(quantreg)

taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)

# One row per hospital x cohort sub-experiment
ancova_data <- stacked_data %>%
  group_by(mcrnum, df) %>%
  summarise(
    treated           = first(treated),
    Q_weight          = first(Q_weight),
    exp_status        = mean(exp_status[rel_year < 0], na.rm = TRUE),
    median_income_pre = first(median_income_pre),
    pre_op_bed        = mean(op_bed[rel_year < 0],  na.rm = TRUE),
    post_op_bed       = mean(op_bed[rel_year >= 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(pre_op_bed), !is.na(post_op_bed))

cat("Rows:", nrow(ancova_data),
    "| treated:", sum(ancova_data$treated == 1),
    "| control:", sum(ancova_data$treated == 0), "\n")

# post-period level as outcome, pre-period level as a control instead of
# differencing it away
ancova_fit <- rq(
  post_op_bed ~ treated + pre_op_bed + exp_status + median_income_pre,
  tau     = taus,
  data    = ancova_data,
  weights = Q_weight
)

print(coef(ancova_fit))
summary(ancova_fit, se = "boot")
