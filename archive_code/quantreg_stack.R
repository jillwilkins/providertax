###########################################################
# Quantile Regression: full stacked panel (levels, no individual FE)
# Uses every hospital-year row in stacked_data (not collapsed to one
# pre/post pair per hospital), so op_bed enters as a LEVEL at each point
# in time. Individual hospital fixed effects are dropped -- rq() can't
# absorb ~1,800 of them the way feols does -- so state and cohort (df)
# enter as plain dummies instead. Staggered timing is still handled by
# stacked_data's own cohort-relative windows, same as everywhere else in
# this project.
# stacked_data (5_buildstacked.R) is only read here, never modified.
############################################################

options(scipen = 999)

library(dplyr)
library(quantreg)

taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)

stack_data <- stacked_data %>%
  mutate(post = as.integer(rel_year >= 0)) %>%
  filter(!is.na(op_bed))

cat("Rows:", nrow(stack_data),
    "| treated hospitals:", n_distinct(stack_data$mcrnum[stack_data$treated == 1]),
    "| control hospitals:", n_distinct(stack_data$mcrnum[stack_data$treated == 0]), "\n")

stack_fit <- rq(
  op_bed ~ treated * post + exp_status + median_income_pre + factor(state) + factor(df),
  tau     = taus,
  data    = stack_data,
  weights = Q_weight
)

print(coef(stack_fit))
summary(stack_fit, se = "boot")
