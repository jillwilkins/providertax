# ==============================================================================
# WINSORIZE OUTCOME VARIABLES
# ==============================================================================
install.packages("DescTools")  
library(DescTools)  # Has Winsorize function
# Or if not installed: install.packages("DescTools")

# ==============================================================================
# WINSORIZE OUTCOME VARIABLES (Manual Version)
# ==============================================================================

# Simple winsorize function
winsorize_var <- function(x, probs = c(0.01, 0.99)) {
  lower <- quantile(x, probs[1], na.rm = TRUE)
  upper <- quantile(x, probs[2], na.rm = TRUE)
  
  x_win <- x
  x_win[x < lower] <- lower
  x_win[x > upper] <- upper
  
  return(x_win)
}

# Apply to your outcomes
hospdata_analysis_wins <- hospdata_analysis %>%
  mutate(
    # Winsorize at 1st and 99th percentile (standard)
    mcaid_prop_discharges_w = winsorize_var(mcaid_prop_discharges, c(0.01, 0.99)),
    ucc_prop_w = winsorize_var(ucc_prop, c(0.01, 0.99)),
    uncomp_care_w = winsorize_var(uncomp_care, c(.01, .99)),
    op_margin_w = winsorize_var(op_margin, c(0.01, 0.99)),
    net_pat_rev_w = winsorize_var(net_pat_rev, c(0.01, 0.99)),
    tot_operating_exp_w = winsorize_var(tot_operating_exp, c(0.01, 0.99)),
    mcaid_charges_w = winsorize_var(mcaid_charges, c(0.01, 0.99)),

    
    # Or more aggressive: 5th and 95th percentile
    mcaid_prop_discharges_w95 = winsorize_var(mcaid_prop_discharges, c(0.05, 0.95)),
    ucc_prop_w95 = winsorize_var(ucc_prop, c(0.05, 0.95))
  )

# Compare 
cat("\n=== COMPARISON: ORIGINAL VS WINSORIZED ===\n")
cat("\nMedicaid Proportion:\n")
cat("Original:\n")
summary(hospdata_analysis_wins$mcaid_prop_discharges)
cat("\nWinsorized (1%, 99%):\n")
summary(hospdata_analysis_wins$mcaid_prop_discharges_w)

