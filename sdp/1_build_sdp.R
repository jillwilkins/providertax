###########################################################
# SDP (State Directed Payments) preliminary analysis
# Step 1: check/add mcaid_revdis (Medicaid revenue per Medicaid
# discharge) on the RAW hospital panel -- hospdata_analysis --
# not the stacked/event-study design.
#
# mcaid_revdis = net_mcaid_rev / mcaid_discharges
#   net_mcaid_rev    HCRIS S100000, line 00200 col 00100 (v2010)
#   mcaid_discharges HCRIS S300001, line 00100 col 01400 (v2010)
# Set to NA (not Inf) when mcaid_discharges <= 0 or missing.
###########################################################

library(dplyr)

data_output_path <- "/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/output/"

hospdata_analysis <- read.csv(paste0(data_output_path, "hospdata_analysis.csv"))

if (!"mcaid_revdis" %in% names(hospdata_analysis)) {
  hospdata_analysis <- hospdata_analysis %>%
    mutate(mcaid_revdis = ifelse(!is.na(mcaid_discharges) & mcaid_discharges > 0,
                                  net_mcaid_rev / mcaid_discharges,
                                  NA_real_))
  cat("mcaid_revdis added.\n")
} else {
  cat("mcaid_revdis already present -- not recreated.\n")
}

# ------------------------------------------------------------------
# Quick diagnostics (same known issues flagged in
# diagnostics/investigate_mcaidrev.R for net_mcaid_rev/mcaid_discharges:
# non-positive discharge counts and a handful of negative revenues)
# ------------------------------------------------------------------
cat("\n=== mcaid_revdis summary ===\n")
print(summary(hospdata_analysis$mcaid_revdis))

cat("\nn obs:                  ", nrow(hospdata_analysis), "\n")
cat("n missing mcaid_revdis: ", sum(is.na(hospdata_analysis$mcaid_revdis)), "\n")
cat("n mcaid_discharges<=0:  ", sum(hospdata_analysis$mcaid_discharges <= 0, na.rm = TRUE), "\n")
cat("n mcaid_discharges<10:  ", sum(hospdata_analysis$mcaid_discharges < 10, na.rm = TRUE), "(unstable ratio -- flag only, not dropped)\n")
cat("n net_mcaid_rev<0:      ", sum(hospdata_analysis$net_mcaid_rev < 0, na.rm = TRUE), "\n")
