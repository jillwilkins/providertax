# ==============================================================================
# SERVICE LINE OFFERINGS: TREATED VS. NEVER-TREATED SUMMARY STATS
# AHA service-line variables (emergency dept, psych/behavioral health,
# trauma, cath lab, ESWL, etc.), compared between hospitals that ever adopt
# a provider tax (treated) and hospitals that never do (never), pre-tax
# framing to keep this a baseline/descriptive comparison rather than one
# contaminated by any actual treatment effect on service-line offerings
# (that causal question is handled separately by the stacked DiD).
#   - Treated hospitals: averaged over each hospital's own pre-treatment
#     years only (year < gname), same convention as pre_beds_avg /
#     heterogeneity_quartile_table.R / attrition_analysis.R.
#   - Never-treated hospitals: averaged over all observed years (no
#     treatment date to define "pre" against).
# Binary flags (emdephos, psyemhos, alchhos, traumhos, adtchos, iclabhos,
# eswlhos) are reported as proportions (% of hospitals ever offering that
# service pre-treatment); count variables (vem, psybd, alchbd) as means.
# Requires hospdata_analysis already in the environment.
# ==============================================================================

library(dplyr)

binary_vars <- c("emdephos", "psyemhos", "alchhos", "traumhos", "adtchos", "iclabhos", "eswlhos")
count_vars  <- c("vem", "psybd", "alchbd")
service_line_vars <- c(binary_vars, count_vars)

var_labels <- c(
  emdephos = "Emergency Department",
  psyemhos = "Psychiatric Emergency Services",
  alchhos  = "Alcohol/Chemical Dependency Unit",
  traumhos = "Trauma Center",
  adtchos  = "Adult Day Care Program",
  iclabhos = "Cardiac Catheterization Lab",
  eswlhos  = "Extracorporeal Shock Wave Lithotripsy (ESWL)",
  vem      = "Emergency Dept. Visits",
  psybd    = "Psychiatric Beds",
  alchbd   = "Alcohol/Chemical Dependency Beds"
)

# ------------------------------------------------------------------------------
# 1. Hospital-level values: pre-treatment mean for treated, all-years mean
#    for never-treated
# ------------------------------------------------------------------------------
hosp_service_lines <- hospdata_analysis %>%
  group_by(mcrnum, ever_treat, gname) %>%
  summarise(
    across(all_of(service_line_vars), ~ {
      if (first(ever_treat) == 1) {
        mean(.x[year < first(gname)], na.rm = TRUE)
      } else {
        mean(.x, na.rm = TRUE)
      }
    }, .names = "{.col}"),
    .groups = "drop"
  ) %>%
  mutate(group = ifelse(ever_treat == 1, "Treated", "Never"))

cat("\n=== N HOSPITALS BY GROUP ===\n")
hosp_service_lines %>% count(group) %>% print()

# ------------------------------------------------------------------------------
# 2. Group means + t-tests
# ------------------------------------------------------------------------------
fmt_stat <- function(v, is_binary) {
  if (is_binary) sprintf("%.1f%%", mean(v, na.rm = TRUE) * 100)
  else sprintf("%.2f", mean(v, na.rm = TRUE))
}

results <- lapply(service_line_vars, function(v) {
  is_bin <- v %in% binary_vars
  treated_vals <- hosp_service_lines %>% filter(group == "Treated") %>% pull(!!v)
  never_vals   <- hosp_service_lines %>% filter(group == "Never") %>% pull(!!v)

  t <- tryCatch(t.test(treated_vals, never_vals), error = function(e) NULL)

  data.frame(
    Variable      = var_labels[[v]],
    Type          = ifelse(is_bin, "% offering", "Mean count"),
    N_Treated     = sum(!is.na(treated_vals)),
    N_Never       = sum(!is.na(never_vals)),
    Treated       = fmt_stat(treated_vals, is_bin),
    Never         = fmt_stat(never_vals, is_bin),
    Diff          = if (is_bin) {
      sprintf("%.1fpp", (mean(treated_vals, na.rm=TRUE) - mean(never_vals, na.rm=TRUE)) * 100)
    } else {
      sprintf("%.2f", mean(treated_vals, na.rm=TRUE) - mean(never_vals, na.rm=TRUE))
    },
    P_Value       = if (!is.null(t)) round(t$p.value, 3) else NA,
    stringsAsFactors = FALSE
  )
}) %>% bind_rows()

cat("\n=== SERVICE LINE OFFERINGS: TREATED VS. NEVER-TREATED (PRE-TREATMENT) ===\n")
print(results, row.names = FALSE)

cat("\nNote: missingness in these AHA fields is substantial (~50% of hospital-years) --\n")
cat("N_Treated/N_Never above are non-missing hospital counts within each group, not full group size.\n")
