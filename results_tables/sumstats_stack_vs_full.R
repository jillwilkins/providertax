# ==============================================================================
# SUMMARY STATISTICS: STACKED SAMPLE vs FULL ANALYSIS SAMPLE, 2001-2022
# One row per characteristic, one column per sample. Hospital-level: each
# hospital is classified once from its own rows in 2001-2022.
#   - Stacked sample: distinct hospital-years in stacked_data (5_buildstacked.R),
#     any role (treated or control), years 2001-2022.
#   - Full sample: hospdata_analysis, years 2001-2022.
#
# Hospital-level definitions (most common value across the hospital's
# non-missing years in the window; ties go to the first value by year):
#   - Ownership: per row, typectrl 1-2 / cntrl 21,23 = nonprofit; typectrl 3-6 /
#     cntrl 30-33 = for-profit (same mapping as 2_dataclean.R). typectrl
#     (HCRIS) covers 2001-2010, cntrl (AHA) 2005-2022.
#   - In system: sysid non-missing. AHA fields only exist from 2005, so a
#     row counts as "not in system" only if it has an AHA record (cntrl
#     non-missing) and sysid is missing; pre-2005 rows are unknown.
#   - Rural: rurlhos (AHA), only populated 2010+.
#   - Beds: each hospital's mean beds over its window years; table reports
#     the median across hospitals.
#   - Medicaid expansion states: states in the sample with expyear in
#     2014-2022 (expanded by the end of the window).
# Percentages are of hospitals with a non-missing classification; the
# number with no data is reported underneath each.
#
# Requires stacked_data and hospdata_analysis. Output:
#   results_tables/sumstats_stack_vs_full.tex
# ==============================================================================

library(dplyr)
library(kableExtra)

yr_lo <- 2001
yr_hi <- 2022

mode_first <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

row_flags <- function(d) {
  d %>%
    mutate(
      own_row = case_when(
        typectrl %in% 1:2         ~ "Nonprofit",
        typectrl %in% 3:6         ~ "For-profit",
        cntrl %in% c(21, 23)      ~ "Nonprofit",
        cntrl %in% 30:33          ~ "For-profit",
        TRUE                      ~ NA_character_
      ),
      sys_row = case_when(
        !is.na(sysid)  ~ 1,
        !is.na(cntrl)  ~ 0,
        TRUE           ~ NA_real_
      )
    )
}

hosp_level <- function(d) {
  d %>%
    row_flags() %>%
    arrange(mcrnum, year) %>%
    group_by(mcrnum) %>%
    summarise(
      state     = mode_first(state),
      expyear   = mode_first(expyear),
      ownership = mode_first(own_row),
      in_system = mode_first(sys_row),
      rural     = mode_first(rurlhos),
      beds      = mean(beds, na.rm = TRUE),
      .groups   = "drop"
    )
}

summarise_sample <- function(h) {
  pct <- function(x) sprintf("%.1f", 100 * mean(x, na.rm = TRUE))
  states <- h %>% distinct(state, expyear) %>% group_by(state) %>% summarise(expyear = mode_first(expyear))
  c(
    "Unique hospitals"                 = scales::comma(nrow(h)),
    "States"                           = as.character(nrow(states)),
    "Medicaid expansion states (by 2022)" = as.character(sum(states$expyear >= 2014 & states$expyear <= yr_hi, na.rm = TRUE)),
    "Median beds"                      = sprintf("%.0f", median(h$beds, na.rm = TRUE)),
    "Nonprofit (\\%)"                  = pct(h$ownership == "Nonprofit"),
    "For-profit (\\%)"                 = pct(h$ownership == "For-profit"),
    "\\quad Ownership missing (N)"     = as.character(sum(is.na(h$ownership))),
    "Rural (\\%)"                      = pct(h$rural == 1),
    "\\quad Rural missing (N)"         = as.character(sum(is.na(h$rural))),
    "In a system (\\%)"                = pct(h$in_system == 1),
    "\\quad System missing (N)"        = as.character(sum(is.na(h$in_system)))
  )
}

stack_rows <- stacked_data %>%
  filter(year >= yr_lo, year <= yr_hi) %>%
  distinct(mcrnum, year, .keep_all = TRUE)

full_rows <- hospdata_analysis %>%
  filter(year >= yr_lo, year <= yr_hi)

sumstats <- data.frame(
  Characteristic = names(summarise_sample(hosp_level(stack_rows))),
  Stack          = unname(summarise_sample(hosp_level(stack_rows))),
  Full           = unname(summarise_sample(hosp_level(full_rows))),
  stringsAsFactors = FALSE
)

cat("\n=== SUMMARY STATISTICS, ", yr_lo, "-", yr_hi, " ===\n", sep = "")
print(sumstats, row.names = FALSE)

tex_lines <- sumstats %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    col.names = c("", "Stacked Sample", "Full Sample"),
    align     = c("l", "r", "r"),
    caption   = "Hospital Characteristics, 2001--2022"
  ) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  as.character() %>%
  strsplit("\n") %>%
  unlist()

bottomrule_idx <- which(tex_lines == "\\bottomrule")
tex_lines <- append(tex_lines, c(
  "",
  "\\multicolumn{3}{l}{\\footnotesize Hospital-level; each hospital classified by its most common value over 2001--2022.}\\\\",
  "\\multicolumn{3}{l}{\\footnotesize Percentages are of hospitals with non-missing data. Rural status is observed from 2010;}\\\\",
  "\\multicolumn{3}{l}{\\footnotesize system membership from 2005 (AHA). Expansion states: expanded Medicaid by 2022.}\\\\"
), after = bottomrule_idx)

writeLines(tex_lines, "results_tables/sumstats_stack_vs_full.tex")
cat("\nSaved: results_tables/sumstats_stack_vs_full.tex\n")
