# ==============================================================================
# Script that prepares my results table to include in Poster Presentation (4/23)
# This is based on the stacked data set created in fix_stacked.R
# ==============================================================================
 
library(tidyverse)
library(fixest)
library(kableExtra)

# ------------------------------------------------------------------------------
# 1. Hospital Level Outcomes
# ------------------------------------------------------------------------------
outcome_info <- list(
  list(var = "op_bed",                  label = "Operating Expenses per Bed"),
  list(var = "npr_bed",                 label = "Net Patient Revenue per Bed"), 
  list(var = "op_margin",               label = "Operating Margin"),
  list(var = "mcaid_prop_discharges",   label = "Medicaid Share of Discharges"),
  list(var = "private_prop_discharges", label = "Commercial Share of Discharges"),
  list(var = "uncomp_bed",              label = "Uncompensated Care per Bed")
)

# ------------------------------------------------------------------------------
# 2. Pre-period baseline means
# ------------------------------------------------------------------------------
pre_treat_means <- stacked_data %>%
  filter(treated == 1, rel_year < 0) %>%
  summarise(
    mn_mcaid     = mean(mcaid_prop_discharges,   na.rm = TRUE),
    mn_priv      = mean(private_prop_discharges, na.rm = TRUE),
    mn_uncomp    = mean(uncomp_bed,              na.rm = TRUE),
    mn_op        = mean(op_bed,                  na.rm = TRUE),
    mn_npr       = mean(npr_bed,                 na.rm = TRUE),
    mn_op_margin = mean(op_margin,               na.rm = TRUE)
  )

# Map variable names to their pre-period means
pre_means_lookup <- list(
  op_bed                  = round(pre_treat_means$mn_op,       0),
  npr_bed                 = round(pre_treat_means$mn_npr,      0),
  op_margin               = round(pre_treat_means$mn_op_margin, 3),
  mcaid_prop_discharges   = round(pre_treat_means$mn_mcaid,    3),
  private_prop_discharges = round(pre_treat_means$mn_priv,     3),
  uncomp_bed              = round(pre_treat_means$mn_uncomp,   0)
)

# ------------------------------------------------------------------------------
# 3. Run model and extract numbers
# ------------------------------------------------------------------------------
data_use <- stacked_data %>%
  mutate(post = as.integer(rel_year >= 0))

summary_table_raw <- map_df(outcome_info, function(oi) {
  
  formula_str <- paste0(
    oi$var,
    " ~ treated:post +",
    " i(rel_year, treated, ref = -1, keep = -4:-2) +",
    " exp_status + median_income_pre",
    " | mcrnum^df + year^df"
  )
  
  result <- feols(
    as.formula(formula_str),
    data    = data_use,
    weights = ~Q_weight,
    cluster = ~state
  )
  
  coef_names <- names(coef(result))
  post_term  <- coef_names[grepl("treated.*post|post.*treated", coef_names)]
  
  beta <- as.numeric(coef(result)[post_term])
  se   <- as.numeric(sqrt(diag(vcov(result)))[post_term])
  pval <- as.numeric(pvalue(result)[post_term])
  
  stars <- case_when(
    pval < 0.01 ~ "***",
    pval < 0.05 ~ "**",
    pval < 0.10 ~ "*",
    TRUE        ~ ""
  )
  
  data.frame(
    Outcome = oi$label,
    Mean    = pre_means_lookup[[oi$var]],
    Beta    = round(beta, 3),
    SE      = round(se,   2),
    Stars   = stars,
    stringsAsFactors = FALSE
  )
})

# ------------------------------------------------------------------------------
# 4. Save LaTeX
# ------------------------------------------------------------------------------
summary_table_raw %>%
  mutate(Beta_SE = paste0(Beta, Stars, " & (", SE, ")")) %>%
  select(Outcome, Mean, Beta_SE) %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    col.names = c("Outcome", "Mean", "$\\hat{\\beta}$ (SE)"),
    align     = c("l", "r", "r")
  ) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  pack_rows("Financial Outcomes",          1, 3) %>%
  pack_rows("Payer Mix", 4, 6) %>%
  add_footnote(
    paste0("Unique treated hospitals: ", scales::comma(n_hosp),
           ". SEs clustered by state.",
           " * p<0.10, ** p<0.05, *** p<0.01."),
    notation = "none"
  ) %>%
  writeLines("summarytable.tex")

cat("Done — summarytable.tex saved.\n")

# ==============================================================================
# State-level outcomes table
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. State Outcomes
# ------------------------------------------------------------------------------
state_outcome_info <- list(
  list(var = "medicaid_enrollment_bed", label = "Medicaid Enrollment"),
  list(var = "eligibility",         label = "Eligibility Criteria (FPL)")
)

# ------------------------------------------------------------------------------
# 2. Run state model and extract numbers
# ------------------------------------------------------------------------------
state_data_use <- state_data %>%
  filter(year <= 2022, year >= 2002,
         (is.na(rel_year) | (rel_year >= -5 & rel_year <= 5)))

state_summary_raw <- map_df(state_outcome_info, function(oi) {
  
  formula_str <- paste0(
    oi$var,
    " ~ post_treat:ever_treat + median_income_pre + exp_status",
    " | state + year"
  )
  
  result <- feols(
    as.formula(formula_str),
    data    = state_data_use,
    cluster = ~state
  )
  
  coef_names <- names(coef(result))
  post_term  <- coef_names[grepl("post_treat.*ever_treat|ever_treat.*post_treat", coef_names)]
  
  beta <- as.numeric(coef(result)[post_term])
  se   <- as.numeric(sqrt(diag(vcov(result)))[post_term])
  pval <- as.numeric(pvalue(result)[post_term])
  
  mn <- state_data %>%
    filter(rel_year == -1, ever_treat == 1) %>%
    summarise(m = mean(.data[[oi$var]], na.rm = TRUE)) %>%
    pull(m)
  
  stars <- case_when(
    pval < 0.01 ~ "***",
    pval < 0.05 ~ "**",
    pval < 0.10 ~ "*",
    TRUE        ~ ""
  )
  
  data.frame(
    Outcome = oi$label,
    Mean    = round(mn,   2),
    Beta    = round(beta, 2),
    SE      = round(se,   2),
    Stars   = stars,
    stringsAsFactors = FALSE
  )
})

# ------------------------------------------------------------------------------
# 3. N treated states
# ------------------------------------------------------------------------------
n_states <- state_data %>%
  filter(ever_treat == 1) %>%
  distinct(state) %>%
  nrow()

# ------------------------------------------------------------------------------
# 4. Save LaTeX
# ------------------------------------------------------------------------------
state_summary_raw %>%
  mutate(Beta_SE = paste0(Beta, Stars, " & (", SE, ")")) %>%
  select(Outcome, Mean, Beta_SE) %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    col.names = c("Outcome", "Mean", "$\\hat{\\beta}$ (SE)"),
    align     = c("l", "r", "r")
  ) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  pack_rows("State-Level Outcomes", 1, 2) %>%
  add_footnote(
    paste0("Unique treated states: ", n_states,
           ". SEs clustered by state.",
           " * p<0.10, ** p<0.05, *** p<0.01."),
    notation = "none"
  ) %>%
  writeLines("state_summarytable.tex")

cat("Done — state_summarytable.tex saved.\n")


# ==============================================================================
# Combined Hospital + State Table for LaTeX
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Reshape both tables so SE is its own row
# ------------------------------------------------------------------------------
format_table <- function(df) {
  df %>%
    mutate(
      Beta_Stars = paste0(Beta, Stars),
      SE         = as.character(SE)        # convert to character to match Beta_Stars
    ) %>%
    select(Outcome, Mean, Beta_Stars, SE) %>%
    pivot_longer(
      cols      = c(Beta_Stars, SE),
      names_to  = "type",
      values_to = "value"
    ) %>%
    mutate(
      Mean    = ifelse(type == "SE", "", as.character(Mean)),
      Outcome = ifelse(type == "SE", "", Outcome),
      value   = ifelse(type == "SE", paste0("(", value, ")"), value)
    ) %>%
    select(Outcome, Mean, value)
}

hosp_formatted  <- format_table(summary_table_raw)
state_formatted <- format_table(state_summary_raw)

# ------------------------------------------------------------------------------
# 2. Add a blank spacer row between sections
# ------------------------------------------------------------------------------
spacer <- data.frame(Outcome = "", Mean = "", value = "", stringsAsFactors = FALSE)

combined <- bind_rows(hosp_formatted, spacer, state_formatted)

# ------------------------------------------------------------------------------
# 3. Row indices for pack_rows
# ------------------------------------------------------------------------------
# Hospital section: 2 outcomes x 2 rows each = rows 1-10
# Spacer: row 11
# State section: 2 outcomes x 2 rows each = rows 12-15

n_hosp_rows  <- nrow(hosp_formatted)   # 10 rows (5 outcomes x 2)
n_state_rows <- nrow(state_formatted)  # 4 rows (2 outcomes x 2)
spacer_row   <- n_hosp_rows + 1
state_start  <- spacer_row + 1
state_end    <- spacer_row + n_state_rows

# ------------------------------------------------------------------------------
# 4. Render and save
# ------------------------------------------------------------------------------
combined %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    col.names = c("Outcome", "Pre-Treatment Mean", "$\\hat{\\beta}$ (SE)"),
    align     = c("l", "r", "r")
  ) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  pack_rows("Hospital-Level Outcomes", 1,           n_hosp_rows)  %>%
  pack_rows("State-Level Outcomes",    state_start, state_end)    %>%
  add_footnote(
    paste0(
      "Unique treated hospitals: ", scales::comma(n_hosp), 
      "; treated states: ", n_states, ".",
      " Hospital-level models include hospital $\\times$ cohort and year $\\times$ cohort fixed effects.",
      " State-level models include state and year fixed effects.",
      " SEs clustered by state.",
      " * p$<$0.10, ** p$<$0.05, *** p$<$0.01."
    ),
    notation = "none"
  ) %>%
  writeLines("combined_summarytable.tex")

cat("Done — combined_summarytable.tex saved.\n")

cat("Done — combined_summarytable.tex saved.\n")


