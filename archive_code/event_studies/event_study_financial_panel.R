# ==============================================================================
# EVENT STUDY: FINANCIAL OUTCOMES (op_bed, npr_bed, op_margin)
# Same visual style as run_stacked_did() in 6_events_paper.R (red
# pre-treatment / blue post-treatment, error bars, same Q-weighted pooled
# spec), refreshed with a taller/roomier layout -- saved as three separate
# figures (not combined into one multi-panel image). No title on any of
# them (added in LaTeX, same convention as tax_amount_by_year.png).
# op_bed/npr_bed are inflation-adjusted to 2023 dollars (medical_cpi.R) --
# the stacked sample pools cohorts that adopted 2005-2017, so nominal
# dollars would otherwise conflate real change with plain inflation over
# that span (same reasoning as the tax-amount figures). 2023 was picked
# just to stay consistent with those figures; adjusted.R's existing 2020
# basis would be equally valid -- the base year doesn't matter as long as
# it's used consistently. op_margin is a ratio and needs no adjustment.
# Self-contained: builds its own Q-weighted stacked_data. Requires
# hospdata_analysis already in the environment.
# ==============================================================================

library(dplyr)
library(fixest)
library(purrr)
library(ggplot2)

source("medical_cpi.R")

# ------------------------------------------------------------------------------
# 1. Build the Q-weighted stacked sample (same construction as
#    5_buildstacked.R / 6_events_paper.R's current default)
# ------------------------------------------------------------------------------
window <- 4
yr_min <- 1999
yr_max <- 2022

hospdata_stack <- hospdata_analysis %>%
  filter(cohort != "Always (2004)")

cohorts <- hospdata_stack %>%
  filter(gname != 0) %>%
  distinct(gname) %>%
  pull() %>%
  sort() %>%
  keep(~ (.x - window) >= yr_min & (.x + window) <= yr_max)

getdata <- function(j) {
  n_required <- 2 * window + 1
  raw <- hospdata_stack %>%
    filter(gname == j | gname == 0 | gname > j + window) %>%
    filter(year >= j - window, year <= j + window) %>%
    mutate(df = j, rel_year = year - j, treated = as.integer(gname == j))
  keep_hosps <- raw %>%
    group_by(mcrnum) %>%
    summarise(n_yrs = n_distinct(rel_year), .groups = "drop") %>%
    filter(n_yrs == n_required) %>%
    pull(mcrnum)
  raw %>% filter(mcrnum %in% keep_hosps)
}

stacked_data <- map_df(cohorts, getdata)

unit_counts <- stacked_data %>%
  distinct(mcrnum, df, treated) %>%
  group_by(df) %>%
  summarise(n_treat_a = sum(treated == 1), n_ctrl_a = sum(treated == 0), .groups = "drop")
n_treat_total <- sum(unit_counts$n_treat_a)
n_ctrl_total  <- sum(unit_counts$n_ctrl_a)

stacked_data <- stacked_data %>%
  left_join(unit_counts, by = "df") %>%
  mutate(Q_weight = if_else(treated == 1, 1, (n_treat_a / n_treat_total) / (n_ctrl_a / n_ctrl_total)))

stacked_data <- stacked_data %>%
  left_join(medical_cpi %>% select(year, inflation_factor_2023), by = "year") %>%
  mutate(
    op_bed_real  = op_bed  * inflation_factor_2023,
    npr_bed_real = npr_bed * inflation_factor_2023
  )

cat("Stacked sample: cohorts =", n_distinct(stacked_data$df),
    "| hospitals =", n_distinct(stacked_data$mcrnum), "\n")

# ------------------------------------------------------------------------------
# 2. Event-study regression + coefficient extraction, one outcome at a time
#    (same spec/logic as run_stacked_did() in 6_events_paper.R)
# ------------------------------------------------------------------------------
run_event_study <- function(outcome_var, controls = "exp_status + median_income_pre") {
  formula_str <- paste0(outcome_var, " ~ i(rel_year, treated, ref = -1) + ", controls, " | mcrnum^df + year^df")
  result <- feols(as.formula(formula_str), data = stacked_data, weights = ~Q_weight, cluster = ~state)

  data.frame(
    rel_year = as.numeric(gsub(".*::(-?[0-9]+):.*", "\\1",
                                names(coef(result))[grepl("rel_year", names(coef(result)))])),
    coef = coef(result)[grepl("rel_year", names(coef(result)))],
    se   = result$se[grepl("rel_year", names(coef(result)))]
  ) %>%
    mutate(ci_lower = coef - 1.96 * se, ci_upper = coef + 1.96 * se) %>%
    bind_rows(data.frame(rel_year = -1, coef = 0, se = 0, ci_lower = 0, ci_upper = 0)) %>%
    arrange(rel_year) %>%
    mutate(period_color = ifelse(rel_year < 0, "Pre-Treatment", "Post-Treatment"))
}

coef_op_bed    <- run_event_study("op_bed_real")
coef_npr_bed   <- run_event_study("npr_bed_real")
coef_op_margin <- run_event_study("op_margin")

# ------------------------------------------------------------------------------
# 3. Panel builder -- same visual style as run_stacked_did(), parameterized
#    for per-panel y-axis formatting. Each figure carries its own x-axis
#    title now that they're saved separately rather than stacked.
# ------------------------------------------------------------------------------
build_panel <- function(coef_data, subtitle, y_labeller) {
  ggplot(coef_data, aes(x = rel_year, y = coef, color = period_color)) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.3, linewidth = 0.8) +
    geom_point(size = 3) +
    geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 0.4) +
    scale_color_manual(values = c("Pre-Treatment" = "#DC4B4B", "Post-Treatment" = "#2484d2")) +
    scale_x_continuous(breaks = seq(min(coef_data$rel_year), max(coef_data$rel_year), 1)) +
    scale_y_continuous(labels = y_labeller) +
    labs(
      subtitle = subtitle,
      x = "Years Relative to Treatment",
      y = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.subtitle    = element_text(face = "bold", size = 13, margin = margin(b = 6)),
      axis.title.x     = element_text(size = 11, margin = margin(t = 8)),
      axis.text        = element_text(size = 10, color = "black"),
      panel.grid.major = element_line(color = "#f2f2f2", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background  = element_rect(fill = "transparent", color = NA),
      legend.position  = "none"
    )
}

p1 <- build_panel(coef_op_bed,    "Operating Expenses per Bed (2023 $)",  scales::dollar_format())
p2 <- build_panel(coef_npr_bed,   "Net Patient Revenue per Bed (2023 $)", scales::dollar_format())
p3 <- build_panel(coef_op_margin, "Operating Margin",                     scales::percent_format(accuracy = 0.1))

ggsave("event_study_op_bed.png",    p1, width = 8, height = 5.5, dpi = 300, bg = "transparent")
ggsave("event_study_npr_bed.png",   p2, width = 8, height = 5.5, dpi = 300, bg = "transparent")
ggsave("event_study_op_margin.png", p3, width = 8, height = 5.5, dpi = 300, bg = "transparent")

cat("Saved event_study_op_bed.png, event_study_npr_bed.png, event_study_op_margin.png\n")
