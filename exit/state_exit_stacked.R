# ==============================================================================
# STATE-YEAR EXIT EVENT STUDY (STACKED DESIGN)
# State-level analog of 5_buildstacked.R: same cohorts/window/control logic,
# but the unit is the state and the outcome is the count of hospitals that
# exit the panel from t-1 to t (rather than a hospital-level financial
# outcome).
#
# One necessary departure from 5_buildstacked.R: its balanced-panel filter
# (getdata()'s Step 2) drops any hospital that doesn't survive the full event
# window -- which would throw out every exit before it could be counted.
# hospdata_stack itself (5_buildstacked.R's line "hospdata_stack <-
# hospdata_analysis %>% filter(cohort != ...)") is NOT balanced -- balancing
# only happens inside getdata(), producing the separate `stacked_data`
# object that 6_events_paper.R consumes. This script mirrors getdata()'s
# Step 1 (same cohort/control logic) but skips the Step 2 balance filter, so
# hospitals are free to leave partway through a window and register as an
# exit. See attrition_analysis_notes.md for the same tension at the hospital
# level.
#
# SELF-CONTAINED / NAMESPACED: this script deliberately never assigns to
# hospdata_stack, stacked_data, window, yr_min, yr_max, cohorts,
# unit_counts, n_treat_total, or n_ctrl_total -- those names are reused as
# bare globals across 5_buildstacked.R, 6_events_paper.R, and several other
# scripts in this repo, so overwriting them here would corrupt whatever
# those scripts built earlier in the same session. Every object this script
# creates is prefixed exit_ (or otherwise uniquely named) instead.
#
# Requires only hospdata_analysis (read-only) in the environment.
# ==============================================================================
library(dplyr)
library(purrr)
library(tidyr)
library(fixest)
library(ggplot2)
library(scales)

# ------------------------------------------------------------------------------
# 0. Parameters (namespaced -- see note above)
# ------------------------------------------------------------------------------
exit_window <- 4
exit_yr_min <- 1999
exit_yr_max <- 2022

hospdata_exit_base <- hospdata_analysis %>%
  filter(cohort != "Always (2004)")   # never-treated (cohort == "Never (0)") stays

exit_cohorts <- hospdata_exit_base %>%
  filter(gname != 0) %>%
  distinct(gname) %>%
  pull() %>%
  sort() %>%
  keep(~ (.x - exit_window) >= exit_yr_min &
          (.x + exit_window) <= exit_yr_max)

cat("\n=== TREATMENT COHORTS ===\n")
cat("Valid cohorts:", exit_cohorts, "\n")

# ------------------------------------------------------------------------------
# 1. Sub-experiment builder -- state x event-year exit counts
# ------------------------------------------------------------------------------
get_state_exit_data <- function(j) {

  raw <- hospdata_exit_base %>%
    filter(
      gname == j |                  # treated cohort j
      gname == 0 |                  # never-treated controls
      gname > j + exit_window       # not-yet-treated clean controls
    ) %>%
    filter(
      year >= j - exit_window,
      year <= j + exit_window
    ) %>%
    mutate(
      df       = j,
      rel_year = year - j,
      treated  = as.integer(gname == j)
    )

  # Risk set: any hospital observed at any point within this window -- not
  # restricted to hospitals present at the window's first year. Requiring
  # presence at rel_year == -exit_window would silently drop late entrants
  # (and anyone not there on day one) from ever being able to register an
  # exit.
  hosp_run <- raw %>%
    group_by(mcrnum) %>%
    summarise(
      state     = first(state),
      treated   = first(treated),
      n_yrs     = n_distinct(rel_year),
      first_rel = min(rel_year),
      last_rel  = max(rel_year),
      .groups   = "drop"
    ) %>%
    mutate(
      contiguous = n_yrs == (last_rel - first_rel + 1),
      # Clean exit within this window: contiguous run, gone before window end.
      # Hospitals with a mid-window gap are excluded from both the exit and
      # survivor counts (same "ambiguous, not a clean exit" treatment as
      # attrition_analysis.R).
      exits    = contiguous & last_rel < exit_window,
      exit_rel = last_rel + 1   # the t-1 -> t transition: first year absent
    )

  state_treated <- hosp_run %>% distinct(state, treated)

  exit_counts <- hosp_run %>%
    filter(exits) %>%
    count(state, rel_year = exit_rel, name = "exit_count")

  out <- expand_grid(
    state    = unique(hosp_run$state),
    rel_year = (-exit_window + 1):exit_window
  ) %>%
    left_join(state_treated, by = "state") %>%
    left_join(exit_counts, by = c("state", "rel_year")) %>%
    mutate(
      exit_count = coalesce(exit_count, 0L),
      df         = j,
      year       = j + rel_year
    )

  cat(sprintf(
    "Sub-exp %d | hosps observed: %4d | mid-window gaps excluded: %3d | states: %3d (treated %d / control %d)\n",
    j, nrow(hosp_run), sum(!hosp_run$contiguous), n_distinct(out$state),
    sum(state_treated$treated == 1), sum(state_treated$treated == 0)
  ))

  out
}

# ------------------------------------------------------------------------------
# 2. Build stacked state-year exit data
# ------------------------------------------------------------------------------
cat("\n=== BUILDING STATE-YEAR EXIT SUB-EXPERIMENTS ===\n")
state_exit_stacked <- map_df(exit_cohorts, get_state_exit_data)

cat("\n=== STACK SUMMARY ===\n")
cat("Total rows     :", nrow(state_exit_stacked),               "\n")
cat("Total states   :", n_distinct(state_exit_stacked$state),   "\n")
cat("Sub-experiments:", n_distinct(state_exit_stacked$df),      "\n")

cat("\n=== STACKED DATA COMPOSITION BY SUB-EXPERIMENT ===\n")
state_exit_stacked %>%
  group_by(df) %>%
  summarise(
    n_states     = n_distinct(state),
    n_treated    = n_distinct(state[treated == 1]),
    n_control    = n_distinct(state[treated == 0]),
    total_exits  = sum(exit_count),
    .groups      = "drop"
  ) %>%
  print()

# ------------------------------------------------------------------------------
# 3. Q-weights -- same reweighting logic as 5_buildstacked.R, counting
#    treated/control STATES (the unit here) instead of hospitals.
# ------------------------------------------------------------------------------
exit_unit_counts <- state_exit_stacked %>%
  distinct(state, df, treated) %>%
  group_by(df) %>%
  summarise(
    n_treat_a = sum(treated == 1),
    n_ctrl_a  = sum(treated == 0),
    .groups   = "drop"
  )

exit_n_treat_total <- sum(exit_unit_counts$n_treat_a)
exit_n_ctrl_total  <- sum(exit_unit_counts$n_ctrl_a)

state_exit_stacked <- state_exit_stacked %>%
  left_join(exit_unit_counts, by = "df") %>%
  mutate(
    treat_share = n_treat_a / exit_n_treat_total,
    ctrl_share  = n_ctrl_a  / exit_n_ctrl_total,
    Q_weight    = if_else(treated == 1, 1, treat_share / ctrl_share)
  )

cat("\n=== Q-WEIGHT INPUTS ===\n")
exit_unit_counts %>%
  mutate(
    treat_share = n_treat_a / exit_n_treat_total,
    ctrl_share  = n_ctrl_a  / exit_n_ctrl_total,
    Q           = treat_share / ctrl_share
  ) %>%
  print()

# ------------------------------------------------------------------------------
# 4. Event-study regression
#    Unit is the state, so state^df plays the role mcrnum^df played in the
#    hospital-level design. cluster=~state (not state^df) to match the
#    convention used by this project's other state-level stacked event
#    studies (results_tables/state_event_studies_stacked.R).
# ------------------------------------------------------------------------------
state_exit_result <- feols(
  exit_count ~ i(rel_year, treated, ref = -1) | state^df + year^df,
  data    = state_exit_stacked,
  weights = ~Q_weight,
  cluster = ~state
)

print(summary(state_exit_result))

# ------------------------------------------------------------------------------
# 5. Plot -- same visual convention as run_state_stacked_did() in
#    results_tables/state_event_studies_stacked.R
# ------------------------------------------------------------------------------
exit_coef_names <- names(coef(state_exit_result))
exit_is_rel_year <- grepl("rel_year", exit_coef_names)

exit_coef_data <- data.frame(
  rel_year = as.numeric(gsub(".*::(-?[0-9]+):.*", "\\1", exit_coef_names[exit_is_rel_year])),
  coef     = coef(state_exit_result)[exit_is_rel_year],
  se       = state_exit_result$se[exit_is_rel_year]
) %>%
  mutate(ci_lower = coef - 1.96 * se, ci_upper = coef + 1.96 * se) %>%
  bind_rows(data.frame(rel_year = -1, coef = 0, se = 0, ci_lower = 0, ci_upper = 0)) %>%
  arrange(rel_year) %>%
  mutate(period_color = ifelse(rel_year < 0, "Pre-Treatment", "Post-Treatment"))

p_exit <- ggplot(exit_coef_data, aes(x = rel_year, y = coef, color = period_color)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.3, size = 0.8) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Pre-Treatment" = "#DC4B4B", "Post-Treatment" = "#2484d2")) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.4) +
  labs(x = "Years Relative to Treatment", y = " ") +
  scale_x_continuous(breaks = seq(min(exit_coef_data$rel_year), max(exit_coef_data$rel_year), 1)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(face = "bold", size = 20, hjust = 0.5),
    axis.title.x    = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y    = element_text(size = 12, margin = margin(r = 10)),
    axis.text       = element_text(size = 11, color = "black"),
    panel.grid.major = element_line(color = "#f2f2f2", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    legend.position  = "none"
  )

print(p_exit)
ggsave("exit/state_exit_stacked.png", plot = p_exit, width = 10, height = 6.5, dpi = 300)
cat("\nPlot saved to: exit/state_exit_stacked.png\n")
