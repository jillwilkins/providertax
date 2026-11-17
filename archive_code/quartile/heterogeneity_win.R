# ==============================================================================
# HETEROGENEITY: WHICH HOSPITALS "WIN" THE MOST FROM THE PROVIDER TAX,
# AND WHICH OTHER OUTCOMES MOVE ALONGSIDE THAT WIN
# Individual hospital effect on an outcome = actual value minus the
# DiD-implied counterfactual (own fixed effect + year-within-stack fixed
# effect + controls, with the treatment interaction zeroed out).
# Requires stacked_data from 5_buildstacked.R already in the environment.
# ==============================================================================

library(dplyr)
library(fixest)
library(ggplot2)

# same manual winsorizer used in wins.R
winsorize_var <- function(x, probs = c(0.01, 0.99)) {
  lower <- quantile(x, probs[1], na.rm = TRUE)
  upper <- quantile(x, probs[2], na.rm = TRUE)
  x_win <- x
  x_win[x < lower] <- lower
  x_win[x > upper] <- upper
  x_win
}

# ------------------------------------------------------------------------------
# 1. Individual hospital effect for one outcome: actual value minus
#    counterfactual (fitted with treated forced to 0, so every rel_year:treated
#    term drops out and only FE + controls remain -- i.e. what the outcome
#    would have been on the same trajectory without the tax). Returns one row
#    per treated hospital with the raw and winsorized (1%/99%) average
#    post-treatment effect. Same spec as run_stacked_did() in 6_events_paper.R.
# ------------------------------------------------------------------------------
estimate_hosp_win <- function(outcome_var, data = stacked_data) {
  formula_str <- paste0(
    outcome_var, " ~ i(rel_year, treated, ref = -1) + exp_status + median_income_pre",
    " | mcrnum^df + year^df"
  )
  model <- feols(as.formula(formula_str), data = data, cluster = ~state)

  cf     <- predict(model, newdata = data %>% mutate(treated = 0))
  effect <- data[[outcome_var]] - cf

  # Sanity checks: control obs should average ~0; treated-post average should
  # match the ATT implied by the event-study coefficients
  att_from_coefs     <- mean(coef(model)[grepl("rel_year::[0-9]+:treated", names(coef(model)))])
  att_from_residuals <- mean(effect[data$treated == 1 & data$rel_year >= 0], na.rm = TRUE)
  cat(sprintf(
    "\n[%s] control-mean effect (should be ~0): %.5f | ATT from coefs: %.5f | ATT from residuals: %.5f\n",
    outcome_var,
    mean(effect[data$treated == 0], na.rm = TRUE),
    att_from_coefs,
    att_from_residuals
  ))

  data %>%
    mutate(.effect = effect) %>%
    filter(treated == 1, rel_year >= 0, !is.na(.effect)) %>%
    group_by(mcrnum) %>%
    summarise(win = mean(.effect, na.rm = TRUE), n_post_years = n(), .groups = "drop") %>%
    mutate(win_w = winsorize_var(win, c(0.01, 0.99)))
}

# same definition as 6_events_paper.R; computed here too in case this script
# runs before that one populates it
stacked_data <- stacked_data %>%
  mutate(mcaid_dis_bed = mcaid_discharges / pre_beds_avg)

# op_margin is the primary "win" metric everything below is ranked on
win_op_margin <- estimate_hosp_win("op_margin")

cat("\nHospitals with a win estimate:", nrow(win_op_margin), "\n")
cat("\nRaw win:\n")
print(summary(win_op_margin$win))
cat("\nWinsorized win (1%/99%):\n")
print(summary(win_op_margin$win_w))

# The "moving alongside" outcomes: same individual-effect procedure, just
# renamed so they can sit next to op_margin's win without clobbering it
win_ucc2npr <- estimate_hosp_win("ucc2npr") %>%
  rename(ucc2npr_win = win, ucc2npr_win_w = win_w) %>%
  select(mcrnum, ucc2npr_win, ucc2npr_win_w)

win_uncomp_bed <- estimate_hosp_win("uncomp_bed") %>%
  rename(uncomp_bed_win = win, uncomp_bed_win_w = win_w) %>%
  select(mcrnum, uncomp_bed_win, uncomp_bed_win_w)

win_mcaid <- estimate_hosp_win("mcaid_dis_bed") %>%
  rename(mcaid_dis_bed_win = win, mcaid_dis_bed_win_w = win_w) %>%
  select(mcrnum, mcaid_dis_bed_win, mcaid_dis_bed_win_w)

# Cost-side and revenue-side channels that make up op_margin, so the tandem
# analysis below can separate "cost saving" from "revenue generating"
# mechanisms instead of correlating everything against the margin itself
# (which ucc2npr is mechanically part of)
win_op_bed <- estimate_hosp_win("op_bed") %>%
  rename(op_bed_win = win, op_bed_win_w = win_w) %>%
  select(mcrnum, op_bed_win, op_bed_win_w)

win_npr_bed <- estimate_hosp_win("npr_bed") %>%
  rename(npr_bed_win = win, npr_bed_win_w = win_w) %>%
  select(mcrnum, npr_bed_win, npr_bed_win_w)

hosp_win <- win_op_margin %>%
  left_join(win_ucc2npr, by = "mcrnum") %>%
  left_join(win_uncomp_bed, by = "mcrnum") %>%
  left_join(win_mcaid, by = "mcrnum") %>%
  left_join(win_op_bed, by = "mcrnum") %>%
  left_join(win_npr_bed, by = "mcrnum")

# ------------------------------------------------------------------------------
# 2. Pre-treatment characteristics, from each hospital's own (treated)
#    sub-experiment
# ------------------------------------------------------------------------------
hosp_pre_chars <- stacked_data %>%
  filter(treated == 1, rel_year < 0) %>%
  group_by(mcrnum) %>%
  summarise(
    state                     = first(state),
    pre_beds                  = mean(beds, na.rm = TRUE),
    pre_op_margin             = mean(op_margin, na.rm = TRUE),
    pre_tot_discharges        = mean(tot_discharges, na.rm = TRUE),
    pre_mcaid_prop_discharges = mean(mcaid_prop_discharges, na.rm = TRUE),
    pre_mcaid_dis_bed         = mean(mcaid_dis_bed, na.rm = TRUE),
    pre_uncomp_bed            = mean(uncomp_bed, na.rm = TRUE),
    pre_ucc2npr               = mean(ucc2npr, na.rm = TRUE),
    pct_rural                 = mean(rurlhos, na.rm = TRUE),
    typectrl                  = first(typectrl),
    cntrl                     = first(cntrl),
    .groups                   = "drop"
  ) %>%
  mutate(
    # same HCRIS/AHA ownership classification used in 2_dataclean.R
    ownership = case_when(
      !is.na(typectrl) & typectrl %in% 1:2 ~ "Nonprofit",
      !is.na(typectrl) & typectrl %in% 3:6 ~ "For-profit",
      !is.na(cntrl) & cntrl %in% c(21, 23) ~ "Nonprofit",
      !is.na(cntrl) & cntrl %in% c(30, 31, 32, 33) ~ "For-profit",
      TRUE ~ "Unknown"
    )
  )

# ------------------------------------------------------------------------------
# 3. Combine and rank (still ranked on the op_margin win)
# ------------------------------------------------------------------------------
hosp_heterogeneity <- hosp_win %>%
  left_join(hosp_pre_chars, by = "mcrnum") %>%
  arrange(desc(win_w))

View(hosp_heterogeneity)

cat("\n=== TOP 15 'WINNERS' (largest winsorized op_margin effect) ===\n")
print(head(hosp_heterogeneity, 15))

cat("\n=== BOTTOM 15 (largest winsorized op_margin losses) ===\n")
print(tail(hosp_heterogeneity, 15))

# ------------------------------------------------------------------------------
# 4. Characteristics by win quartile (4 = biggest winners), ranked on the
#    winsorized measure. Pre-period rows are levels (stationary); the
#    *_win_mean rows are the other outcomes' own post-treatment effects, to
#    see whether they move in tandem with the op_margin win.
# ------------------------------------------------------------------------------
quartile_summary <- hosp_heterogeneity %>%
  mutate(win_quartile = ntile(win_w, 4)) %>%
  group_by(win_quartile) %>%
  summarise(
    n_hospitals                  = n(),
    win_mean                     = mean(win_w, na.rm = TRUE),
    ucc2npr_win_mean              = mean(ucc2npr_win_w, na.rm = TRUE),
    uncomp_bed_win_mean           = mean(uncomp_bed_win_w, na.rm = TRUE),
    mcaid_dis_bed_win_mean        = mean(mcaid_dis_bed_win_w, na.rm = TRUE),
    pre_beds_mean                = mean(pre_beds, na.rm = TRUE),
    pre_op_margin_mean           = mean(pre_op_margin, na.rm = TRUE),
    pre_tot_discharges_mean      = mean(pre_tot_discharges, na.rm = TRUE),
    pre_mcaid_prop_dis_mean      = mean(pre_mcaid_prop_discharges, na.rm = TRUE),
    pre_mcaid_dis_bed_mean       = mean(pre_mcaid_dis_bed, na.rm = TRUE),
    pre_uncomp_bed_mean          = mean(pre_uncomp_bed, na.rm = TRUE),
    pre_ucc2npr_mean             = mean(pre_ucc2npr, na.rm = TRUE),
    pct_rural                    = mean(pct_rural, na.rm = TRUE) * 100,
    pct_nonprofit                = mean(ownership == "Nonprofit", na.rm = TRUE) * 100,
    pct_forprofit                = mean(ownership == "For-profit", na.rm = TRUE) * 100,
    .groups                      = "drop"
  )

cat("\n=== HOSPITAL CHARACTERISTICS BY OP MARGIN 'WIN' QUARTILE (4 = biggest winners) ===\n")
print(quartile_summary)
View(quartile_summary)

# ------------------------------------------------------------------------------
# 5. Do the cost-side and revenue-side channels move in tandem with their
#    respective outcome? Rather than correlating against op_margin itself
#    (which ucc2npr is mechanically part of), pair each channel with the
#    outcome it should plausibly be driving:
#      cost side:    ucc2npr, uncomp_bed  <->  op_bed  (operating expenses)
#      revenue side: mcaid_dis_bed        <->  npr_bed (net patient revenue)
# ------------------------------------------------------------------------------
tandem_pairs <- list(
  list(x = "ucc2npr_win_w",        y = "op_bed_win_w",  label = "UCC-to-NPR effect vs. Operating Expenses/Bed effect"),
  list(x = "uncomp_bed_win_w",     y = "op_bed_win_w",  label = "Uncompensated Care/Bed effect vs. Operating Expenses/Bed effect"),
  list(x = "mcaid_dis_bed_win_w",  y = "npr_bed_win_w", label = "Medicaid Discharges/Bed effect vs. Net Patient Revenue/Bed effect")
)

tandem_correlations <- lapply(tandem_pairs, function(p) {
  ct <- cor.test(hosp_heterogeneity[[p$x]], hosp_heterogeneity[[p$y]])
  data.frame(
    pair        = p$label,
    x           = p$x,
    y           = p$y,
    correlation = unname(ct$estimate),
    p_value     = ct$p.value
  )
}) %>% bind_rows()

cat("\n=== COST-SIDE VS REVENUE-SIDE MECHANISM CORRELATIONS ===\n")
print(tandem_correlations)

make_tandem_scatter <- function(xvar, yvar, xlab, ylab, filename) {
  p <- ggplot(hosp_heterogeneity, aes(x = .data[[xvar]], y = .data[[yvar]])) +
    geom_point(alpha = 0.4, color = "#2484d2") +
    geom_smooth(method = "lm", color = "#DC4B4B", se = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    labs(x = xlab, y = ylab) +
    theme_minimal(base_size = 14)

  ggsave(filename, plot = p, width = 8, height = 6, dpi = 300)
  p
}

print(make_tandem_scatter(
  "ucc2npr_win_w", "op_bed_win_w",
  "UCC-to-NPR Effect (winsorized)", "Operating Expenses/Bed Effect (winsorized)",
  "heterogeneity_ucc2npr_vs_opbed.png"
))

print(make_tandem_scatter(
  "uncomp_bed_win_w", "op_bed_win_w",
  "Uncompensated Care/Bed Effect (winsorized)", "Operating Expenses/Bed Effect (winsorized)",
  "heterogeneity_uncompbed_vs_opbed.png"
))

print(make_tandem_scatter(
  "mcaid_dis_bed_win_w", "npr_bed_win_w",
  "Medicaid Discharges/Bed Effect (winsorized)", "Net Patient Revenue/Bed Effect (winsorized)",
  "heterogeneity_mcaiddisbed_vs_nprbed.png"
))
