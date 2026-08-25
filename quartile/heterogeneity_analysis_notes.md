# Heterogeneity Analysis: Methods Notes

A walkthrough of the decisions behind `heterogeneity_win.R`, for anyone
(including future me) picking this up later. Written alongside the code on
2026-08-04.

## Goal

The main event studies show that, on average, hospitals see higher operating
expenses and net patient revenue, more Medicaid patients, and less
uncompensated care after the provider tax. This analysis asks: **which
hospitals benefit the most, and do the other outcomes move together with
that benefit?**

## 1. Defining a hospital's "win"

We proxy a hospital's benefit from the tax using its change in **operating
margin**. The naive approach — each hospital's raw pre-period vs. post-period
average margin — was rejected early on: it bakes in secular trends and mean
reversion that have nothing to do with the tax.

Instead, `estimate_hosp_win()` computes each hospital's **individual
effect**: its actual value minus a *counterfactual* prediction (what its
outcome would have been on the same trajectory without the tax). The
counterfactual comes from the same stacked event-study model used in
`6_events_paper.R` (`mcrnum^df` + `year^df` fixed effects, `exp_status` +
`median_income_pre` controls), but with `treated` forced to 0 in `predict()`
— this zeroes out every `rel_year:treated` term and leaves only the
hospital's own fixed effect, the common year trend, and controls.

**Validated against the pooled model:** averaging these individual effects
across all treated, post-period hospital-years reproduces the ATT implied by
the event-study coefficients almost exactly (confirmed for `op_margin`,
`ucc2npr`, `uncomp_bed`, `mcaid_prop_discharges` — control-group hospitals
average to ~0 as they should). This is what the `cat()` sanity-check lines
in the function print each time it runs.

A hospital's `win` is the average of this individual effect across its own
post-treatment years (`rel_year >= 0`).

## 2. Winsorizing

Because each hospital's effect rests on only ~5 post-treatment years with no
cross-unit averaging, a single anomalous year (accounting one-offs, etc.)
can produce an extreme `win`. Raw values ranged from -0.81 to +0.84 — far
outside any plausible true margin effect. We winsorize at the 1st/99th
percentile (`winsorize_var()`, same threshold `wins.R` already uses
elsewhere), which pulled the range in to about ±0.12 (about 14 hospitals
capped on each end, out of 1,354). Quartile-level characteristics barely
moved when we did this — evidence the outliers were never driving the
quartile comparisons, just individual point estimates. One side effect:
several capped hospitals now tie at the winsorized bound, so the
top/bottom-15 printout is less meaningful than the quartile table for
drawing conclusions.

## 3. Hospital characteristics

Pre-period (rel_year < 0) averages, pulled from each treated hospital's own
sub-experiment: beds, operating margin, total discharges, Medicaid share
*and* Medicaid discharges/bed (both — see §5), uncompensated care/bed,
`ucc2npr`, % rural, and ownership (Nonprofit/For-profit, reconstructed using
the same HCRIS/AHA `typectrl`/`cntrl` classification as `2_dataclean.R`).

We dropped system affiliation (`in_system`/`pct_in_system`) — it wasn't
differentiating winners from losers.

## 4. Ranking

Hospitals are quartile-ranked on the winsorized op_margin `win`
(`quartile_summary`). Quartile 4 = biggest winners. This table is the main
"who wins" output — it's where the ownership, bed size, and payer-mix
patterns live.

## 5. Do other outcomes move in tandem?

Initially we correlated `ucc2npr`, `uncomp_bed`, and `mcaid_prop_discharges`
effects directly against the op_margin `win`. That's a flawed comparison for
`ucc2npr` specifically, since it's *part of* the margin calculation
(uncompensated care over net patient revenue) — any correlation there is
partly mechanical, not two independent things moving together.

We replaced this with a **mechanism decomposition**: pair each outcome with
the side of the margin it should plausibly be driving, rather than with the
margin itself.

| Channel | Pair | Correlation | p-value |
|---|---|---|---|
| Cost-side | `ucc2npr` effect ↔ `op_bed` effect | -0.132 | <0.001 |
| Cost-side | `uncomp_bed` effect ↔ `op_bed` effect | +0.177 | <0.001 |
| Revenue-side | `mcaid_dis_bed` effect ↔ `npr_bed` effect | +0.173 | <0.001 |

Each pair gets a `cor.test()` and a scatterplot with a linear fit
(`heterogeneity_ucc2npr_vs_opbed.png`, `heterogeneity_uncompbed_vs_opbed.png`,
`heterogeneity_mcaiddisbed_vs_nprbed.png`).

**Note the cost-side pairs point in opposite directions** — `uncomp_bed` and
`ucc2npr` aren't telling the identical story, likely because `ucc2npr`'s
revenue denominator is doing some of the work. Worth deciding which one you
lean on before writing this up, or presenting both with that caveat.

## 6. Medicaid share vs. Medicaid volume

The revenue-side pairing originally used `mcaid_prop_discharges` (share of
discharges) and came back essentially uncorrelated with `npr_bed` (r=0.003,
p=0.91). Swapping in `mcaid_dis_bed` (Medicaid discharges per bed —
`mcaid_discharges / pre_beds_avg`, same definition as `6_events_paper.R`)
produced a real relationship (r=0.173, p<0.001). Volume, not payer-mix
share, is what tracks revenue here. Both versions are now kept side by side
in the pre-period characteristics and quartile table (`pre_mcaid_prop_discharges`
and `pre_mcaid_dis_bed`) so either framing is available.

## Caveats to keep in mind

- **Individual-hospital effects are noisy.** No cross-unit averaging within
  a hospital's own estimate — treat single-hospital point estimates (the
  top/bottom-15 lists) as illustrative, not precise. The quartile-level
  aggregates (averaged over ~338 hospitals each) are far more trustworthy.
- **Correlating two noisy effects attenuates the correlation toward zero**
  relative to the true relationship. The mechanism correlations above (all
  ~0.13–0.18) likely understate, not overstate, the real co-movement.
- **`ucc2npr` has a mechanical link to `op_margin`** — keep that in mind
  anywhere it's used as an "independent" outcome.

## Files produced

- `heterogeneity_win.R` — the full analysis script (requires `stacked_data`
  from `5_buildstacked.R` already in the environment).
- `heterogeneity_ucc2npr_vs_opbed.png`, `heterogeneity_uncompbed_vs_opbed.png`,
  `heterogeneity_mcaiddisbed_vs_nprbed.png` — mechanism scatterplots.
