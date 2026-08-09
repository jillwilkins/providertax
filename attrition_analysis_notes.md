# Attrition Analysis: Methods Notes

A walkthrough of the decisions behind `attrition_analysis.R`, for anyone
(including future me) picking this up later. Written alongside the code on
2026-08-05/06.

## Goal

`5_buildstacked.R` only keeps hospitals present in **every** event-time year
within a sub-experiment (a balanced-panel requirement). This raises an
obvious question: could the provider tax itself be pushing hospitals out of
that balanced sample -- e.g. financial distress leading to closure -- which
would mean the main results are estimated on a survivor-biased sample?

## 1. First attempt (rejected): stacked-design version

The first version rebuilt each cohort's sub-experiment (mirroring
`getdata()` in `5_buildstacked.R`) and tracked who failed the balanced-panel
filter, split by treated vs. control. This was **abandoned** because the
stacked design reuses the same not-yet-treated hospital as a "control" in
several different cohorts' sub-experiments -- so a single physical hospital
could contribute multiple rows to the attrition count, inflating N and
making "post-adoption" ambiguous for controls (they have no adoption year
of their own within a borrowed cohort window). A related multi-sub-experiment
subtlety came up later in `expansion_analysis_notes.md`.

## 2. Final approach: one row per hospital, no stacking

`attrition_analysis.R` works directly off the raw panel (`hospdata_analysis`,
excluding the "Always (2004)" cohort, same exclusion as `5_buildstacked.R`):

- One row per `mcrnum`: first/last year observed, whether the run is
  **contiguous** (no gaps between first and last appearance), and
  `ever_treat`.
- `panel_end_year` is computed dynamically as `max(year)` rather than
  hardcoded -- confirmed 2023 has solid coverage (2,346 hospitals vs. 2,375
  in 2022), so it's real data, not a stub/lag year.
- A "clean exit" = contiguous AND its last observed year is before
  `panel_end_year` (present, then gone for good -- not just a mid-panel
  reporting gap, which is a different phenomenon).
- For treated hospitals, exit timing is measured relative to **their own**
  `firsttax` adoption year (not a borrowed cohort clock), since only
  treated hospitals have a real adoption year to be "before/after" of.

## Key results

- **Ever-treated hospitals exit *less* often than never-treated ones**:
  32.9% vs. 50.8% clean-exit rate (two-proportion test, p < 0.001). No
  evidence the tax pushes more hospitals out of the sample -- if anything
  the opposite.
- Among treated hospitals that do cleanly exit, **71.2% exit before their
  own state's tax adoption**, 28.8% at/after. Checked this isn't a
  mechanical artifact of unequal pre/post exposure windows -- median
  pre-adoption exposure is 11 years, median post is 12, so it's not just
  "less time available to exit after adoption."
- Hospitals that cleanly exit (treated or not) look like the generic
  financially-vulnerable-hospital profile: much smaller (105 vs. 224 avg.
  beds), negative pre-period operating margin (-0.085 vs. +0.017), far
  fewer discharges (3,628 vs. 12,672), more rural (11.6% vs. 9.7%), more
  for-profit (37.3% vs. 19.6%).

## Caveats

- **"Exits the balanced/reporting panel" is not proof of hospital
  closure.** Could also reflect a merger (acquisition changes `mcrnum`), an
  ownership-type change (e.g. becoming government-owned, which
  `2_dataclean.R` already filters out), or a non-filing/reporting gap.
  Next step (in progress as of this writing): check the AHA/HCRIS source
  data for an actual status or merger flag to disambiguate.
- The pooled treated/control comparison isn't adjusted for hospital
  characteristics -- it's a simple two-proportion test, not a regression.
- Read the headline as "no evidence the tax differentially pushes hospitals
  out of the sample, and the ones that do leave look like generically
  vulnerable hospitals" -- not "the tax has no effect on hospital
  survival."

## Files produced

- `attrition_analysis.R` -- self-contained; only requires `hospdata_analysis`
  in the environment (builds its own per-hospital presence table, no
  dependency on `stacked_data`).
