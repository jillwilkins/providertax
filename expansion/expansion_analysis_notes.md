# Medicaid Expansion Analysis: Methods Notes

A walkthrough of the decisions behind `expansion_analysis.R` and
`expansion_table.R`, for anyone (including future me) picking this up
later. Written alongside the code on 2026-08-05/06.

## Goal

Check whether ACA Medicaid expansion is confounding the provider tax
results: (1) do the main results hold up in a pre-expansion-only sample,
and (2) does the estimated tax effect actually differ between expansion and
non-expansion states?

## 0. Prerequisite: the `exp_status` bug

Before any of this, found that **`exp_status`** -- the state-level control
already used in virtually every model (`+ exp_status + median_income_pre`)
-- was broken. Its construction (`1_data.R`) checked `is.na(expyear)` to
flag non-expansion states as 0, but non-expansion states are coded
`expyear = 0`, not `NA`. So the check never caught them, and `year >=
expyear` (i.e. `year >= 0`) was true for every real year. Confirmed on
Texas: `exp_status = 1` in 100% of its state-years despite Texas never
expanding Medicaid. All 10 never-expansion states (AL, FL, GA, KS, MS, SC,
TN, TX, WI, WY) were affected -- `exp_status = 1` in 100% of their rows.
Overall panel-wide share dropped from 41.4% to 20.5% after the fix.

**Fixed in two places:**
- `1_data.R` -- corrected the `case_when` to also check `expyear == 0`.
- `hospdata_analysis.csv` -- patched in place (recomputed `exp_status` from
  the existing `expyear`/`year` columns, no need to rerun the full
  pipeline). Original backed up to
  `hospdata_analysis_backup_pre_expstatus_fix.csv`.

**Checked how much it mattered:** reran the pooled `treated:post` ATT
(Q-weighted, same spec as `table_for_pres.R`) with the old vs. fixed
control, across 7 core outcomes. No sign flips, and nothing that was
significant became insignificant or vice versa -- magnitudes shifted
modestly (roughly 2-8% for the significant outcomes; op_bed
$36,430→$35,670, mcaid_prop_discharges 0.0140→0.0131, etc.). Good news: the
fix changes precision slightly, not conclusions.

**Also found and removed:** `exp_during` (same file) had the same root
cause plus its two boundary conditions were OR'd instead of AND'd, making
it far more permissive than its "within 3 years pre / 5 years post"
comment described. It wasn't used anywhere downstream, so rather than guess
at the intended fix, it was deleted from `1_data.R` -- to be properly
redefined when actually needed (e.g. it's a natural fit for a sharper
concurrent-timing version of this expansion analysis).

**One more wrinkle, unrelated to the bug:** `expyear` is `NA` for a large
share of rows even in genuine expansion states (e.g. NY shows `expyear =
2014` in 2,569 rows but `NA` in 822 rows -- looks like a pre-existing merge
gap). This meant a row-wise `ever_expanded` check misclassified real
expansion states as "never expanded" whenever they had any NA row. Fixed by
classifying at the **state level**: a state counts as ever-expanded if
*any* of its rows shows a real (non-NA, >0) `expyear`.

**Also noted, not fixed:** 18 duplicate `mcrnum`-year rows in
`hospdata_analysis.csv` (0.03% of rows) -- pre-existing, unrelated, judged
not worth chasing right now.

## 1. Full timing vs. "pretend expansion never happened" (year < 2014)

Rebuilds the stacked sample restricted to `yr_max = 2013` instead of 2022,
using a parameterized version of `5_buildstacked.R`'s builder
(`build_stacked_data()` in `expansion_analysis.R`, so both windows are
built identically).

**Caveat that matters a lot:** this is *not* "the same analysis with less
data." Cohorts adopted 2010+ need post-period data past 2014, which the
cutoff removes -- so the restricted sample only retains the 2005, 2006, and
2009 cohorts (1,640 hospitals vs. 1,799 in the full sample). It's really a
comparison against the *earliest-adopting states only*, not a clean
time-truncation of the same sample.

**Result:** the cost-side story (uncompensated care, `ucc2npr`) gets much
*stronger* and significant pre-2014, while the Medicaid-volume story
(`mcaid_prop_discharges`, `mcaid_dis_bed`) *loses* significance. Op_bed and
npr_bed hold up in both. Interesting, not something to over-interpret given
the cohort-composition caveat above, but consistent with the expansion-vs-
non-expansion split (see below) telling the same directional story.

## 2. Expansion vs. non-expansion states, split sample

Filtered `hospdata_analysis` to ever-expanded states vs. never-expanded
states (the state-level `ever_expanded` flag from §0) and reran the full
stacked pipeline separately on each. Non-expansion states are a much
smaller sample (383 hospitals / 5 cohorts vs. 1,399 / 9 cohorts for
expansion states), so almost nothing clears significance in either half on
its own -- this split-sample view is superseded by the formal test below,
but it's what motivated it: Medicaid share/volume point estimates were
consistently larger in non-expansion states (0.0132 vs. 0.0056 for share;
0.749 vs. 0.202 for volume), suggesting states that already expanded had
less "room" for the tax to additionally move Medicaid volume.

## 3. Formal test: triple interaction

The right way to test "does the ATT differ by expansion status" is one
pooled model, not eyeballing two separately-estimated noisy coefficients.
Added `treated:post:ever_expanded` to the pooled spec, on the **full-timing**
sample:

```
outcome ~ treated:post + treated:post:ever_expanded + post:ever_expanded +
           i(rel_year, treated, ref = -1, keep = -4:-2) +
           exp_status + median_income_pre
         | mcrnum^df + year^df
```

`post:ever_expanded` is included so the interaction isn't just picking up a
generic differential post-period trend between expansion/non-expansion
states that has nothing to do with the tax. `ever_expanded` and
`treated:ever_expanded` alone are time-invariant within each hospital x
cohort cell and are absorbed by the `mcrnum^df` fixed effect, so they don't
need to be added separately.

**Result: no significant differential effect on any of the 7 outcomes**
(all p > 0.10, several around 0.7-0.9). This is the clean, defensible
result for the paper -- formal evidence that the tax's effect doesn't
differ by expansion status, supporting the argument that expansion isn't
driving the main results.

**Caveat:** the interaction terms have wide standard errors (e.g. ±$33,051
on npr_bed against a base ATT around $33,766) -- this is a low-power test
against small-to-moderate true differences. Frame as "no evidence of a
differential effect," not "proof the true difference is zero."

## Table output

`expansion_table.R` combines Part 1 and Part 3 (not Part 2, which was
superseded) into one two-panel LaTeX table (`expansion_table.tex`):
- **Panel A: Triple Interaction** -- Non-Expansion (baseline ATT) /
  Expansion Diff. (the coefficient of interest)
- **Panel B: Full Timing vs. Pre-2014-Only**

Note: `add_footnote()` in kableExtra double-escapes LaTeX math (`\times`,
`$<$`) into `\textbackslash{}` garbage -- avoid inline math in footnotes
entirely (matches how `table_for_pres.R`'s own footnotes already do this,
plain "p<0.10" not "p$<$0.10").

## Files produced

- `expansion_analysis.R` -- self-contained; builds its own stacked samples
  (needs more than one window), only requires `hospdata_analysis`.
- `expansion_table.R` -- requires `results_full`, `results_pre2014`,
  `triple_results` already in the environment.
- `expansion_table.tex`
