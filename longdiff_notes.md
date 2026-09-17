# Quantile Regression Notes: What We're Actually Doing, and Why

A walkthrough of the reasoning behind `longdiff.R` (renamed from
`quant_reg.R`), for anyone (including future me) picking this up later.
`quantreg_ancova.R` and `quantreg_stack.R` were built and compared against
the long-difference approach (sections 8-9 below) but have since been
removed -- long-difference is the one retained going forward. The
comparison is kept here because it's the reasoning *for* that choice, not
because the scripts still exist.

## 1. The motivating question

The main event-study results show the tax's *average* effect. The natural
follow-up: does the tax affect hospitals differently depending on where they
sit in the distribution, rather than just on average?

## 2. Two different things both called "quantile regression"

- **Covariate-quantile-bin heterogeneity** (`quartile/heterogeneity_pretax_quantiles.R`):
  split hospitals into bins by a *pre-treatment* covariate (pre-tax Medicaid
  share, pre-tax op_margin), then estimate the normal stacked DiD separately
  by bin. The bin is fixed before treatment and never changes.
- **Koenker-Bassett quantile regression** (`rq()`): models different points
  (τ) of the *outcome's own* conditional distribution. Does not group
  hospitals into anything -- τ describes a position in Y's distribution,
  which treatment itself can move hospitals into or out of.

These answer genuinely different questions. This file is about the second one.

## 3. Why fixed effects don't carry over from OLS to quantile regression

`feols()` absorbs high-dimensional fixed effects (`mcrnum^df`) because the
conditional *mean* is a linear operator: `E[Y - c] = E[Y] - c` for *any* `c`,
including an estimated one. Demeaning works because of this.

Quantiles are not linear operators. `Q_tau(Y - c) != Q_tau(Y) - c` in general
when `c` is estimated from the same data and varies across units. This is
the whole reason `rq()` can't just absorb `mcrnum` fixed effects the way
`feols()` does, and why `rqpd::pfe` (which tries to *estimate* each
hospital's fixed effect directly, penalized) turned out to be broken for
this data: single-tau `pfe` calls returned the *identical* coefficient
regardless of which tau was requested. Not a data problem -- a real limit of
that estimator (the "incidental parameters problem": with ~1,800 hospitals
and only ~8-9 years each, jointly estimating that many nuisance parameters
under a non-quadratic loss biases the coefficient you actually care about,
and that bias doesn't shrink just because you have more hospitals -- only
more years per hospital fixes it, and we don't have that).

## 4. Worked example: why demeaning distorts quantiles (but not means)

Two hospitals, three years each:

| | Year 1 | Year 2 | Year 3 | Own average |
|---|---|---|---|---|
| A | 9 | 10 | 11 | 10 |
| B | 2 | 5 | 8 | 5 |

In the raw data, *every* value of A is higher than *every* value of B.

Subtract each hospital's own average:

| | Year 1 | Year 2 | Year 3 |
|---|---|---|---|
| A residual | -1 | 0 | 1 |
| B residual | -3 | 0 | 3 |

B's top residual (3) is now *higher* than A's top residual (1) -- B
leapfrogged A, purely because B has more spread around its own average, not
because B became more expensive. The clean separation in the raw data is
gone.

Check the bookkeeping. Pooled raw mean = 7.5; pooled residual mean = 0;
and 7.5 minus the average of the two hospital means (10, 5) = 0 exactly --
holds every time, for any numbers. Now the median: pooled raw median = 8.5;
pooled residual median = 0; but 8.5 minus that same average-of-means (7.5)
= 1, not 0. The identity that's *always* exact for means is simply wrong
for medians/quantiles. Means only depend on sums (unaffected by how
subtracting different amounts from different groups scrambles rank order);
quantiles depend on rank order (which that scrambling directly corrupts).

## 5. Worked example: why differencing avoids the distortion

Split each hospital's own years into pre/post instead of pooling with the
other hospital:

| | Pre (avg) | Post |
|---|---|---|
| A | 9.5 | 11 |
| B | 3.5 | 8 |

Delta_A = 11 - 9.5 = 1.5. Delta_B = 8 - 3.5 = 4.5.

Decompose explicitly: if A's fixed flavor is `a_A = 9.5`, its years are
`a_A - 0.5`, `a_A + 0.5` (pre-avg = `a_A + 0`), and post = `a_A + 1.5`.
`Delta_A = (a_A + 1.5) - (a_A + 0) = 1.5` -- `a_A` cancels *regardless of
its value*. Same for B, regardless of `a_B`.

Pool the deltas {1.5, 4.5} and ask for a quantile: totally clean, because
each delta was built using *only that hospital's own two numbers* -- B's
data never touched A's calculation. There's no cross-hospital residual
comparison here for rank order to get scrambled in. The question has fully
changed to be about *changes*, and once you accept that, nothing is left to
distort.

## 6. What this buys you, precisely

Don't try to *estimate* each hospital's fixed effect (that's what `pfe`
does, fragile under quantile loss with small T). Instead, *difference it
away* with plain arithmetic before running any quantile regression at all,
then run a completely ordinary `rq()` on what's left, with nothing
hospital-specific in the formula -- because there's nothing left to control
for.

Important nuance: for *mean* regression, first-differencing and
fixed-effects estimation are the same trick wearing different clothes (with
T=2 they're numerically identical). They diverge specifically for quantile
regression, because only one of the two strategies (differencing) avoids
feeding noisy per-unit estimates into a loss function that can't average
that noise away.

## 7. Levels vs. changes: what question are we actually answering

Differencing changes the *economic* question, not just the estimation
method.

- **Levels** (`op_bed ~ tax_on + ...`, no differencing): "low end" / "high
  end" means cheap vs. expensive hospitals -- mostly a size/type distinction.
  Answers: does the tax affect small vs. large hospitals differently?
- **Changes** (`delta_op_bed ~ treated + ...`, what `quant_reg.R` runs):
  "low end" / "high end" means hospitals whose op_bed barely moved (or fell)
  vs. hospitals whose op_bed grew the most. Answers: does the tax's added
  effect depend on whether a hospital was already flat or already rapidly
  changing?

Neither is more "correct" -- they're different questions. Know which one
you're writing up.

## 8. Getting back to a levels question, two ways

- **ANCOVA** (`quantreg_ancova.R`): `post_op_bed ~ treated + pre_op_bed +
  controls`, still one row per hospital x cohort. Instead of forcing a
  strict 1-for-1 pre/post relationship (what differencing assumes), the
  pre-period level enters as its own covariate with an estimated
  coefficient. Reuses the exact same collapsed data as the long-difference
  version.
- **Full stack** (`quantreg_stack.R`): every hospital-year row from
  `stacked_data`, `op_bed` as a raw level, `treated*post`, with `state` and
  `df` (cohort) as plain dummies instead of any individual hospital control.

These gave *very* different answers on `op_bed` (ANCOVA: 27K down to -20K
across tau; full stack: 23K up to 191K, monotonically rising). Why: op_bed's
level varies enormously with hospital size, and full-stack has no way to
net that out beyond state averages -- a large hospital's op_bed can land in
a high tau purely because it's large, not because of treatment. ANCOVA
nets out each hospital's own starting point via `pre_op_bed`, which is a
much stronger control for size/type than a state dummy.

**Lord's Paradox**: this specific kind of divergence between a
level-on-lagged-level model and a change-score model, when groups differ at
baseline, has a name (Lord, 1967) and a real literature (Holland & Rubin,
1983, on when each is the "correct" choice -- it depends on the assumed
assignment/growth process, there's no universal answer). One concrete tell
in our data: the coefficient on `pre_op_bed` in the ANCOVA fit is 1.04-1.44,
not 1 -- meaning growth doesn't scale dollar-for-dollar with baseline size,
so plain differencing (which *forces* that coefficient to be exactly 1) may
be misspecified for this outcome specifically.

## 9. Where this sits in the literature

Koenker's own guidance for panel quantile regression, when you can't
estimate real fixed effects (our situation -- `rq()` can't absorb ~1,800
hospital dummies), is to difference the outcome first, removing the
individual effect algebraically, then run ordinary QR on what's left. Our
long-difference approach, and ANCOVA as its generalization (differencing =
ANCOVA with the pre-period coefficient forced to 1), are the family with
real backing here (also in the spirit of Athey & Imbens' 2006
changes-in-changes, which compares distributions of *changes*, not levels).
The full-stack, raw-levels-with-coarse-dummies version doesn't correspond to
an established quantile-DiD recommendation -- it's closer to "quantile
regression version of naive TWFE," without a fix for the individual
heterogeneity that TWFE's fixed effects exist to solve.

**Given `pre_op_bed`'s coefficient isn't 1, ANCOVA is the most defensible of
the three for `op_bed`** on purely econometric grounds -- treat that as the
formal conclusion of this comparison. In practice, long-difference was kept
as the sole retained approach going forward, favoring simplicity and
consistency across all 5 outcomes over ANCOVA's slightly stronger
theoretical footing for `op_bed` specifically. Worth remembering that's a
deliberate simplicity-over-precision tradeoff, not a claim that
long-difference won the econometric argument.

## 10. Two distinct panel problems -- don't conflate them

- **Bias in the point estimate**: if a hospital's own fixed "flavor" is
  correlated with treatment, leaving it uncontrolled biases the coefficient.
  *Correction from an earlier draft of these notes*: this is NOT about
  hospital-level selection into treatment -- the state adopts the tax, not
  the hospital, so no hospital's own type causes it to be treated. The more
  relevant channel is state-level (do adopting states have systematically
  different hospital cost profiles?), which the stacked cohort design
  already targets; hospital differencing's main remaining job is avoiding
  the size-driven rank-scrambling problem in section 4, not fixing
  selection bias.
- **Invalid inference (SEs, p-values)**: hospital A's 2010 observation and
  its 2011 observation are not independent draws, regardless of how
  treatment was assigned -- pure data-structure fact. Standard `rq()`
  inference assumes i.i.d. rows, and `ld_hosp_cohort` can contain more than
  one row per hospital (once per cohort it appears as a control in) on top
  of that. **Resolved**: `longdiff.R` now uses `cluster_boot_rq()`, a
  cluster bootstrap that resamples whole *states* (not hospitals, and not
  individual rows) with replacement and refits `rq()` on each resample --
  state, not hospital, because treatment is assigned at the state level
  (every hospital in an adopting state is treated simultaneously), which is
  also the level every mean-based DiD model in this project clusters at.
  The effect of fixing this was large, not cosmetic: under the old
  row-level bootstrap, `mcaid_prop_discharges` was significant at every
  tau; under the state-cluster bootstrap it's significant at none of them.
  `op_bed` and `npr_bed` lost almost all their significance too.
  `uncomp_bed_w95` and `op_margin` are the two outcomes where real signal
  survives properly clustered.

## 11. Panel vs. cross-section

The entire fixed-effect-in-quantile-regression problem is a *panel*
problem specifically -- it requires the same unit observed more than once.
With one observation per hospital (a pure cross-section, or repeated
cross-sections where no hospital is ever tracked across years), there's no
repeated component to net out at all -- whatever hospital-specific
heterogeneity exists just becomes part of the ordinary residual, which
`rq()` handles natively. Standard ways to take a panel toward a
cross-section:

- **First-difference**: `Y_t - Y_{t-1}`, adjacent periods.
- **Long-difference** (what we use): average several pre-periods, average
  several post-periods, subtract -- less noisy than a single-year
  first-difference.
- **Between estimator**: collapse each unit to its own full-period average
  (no pre/post split) -- used for identifying effects of time-invariant
  characteristics; would wash out our pre/post treatment timing, wrong tool
  here.
- **Pseudo-panels** (Deaton, 1985): the mirror problem -- no true panel, but
  stable cohorts let you build cohort-level averages that behave like one.
- **Mundlak / correlated random effects** (`rqpd`'s `"cre"` method): doesn't
  collapse rows at all -- adds each unit's own time-averaged X as an extra
  regressor in the full panel instead.

## Files in this project

- `longdiff.R` (renamed from `quant_reg.R`) -- long-difference quantile DiD
  (Koenker's differencing suggestion), all 5 outcomes: `op_bed`, `npr_bed`,
  `mcaid_prop_discharges`, `uncomp_bed_w95` (winsorized 1%-95%, matching
  `results_tables/main_results_table.R`), `op_margin`. Reads `stacked_data`
  read-only except for adding the `uncomp_bed_w95` column defensively (only
  if missing). SEs are a state-level cluster bootstrap (`cluster_boot_rq()`,
  500 replicates) -- see section 10.
- `longdiff_results_table.R` (renamed from `quantreg_results_table.R`) --
  builds `longdiff_results_table.tex` from `longdiff.R`'s `ld_all_results`.
  Requires `longdiff.R` already run in the session.
- `quantreg_ancova.R`, `quantreg_stack.R` -- no longer in the repo. Built to
  compare against long-difference (sections 8-9), conclusion recorded above,
  not kept as ongoing scripts.
- `canay_qr.R` and related files -- a more literal implementation of
  Canay (2011)'s actual two-step estimator (first-stage panel FE regression
  to estimate each hospital's fixed effect, subtract it, then pooled
  quantile regression on the residual), as opposed to `longdiff.R`'s
  simpler pre/post-average differencing. See that file for specifics once
  built out.
