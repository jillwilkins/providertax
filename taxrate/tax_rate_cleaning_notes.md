# Tax Rate Data: Methods Notes

A walkthrough of the decisions behind `clean_taxrates.R` and
`calc_tax_amounts.R`, for anyone (including future me) picking this up
later. Written alongside the code on 2026-08-06. Covers the **rate-side**
work only -- hospital-level recorded dollar amounts (e.g. Massachusetts
2018-2022) are a separate, not-yet-started piece (see "What's still
pending" below).

## Goal

Provide defensible descriptive statistics on the scope and size of the
provider tax: (1) the average hospital **payment** by years since adoption,
and (2) the average tax **rate as a share of net patient revenue** across
US hospitals. Manually-collected tax structure data varies a lot by state
(some states' documentation gives a dollar amount actually assessed, most
give a statutory rate), so the design goal is to record whichever is
available and estimate the other, always with a flag saying which is which
-- never silently blend a recorded figure with an estimated one.

## 0. Why a rebuild

The old pipeline (`rates_table.R` -> `hosp_taxamounts.R` ->
`hosp_taxamounts_impr.R` -> `hosp_taxamt_table.R`) reimplemented the same
join/calculation logic three times with undocumented drift between
versions (e.g. three different definitions of `effective_rate` across the
three files), fragile string-matching against free-text tax base
categories, and a lot of interactive debugging code mixed into what should
have been the final pipeline. Full review is a prior conversation; the
short version is these four files are being superseded by
`clean_taxrates.R` and `calc_tax_amounts.R` and can be archived.

## 1. Spreadsheet cleanup (`taxrates.xlsx`)

Before any code changes, the manually-collected source spreadsheet itself
needed fixes. All done directly in the spreadsheet (not patched around in
code), consistent with treating it as the source of truth. Original backed
up to `taxrates_backup_pre_cleanup.xlsx` before any edits. This spreadsheet 
was created through reaching out to each states provider tax administrator or 
collecting available rate information from state websites or posted legislation. 

- **"deleware" -> "delaware"** (typo would have silently failed to match
  `state.name`/`state.abb`).
- **"oregon * DRG" merged into "oregon"** -- confirmed 
  this was just a note on Oregon's first year entry, not a second distinct
  tax track.
- **Dropped `rate3`/`base3`** (0% populated placeholder columns) and a
  blank spacer column.
- **Alaska's placeholder row dropped** -- `rate = 0`, `base = "0"`,
  `base_year = "0"`, nothing else filled in. Confirmed this wasn't meant as
  "verified $0 tax," just an unfinished row, not worth carrying forward.
- **Wisconsin FY2010 rate filled in** (the row spanning 2009-07-01 to
  2010-06-30 had base info but no rate) using FY2009's rate (1.2768, no
  interpolation), since there's no indication it changed. Documented with a
  note in that row's notes column so it's traceable as an assumption, not a
  verified figure.
- **Connecticut's base year "2016 net revenue" -> "2016"** (stray
  descriptive text mixed into what should be a plain year).
- **Ohio's tax base forward-filled for 2021-2025** ("total facility costs
  (non medicare)") -- only 2020 had it filled in, but the rate was present
  for all later years too. Safe to forward-fill since nothing suggested the
  base changed; this also means Ohio no longer needs a hardcoded
  state-specific patch in code (the old scripts special-cased Ohio's
  operating-expense base directly in a `case_when`), it's now just another
  properly-labeled category.
- **Massachusetts' tax base forward-filled for 2018, 2020, and 2021**
  ("net patient revenue", matching 2017). This one needed more care than
  Ohio: MA's structure visibly changes by 2022 (switches to a split
  inpatient/outpatient basis, rate jumps from ~1.3-1.6% to ~8.5%), so the
  fill was confirmed explicitly rather than assumed automatically. 2019 is
  still a genuine gap (no rate at all that year, not a base-mapping issue)
  and is left alone.

## 2. `clean_taxrates.R`

Reads the cleaned spreadsheet and produces one tidy row per state-year:
`state` (abbreviation), `year`, `tax_rate`, `tax_base`, `tax_rate2`,
`tax_base2`, `base_year`, plus `website`/`notes` for reference- which 
'website/notes' will continuously be updated as emails come in. 

- State name -> abbreviation via `state.name`/`state.abb` matching.
- Year extraction is now trivial -- `readxl` parses the year column as a
  real date, so the old manual Excel-serial-number conversion
  (`as.Date(as.numeric(...), origin = "1899-12-30")`) is gone. If this
  script is ever pointed at a differently-formatted source file, watch for
  that assumption.
- Rows with no primary rate (blank scaffold/placeholder rows for states
  with no confirmed tax yet) are dropped here.
- **Mid-year rate-change collisions, resolved by duration.** Some states
  (Oregon, notably) change rates more than once within the same calendar
  year, which would otherwise collapse into duplicate state-year rows.
  Resolved by keeping whichever effective-dated entry was in effect the
  longest: its own `end_date` if given, else the next chronologically
  effective date for that state, else an open-ended sentinel (2099-01-01)
  for the most current/last-known rate. Ties are broken toward the later
  effective date. `duration_days` is kept in the output so any pick can be
  audited later. Verified this resolves all of Oregon's four collision
  years (2009, 2011, 2017, 2025) sensibly, including catching that 2025 is
  a genuine structural change (Oregon splits into inpatient/outpatient
  bases that year), not just a rate bump.

**Result:** 258 rows, zero duplicate state-years, **22 states currently
have any usable rate data** (AL, AR, AZ, CT, DE, FL, GA, KY, MA, ME, MI,
MN, MO, MS, NV, OH, OK, OR, PA, VT, WI, WV) -- the other 28 are still
blank placeholders in the source spreadsheet.

## 3. `calc_tax_amounts.R`

Joins `taxrates_clean` onto `hospdata_analysis` and estimates each
hospital-year's tax payment.

- **Lookup year**: `base_year` if specified (and not the "unspecified"
  sentinel 0), else the prior year -- same convention as the old scripts,
  now applied consistently.
- **Base mapping**: every `tax_base`/`tax_base2` string is mapped to a
  hospital-level column (net patient revenue, discharges, beds, total
  charges, or -- new -- `tot_operating_exp` for "total facility costs (non
  medicare)"). "per bed", "per discharge", and "per facility" are flat
  dollar amounts (multiply directly); everything else is a percentage
  (divide by 100 first).
- **`n_rate_components`** (1 or 2) flags hospital-years with a genuine
  two-part tax structure (now known to be common -- AZ, CT, FL, MA, MI, NV,
  OR, PA all have one) so the split is visible in any average rather than
  needing states to be excluded or separately curated.
- **Provenance schema**: `tax_amount_recorded` (placeholder, all `NA` until
  real hospital-level dollar records are merged in), `tax_amount_estimated`
  (rate x base), `tax_amount_best` = `coalesce(recorded, estimated)`, with
  an `amount_source` flag. Keeping this shape now means the eventual merge
  of recorded dollar data (e.g. Massachusetts 2018-2022, once that folder
  arrives) won't require touching the calculation logic.
- **`tax_as_npr_best`** = `tax_amount_best / net_pat_rev_lookup * 100` --
  deliberately uses the *same* lookup-year revenue the dollar amount itself
  was calculated from, not the current year's revenue. The old scripts
  mixed current-year NPR as the denominator with prior-year NPR as the tax
  base; this version keeps numerator and denominator on the same period.

### Bugs caught and fixed while building this

1. **Silent-zero bug (real bug, not a data issue).**
   `rowSums(cbind(tax_amount1, tax_amount2), na.rm = TRUE)` treated an
   *unresolvable* base exactly the same as a *genuinely absent* second
   rate -- both collapsed to a $0 contribution instead of the unresolvable
   ones correctly showing as unknown. Fixed: a present `tax_rate2` with an
   unresolved `base2_value` now correctly stays `NA`; only a genuinely
   absent `tax_rate2` contributes 0.
2. **Many-to-many join from pre-existing duplicate rows.** `hospdata_analysis`
   has ~18 duplicate `mcrnum`-year rows (noted separately, previously judged
   not worth chasing) that happen to overlap MI, MO, PA, and WI -- states we
   have rate data for. Left un-deduped, the lookup-year join was silently
   multiplying some hospital-tax-year rows. Fixed by deduping both the base
   panel and the lookup table on `(mcrnum, year)` before joining, without
   touching the underlying CSV.
3. **Misdiagnosed, then corrected: "unresolved base" rows are NOT mapping
   bugs.** Every `tax_base` string genuinely is mapped in the calculation
   logic. Rows with no computable `base1_value` are cases where the
   hospital has no financial data in `hospdata_analysis` for the specific
   lookup year -- most visible in states with a *fixed* `base_year` (e.g.
   Oklahoma pins to 2009 for every tax year), where a hospital that entered
   the panel later has nothing to look up. Genuine missing data, not
   something to fix in the mapping.
4. **Checked, not a bug: Michigan's small dollar amounts.** Michigan's flat
   per-bed rate ($8.28/bed pre-2015, then $10/bed + $500/facility) produces
   a genuinely small average tax amount (~$2,357) compared to the
   revenue-percentage states (millions). Confirmed this is a real flat fee,
   not a per-day rate needing a x365 multiplier, and confirmed the flat-base
   multiply logic (no `/100`) is already applying it correctly.

**Result:** 21 of the 22 rate-covered states actually match into
`hospdata_analysis` (West Virginia's only entry is a 2025 rate, and the
hospital panel currently only runs through ~2023 -- not a bug, just a
coverage gap that'll resolve once the panel catches up). ~13,200 hospital-
year rows with a computable `tax_amount_best`.

## What's still pending

- **Hospital-level recorded dollar amounts** (e.g. Massachusetts
  2018-2022) -- a separate data-collection effort. Include `mcrnum` (or
  hospital name + state) + `year` + dollar amount when that's ready; it
  merges into `tax_amount_recorded` and automatically takes precedence over
  the rate-based estimate via the existing `coalesce()`.
- **28 states still have no rate data at all** in the source spreadsheet
  (blank placeholder rows).
- **West Virginia** needs either an earlier-year rate entry or for the
  hospital panel to extend to 2025 before it can contribute to any
  hospital-level estimate.
- Summary tables/statistics themselves haven't been built yet -- next step.

## Files produced

- `clean_taxrates.R` -- reads `taxrates.xlsx`, produces `taxrates_clean`.
- `calc_tax_amounts.R` -- requires `hospdata_analysis` and `taxrates_clean`
  already in the environment; produces `tax_amounts`.
- `taxrates.xlsx` -- cleaned in place (see §1).
- `taxrates_backup_pre_cleanup.xlsx` -- pre-cleanup backup.

## Files superseded (candidates to archive)

- `rates_table.R`
- `hosp_taxamounts.R`
- `hosp_taxamounts_impr.R`
- `hosp_taxamt_table.R`

(`hospdata_rates.R` is a separate side-analysis -- a dose-response
regression of the raw tax rate against outcomes on the stacked DiD sample
-- not part of this descriptive-table chain, not superseded by this work.)
