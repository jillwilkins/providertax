# ==============================================================================
# CLEAN TAX RATES: state-level provider tax rate/base data
# Replaces rates_table.R. Reads the manually-collected rate spreadsheet and
# produces one tidy row per state-year-component: state (abbreviation),
# year, tax rate, tax base (up to two rate/base pairs per state-year).
# ==============================================================================

library(readxl)
library(dplyr)

taxrates_path <- "/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/Tax Info/taxrates.xlsx"

taxrates_raw <- read_excel(taxrates_path, sheet = 1)

# ------------------------------------------------------------------------------
# Clean: state -> abbreviation, year -> single integer (readxl already parses
# the year column as a real date, so no manual Excel-serial conversion is
# needed here, unlike the old rates_table.R), rate/base columns tidied.
# Rows with no primary rate are placeholder/scaffold rows (e.g. a state with
# no confirmed tax yet) and carry no analytical information, so they're
# dropped here rather than downstream.
# ------------------------------------------------------------------------------
taxrates_clean <- taxrates_raw %>%
  rename(
    year_raw  = `year (assumed fiscal unless otherwise stated)`,
    end_date  = `end date`,
    tax_rate  = rate,
    tax_base  = `base amount`,
    tax_rate2 = rate2,
    tax_base2 = base2,
    base_year = `base year`
  ) %>%
  mutate(
    state     = state.abb[match(tools::toTitleCase(tolower(state)), state.name)],
    year_raw  = as.Date(year_raw),
    end_date  = as.Date(end_date),
    year      = as.integer(format(year_raw, "%Y")),
    tax_base  = tolower(trimws(tax_base)),
    tax_base2 = tolower(trimws(tax_base2)),
    base_year = suppressWarnings(as.numeric(base_year))
  ) %>%
  filter(!is.na(tax_rate), !is.na(state), !is.na(year)) %>%
  # Some states change their rate mid-calendar-year (e.g. Oregon), which would
  # otherwise collide into duplicate state-year rows here. Resolve by keeping
  # whichever effective-dated row was in effect the longest: its own end_date
  # if given, else the next chronologically effective date for that state,
  # else an open-ended sentinel for the most current/last-known rate. Ties
  # (equal duration) are broken toward the later effective date.
  arrange(state, year_raw) %>%
  group_by(state) %>%
  mutate(
    next_start    = lead(year_raw),
    effective_end = coalesce(end_date, next_start, as.Date("2099-01-01")),
    duration_days = as.numeric(effective_end - year_raw)
  ) %>%
  ungroup() %>%
  group_by(state, year) %>%
  arrange(desc(duration_days), desc(year_raw), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  select(state, year, tax_rate, tax_base, tax_rate2, tax_base2, base_year,
         duration_days, website, notes = `notes to obtain the pdf`) %>%
  distinct()

# ------------------------------------------------------------------------------
# Sanity checks
# ------------------------------------------------------------------------------
cat("Rows:", nrow(taxrates_clean), "\n")
cat("States covered:", n_distinct(taxrates_clean$state), "of 50\n")
cat("Year range:", paste(range(taxrates_clean$year), collapse = "-"), "\n")

cat("\nStates in raw file with no state.abb match (should be none):\n")
print(taxrates_raw %>%
        mutate(matched = tools::toTitleCase(tolower(state)) %in% state.name) %>%
        filter(!matched) %>%
        distinct(state))

cat("\nDuplicate state-year rows (more than one row per state-year is expected\n")
cat("only when a state has a genuine second rate/base pair already folded into\n")
cat("tax_rate2/tax_base2 -- true duplicates here would mean two DIFFERENT\n")
cat("primary rates claimed for the same state-year):\n")
print(taxrates_clean %>% count(state, year) %>% filter(n > 1))

cat("\ntax_rate summary:\n")
print(summary(taxrates_clean$tax_rate))

cat("\ntax_base values:\n")
print(sort(unique(taxrates_clean$tax_base)))

cat("\ntax_base2 values:\n")
print(sort(unique(taxrates_clean$tax_base2)))
