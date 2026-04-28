# ============================================================
# This was done with available rates on 4/27/2026
# Tax Rate & Base Data: Load, Clean, and Export to LaTeX
# ============================================================
# Required packages: readxl, dplyr, tidyr, knitr, kableExtra
# Install if needed:
# install.packages(c("readxl", "dplyr", "tidyr", "knitr", "kableExtra"))

library(readxl)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# ============================================================
# 1. LOAD DATA
# ============================================================
# Update the path and sheet name to match your file
taxrates_raw <- read_excel("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/Tax Info/taxrates.xlsx", sheet = 1)

# Preview structure
glimpse(taxrates_raw)

# ============================================================
# 2. CLEAN & RESHAPE
# ============================================================
# Based on the screenshot, columns are:
#   state, year, assumption date, end date,
#   rate, base amount, rate2, base2, base year, ...
#
# We want: state | year | tax rate | tax base
# The data has two rate/base pairs per row (rate+base, rate2+base2).
# We'll pivot these into a long format so each row = one observation.
names(taxrates_raw %>% rename_with(~ tolower(gsub("\\s+", "_", .x))))

# verify mid year changes 
taxrates_raw %>%
  rename_with(~ tolower(gsub("\\s+", "_", .x))) %>%
  rename(year_start = `year_(assumed_fiscal_unless_otherwise_stated)`) %>%
  mutate(year = as.integer(format(as.Date(as.numeric(year_start), origin = "1899-12-30"), "%Y"))) %>%
  select(state, year, rate, base_amount) %>%
  group_by(state, year) %>%
  filter(n() > 1) %>%
  arrange(state, year) %>%
  print(n = 20)

taxrates_raw %>%
  rename_with(~ tolower(gsub("\\s+", "_", .x))) %>%
  filter(tolower(state) == "connecticut") %>%
  select(state, rate, base_amount, rate2, base2) %>%
  head(10)


taxrates_clean <- taxrates_raw %>%
  rename_with(~ tolower(gsub("\\s+", "_", .x))) %>%
  rename(year_start = `year_(assumed_fiscal_unless_otherwise_stated)`) %>%
  mutate(year = as.integer(format(as.Date(as.numeric(year_start), origin = "1899-12-30"), "%Y"))) %>%
  select(state, year, rate, base_amount, rate2, base2) %>%
  filter(!is.na(rate) | !is.na(rate2)) %>%
  mutate(
    state    = tools::toTitleCase(tolower(state)),
    tax_rate  = rate,
    tax_base  = base_amount,
    tax_rate2 = rate2,
    tax_base2 = base2
  ) %>%
  select(state, year, tax_rate, tax_base, tax_rate2, tax_base2) %>%
  distinct()


# remove never taxed or incomplete rows 
taxrates_clean <- taxrates_clean %>%
select(state, year, tax_rate, tax_base, tax_rate2, tax_base2) %>%
  distinct() %>%
  filter(!is.na(tax_rate))


# ============================================================
# 3. OPTIONAL: Summarize to one row per state-year
#    (if you want the primary rate/base only, not both pairs)
# ============================================================
# Uncomment this block if you want just the first non-NA rate per state-year:
#
# df_clean <- df_clean %>%
#   group_by(state, year) %>%
#   slice(1) %>%
#   ungroup()

# ============================================================
# 4. FORMAT COLUMNS FOR DISPLAY
# ============================================================
library(knitr)
library(kableExtra)


df_display <- taxrates_clean %>%
  # Drop rows with no valid year
  filter(!is.na(year)) %>%
  mutate(pair = 1) %>%
  bind_rows(
    taxrates_clean %>%
      filter(!is.na(tax_rate2), !is.na(year)) %>%
      mutate(
        tax_rate = tax_rate2,
        tax_base = tax_base2,
        pair     = 2
      )
  ) %>%
  arrange(state, year, pair) %>%
  mutate(
  tax_rate = ifelse(
    tax_rate > 10,
    paste0("\\$", formatC(tax_rate, format = "f", digits = 2)),
    paste0(formatC(tax_rate, format = "f", digits = 2), "\\%")
  )
) %>%
  select(State = state, Year = year, Rate = tax_rate, Base = tax_base)
