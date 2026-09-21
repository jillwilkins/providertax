###########################################################
# SDP preliminary analysis
# Step 5: clean SDP_Adoption_Tracker.csv into a state-level panel:
#   sdp_year   -- standardized first year (see rules below)
#   hosp_sdp   -- 1 if the SDP's provider type(s) include hospitals
#   sdp_adopt  -- 1 if the state has any SDP; 0 otherwise
# Then expand to the FULL 51-state universe (50 + DC, matching the
# `state` coding in hospdata_analysis) -- any state absent from the
# tracker is a non-adopter.
#
# Year-parsing rule: take the first 4-digit year token in the
# "Earliest SDP Year (Est.)" string (handles "2017-18" -> 2017,
# "2018-2020" -> 2018, etc.).
#
# Two rows need a call, not a mechanical parse -- flagged below,
# not silently resolved:
#   - Pennsylvania: "2018-19 (assessment); 2023 (SDP)" -- 2018-19 is
#     the PROVIDER ASSESSMENT date, not the SDP. Taking "first year
#     listed" literally gives 2018, which would conflate the
#     assessment with the SDP itself. 2023 is likely the correct
#     sdp_year.
#   - Oregon: "2018-2022 (hospital pools); 2023 (BH)" -- 2018 is a
#     range estimate for the hospital pools (the relevant component
#     given hosp_sdp), 2023 is behavioral health only. Literal first
#     year (2018) is probably fine here but the range itself is wide.
#
# Alabama is a special case: Earliest SDP Year = "N/A - EXCLUDED",
# Program Name = "None", Confidence = "Confirmed absence" -- i.e. it
# is a CONFIRMED NON-adopter that happens to have a row in the
# tracker (to document it was checked). Coded as sdp_adopt = 0, not
# folded in as a missing-year adopter.
#
# Four states have a program but unconfirmed timing (Confidence =
# "Not found"): Georgia, South Carolina, New Jersey, West Virginia.
# Coded as sdp_adopt = 1, sdp_year = NA.
###########################################################

library(dplyr)

data_input_path <- "/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/input/"

sdp_raw <- read.csv(paste0(data_input_path, "SDP_Adoption_Tracker.csv"), fileEncoding = "UTF-8-BOM")

names(sdp_raw) <- c("state_name", "year_raw", "program_name", "provider_type", "confidence", "source_url", "notes")

# NOTE: an earlier version of this used
#   regmatches(x, regexpr("\\d{4}", x))
# which silently DROPS elements with no match instead of returning NA,
# shrinking the result vector -- recycling that shorter vector inside
# ifelse() then misaligns every year after the first non-matching row
# (e.g. Alabama's "N/A - EXCLUDED") with the WRONG state. Confirmed
# this corrupted sdp_year for North Carolina (should be 2023, was
# silently 2017) and likely others. Fixed by matching one string at a
# time so a no-match never shifts positions.
first_year <- function(x) {
  vapply(x, function(s) {
    m <- regmatches(s, regexpr("\\d{4}", s))
    if (length(m) == 0) NA_integer_ else as.integer(m)
  }, integer(1), USE.NAMES = FALSE)
}

sdp_clean <- sdp_raw %>%
  mutate(
    sdp_year = first_year(year_raw),
    hosp_sdp = as.integer(grepl("hospital", provider_type, ignore.case = TRUE)),
    sdp_adopt = 1L
  ) %>%
  # Alabama: confirmed non-adopter despite having a tracker row
  mutate(
    sdp_adopt = ifelse(confidence == "Confirmed absence", 0L, sdp_adopt),
    sdp_year  = ifelse(confidence == "Confirmed absence", NA_integer_, sdp_year),
    hosp_sdp  = ifelse(confidence == "Confirmed absence", 0L, hosp_sdp)
  )

cat("=== Rows needing a manual call on sdp_year (shown as parsed) ===\n")
print(sdp_clean %>% filter(state_name %in% c("Pennsylvania", "Oregon")) %>%
        select(state_name, year_raw, sdp_year, hosp_sdp))

cat("\n=== Alabama (confirmed non-adopter) ===\n")
print(sdp_clean %>% filter(state_name == "Alabama") %>%
        select(state_name, year_raw, sdp_adopt, sdp_year, hosp_sdp))

cat("\n=== Unconfirmed-timing adopters (sdp_year = NA) ===\n")
print(sdp_clean %>% filter(is.na(sdp_year), sdp_adopt == 1) %>%
        select(state_name, year_raw, sdp_adopt, hosp_sdp))

# ------------------------------------------------------------------
# Map full state names -> 2-letter codes (matching hospdata_analysis)
# ------------------------------------------------------------------
sdp_clean <- sdp_clean %>%
  mutate(state = ifelse(state_name == "District of Columbia", "DC",
                         state.abb[match(state_name, state.name)]))

stopifnot(!any(is.na(sdp_clean$state)))

# ------------------------------------------------------------------
# Expand to the full 51-state universe -- states absent from the
# tracker (and Alabama, handled above) are non-adopters.
# ------------------------------------------------------------------
all_states <- c(state.abb, "DC")

sdp_panel <- tibble(state = all_states) %>%
  left_join(
    sdp_clean %>% select(state, state_name, sdp_adopt, sdp_year, hosp_sdp, program_name, provider_type, confidence),
    by = "state"
  ) %>%
  mutate(
    sdp_adopt = ifelse(is.na(sdp_adopt), 0L, sdp_adopt),
    hosp_sdp  = ifelse(is.na(hosp_sdp), 0L, hosp_sdp)
  )

cat("\n=== Full 51-state panel: adoption summary ===\n")
cat("n adopters (sdp_adopt=1):    ", sum(sdp_panel$sdp_adopt == 1), "\n")
cat("n non-adopters (sdp_adopt=0):", sum(sdp_panel$sdp_adopt == 0), "\n")
cat("n adopters with hosp_sdp=1:  ", sum(sdp_panel$sdp_adopt == 1 & sdp_panel$hosp_sdp == 1), "\n")
cat("n adopters with hosp_sdp=0:  ", sum(sdp_panel$sdp_adopt == 1 & sdp_panel$hosp_sdp == 0), "\n")

cat("\n=== States coded as non-adopters (sdp_adopt = 0) ===\n")
print(sdp_panel %>% filter(sdp_adopt == 0) %>% select(state) %>% arrange(state), n = Inf)

cat("\n=== Full panel ===\n")
print(sdp_panel %>% select(state, sdp_adopt, sdp_year, hosp_sdp) %>% arrange(state), n = Inf)

write.csv(sdp_panel, "sdp/sdp_state_panel_tmp.csv", row.names = FALSE)
file.rename("sdp/sdp_state_panel_tmp.csv", "sdp/sdp_state_panel.csv")
