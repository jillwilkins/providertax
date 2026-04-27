# ------------------------------------------------------------------------------
# Hospital Characteristics Table
# ------------------------------------------------------------------------------
library(dplyr)
library(kableExtra)

colnames(stacked_data)

stacked_data <- stacked_data %>%
  mutate(nonprofit = as.integer(typectrl %in% c(1, 2, 23)))

hosp_chars <- stacked_data %>%
  distinct(mcrnum, .keep_all = TRUE) %>%
  summarise(
    n_hospitals = n_distinct(mcrnum),
    n_states    = n_distinct(state),        # adjust if your state variable differs
    avg_beds    = mean(beds, na.rm = TRUE),
    pct_rural   = mean(rurlhos    == 1, na.rm = TRUE) * 100,
    pct_nonprofit = mean(nonprofit == 1, na.rm = TRUE) * 100
  ) %>%
  mutate(
    avg_beds      = round(avg_beds, 1),
    pct_rural     = round(pct_rural, 3),
    pct_nonprofit = round(pct_nonprofit, 1)
  )

# Reshape to long format for a clean single-column table
chars_long <- tibble(
  Characteristic = c(
    "Number of Hospitals",
    "Number of States",
    "Average Bed Size",
    "Rural (\\%)",
    "Non-Profit (\\%)"
  ),
  Value = c(
    scales::comma(hosp_chars$n_hospitals),
    as.character(hosp_chars$n_states),
    as.character(hosp_chars$avg_beds),
    as.character(hosp_chars$pct_rural),
    as.character(hosp_chars$pct_nonprofit)
  )
)

chars_long %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    col.names = c("Characteristic", "Value"),
    align     = c("l", "c"),
    caption   = "Hospital Characteristics"
  ) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  add_footnote(
    "Sample restricted to unique hospitals. Rural and Non-Profit are indicator variables.",
    notation = "none"
  ) %>%
  writeLines("hospital_characteristics.tex")

cat("Done — hospital_characteristics.tex saved.\n")
