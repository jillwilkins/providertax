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
    pct_nonprofit = mean(nonprofit == 1, na.rm = TRUE) * 100,
    avg_op_bed = mean(op_bed, na.rm = TRUE),
    avg_npr_bed = mean(npr_bed, na.rm = TRUE),
    avg_op_margin = mean(op_margin, na.rm = TRUE), 
    avg_mcaid_prop = mean(mcaid_prop_discharges, na.rm = TRUE),
    avg_uncomp_bed = mean(uncomp_bed, na.rm = TRUE),

  ) %>%
  mutate(
    avg_beds      = round(avg_beds, 1),
    pct_nonprofit = round(pct_nonprofit, 1),
    avg_op_bed = round(avg_op_bed, 2),
    avg_npr_bed = round(avg_npr_bed, 2),
    avg_op_margin = round(avg_op_margin, 1),
    avg_mcaid_prop = round(avg_mcaid_prop, 3),
    avg_uncomp_bed = round(avg_uncomp_bed, 2)
  )

# Reshape to long format for a clean single-column table
chars_long <- tibble(
  Characteristic = c(
    "Number of Hospitals",
    "Number of States",
    "Average Bed Size",
    "Non-Profit (\\%)", 
    "Average Operating Expense Per Bed",
    "Average Net Patient Revenue Per Bed",
    "Average Operating Margin",
    "Average Medicaid Proportion",
    "Average Uncompensated Care Per Bed"

  ),
  Value = c(
    scales::comma(hosp_chars$n_hospitals),
    as.character(hosp_chars$n_states),
    as.character(hosp_chars$avg_beds),
    as.character(hosp_chars$pct_nonprofit), 
    as.character(hosp_chars$avg_op_bed),
    as.character(hosp_chars$avg_npr_bed),
    as.character(hosp_chars$avg_op_margin),
    as.character(hosp_chars$avg_mcaid_prop),
    as.character(hosp_chars$avg_uncomp_bed)
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
