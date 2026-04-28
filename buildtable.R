# ============================================================
# Follows rates_table.R 
# This was done with available rates on 4/27/2026
# Bild the LaTex table from the cleaned data
# ============================================================

rows <- c()

states <- unique(df_display$State)

for (s in states) {
  state_data <- df_display %>% filter(State == s)
  years <- unique(state_data$Year)
  first_state <- TRUE
  
  for (y in years) {
    year_data <- state_data %>% filter(Year == y)
    first_year <- TRUE
    
    for (i in 1:nrow(year_data)) {
      state_col <- if (first_state & first_year & i == 1) s else ""
      year_col  <- if (first_year & i == 1) as.character(y) else ""
      rate_col  <- year_data$Rate[i]
      base_col  <- year_data$Base[i]
      row <- paste0(state_col, " & ", year_col, " & ", rate_col, " & ", base_col, " \\\\[0pt]")
      rows <- c(rows, row)
      first_year <- FALSE
    }
    first_state <- FALSE
  }
  rows <- c(rows, "\\midrule")
}

rows <- rows[-length(rows)]

header <- "State & Year & Rate & Base \\\\"

latex_out <- c(
  "\\begin{longtable}{llcc}",
  "\\toprule",
  header,
  "\\midrule",
  "\\endfirsthead",
  "\\toprule",
  header,
  "\\midrule",
  "\\endhead",
  "\\bottomrule",
  "\\endlastfoot",
  rows,
  "\\bottomrule",
  "\\end{longtable}"
)

writeLines(latex_out, "tax_table.tex")
cat("Done! tax_table.tex written.\n")

View(taxrates_clean)
