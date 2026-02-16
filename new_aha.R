# Load AHA data
aha <- read_csv("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/input/AHAdata_20052023.csv")

#cahnge column names to lowercase
colnames(aha) <- tolower(colnames(aha))

# Check what years you have
cat("AHA year range:", min(aha$year), "to", max(aha$year), "\n")

# Check the control codes
cat("\n=== AHA Control Codes (cntrl) ===\n")
table(aha$cntrl, useNA = "always")

# Check the service codes
cat("\n=== AHA Service Codes (serv) ===\n")
table(aha$serv, useNA = "always")

# Check coverage by year
aha %>%
  group_by(year) %>%
  summarise(
    n_hospitals = n(),
    n_missing_cntrl = sum(is.na(cntrl)),
    n_missing_serv = sum(is.na(serv)),
    pct_missing_cntrl = round(n_missing_cntrl / n_hospitals * 100, 1),
    pct_missing_serv = round(n_missing_serv / n_hospitals * 100, 1)
  )
