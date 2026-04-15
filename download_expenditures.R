# ==============================================================================
# The purpose of this script is to download the Medicaid expenditure data from the CMS website.
# ==============================================================================

library(httr)
library(tools)

dir.create(data_input_path, showWarnings = FALSE)

# URLs — note 2012 and 2013 share one zip
zip_urls <- list(
  "2012_13" = "https://www.medicaid.gov/medicaid/downloads/financial-management-report-fy2012-13.zip",
  "2014"    = "https://www.medicaid.gov/medicaid/downloads/financial-management-report-fy2014.zip",
  "2015"    = "https://www.medicaid.gov/medicaid/downloads/financial-management-report-fy2015.zip",
  "2016"    = "https://www.medicaid.gov/medicaid/downloads/financial-management-report-fy2016.zip",
  "2017"    = "https://www.medicaid.gov/medicaid/downloads/financial-management-report-fy2017.zip",
  "2018"    = "https://www.medicaid.gov/medicaid/financial-management/downloads/financial-management-report-fy2018.zip",
  "2019"    = "https://www.medicaid.gov/medicaid/financial-management/downloads/financial-management-report-fy2019.zip",
  "2020"    = "https://www.medicaid.gov/medicaid/financial-management/downloads/financial-management-report-fy2020.zip",
  "2021"    = "https://www.medicaid.gov/medicaid/financial-management/downloads/financial-management-report-fy2021.zip",
  "2022"    = "https://www.medicaid.gov/medicaid/financial-management/downloads/financial-management-report-fy2022.zip",
  "2023"    = "https://www.medicaid.gov/medicaid/financial-management/downloads/financial-management-report-fy2023.zip",
  "2024"    = "https://www.medicaid.gov/medicaid/financial-management/downloads/financial-management-report-fy2024.zip"
)

for (label in names(zip_urls)) {
  url      <- zip_urls[[label]]
  zip_path <- file.path(data_input_path, paste0("FMR_FY", label, ".zip"))
  
  cat("Downloading FY", label, "...\n")
  resp <- GET(url, write_disk(zip_path, overwrite = TRUE),
              progress(), timeout(120))
  
  if (http_error(resp)) {
    warning("Failed FY ", label, ": HTTP ", status_code(resp))
    next
  }
  
  # Unzip
  tmp_dir   <- file.path(data_input_path, paste0("tmp_", label))
  unzip(zip_path, exdir = tmp_dir)
  all_files <- list.files(tmp_dir, recursive = TRUE, full.names = TRUE)
  
  cat("  Found in zip:", paste(basename(all_files), collapse = ", "), "\n")
  
  # Grab Net Expenditure xlsx files (skip CHIP files)
  net_files <- all_files[grepl("Net.Expenditure|FMR_Net", all_files, ignore.case = TRUE) &
                         grepl("\\.xlsx?$", all_files, ignore.case = TRUE) &
                         !grepl("CHIP", all_files, ignore.case = TRUE)]
  
  # Fallback: any xlsx that isn't CHIP
  if (length(net_files) == 0) {
    net_files <- all_files[grepl("\\.xlsx?$", all_files, ignore.case = TRUE) &
                           !grepl("CHIP", all_files, ignore.case = TRUE)]
  }
  
  for (f in net_files) {
    # Extract year from filename if possible, else use label
    yr_match <- regmatches(basename(f), regexpr("FY\\d{2,4}", basename(f), ignore.case = TRUE))
    yr_tag   <- if (length(yr_match) > 0) toupper(yr_match) else paste0("FY", label)
    dest     <- file.path(data_input_path,
                          paste0("FMR_Net_Expenditures_", yr_tag, ".", file_ext(f)))
    file.copy(f, dest, overwrite = TRUE)
    cat("  Saved:", basename(dest), "\n")
  }
  
  unlink(tmp_dir, recursive = TRUE)
  file.remove(zip_path)
}

cat("\nAll done! Files saved:\n")
cat(paste(list.files(data_input_path, pattern = "\\.xlsx?$"), collapse = "\n"), "\n")
