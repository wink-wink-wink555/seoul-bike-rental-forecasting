raw_path <- if (exists(".data_input_path", envir = .GlobalEnv, inherits = FALSE)) {
  get(".data_input_path", envir = .GlobalEnv)
} else {
  path_for("data", "raw", "IPG2211A2N_fred_snapshot_2000_2026-09.csv")
}
if (!file.exists(raw_path)) stop("Missing FRED snapshot: ", raw_path)
raw <- utils::read.csv(raw_path, check.names = FALSE, stringsAsFactors = FALSE)
if (!identical(names(raw), c("observation_date", "IPG2211A2N"))) {
  stop("Unexpected columns in the FRED snapshot.")
}
parsed_date <- as.Date(raw$observation_date, format = "%Y-%m-%d")
value <- suppressWarnings(as.numeric(raw$IPG2211A2N))
analysis_rows <- !is.na(parsed_date) & parsed_date >= as.Date("2000-01-01")
parsed_date <- parsed_date[analysis_rows]
value <- value[analysis_rows]
if (anyNA(parsed_date) || anyNA(value) || any(!is.finite(value))) {
  stop("Invalid dates or missing/non-numeric observations; no rows were removed.")
}
if (anyDuplicated(parsed_date)) stop("Duplicate observation dates found.")
if (any(parsed_date != as.Date(format(parsed_date, "%Y-%m-01")))) {
  stop("At least one observation is not dated on the first day of a month.")
}
expected_dates <- seq(min(parsed_date), max(parsed_date), by = "month")
if (length(expected_dates) != length(parsed_date) ||
    !identical(parsed_date, expected_dates)) {
  stop("Monthly dates are not continuous; no interpolation was attempted.")
}
if (min(parsed_date) != as.Date("2000-01-01") || max(parsed_date) > Sys.Date()) {
  stop("Unexpected date range in the local snapshot.")
}
processed <- data.frame(date = parsed_date, value = value,
                        year = as.integer(format(parsed_date, "%Y")),
                        month = as.integer(format(parsed_date, "%m")))
write_csv(processed, "data", "processed", "IPG2211A2N_clean.csv")
checks <- data.frame(
  check = c("rows", "start_date", "end_date", "duplicate_dates", "missing_values",
            "non_finite_values", "monthly_continuity", "raw_snapshot_md5"),
  result = c(nrow(processed), as.character(min(parsed_date)), as.character(max(parsed_date)),
             sum(duplicated(parsed_date)), sum(is.na(value)), sum(!is.finite(value)),
             "PASS", unname(tools::md5sum(raw_path)))
)
write_csv(checks, "results", "data_checks.csv")
metadata <- data.frame(
  item = c("series_id", "frequency", "units", "seasonal_adjustment", "source", "series_url",
           "table_data_url", "snapshot_method", "local_download_error", "retrieved_on",
           "table_page_coverage", "latest_page_value"),
  value = c("IPG2211A2N", "Monthly", "Index 2017=100", "Not Seasonally Adjusted",
            "Board of Governors of the Federal Reserve System (US), via FRED, Federal Reserve Bank of St. Louis",
            "https://fred.stlouisfed.org/series/IPG2211A2N",
            "https://fred.stlouisfed.org/data/IPG2211A2N",
            "Official FRED table page through 2026-08 plus the latest displayed series-page value",
            "PowerShell request to fredgraph.csv failed: Unable to connect to remote server",
            "2026-10-20", "2000-01 through 2026-08",
            "2026-09: 114.8603; series page displayed update 2026-10-16")
)
utils::write.csv(metadata, path_for("results", "data_metadata.csv"),
                 row.names = FALSE, fileEncoding = "UTF-8")
message("Validated ", nrow(processed), " monthly observations: ", min(parsed_date),
        " to ", max(parsed_date), "; no missing or duplicate dates.")
