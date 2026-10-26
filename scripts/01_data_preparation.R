raw_path <- if (exists(".data_input_path", envir = .GlobalEnv, inherits = FALSE)) {
  get(".data_input_path", envir = .GlobalEnv)
} else {
  path_for("data", "raw", .experiment_config$raw_snapshot)
}
if (!file.exists(raw_path)) stop("Missing FRED snapshot: ", raw_path)
config <- .experiment_config
raw <- utils::read.csv(raw_path, check.names = FALSE, stringsAsFactors = FALSE)
series_id <- config$series_id
if (!identical(names(raw), c("observation_date", series_id))) {
  stop("Unexpected columns in the FRED snapshot.")
}
parsed_date <- as.Date(raw$observation_date, format = "%Y-%m-%d")
value <- suppressWarnings(as.numeric(raw[[series_id]]))
if (anyNA(parsed_date) || anyDuplicated(parsed_date)) {
  stop("The source snapshot contains missing or duplicate observation dates.")
}
snapshot_start <- min(parsed_date)
snapshot_end <- max(parsed_date)
analysis_rows <- !is.na(parsed_date) & parsed_date >= config$analysis_start &
  parsed_date <= config$analysis_end
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
if (min(parsed_date) != config$analysis_start || max(parsed_date) != config$analysis_end) {
  stop("The selected official snapshot does not cover the fixed analysis window ",
       config$analysis_start, " through ", config$analysis_end, ".")
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
snapshot_metadata <- if (exists(".snapshot_metadata", envir = .GlobalEnv, inherits = FALSE)) {
  get(".snapshot_metadata", envir = .GlobalEnv)
} else NULL
previous_metadata_path <- path_for("results", "data_metadata.csv")
if (is.null(snapshot_metadata) && file.exists(previous_metadata_path)) {
  previous_metadata <- utils::read.csv(previous_metadata_path, fileEncoding = "UTF-8",
                                       stringsAsFactors = FALSE)
  if (all(c("item", "value") %in% names(previous_metadata)) &&
      any(previous_metadata$item == "snapshot_file" &
            previous_metadata$value == basename(raw_path))) {
    snapshot_metadata <- stats::setNames(previous_metadata$value, previous_metadata$item)
  }
}
snapshot_meta_value <- function(name, fallback) {
  if (!is.null(snapshot_metadata) && name %in% names(snapshot_metadata) &&
      !is.null(snapshot_metadata[[name]]) && length(snapshot_metadata[[name]]) == 1L &&
      !is.na(snapshot_metadata[[name]])) return(as.character(snapshot_metadata[[name]]))
  fallback
}
metadata <- data.frame(
  item = c("series_id", "frequency", "units", "seasonal_adjustment", "source", "series_url",
           "table_data_url", "snapshot_file", "snapshot_md5", "snapshot_method",
           "version_consistency", "retrieved_on", "snapshot_start", "snapshot_end",
           "analysis_start", "analysis_end", "analysis_n", "refresh_status"),
  value = c(series_id, "Monthly", "Index 2017=100", "Not Seasonally Adjusted",
            "Board of Governors of the Federal Reserve System (US), via FRED, Federal Reserve Bank of St. Louis",
            paste0("https://fred.stlouisfed.org/series/", series_id),
            paste0("https://fred.stlouisfed.org/data/", series_id), basename(raw_path),
            unname(tools::md5sum(raw_path)),
            snapshot_meta_value("snapshot_method",
              "本地历史快照：FRED 官方表格页截至 2026-08，加官方序列页 2026-09 观测值"),
            snapshot_meta_value("version_consistency",
              "混合版本：两个官方页面组件，未能验证为同一修订时点"),
            snapshot_meta_value("retrieved_on", "2026-10-20"),
            as.character(snapshot_start), as.character(snapshot_end),
            as.character(config$analysis_start), as.character(config$analysis_end),
            as.character(nrow(processed)),
            snapshot_meta_value("refresh_status",
              "本次未请求刷新；保留固定原始快照，版本限制见 data/raw/SOURCE_NOTES.txt"))
)
utils::write.csv(metadata, path_for("results", "data_metadata.csv"),
                 row.names = FALSE, fileEncoding = "UTF-8")
message("Validated ", nrow(processed), " monthly observations: ", min(parsed_date),
        " to ", max(parsed_date), "; no missing or duplicate dates.")
