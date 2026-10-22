args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[1]) else NULL
if (is.null(script_path) || !nzchar(script_path)) {
  ofiles <- vapply(sys.frames(), function(frame) {
    if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)[1]
  }, character(1))
  ofiles <- stats::na.omit(ofiles)
  if (length(ofiles)) script_path <- tail(ofiles, 1)
}
if (is.null(script_path) || !nzchar(script_path)) {
  candidates <- c(file.path(getwd(), "scripts", "run_all.R"),
                  file.path(dirname(getwd()), "scripts", "run_all.R"))
  found <- candidates[file.exists(candidates)]
  if (length(found)) script_path <- found[1]
}
if (is.null(script_path) || !file.exists(script_path)) {
  stop("Cannot locate scripts/run_all.R. Open the project and run it from the project root.")
}
.project_root <- normalizePath(file.path(dirname(script_path), ".."),
                               winslash = "/", mustWork = TRUE)
assign(".project_root", .project_root, envir = .GlobalEnv)
for (d in c("data/processed", "figures", "results", "logs")) {
  dir.create(file.path(.project_root, d), recursive = TRUE, showWarnings = FALSE)
}
run_pipeline <- function() {
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_file <- file.path(.project_root, "logs", paste0("run_", stamp, ".log"))
  log_con <- file(log_file, open = "wt", encoding = "UTF-8")
  sink(log_con, split = TRUE)
  sink(log_con, type = "message")
  on.exit({
    sink(type = "message")
    sink()
    close(log_con)
  }, add = TRUE)
  message("Run started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
  source(file.path(.project_root, "scripts", "00_setup.R"), local = .GlobalEnv,
         encoding = "UTF-8")

  if ("--refresh-data" %in% commandArgs(trailingOnly = TRUE)) {
    refresh_url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=IPG2211A2N"
    tmp <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    message("Explicit refresh requested; downloading from ", refresh_url)
    utils::download.file(refresh_url, tmp, mode = "wb", quiet = FALSE)
    refreshed <- utils::read.csv(tmp, check.names = FALSE, stringsAsFactors = FALSE)
    if (!all(c("observation_date", "IPG2211A2N") %in% names(refreshed))) {
      stop("FRED refresh returned unexpected CSV columns; existing snapshots were kept.")
    }
    refresh_path <- file.path(.project_root, "data", "raw",
                              paste0("IPG2211A2N_refresh_", stamp, ".csv"))
    if (file.exists(refresh_path)) stop("Refresh snapshot already exists: ", refresh_path)
    if (!file.copy(tmp, refresh_path, overwrite = FALSE)) {
      stop("Could not save the new refresh snapshot; existing data were kept.")
    }
    .data_input_path <- refresh_path
    assign(".data_input_path", .data_input_path, envir = .GlobalEnv)

    archive <- file.path(.project_root, "archive", paste0("before_refresh_", stamp))
    if (dir.exists(archive)) stop("Refresh archive already exists: ", archive)
    if (!dir.create(archive, recursive = TRUE, showWarnings = FALSE) &&
        !dir.exists(archive)) {
      stop("Could not create refresh archive: ", archive)
    }
    for (item in c("results", "figures", "data/processed", "report.md",
                   "report_preview.md")) {
      source_item <- file.path(.project_root, item)
      if (file.exists(source_item)) {
        target <- file.path(archive, item)
        dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
        copied <- file.copy(source_item, target, recursive = TRUE, overwrite = FALSE)
        if (!all(copied)) {
          stop("Could not archive existing output before refresh: ", source_item)
        }
      }
    }
    message("New data saved to ", refresh_path,
            "; previous outputs copied to ", archive)
  } else {
    .data_input_path <- file.path(.project_root, "data", "raw",
                                  "IPG2211A2N_fred_snapshot_2000_2026-09.csv")
    assign(".data_input_path", .data_input_path, envir = .GlobalEnv)
  }

  steps <- c("01_data_preparation.R", "02_exploratory_analysis.R",
             "03_stationarity_analysis.R", "04_arima_modeling.R",
             "05_neural_network_model.R", "06_forecast_comparison.R",
             "08_generate_report_preview.R", "09_validate_outputs.R")
  for (step in steps) {
    message("=== ", step, " ===")
    source(file.path(.project_root, "scripts", step), local = .GlobalEnv,
           encoding = "UTF-8")
  }
  message("Run completed successfully: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
  message("Log: ", log_file)
}
run_pipeline()
