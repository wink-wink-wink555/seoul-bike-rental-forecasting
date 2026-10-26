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

  raw_dir <- file.path(.project_root, "data", "raw")
  active_manifest <- file.path(raw_dir, "active_snapshot.txt")
  fallback_path <- file.path(raw_dir, .experiment_config$raw_snapshot)
  resolve_active_snapshot <- function() {
    if (!file.exists(active_manifest)) return(fallback_path)
    active_name <- trimws(readLines(active_manifest, warn = FALSE, encoding = "UTF-8")[1])
    active_path <- file.path(raw_dir, active_name)
    if (!nzchar(active_name) || !identical(basename(active_name), active_name) ||
        !file.exists(active_path)) {
      stop("The active FRED snapshot manifest is invalid: ", active_manifest)
    }
    active_path
  }
  load_snapshot_metadata <- function(snapshot_path) {
    sidecar <- paste0(snapshot_path, ".meta.csv")
    metadata_path <- file.path(.project_root, "results", "data_metadata.csv")
    if (file.exists(sidecar)) {
      tab <- utils::read.csv(sidecar, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    } else if (file.exists(metadata_path)) {
      tab <- utils::read.csv(metadata_path, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
      if (!"item" %in% names(tab) || !"value" %in% names(tab) ||
          !any(tab$item == "snapshot_file" & tab$value == basename(snapshot_path))) {
        return(NULL)
      }
    } else return(NULL)
    if (!all(c("item", "value") %in% names(tab))) return(NULL)
    values <- stats::setNames(tab$value, tab$item)
    as.list(values[intersect(c("snapshot_method", "version_consistency", "retrieved_on",
                               "refresh_status"), names(values))])
  }

  fallback_path <- file.path(.project_root, "data", "raw",
                             .experiment_config$raw_snapshot)
  if ("--refresh-data" %in% commandArgs(trailingOnly = TRUE)) {
    refresh_url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=",
                          .experiment_config$series_id)
    tmp <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    message("Explicit refresh requested; downloading official FRED CSV from ", refresh_url)
    refresh_error <- NULL
    refreshed <- tryCatch({
      utils::download.file(refresh_url, tmp, mode = "wb", quiet = FALSE)
      utils::read.csv(tmp, check.names = FALSE, stringsAsFactors = FALSE)
    }, error = function(e) {
      refresh_error <<- conditionMessage(e)
      NULL
    })
    series_id <- .experiment_config$series_id
    refresh_dates <- if (!is.null(refreshed) &&
                         all(c("observation_date", series_id) %in% names(refreshed))) {
      as.Date(refreshed$observation_date, format = "%Y-%m-%d")
    } else as.Date(character())
    refresh_values <- if (length(refresh_dates))
      suppressWarnings(as.numeric(refreshed[[series_id]])) else numeric()
    in_window <- !is.na(refresh_dates) &
      refresh_dates >= .experiment_config$analysis_start &
      refresh_dates <= .experiment_config$analysis_end
    expected_window <- seq(.experiment_config$analysis_start,
                           .experiment_config$analysis_end, by = "month")
    valid_refresh <- length(refresh_dates) > 0L &&
      length(refresh_dates) == nrow(refreshed) && !anyNA(refresh_dates) &&
      !anyDuplicated(refresh_dates) && all(is.finite(refresh_values[in_window])) &&
      identical(refresh_dates[in_window], expected_window)
    if (!valid_refresh && is.null(refresh_error)) {
      refresh_error <- paste0("downloaded official CSV does not contain a finite, ",
                              "continuous series through the fixed analysis end ",
                              .experiment_config$analysis_end)
    }

    if (valid_refresh) {
      refresh_path <- file.path(.project_root, "data", "raw",
        paste0("IPG2211A2N_fred_snapshot_retrieved_", stamp, ".csv"))
      if (file.exists(refresh_path) || file.exists(paste0(refresh_path, ".meta.csv"))) {
        stop("Refresh snapshot already exists: ", refresh_path)
      }
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
      if (!file.copy(tmp, refresh_path, overwrite = FALSE)) {
        stop("Could not preserve the downloaded snapshot; existing data were kept.")
      }
      retrieved_on <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
      .snapshot_metadata <- list(
        snapshot_method = paste0("Single-request official FRED graph CSV, retrieved ",
                                 retrieved_on),
        version_consistency = "One official CSV response downloaded in a single request",
        retrieved_on = retrieved_on,
        refresh_status = "SUCCESS: official CSV covered the configured analysis window"
      )
      utils::write.csv(data.frame(item = names(.snapshot_metadata),
                                  value = unlist(.snapshot_metadata, use.names = FALSE)),
                       paste0(refresh_path, ".meta.csv"), row.names = FALSE,
                       fileEncoding = "UTF-8")
      writeLines(basename(refresh_path), active_manifest, useBytes = TRUE)
      .data_input_path <- refresh_path
      assign(".data_input_path", .data_input_path, envir = .GlobalEnv)
      assign(".snapshot_metadata", .snapshot_metadata, envir = .GlobalEnv)
      message("Consistent official snapshot saved to ", refresh_path,
              "; previous outputs copied to ", archive)
    } else {
      fallback_path <- resolve_active_snapshot()
      if (!file.exists(fallback_path)) {
        stop("FRED refresh failed and the preserved local snapshot is missing: ",
             refresh_error)
      }
      .data_input_path <- fallback_path
      prior_metadata <- load_snapshot_metadata(fallback_path)
      prior_method <- if (!is.null(prior_metadata$snapshot_method))
        prior_metadata$snapshot_method else
          "Preserved local snapshot assembled from two official FRED pages"
      prior_consistency <- if (!is.null(prior_metadata$version_consistency))
        prior_metadata$version_consistency else
          "Mixed source components; same revision vintage not verified"
      prior_retrieved <- if (!is.null(prior_metadata$retrieved_on))
        prior_metadata$retrieved_on else "2026-10-20"
      .snapshot_metadata <- list(
        snapshot_method = prior_method,
        version_consistency = prior_consistency,
        retrieved_on = prior_retrieved,
        refresh_status = paste0("NON-FATAL refresh failure on ",
                                format(Sys.Date(), "%Y-%m-%d"), ": ", refresh_error,
                                "; retained the original local snapshot")
      )
      assign(".data_input_path", .data_input_path, envir = .GlobalEnv)
      assign(".snapshot_metadata", .snapshot_metadata, envir = .GlobalEnv)
      message("WARNING: FRED refresh failed; continuing with the preserved snapshot. ",
              "Reason: ", refresh_error)
    }
  } else {
    .data_input_path <- resolve_active_snapshot()
    assign(".data_input_path", .data_input_path, envir = .GlobalEnv)
    .snapshot_metadata <- load_snapshot_metadata(.data_input_path)
    if (!is.null(.snapshot_metadata)) {
      assign(".snapshot_metadata", .snapshot_metadata, envir = .GlobalEnv)
    }
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
