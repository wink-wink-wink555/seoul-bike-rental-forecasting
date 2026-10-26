args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
.project_root <- normalizePath(file.path(dirname(script), ".."), winslash = "/", mustWork = TRUE)
dir.create(file.path(.project_root, "logs"), showWarnings = FALSE)
log_path <- file.path(.project_root, "logs", paste0("seoul_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
.run_log_connection <- file(log_path, "wt", encoding = "UTF-8")
sink(.run_log_connection, split = TRUE); sink(.run_log_connection, type = "message")
tryCatch({
  source(file.path(.project_root, "scripts", "00_setup.R"), encoding = "UTF-8")
  if (!file.exists(path_for("data","raw","SeoulBikeData.csv"))) {
    source(path_for("scripts","fetch_data.R"),encoding="UTF-8")
  }
  steps <- c("01_data_preparation.R", "02_exploratory_analysis.R", "03_stationarity_analysis.R",
    "04_arima_experiments.R", "05_random_forest.R", "06_final_evaluation.R", "07_report_materials.R", "08_validate_outputs.R")
  requested <- commandArgs(trailingOnly = TRUE)
  if (length(requested) && any(grepl("^--steps=", requested))) {
    ids <- as.integer(strsplit(sub("^--steps=", "", requested[grepl("^--steps=", requested)][1]), ",")[[1]])
    steps <- steps[ids]
  }
  for (step in steps) {
    message("Starting ", step, " at ", Sys.time())
    source(path_for("scripts", step), encoding = "UTF-8")
    if(step=="04_arima_experiments.R") source(path_for("scripts","04_refinement.R"),encoding="UTF-8")
    message("Completed ", step)
  }
  message("All requested steps completed at ", Sys.time())
}, error = function(e) { message("FAILED: ", conditionMessage(e)); stop(e) },
finally = { sink(type = "message"); sink(); close(.run_log_connection) })
