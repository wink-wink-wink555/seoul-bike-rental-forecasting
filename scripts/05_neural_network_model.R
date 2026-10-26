train_ts <- get(".training_ts", envir = .GlobalEnv)
test_data <- get(".test_data", envir = .GlobalEnv)
config <- .experiment_config
seed <- config$nnar$seed
set.seed(seed)
nnar_fit <- forecast::nnetar(train_ts, p = config$nnar$p, P = config$nnar$P,
                             size = config$nnar$size, repeats = config$nnar$repeats,
                             scale.inputs = TRUE)
assign(".nnar_fit", nnar_fit, envir = .GlobalEnv)
saveRDS(nnar_fit, path_for("results", "nnar_train_fit.rds"))
utils::write.csv(data.frame(
  model = sprintf("NNAR(%d,%d,%d)[%d]", config$nnar$p, config$nnar$P,
                  config$nnar$size, config$season_period),
  p = config$nnar$p, P = config$nnar$P,
  hidden_units = config$nnar$size, repeats = config$nnar$repeats,
  seed = seed, training_n = length(train_ts), test_n = nrow(test_data)
), path_for("results", "nnar_specification.csv"), row.names = FALSE,
fileEncoding = "UTF-8")
nnar_summary <- trimws(capture.output(print(nnar_fit)), which = "right")
writeLines(nnar_summary, path_for("results", "nnar_model_summary.txt"))
message("NNAR trained on ", length(train_ts), " observations with seed ", seed,
        "; p=", config$nnar$p, ", P=", config$nnar$P,
        ", size=", config$nnar$size, ", repeats=", config$nnar$repeats, ".")
