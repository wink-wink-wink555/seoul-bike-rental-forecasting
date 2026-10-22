train_ts <- get(".training_ts", envir = .GlobalEnv)
test_data <- get(".test_data", envir = .GlobalEnv)
seed <- 20261020L
set.seed(seed)
nnar_fit <- forecast::nnetar(train_ts, p = 11, P = 1, size = 6, repeats = 30,
                             scale.inputs = TRUE)
assign(".nnar_fit", nnar_fit, envir = .GlobalEnv)
saveRDS(nnar_fit, path_for("results", "nnar_train_fit.rds"))
utils::write.csv(data.frame(
  model = "NNAR(11,1,6)[12]", p = 11L, P = 1L,
  hidden_units = 6L, repeats = 30L,
  seed = seed, training_n = length(train_ts), test_n = nrow(test_data)
), path_for("results", "nnar_specification.csv"), row.names = FALSE,
fileEncoding = "UTF-8")
capture.output(print(nnar_fit), file = path_for("results", "nnar_model_summary.txt"))
message("NNAR trained on ", length(train_ts), " observations with seed ", seed,
        "; p=11, P=1, size=6, repeats=30.")
