project_root <- get(".project_root", envir = .GlobalEnv)
if (.Platform$OS.type == "windows") Sys.setlocale("LC_CTYPE", ".UTF-8")
.libPaths(c(file.path(project_root, ".Rlib"), .libPaths()))
required <- c("forecast", "ggplot2", "urca", "ranger", "jsonlite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if(length(missing)&&.Platform$OS.type=="windows")for(pkg in missing) {
  archives<-list.files(file.path(project_root,"data","r_packages"),pattern=paste0("^",pkg,"_.*\\.zip$"),full.names=TRUE)
  if(length(archives))utils::install.packages(archives[1],repos=NULL,type="win.binary",lib=file.path(project_root,".Rlib"))
}
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required R packages: ", paste(missing, collapse = ", "))
options(stringsAsFactors = FALSE, scipen = 7, warn = 1)
Sys.setenv(TZ = "Asia/Seoul")
config <- list(seed = 20261026L, timezone = "Asia/Seoul", horizon = 24L,
  initial_end = as.Date("2018-07-31"), validation_start = as.Date("2018-08-01"),
  validation_end = as.Date("2018-09-30"), test_start = as.Date("2018-10-01"),
  test_end = as.Date("2018-11-30"), refit_every_days = 7L,
  refit_weekday = 1L, protocol_version = "v2_full_daily_monday", arima_workers = 4L,
  primary_metric = "RMSE_all_hours", seasonal_scale = 168L)
set.seed(config$seed)
path_for <- function(...) file.path(project_root, ...)
for (d in c("data/processed", "figures", "results", "results/cache", "results/models", "logs"))
  dir.create(path_for(d), recursive = TRUE, showWarnings = FALSE)
write_csv <- function(x, ...) utils::write.csv(x, path_for(...), row.names = FALSE,
  fileEncoding = "UTF-8", na = "")
read_csv <- function(...) utils::read.csv(path_for(...), fileEncoding = "UTF-8", check.names = FALSE)
font_family <- "Microsoft YaHei"
if (.Platform$OS.type == "windows") grDevices::windowsFonts(yahei = grDevices::windowsFont(font_family))
ggplot2::theme_set(ggplot2::theme_minimal(base_size = 11, base_family = font_family) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14),
    plot.subtitle = ggplot2::element_text(size = 9, colour = "#52616B"),
    legend.position = "bottom", legend.title = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()))
palette <- c(Naive24 = "#88949E", Naive168 = "#B18D47", ARIMA = "#8F68A6",
  SARIMA24 = "#D75D43", DHR_ARIMA = "#247BA0", SARIMAX24 = "#3456A4",
  RandomForest = "#16846F", Mean7 = "#A06F2D", Median7 = "#BE9849", DayTypeMean28 = "#667743")
figure_registry <- if (file.exists(path_for("results", "figure_manifest.csv"))) read_csv("results", "figure_manifest.csv") else data.frame()
save_plot <- function(p, file, caption, chapter, width = 10, height = 5.8, note = "") {
  ggplot2::ggsave(path_for("figures", file), p, width = width, height = height,
    dpi = 300, bg = "white", device = grDevices::png, type = "cairo", family = font_family)
  if (nrow(figure_registry)) figure_registry <<- figure_registry[figure_registry$file != file, ]
  figure_registry <<- rbind(figure_registry, data.frame(file = file, chapter = chapter,
    caption = caption, note = note, width_inches = width, height_inches = height, dpi = 300))
  write_csv(figure_registry, "results", "figure_manifest.csv")
}
hourly <- function() {
  d <- read_csv("data", "processed", "seoul_hourly.csv")
  d$timestamp <- as.POSIXct(paste(d$date, sprintf("%02d:00:00", d$hour)),
    format = "%Y-%m-%d %H:%M:%S", tz = config$timezone)
  stopifnot(!anyNA(d$timestamp), all(diff(as.numeric(d$timestamp)) == 3600))
  d$date <- as.Date(d$date)
  d
}
metrics <- function(actual, predicted, scale) {
  e <- actual - predicted; den <- abs(actual) + abs(predicted)
  c(n = length(e), MAE = mean(abs(e)), RMSE = sqrt(mean(e^2)),
    sMAPE = mean(ifelse(den == 0, 0, 200 * abs(e) / den)),
    MASE168 = mean(abs(e)) / scale, bias = mean(predicted - actual))
}
scale_at <- function(y, end) mean(abs(y[169:end] - y[1:(end - 168)]))
score_predictions <- function(pred, scale) {
  groups <- split(pred, pred$model)
  do.call(rbind, lapply(groups, function(g) do.call(rbind, lapply(c("all", "operating"), function(subset) {
    z <- if (subset == "all") g else g[g$operating == 1, ]
    data.frame(model = g$model[1], subset = subset, as.list(metrics(z$actual, z$prediction, scale)),
      row.names = NULL)
  }))))
}
stopifnot(config$validation_end < config$test_start)
write_csv(data.frame(item = names(config), value = vapply(config, as.character, character(1))),
  "results", "experiment_protocol.csv")
writeLines(capture.output(sessionInfo()), path_for("results", "r_session_info.txt"))
write_csv(data.frame(package=required,version=vapply(required,function(p)as.character(utils::packageVersion(p)),character(1))),
  "results","r_package_versions.csv")
