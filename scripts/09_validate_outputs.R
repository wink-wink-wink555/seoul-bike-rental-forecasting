record <- function(name, passed, evidence) {
  data.frame(item = name, passed = isTRUE(passed), evidence = as.character(evidence),
             stringsAsFactors = FALSE)
}
items <- list()
add <- function(name, passed, evidence) {
  items[[length(items) + 1L]] <<- record(name, passed, evidence)
}
near <- function(x, y, tolerance = 1e-8) {
  length(x) == length(y) && all(is.finite(x)) && all(is.finite(y)) &&
    all(abs(as.numeric(x) - as.numeric(y)) <= tolerance)
}
read_csv_utf8 <- function(path) {
  if (!file.exists(path)) return(NULL)
  utils::read.csv(path, fileEncoding = "UTF-8", stringsAsFactors = FALSE,
                  check.names = FALSE)
}
read_text <- function(path) {
  if (!file.exists(path)) return("")
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}
fmt3 <- function(x) sprintf("%.3f", as.numeric(x))
config <- .experiment_config
result_path <- function(name) path_for("results", name)
data_path <- path_for("data", "processed", "IPG2211A2N_clean.csv")
dat <- read_csv_utf8(data_path)
if (!is.null(dat)) dat$date <- as.Date(dat$date)

# 源数据与固定窗口
raw_path <- if (exists(".data_input_path", envir = .GlobalEnv, inherits = FALSE)) {
  get(".data_input_path", envir = .GlobalEnv)
} else path_for("data", "raw", config$raw_snapshot)
raw <- read_csv_utf8(raw_path)
source_notes <- read_text(path_for("data", "raw", "SOURCE_NOTES.txt"))
metadata <- read_csv_utf8(result_path("data_metadata.csv"))
metadata_map <- if (!is.null(metadata)) stats::setNames(metadata$value, metadata$item) else character()
metadata_required <- c("series_id", "frequency", "units", "seasonal_adjustment", "source",
                       "series_url", "table_data_url", "snapshot_file", "snapshot_md5",
                       "snapshot_method", "version_consistency", "retrieved_on",
                       "analysis_start", "analysis_end", "analysis_n", "refresh_status")
metadata_ok <- length(metadata_map) > 0L && all(metadata_required %in% names(metadata_map)) &&
  identical(unname(metadata_map["series_id"]), config$series_id) &&
  identical(unname(metadata_map["analysis_start"]), as.character(config$analysis_start)) &&
  identical(unname(metadata_map["analysis_end"]), as.character(config$analysis_end))
add("FRED 来源、序列元信息和快照版本均有记录", metadata_ok,
    if (metadata_ok) paste0(metadata_map["series_id"], "; ", metadata_map["frequency"],
      "; ", metadata_map["units"], "; retrieved_on=", metadata_map["retrieved_on"],
      "; ", metadata_map["version_consistency"]) else "元信息缺失或与实验配置不一致")
version_documented <- metadata_ok && (
  grepl("One official CSV response", metadata_map["version_consistency"], fixed = TRUE) ||
  (grepl("Mixed source components", metadata_map["version_consistency"], fixed = TRUE) &&
     (grepl("混合版本", source_notes, fixed = TRUE) ||
        grepl("cannot be verified as a single FRED revision vintage", source_notes,
              fixed = TRUE))))
add("官方一致版本刷新状态或混合快照限制已如实记录", version_documented,
    if (version_documented) paste(metadata_map["refresh_status"],
                                  metadata_map["version_consistency"]) else
      "既未确认单次官方 CSV，也未记录混合版本限制")
raw_ok <- !is.null(raw) && identical(names(raw), c("observation_date", config$series_id)) &&
  file.exists(raw_path) && !is.na(file.info(raw_path)$size) && file.info(raw_path)$size > 1000
add("本次原始 FRED 快照文件真实存在且格式正确", raw_ok,
    if (raw_ok) paste0(basename(raw_path), "; bytes=", file.info(raw_path)$size) else
      "原始 CSV 缺失、空文件或列结构错误")
window_ok <- !is.null(dat) && all(c("date", "value") %in% names(dat)) &&
  identical(dat$date, seq(config$analysis_start, config$analysis_end, by = "month")) &&
  all(is.finite(dat$value)) && !anyDuplicated(dat$date) &&
  nrow(dat) == length(seq(config$analysis_start, config$analysis_end, by = "month"))
add("处理数据覆盖固定窗口且月份连续、无缺失和重复", window_ok,
    if (window_ok) paste(nrow(dat), min(dat$date), max(dat$date)) else
      "日期窗口、连续性、重复值或数值有效性失败")
if (raw_ok && window_ok) {
  raw_dates <- as.Date(raw$observation_date, format = "%Y-%m-%d")
  raw_values <- suppressWarnings(as.numeric(raw[[config$series_id]]))
  raw_match <- !is.na(raw_dates) & raw_dates >= config$analysis_start &
    raw_dates <= config$analysis_end
  source_match <- identical(raw_dates[raw_match], dat$date) &&
    near(raw_values[raw_match], dat$value, tolerance = 1e-12)
} else source_match <- FALSE
add("清洗数据逐月对应本地原始官方快照", source_match,
    if (source_match) "固定分析窗口的日期和值逐行一致" else "原始快照与处理数据不一致")

needed_results <- c("data_checks.csv", "data_metadata.csv", "descriptive_statistics.csv",
  "monthly_seasonality_summary.csv", "stationarity_tests.csv", "stl_components.csv",
  "acf_pacf_identification.csv", "candidate_models.csv", "selected_model.csv",
  "selected_model_parameters.csv", "selected_model_diagnostics.csv",
  "selected_sarima_train_fit.rds", "all_arima_candidates.rds", "nnar_specification.csv",
  "nnar_train_fit.rds", "split_information.csv", "test_predictions.csv",
  "model_comparison.csv", "prediction_interval_coverage.csv",
  "future_12_month_forecast.csv", "selected_sarima_full_sample_fit.rds")
results_exist <- all(file.exists(vapply(needed_results, result_path, character(1))))
add("核心分析、模型、预测及预测区间结果均已保存", results_exist,
    paste(needed_results[!file.exists(vapply(needed_results, result_path, character(1)))],
          collapse = ", "))
required_scripts <- c("00_setup.R", "01_data_preparation.R", "02_exploratory_analysis.R",
  "03_stationarity_analysis.R", "04_arima_modeling.R", "05_neural_network_model.R",
  "06_forecast_comparison.R", "08_generate_report_preview.R", "09_validate_outputs.R",
  "run_all.R")
scripts_ok <- all(file.exists(path_for("scripts", required_scripts)))
add("R 模块脚本和统一入口完整", scripts_ok,
    paste(required_scripts[!file.exists(path_for("scripts", required_scripts))], collapse = ", "))
run_source <- read_text(path_for("scripts", "run_all.R"))
refresh_logic_ok <- grepl("fred.stlouisfed.org/graph/fredgraph.csv", run_source, fixed = TRUE) &&
  grepl("NON-FATAL refresh failure", run_source, fixed = TRUE) &&
  grepl("analysis_end", run_source, fixed = TRUE) &&
  grepl("overwrite = FALSE", run_source, fixed = TRUE) &&
  grepl("archive", run_source, fixed = TRUE)
add("官方数据刷新固定窗口校验失败时保留本地快照继续运行", refresh_logic_ok,
    if (refresh_logic_ok) "仅接受 FRED 官方 CSV；完整性校验通过后存档输出；失败非致命" else
      "刷新来源、固定窗口或失败回退逻辑不完整")

# ACF/PACF 由实际训练序列重新计算，再与输出 CSV 对齐核验。
correlation_csv <- read_csv_utf8(result_path("acf_pacf_identification.csv"))
train_data <- if (window_ok) dat[seq_len(nrow(dat) - config$test_horizon), , drop = FALSE] else NULL
calc_corr <- function(x, partial, lag_max = config$acf_lag_max) {
  obj <- if (partial) stats::pacf(as.numeric(x), lag.max = lag_max, plot = FALSE) else
    stats::acf(as.numeric(x), lag.max = lag_max, plot = FALSE)
  lags <- as.numeric(obj$lag)
  correlations <- as.numeric(obj$acf)
  keep <- if (partial) lags >= 1 else lags > 0
  data.frame(lag = as.integer(round(lags[keep])), correlation = correlations[keep])
}
correlation_check <- function(x, label) {
  ac <- calc_corr(x, FALSE)
  pc <- calc_corr(x, TRUE)
  acf1_pacf1_ok <- near(ac$correlation[1], pc$correlation[1], 1e-10)
  lag_sequence_ok <- identical(ac$lag, seq_len(config$acf_lag_max)) &&
    identical(pc$lag, seq_len(config$acf_lag_max)) && !anyDuplicated(ac$lag) &&
    !anyDuplicated(pc$lag)
  saved <- correlation_csv[correlation_csv$series == label, , drop = FALSE]
  row_count <- config$identification_table_lags
  saved_ok <- nrow(saved) == row_count && identical(as.integer(saved$lag), seq_len(row_count))
  if (saved_ok) {
    aa <- ac[match(saved$lag, ac$lag), "correlation"]
    pp <- pc[match(saved$lag, pc$lag), "correlation"]
    saved_ok <- near(saved$acf, aa, 1e-10) && near(saved$pacf, pp, 1e-10) &&
      near(saved$acf[1], saved$pacf[1], 1e-10)
  }
  c(ok = acf1_pacf1_ok && lag_sequence_ok && saved_ok,
    lag_one_error = abs(ac$correlation[1] - pc$correlation[1]),
    rows = if (nrow(saved)) nrow(saved) else 0L)
}
if (!is.null(train_data) && !is.null(correlation_csv) &&
    all(c("series", "lag", "acf", "pacf") %in% names(correlation_csv))) {
  train_ts <- stats::ts(train_data$value,
    start = c(as.integer(format(train_data$date[1], "%Y")),
              as.integer(format(train_data$date[1], "%m"))),
    frequency = config$season_period)
  corr_original <- correlation_check(train_ts, "训练段原序列")
  corr_seasonal <- correlation_check(diff(train_ts, lag = config$season_period),
                                    "训练段季节差分")
  corr_ok <- isTRUE(as.logical(corr_original["ok"])) &&
    isTRUE(as.logical(corr_seasonal["ok"]))
  corr_evidence <- paste0("original PACF(1)-ACF(1)=", signif(corr_original["lag_one_error"], 4),
    "; seasonal PACF(1)-ACF(1)=", signif(corr_seasonal["lag_one_error"], 4),
    "; each saved table has lags 1-", config$identification_table_lags)
} else {
  corr_ok <- FALSE
  corr_evidence <- "序列或识别表缺失，无法重新计算 ACF/PACF"
}
add("原序列和季节差分序列 PACF(1)=ACF(1)，滞后连续并按 lag 对齐", corr_ok,
    corr_evidence)

# 模型阶数、估计参数、AICc 选择和残差检验。
selected <- read_csv_utf8(result_path("selected_model.csv"))
candidates <- read_csv_utf8(result_path("candidate_models.csv"))
selected_fit <- tryCatch(readRDS(result_path("selected_sarima_train_fit.rds")),
                        error = function(e) NULL)
parameters <- read_csv_utf8(result_path("selected_model_parameters.csv"))
diagnostics <- read_csv_utf8(result_path("selected_model_diagnostics.csv"))
model_ok <- !is.null(selected) && nrow(selected) == 1L && !is.null(selected_fit) &&
  all(c("p", "d", "q", "P", "D", "Q", "aicc") %in% names(selected))
if (model_ok) {
  actual_order <- as.integer(forecast::arimaorder(selected_fit))
  stored_order <- as.integer(unlist(selected[1, c("p", "d", "q", "P", "D", "Q")],
                                    use.names = FALSE))
  selected_candidate_row <- if (!is.null(candidates))
    candidates[candidates$name == selected$selected_name[1], , drop = FALSE] else NULL
  model_ok <- length(actual_order) >= 7L &&
    identical(actual_order[1:6], stored_order) &&
    actual_order[7] == config$season_period &&
    !is.null(selected_candidate_row) && nrow(selected_candidate_row) == 1L &&
    near(selected$aicc[1], selected_candidate_row$AICc[1], 1e-8)
}
add("报告/结果模型阶数与训练 SARIMA 模型对象一致", model_ok,
    if (model_ok) paste0("SARIMA(", paste(actual_order[1:3], collapse = ","), ")(",
      paste(actual_order[4:6], collapse = ","), ")[", config$season_period,
      "]; AICc=", fmt3(selected$aicc[1])) else "模型阶数或 AICc 与 RDS 模型不一致")
if (!is.null(candidates) && model_ok && nrow(candidates)) {
  eligible <- candidates[is.finite(candidates$AICc) &
                            candidates$ljung_box_p_value > .05, , drop = FALSE]
  pool <- if (nrow(eligible)) eligible else candidates[is.finite(candidates$AICc), , drop = FALSE]
  selected_by_rule <- nrow(pool) > 0L &&
    identical(selected$selected_name[1], pool$name[which.min(pool$AICc)])
  selection_text_ok <- grepl("Ljung-Box", selected$selection_rule[1], fixed = TRUE) &&
    grepl("AICc", selected$selection_rule[1], fixed = TRUE)
  selection_ok <- selected_by_rule && selection_text_ok
} else selection_ok <- FALSE
add("SARIMA 候选按训练段 AICc 与 Ljung-Box 规则可复算", selection_ok,
    if (selection_ok) paste0("selected=", selected$selected_name[1],
      "; candidate count=", nrow(candidates)) else "选模规则与候选结果不匹配")
if (!is.null(selected_fit) && !is.null(parameters)) {
  coef_values <- stats::coef(selected_fit)
  parameter_ok <- length(coef_values) > 0L && all(is.finite(coef_values)) &&
    !anyDuplicated(parameters$parameter) &&
    setequal(names(coef_values), parameters$parameter) &&
    near(unname(coef_values[parameters$parameter]), parameters$estimate, 1e-8) &&
    all(is.finite(parameters$std_error)) && all(parameters$std_error > 0)
} else parameter_ok <- FALSE
add("SARIMA 参数估计有限并与拟合对象一致", parameter_ok,
    if (parameter_ok) paste(parameters$parameter, collapse = ", ") else
      "参数值、标准误或拟合对象对应失败")
if (!is.null(selected_fit) && !is.null(diagnostics) && nrow(diagnostics) == 1L) {
  residual <- as.numeric(stats::na.omit(stats::residuals(selected_fit)))
  ord <- forecast::arimaorder(selected_fit)
  fitdf <- sum(ord[c(1, 3, 4, 6)])
  lag <- min(config$residual_ljung_box_lag, floor(length(residual) / 5))
  lb <- stats::Box.test(residual, lag = lag, type = "Ljung-Box", fitdf = fitdf)
  residual_ok <- length(residual) > lag && all(is.finite(residual)) && fitdf < lag &&
    is.finite(lb$statistic) && is.finite(lb$p.value) && lb$p.value >= 0 && lb$p.value <= 1 &&
    near(diagnostics$statistic[1], unname(lb$statistic), 1e-8) &&
    near(diagnostics$p_value[1], lb$p.value, 1e-8) &&
    near(diagnostics$fitdf[1], fitdf, 1e-8)
} else residual_ok <- FALSE
add("模型残差有限且 Ljung-Box 统计量由 RDS 残差重新计算相符", residual_ok,
    if (residual_ok) paste0("lag=", lag, "; fitdf=", fitdf, "; p=", signif(lb$p.value, 5)) else
      "残差或诊断结果无效，或与模型对象不一致")

# 测试集日期和四模型误差从实际数据重算。
pred <- read_csv_utf8(result_path("test_predictions.csv"))
metrics <- read_csv_utf8(result_path("model_comparison.csv"))
split_info <- read_csv_utf8(result_path("split_information.csv"))
prediction_ok <- !is.null(pred) && window_ok && nrow(pred) == config$test_horizon &&
  all(c("date", "actual", "seasonal_naive", "sarima", "nnar", "ets") %in% names(pred))
if (prediction_ok) {
  pred$date <- as.Date(pred$date)
  expected_test <- dat[(nrow(dat) - config$test_horizon + 1L):nrow(dat), , drop = FALSE]
  expected_train <- dat[seq_len(nrow(dat) - config$test_horizon), , drop = FALSE]
  point_cols <- c("actual", "seasonal_naive", "sarima", "nnar", "ets")
  prediction_ok <- identical(pred$date, expected_test$date) &&
    near(pred$actual, expected_test$value, 1e-12) &&
    all(is.finite(as.matrix(pred[, point_cols]))) &&
    max(expected_train$date) < min(pred$date) &&
    !any(expected_train$date %in% pred$date) &&
    !is.null(split_info) && split_info$training_n[1] == nrow(expected_train) &&
    split_info$test_n[1] == nrow(expected_test) &&
    as.Date(split_info$training_end[1]) < as.Date(split_info$test_start[1])
} else expected_train <- NULL
add("訓練和測試時間不重疊，四模型預測對齊相同實際月份", prediction_ok,
    if (prediction_ok) paste0("train through ", max(expected_train$date), "; test ",
      min(pred$date), " to ", max(pred$date), "; n=", nrow(pred)) else
      "預測日期、實際值或訓練/測試邊界不匹配")
metric_fn <- function(actual, predicted, training, season_period) {
  error <- actual - predicted
  denom <- abs(actual) + abs(predicted)
  smape_term <- ifelse(denom == 0, ifelse(error == 0, 0, NA_real_),
                       200 * abs(error) / denom)
  m <- season_period
  scale <- mean(abs(training[(m + 1L):length(training)] -
                       training[1:(length(training) - m)]))
  c(MAE = mean(abs(error)), RMSE = sqrt(mean(error^2)),
    sMAPE = mean(smape_term, na.rm = TRUE), MASE = mean(abs(error)) / scale)
}
if (prediction_ok && !is.null(metrics) && nrow(metrics) == 4L) {
  column_models <- c(seasonal_naive = "季节性朴素", sarima = "SARIMA",
                     nnar = "NNAR", ets = "ETS")
  metric_ok <- all(c("model", "MAE", "RMSE", "sMAPE", "MASE") %in% names(metrics)) &&
    setequal(metrics$model, unname(column_models))
  if (metric_ok) for (column in names(column_models)) {
    model_row <- metrics[metrics$model == column_models[[column]], , drop = FALSE]
    expected_metrics <- metric_fn(pred$actual, pred[[column]], expected_train$value,
                                  config$season_period)
    if (nrow(model_row) != 1L ||
        !near(as.numeric(unlist(model_row[1, c("MAE", "RMSE", "sMAPE", "MASE")],
                               use.names = FALSE)),
              expected_metrics, 1e-8)) metric_ok <- FALSE
  }
} else metric_ok <- FALSE
add("四模型 MAE/RMSE/sMAPE/MASE 由测试实际值重算一致", metric_ok,
    if (metric_ok) paste(metrics$model, collapse = "; ") else
      "模型集合或至少一项误差指标与重新计算结果不一致")

# 预测区间顺序、覆盖率和区间嵌套。
interval_cols <- c("sarima_lo80", "sarima_hi80", "sarima_lo95", "sarima_hi95")
interval_ok <- prediction_ok && all(interval_cols %in% names(pred)) &&
  all(is.finite(as.matrix(pred[, interval_cols]))) &&
  all(pred$sarima_lo80 <= pred$sarima_hi80) &&
  all(pred$sarima_lo95 <= pred$sarima_hi95) &&
  all(pred$sarima_lo95 <= pred$sarima_lo80) &&
  all(pred$sarima_hi80 <= pred$sarima_hi95)
add("SARIMA 预测区间上下界有限、合法且 95% 区间包含 80% 区间", interval_ok,
    if (interval_ok) "24 个预测点均满足 lo95 ≤ lo80 ≤ hi80 ≤ hi95" else
      "区间包含 NA/Inf、下界大于上界或未正确嵌套")
coverage <- read_csv_utf8(result_path("prediction_interval_coverage.csv"))
if (interval_ok && !is.null(coverage) && nrow(coverage) == 2L) {
  expected_coverage <- do.call(rbind, lapply(seq_along(c(80, 95)), function(i) {
    lower <- pred[[paste0("sarima_lo", c(80, 95)[i])]]
    upper <- pred[[paste0("sarima_hi", c(80, 95)[i])]]
    inside <- pred$actual >= lower & pred$actual <= upper
    c(nominal_coverage = c(.80, .95)[i], actual_coverage = mean(inside),
      covered_n = sum(inside), test_n = nrow(pred), mean_width = mean(upper - lower))
  }))
  coverage_ok <- all(c("nominal_coverage", "actual_coverage", "covered_n", "test_n",
                       "mean_width") %in% names(coverage)) &&
    near(coverage$nominal_coverage, expected_coverage[, "nominal_coverage"], 1e-12) &&
    near(coverage$actual_coverage, expected_coverage[, "actual_coverage"], 1e-12) &&
    near(coverage$covered_n, expected_coverage[, "covered_n"], 1e-12) &&
    near(coverage$test_n, expected_coverage[, "test_n"], 1e-12) &&
    near(coverage$mean_width, expected_coverage[, "mean_width"], 1e-8)
} else coverage_ok <- FALSE
add("80%/95% 预测区间覆盖率按含边界定义重算一致", coverage_ok,
    if (coverage_ok) paste0("80%: ", coverage$covered_n[1], "/", coverage$test_n[1],
      " = ", fmt3(coverage$actual_coverage[1] * 100), "%；95%: ", coverage$covered_n[2],
      "/", coverage$test_n[2], " = ", fmt3(coverage$actual_coverage[2] * 100), "%") else
      "覆盖次数、覆盖率、样本数或平均区间宽度不匹配")

# 全样本未来预测日期与模型阶数。
future <- read_csv_utf8(result_path("future_12_month_forecast.csv"))
full_fit <- tryCatch(readRDS(result_path("selected_sarima_full_sample_fit.rds")),
                     error = function(e) NULL)
future_ok <- !is.null(future) && nrow(future) == config$future_horizon &&
  all(c("date", "forecast", "lo80", "hi80", "lo95", "hi95") %in% names(future)) &&
  all(is.finite(as.matrix(future[, c("forecast", "lo80", "hi80", "lo95", "hi95")]))) &&
  all(future$lo95 <= future$lo80) && all(future$lo80 <= future$hi80) &&
  all(future$hi80 <= future$hi95)
if (future_ok && window_ok) {
  future$date <- as.Date(future$date)
  expected_future <- seq(max(dat$date), by = "month", length.out = config$future_horizon + 1L)[-1]
  future_ok <- identical(future$date, expected_future) && !is.null(full_fit) &&
    identical(as.integer(forecast::arimaorder(full_fit)), as.integer(forecast::arimaorder(selected_fit)))
}
add("全样本模型的未来 12 个月日期连续、预测有效且阶数一致", future_ok,
    if (future_ok) paste(min(future$date), max(future$date)) else
      "预测日期、区间或全样本拟合模型与所选 SARIMA 不一致")

# 图像规格及报告内容和自动生成结果表。
expected_figures <- c("01_original_series.png", "02_stl_decomposition.png",
  "03_monthly_seasonality.png", "04_original_acf.png", "04_original_pacf.png",
  "05_differencing_comparison.png", "06_seasonal_diff_acf.png",
  "06_seasonal_diff_pacf.png", "09_sarima_residual_series.png",
  "10_sarima_residual_acf.png", "11_sarima_residual_qq.png",
  "12_sarima_test_forecast.png", "13_nnar_test_forecast.png",
  "14_all_model_test_comparison.png", "15_model_error_metrics.png",
  "16_future_12_month_forecast.png")
png_info <- function(file) {
  if (!file.exists(file) || file.info(file)$size < 5000) return(c(width = 0, height = 0))
  con <- file(file, open = "rb")
  on.exit(close(con))
  bytes <- readBin(con, what = "raw", n = 24L)
  if (length(bytes) < 24L || !identical(bytes[1:8], as.raw(c(137, 80, 78, 71, 13, 10, 26, 10)))) {
    return(c(width = 0, height = 0))
  }
  to_int <- function(x) sum(as.numeric(x) * 256^(3:0))
  c(width = to_int(bytes[17:20]), height = to_int(bytes[21:24]))
}
png_dimensions <- lapply(path_for("figures", expected_figures), png_info)
figure_ok <- all(vapply(png_dimensions, function(x) all(x > 0), logical(1))) &&
  all(grepl("dpi = 300", read_text(path_for("scripts", "00_setup.R")), fixed = TRUE))
add("所有要求图片均为可读 PNG 且由 300 DPI 输出设置生成", figure_ok,
    paste0(sum(vapply(png_dimensions, function(x) all(x > 0), logical(1))), "/",
           length(expected_figures), " images have valid PNG headers and dimensions"))

report_path <- path_for("report.md")
preview_path <- path_for("report_preview.md")
report <- read_text(report_path)
preview <- read_text(preview_path)
required_headings <- c("第一章 概述", "1.1 研究背景与意义", "1.1.1 研究背景",
  "1.1.2 研究意义", "1.2 数据来源与说明", "1.2.1 数据源", "1.2.2 描述性统计分析",
  "第二章 模型识别", "2.1 平稳性分析", "2.2 相关函数与差分变换",
  "2.3 模型定阶与参数识别", "2.4 白噪声检验", "第三章 模型预测",
  "3.1 短期预测", "3.2 效果分析", "第四章 神经网络自回归（NNAR）模型及其预测",
  "4.1 短期预测", "4.2 模型对比分析", "第五章 课程总结", "参考文献",
  "附录（程序代码）")
headings_ok <- nzchar(report) && all(vapply(required_headings,
  function(h) grepl(h, report, fixed = TRUE), logical(1)))
add("报告保留完整章节结构、参考文献和代码附录", headings_ok,
    if (headings_ok) "课程要求的章节标题均可检出" else "章节标题缺失")

extract_ids <- function(text, type) {
  pattern <- paste0(type, "\\s*[1-4]-[0-9]+")
  matches <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1]]
  if (!length(matches)) return(character())
  gsub("\\s+", "", sub(paste0("^", type), "", matches), perl = TRUE)
}
figure_captions <- regmatches(report,
  gregexpr("\\*\\*图\\s*[1-4]-[0-9]+[^*]*\\*\\*", report, perl = TRUE))[[1]]
table_captions <- regmatches(report,
  gregexpr("\\*\\*表\\s*[1-4]-[0-9]+[^*]*\\*\\*", report, perl = TRUE))[[1]]
caption_ids <- function(x, type) {
  if (!length(x)) return(character())
  regmatches(x, regexpr("[1-4]-[0-9]+", x, perl = TRUE))
}
figure_ids <- caption_ids(figure_captions, "图")
table_ids <- caption_ids(table_captions, "表")
figure_refs <- extract_ids(report, "图")
table_refs <- extract_ids(report, "表")
continuous_ids <- function(ids) {
  if (!length(ids) || anyDuplicated(ids)) return(FALSE)
  by_chapter <- split(as.integer(sub("^[1-4]-", "", ids)),
                      sub("-[0-9]+$", "", ids))
  all(vapply(by_chapter, function(x) identical(sort(x), seq_len(max(x))), logical(1)))
}
numbering_ok <- length(figure_ids) > 0L && length(table_ids) > 0L &&
  !anyDuplicated(figure_ids) && !anyDuplicated(table_ids) &&
  continuous_ids(figure_ids) && continuous_ids(table_ids) &&
  all(figure_refs %in% figure_ids) && all(table_refs %in% table_ids)
add("图表编号唯一连续，正文每个图表引用均有对应标题", numbering_ok,
    if (numbering_ok) paste0("figures=", paste(figure_ids, collapse = ","),
      "; tables=", paste(table_ids, collapse = ",")) else
      "图/表标题重复、编号不连续或正文引用没有对应标题")
placeholder_paths <- regmatches(report,
  gregexpr("figures/[A-Za-z0-9_.-]+\\.png", report, perl = TRUE))[[1]]
placeholder_files <- if (length(placeholder_paths))
  sub("^figures/", "", placeholder_paths) else character()
placeholder_ok <- length(placeholder_files) == length(expected_figures) &&
  setequal(placeholder_files, expected_figures) &&
  all(file.exists(path_for("figures", placeholder_files)))
add("报告图号和图像文件逐一对应", placeholder_ok,
    paste0(length(placeholder_files), " formal figure references; missing: ",
      paste(placeholder_files[!file.exists(path_for("figures", placeholder_files))],
            collapse = ", ")))
preview_links <- regmatches(preview,
  gregexpr("!\\[[^]]+\\]\\(figures/[^)]+\\)", preview, perl = TRUE))[[1]]
preview_paths <- regmatches(preview,
  gregexpr("figures/[A-Za-z0-9_.-]+\\.png", preview, perl = TRUE))[[1]]
preview_files <- if (length(preview_paths)) sub("^figures/", "", preview_paths) else character()
report_lines <- if (file.exists(report_path))
  readLines(report_path, warn = FALSE, encoding = "UTF-8") else character()
preview_lines <- if (file.exists(preview_path))
  readLines(preview_path, warn = FALSE, encoding = "UTF-8") else character()
normalize_figure_lines <- function(lines) {
  for (filename in expected_figures) {
    has_image <- grepl(filename, lines, fixed = TRUE)
    lines[has_image] <- "[[FIGURE]]"
  }
  lines
}
preview_ok <- length(preview_links) == length(expected_figures) &&
  setequal(preview_files, expected_figures) &&
  identical(normalize_figure_lines(report_lines), normalize_figure_lines(preview_lines))
add("report_preview 正文与 report.md 完全一致且图片链接有效", preview_ok,
    if (preview_ok) paste(length(preview_links), "图片链接；去除图片语法后正文逐行一致") else
      "预览图链接数量或正文内容与正式报告不一致")

model_latex <- ""
model_plain <- ""
if (model_ok) {
  model_latex <- sprintf("SARIMA$(%d,%d,%d)(%d,%d,%d)_{%d}$", actual_order[1],
    actual_order[2], actual_order[3], actual_order[4], actual_order[5], actual_order[6],
    config$season_period)
  model_plain <- sprintf("SARIMA(%d,%d,%d)(%d,%d,%d)[%d]", actual_order[1],
    actual_order[2], actual_order[3], actual_order[4], actual_order[5], actual_order[6],
    config$season_period)
  report_model_ok <- grepl(model_latex, report, fixed = TRUE)
} else report_model_ok <- FALSE
add("报告摘要、参数识别和预测中的 SARIMA 阶数与模型结果一致", report_model_ok,
    if (report_model_ok) model_plain else "报告中的模型阶数与 RDS 对象不一致")

report_lines <- readLines(report_path, warn = FALSE, encoding = "UTF-8")
report_block <- function(name) {
  begin <- match(paste0("<!-- ", name, "_BEGIN -->"), report_lines)
  end <- match(paste0("<!-- ", name, "_END -->"), report_lines)
  if (is.na(begin) || is.na(end) || begin >= end) return(character())
  report_lines[(begin + 1L):(end - 1L)]
}
fmt4 <- function(x) sprintf("%.4f", as.numeric(x))
fmt_report_p <- function(x) {
  x <- as.numeric(x)
  if (!is.finite(x)) return("—")
  if (x < .001) "<0.001" else sprintf("%.4f", x)
}
line_present <- function(block, line) any(block == line)
desc_report <- read_csv_utf8(result_path("descriptive_statistics.csv"))
stationarity_report <- read_csv_utf8(result_path("stationarity_tests.csv"))
parameter_report <- parameters
nnar_report <- read_csv_utf8(result_path("nnar_specification.csv"))
acf_report <- read_csv_utf8(result_path("acf_pacf_identification.csv"))
tables_report_ok <- !is.null(desc_report) && !is.null(stationarity_report) &&
  !is.null(parameter_report) && !is.null(nnar_report) && model_ok &&
  !is.null(metrics) && nrow(metrics) == 4L
if (tables_report_ok) {
  b1 <- report_block("AUTO_TABLE_1_1")
  desc_expected <- c(
    paste0("| 观测数 | ", as.integer(desc_report$n[1]), " |"),
    paste0("| 起止月份 | ", format(as.Date(desc_report$start[1]), "%Y-%m"), " 至 ",
           format(as.Date(desc_report$end[1]), "%Y-%m"), " |"),
    paste0("| 缺失值 | ", as.integer(desc_report$missing[1]), " |"),
    paste0("| 均值 | ", fmt3(desc_report$mean[1]), " |"),
    paste0("| 标准差 | ", fmt3(desc_report$sd[1]), " |"),
    paste0("| 中位数 | ", fmt3(desc_report$median[1]), " |"),
    paste0("| 最小值 | ", fmt3(desc_report$min[1]), " |"),
    paste0("| 第一四分位数 | ", fmt3(desc_report$q1[1]), " |"),
    paste0("| 第三四分位数 | ", fmt3(desc_report$q3[1]), " |"),
    paste0("| 最大值 | ", fmt3(desc_report$max[1]), " |"))
  tables_report_ok <- line_present(b1, desc_expected[1]) &&
    all(vapply(desc_expected[-1], function(line) line_present(b1, line), logical(1)))

  b2 <- report_block("AUTO_TABLE_2_1")
  if (tables_report_ok) for (i in seq_len(nrow(stationarity_report))) {
    row <- stationarity_report[i, ]
    expected <- paste0("| ", row$series, " | ", as.integer(row$n), " | ",
      fmt3(row$adf_statistic), " | ", fmt3(row$adf_cv_5pct), " | ", row$adf_p_approx,
      " | ", fmt3(row$kpss_statistic), " | ", fmt3(row$kpss_cv_5pct), " | ",
      row$kpss_p_approx, " |")
    if (!line_present(b2, expected)) tables_report_ok <- FALSE
  }

  b2c <- report_block("AUTO_TABLE_2_2")
  candidate_ids <- if (!is.null(candidates) && nrow(candidates)) {
    ord_ids <- order(candidates$AICc)
    unique(c(ord_ids[seq_len(min(5L, length(ord_ids)))],
             which(candidates$name == selected$selected_name[1])))
  } else integer()
  if (tables_report_ok) for (i in candidate_ids) {
    item <- candidates[i, ]
    label <- if (identical(item$name, selected$selected_name[1])) {
      paste0(model_latex, if ("drift" %in% parameter_report$parameter) "，含漂移" else "",
             "（auto.arima）")
    } else sprintf("SARIMA$(%d,%d,%d)(%d,%d,%d)_{%d}$", item$p, item$d, item$q,
                   item$P, item$D, item$Q, config$season_period)
    expected <- paste0("| ", label, " | ", fmt3(item$AIC), " | ", fmt3(item$AICc),
                       " | ", fmt3(item$BIC), " | ", fmt_report_p(item$ljung_box_p_value), " |")
    if (!line_present(b2c, expected)) tables_report_ok <- FALSE
  }

  b2p <- report_block("AUTO_TABLE_2_3")
  if (tables_report_ok) for (i in seq_len(nrow(parameter_report))) {
    item <- parameter_report[i, ]
    expected <- paste0("| ", item$parameter, " | ", fmt4(item$estimate), " | ",
                       fmt4(item$std_error), " | ", fmt_report_p(item$p_value), " |")
    if (!line_present(b2p, expected)) tables_report_ok <- FALSE
  }

  b3 <- report_block("AUTO_TABLE_3_1")
  for (model_name in c("SARIMA", "季节性朴素")) {
    item <- metrics[metrics$model == model_name, , drop = FALSE]
    label <- if (model_name == "SARIMA") model_latex else model_name
    expected <- paste0("| ", label, " | ", fmt3(item$MAE), " | ", fmt3(item$RMSE),
                       " | ", fmt3(item$sMAPE), " | ", fmt3(item$MASE), " |")
    if (nrow(item) != 1L || !line_present(b3, expected)) tables_report_ok <- FALSE
  }

  b4 <- report_block("AUTO_TABLE_4_1")
  nnar_label <- paste0("NNAR$(", nnar_report$p[1], ",", nnar_report$P[1], ",",
                       nnar_report$hidden_units[1], ")_{", config$season_period, "}$")
  nnar_expected <- c(
    paste0("| NNAR 结构 | ", nnar_label, " |"),
    paste0("| 非季节滞后阶数 $p$ | ", as.integer(nnar_report$p[1]), " |"),
    paste0("| 季节滞后阶数 $P$ | ", as.integer(nnar_report$P[1]), " |"),
    paste0("| 隐层节点数 $k$ | ", as.integer(nnar_report$hidden_units[1]), " |"),
    paste0("| repeats | ", as.integer(nnar_report$repeats[1]), " |"),
    paste0("| 随机种子 | ", as.integer(nnar_report$seed[1]), " |"),
    paste0("| 训练样本数 | ", as.integer(nnar_report$training_n[1]), " |"),
    paste0("| 测试样本数 | ", as.integer(nnar_report$test_n[1]), " |"))
  if (!all(vapply(nnar_expected, function(line) line_present(b4, line), logical(1)))) {
    tables_report_ok <- FALSE
  }
  corr_values <- c(
    acf_report$acf[acf_report$series == "训练段原序列" & acf_report$lag %in% c(1, 6, 12, 24)],
    acf_report$acf[acf_report$series == "训练段季节差分" & acf_report$lag %in% c(1, 12)])
  tables_report_ok <- tables_report_ok &&
    all(vapply(fmt3(corr_values), function(value) grepl(value, report, fixed = TRUE),
               logical(1)))
}
add("报告描述统计、检验、候选模型、参数、识别和 NNAR 配置表与结果 CSV 一致",
    tables_report_ok,
    if (tables_report_ok) "各自动生成表格的数值与对应结果文件逐行匹配" else
      "至少一个描述统计/ADF-KPSS/候选模型/参数/识别或 NNAR 配置表不匹配")

report_metrics_ok <- FALSE
if (prediction_ok && metric_ok && model_ok) {
  marker_text <- regmatches(report, regexpr(
    "<!-- AUTO_TABLE_4_2_BEGIN -->[\\s\\S]*?<!-- AUTO_TABLE_4_2_END -->",
    report, perl = TRUE))
  report_metrics_ok <- length(marker_text) == 1L && nzchar(marker_text)
  if (report_metrics_ok) for (i in seq_len(nrow(metrics))) {
    item <- metrics[i, ]
    row_name <- if (item$model == "SARIMA") model_latex else item$model
    row <- paste0("| ", row_name, " | ", fmt3(item$MAE), " | ", fmt3(item$RMSE),
                  " | ", fmt3(item$sMAPE), " | ", fmt3(item$MASE), " |")
    if (!grepl(row, marker_text, fixed = TRUE)) report_metrics_ok <- FALSE
  }
  if (report_metrics_ok) {
    ranking_block <- regmatches(report, regexpr(
      "<!-- AUTO_MODEL_RANKING_SUMMARY_BEGIN -->[\\s\\S]*?<!-- AUTO_MODEL_RANKING_SUMMARY_END -->",
      report, perl = TRUE))
    metrics_order <- c("MAE", "RMSE", "sMAPE", "MASE")
    best <- vapply(metrics_order, function(column) metrics$model[which.min(metrics[[column]])][1],
                   character(1))
    if (length(unique(best)) == 1L) {
      best_claim <- paste0(best[1], " 在四项误差指标上均最低")
      report_metrics_ok <- grepl(best_claim, ranking_block, fixed = TRUE)
    }
    baseline <- metrics[metrics$model == "季节性朴素", , drop = FALSE]
    for (model_name in c("SARIMA", "NNAR")) {
      model_row <- metrics[metrics$model == model_name, , drop = FALSE]
      all_better <- all(as.numeric(unlist(model_row[1, metrics_order], use.names = FALSE)) <
                          as.numeric(unlist(baseline[1, metrics_order], use.names = FALSE)))
      claim <- if (all_better) paste0(model_name, " 的四项误差均低于季节性朴素") else NULL
      if (!is.null(claim) && !grepl(claim, ranking_block, fixed = TRUE)) report_metrics_ok <- FALSE
      if (model_name == "NNAR" && all_better &&
          model_row$RMSE[1] < baseline$RMSE[1] &&
          (baseline$RMSE[1] - model_row$RMSE[1]) / baseline$RMSE[1] < .01) {
        rmse_pair <- paste0("RMSE 差异较小（", fmt3(model_row$RMSE), " 对比 ",
                            fmt3(baseline$RMSE), "）")
        if (!grepl(rmse_pair, ranking_block, fixed = TRUE)) report_metrics_ok <- FALSE
      }
    }
  }
}
add("报告四模型误差表和模型优劣结论与 CSV 重算结果一致", report_metrics_ok,
    if (report_metrics_ok) "四项指标逐模型精确核对，排名按误差值重算" else
      "报告指标表或 NNAR/SARIMA/ETS/季节性朴素排名结论不一致")

report_coverage_ok <- FALSE
if (coverage_ok) {
  coverage_block <- regmatches(report, regexpr(
    "<!-- AUTO_TABLE_3_2_BEGIN -->[\\s\\S]*?<!-- AUTO_TABLE_3_2_END -->",
    report, perl = TRUE))
  report_coverage_ok <- length(coverage_block) == 1L && nzchar(coverage_block)
  if (report_coverage_ok) for (i in seq_len(nrow(coverage))) {
    row <- paste0("| ", sprintf("%.0f%%", coverage$nominal_coverage[i] * 100), " | ",
      as.integer(coverage$covered_n[i]), " / ", as.integer(coverage$test_n[i]), " | ",
      as.integer(coverage$test_n[i]), " | ", sprintf("%.2f%%", coverage$actual_coverage[i] * 100),
      " | ", fmt3(coverage$mean_width[i]), " |")
    if (!grepl(row, coverage_block, fixed = TRUE)) report_coverage_ok <- FALSE
  }
}
add("报告预测区间覆盖率表与覆盖率 CSV 一致", report_coverage_ok,
    if (report_coverage_ok) "名义水平、覆盖数、样本数、覆盖率和平均宽度均匹配" else
      "覆盖率报告表未与独立 CSV 对齐")
report_future_ok <- future_ok
if (report_future_ok) for (i in seq_len(nrow(future))) {
  row <- paste0("| ", format(as.Date(future$date[i]), "%Y-%m"), " | ",
    fmt3(future$forecast[i]), " | [", fmt3(future$lo80[i]), ", ", fmt3(future$hi80[i]),
    "] | [", fmt3(future$lo95[i]), ", ", fmt3(future$hi95[i]), "] |")
  if (!grepl(row, report, fixed = TRUE)) report_future_ok <- FALSE
}
add("报告未来 12 个月点预测与预测区间逐月对应 CSV", report_future_ok,
    if (report_future_ok) paste(nrow(future), "个月全部数值一致") else
      "至少一个未来月份的预测或预测区间与 CSV 不一致")
if (!is.null(nnar_spec <- read_csv_utf8(result_path("nnar_specification.csv")))) {
  nnar_fit <- tryCatch(readRDS(result_path("nnar_train_fit.rds")), error = function(e) NULL)
  nnar_config_ok <- !is.null(nnar_fit) && nrow(nnar_spec) == 1L &&
    nnar_spec$training_n[1] == nrow(expected_train) &&
    nnar_spec$test_n[1] == config$test_horizon &&
    all(c("p", "P", "hidden_units", "repeats", "seed") %in% names(nnar_spec))
} else nnar_config_ok <- FALSE
add("NNAR 实际设置和训练/测试样本数已保存", nnar_config_ok,
    if (nnar_config_ok) paste0(nnar_spec$model[1], "; train=", nnar_spec$training_n[1],
      "; test=", nnar_spec$test_n[1], "; repeats=", nnar_spec$repeats[1],
      "; seed=", nnar_spec$seed[1]) else "NNAR RDS/配置记录缺失或与实际切分不符")

reference_ok <- nzchar(report) && all(vapply(paste0("[", 1:7, "]"),
  function(ref) grepl(ref, report, fixed = TRUE), logical(1)))
add("参考文献编号完整且序列数据来源可追溯", reference_ok,
    if (reference_ok) "[1] 至 [7] 可检出" else "参考文献编号不全")
readme_ok <- file.exists(path_for("README.md")) && file.exists(path_for("run.ps1")) &&
  file.exists(path_for("Assginment-TimeSeries.Rproj"))
add("README、R Project 和 PowerShell 运行入口存在", readme_ok,
    "README.md / Assginment-TimeSeries.Rproj / run.ps1")
packages <- c("forecast", "ggplot2", "urca")
package_ok <- all(vapply(packages, requireNamespace, logical(1), quietly = TRUE))
package_versions <- if (package_ok) paste(packages, vapply(packages, function(p) {
  as.character(utils::packageVersion(p))
}, character(1)), sep = "=", collapse = "; ") else "缺少必需 R 包"
add("R 及分析依赖可用并记录版本", package_ok,
    paste(R.version.string, package_versions))

checks <- do.call(rbind, items)
all_passed <- all(checks$passed)
status <- if (all_passed) "全部通过" else paste(sum(!checks$passed), "项未通过")
lines <- c("# 实验验收与复核记录", "",
  paste0("- 状态：**", status, "**"),
  paste0("- 生成时间：", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste0("- R：", R.version.string), "",
  "| 验收项目 | 结果 | 证据或未通过原因 |", "|---|---|---|",
  vapply(seq_len(nrow(checks)), function(i) paste0("| ", checks$item[i], " | ",
    if (checks$passed[i]) "通过" else "未通过", " | ",
    gsub("\\|", "/", checks$evidence[i]), " |"), character(1)), "",
  "说明：全部通过仅在所有必需计算型检查均完成时写出；存在关键失败时脚本会在保存此摘要后返回非零状态。")
summary_con <- file(result_path("validation_summary.md"), open = "wt", encoding = "UTF-8")
writeLines(lines, summary_con)
close(summary_con)
message("Validation summary written: ", result_path("validation_summary.md"),
        " -- ", if (all_passed) "all required checks passed" else "validation failed")
if (!all_passed) {
  failed <- paste(checks$item[!checks$passed], collapse = "; ")
  stop("Output validation failed: ", failed, call. = FALSE)
}
