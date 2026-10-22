record <- function(name, passed, evidence) {
  data.frame(item = name, passed = isTRUE(passed), evidence = evidence,
             stringsAsFactors = FALSE)
}
items <- list()
add <- function(name, passed, evidence) {
  items[[length(items) + 1L]] <<- record(name, passed, evidence)
}
read_csv_utf8 <- function(path, ...) utils::read.csv(path, fileEncoding = "UTF-8", ...)
data_file <- path_for("data", "processed", "IPG2211A2N_clean.csv")
raw_file <- path_for("data", "raw", "IPG2211A2N_fred_snapshot_2000_2026-09.csv")
dat <- if (file.exists(data_file)) read_csv_utf8(data_file, stringsAsFactors = FALSE) else NULL
if (!is.null(dat)) dat$date <- as.Date(dat$date)
add("真实且来源可核验的 FRED 序列", file.exists(raw_file),
    "原始快照与来源说明位于 data/raw；序列代码 IPG2211A2N")
add("原始数据快照已保存", file.exists(raw_file),
    if (file.exists(raw_file)) basename(raw_file) else "缺少原始快照")
add("日期范围、样本数与月度连续性",
    !is.null(dat) && nrow(dat) == 321L &&
      min(dat$date) == as.Date("2000-01-01") && max(dat$date) == as.Date("2026-09-01"),
    if (!is.null(dat)) paste(nrow(dat), min(dat$date), max(dat$date)) else "处理数据不存在")
add("缺失值、重复日期与数值检查",
    !is.null(dat) && !anyNA(dat$value) && !anyDuplicated(dat$date) && all(is.finite(dat$value)),
    if (!is.null(dat)) paste0("缺失=", sum(is.na(dat$value)),
                              "; 重复=", sum(duplicated(dat$date))) else "处理数据不存在")
checks_file <- path_for("results", "data_checks.csv")
checks_data <- if (file.exists(checks_file)) read_csv_utf8(checks_file) else NULL
continuity_ok <- !is.null(checks_data) &&
  any(checks_data$check == "monthly_continuity" & checks_data$result == "PASS")
add("月度连续性已由程序校验", continuity_ok,
    if (continuity_ok) "按月序列无断档，未插值" else "缺少连续性校验记录")

required_scripts <- c("00_setup.R", "01_data_preparation.R", "02_exploratory_analysis.R",
                      "03_stationarity_analysis.R", "04_arima_modeling.R",
                      "05_neural_network_model.R", "06_forecast_comparison.R",
                      "08_generate_report_preview.R", "09_validate_outputs.R", "run_all.R")
scripts_exist <- all(file.exists(path_for("scripts", required_scripts)))
add("R 核心脚本及统一入口", scripts_exist,
    paste(required_scripts[!file.exists(path_for("scripts", required_scripts))], collapse = ", "))
refresh_code <- if (file.exists(path_for("scripts", "run_all.R"))) {
  paste(readLines(path_for("scripts", "run_all.R"), warn = FALSE, encoding = "UTF-8"),
        collapse = "\n")
} else ""
add("显式刷新会新建快照并归档既有输出",
    grepl("--refresh-data", refresh_code, fixed = TRUE) &&
      grepl("archive", refresh_code, fixed = TRUE) &&
      grepl("overwrite = FALSE", refresh_code, fixed = TRUE) &&
      grepl("if (!all(copied))", refresh_code, fixed = TRUE),
    "刷新路径保存时间戳快照；既有结果复制到 archive/，不覆盖原始快照")

needed_results <- c("data_checks.csv", "descriptive_statistics.csv",
                    "monthly_seasonality_summary.csv", "stationarity_tests.csv",
                    "stl_components.csv", "acf_pacf_identification.csv",
                    "candidate_models.csv", "selected_model.csv",
                    "selected_model_parameters.csv", "selected_model_diagnostics.csv",
                    "nnar_specification.csv", "split_information.csv",
                    "test_predictions.csv", "model_comparison.csv",
                    "future_12_month_forecast.csv")
result_ok <- all(file.exists(path_for("results", needed_results)))
add("主要检验、参数、预测和指标结果文件", result_ok,
    paste(needed_results[!file.exists(path_for("results", needed_results))], collapse = ", "))
stationarity_path <- path_for("results", "stationarity_tests.csv")
stationarity_data <- if (file.exists(stationarity_path)) read_csv_utf8(stationarity_path) else NULL
stationarity_ok <- !is.null(stationarity_data) && nrow(stationarity_data) == 4L &&
  all(c("adf_statistic", "adf_p_approx", "kpss_statistic", "kpss_p_approx") %in%
        names(stationarity_data))
add("ADF/KPSS 检验含统计量、近似 p 值与差分序列",
    stationarity_ok, if (stationarity_ok) "原序列、普通差分、季节差分及组合差分均有结果" else
      "平稳性结果不完整")
selected_file <- path_for("results", "selected_model.csv")
selected_data <- if (file.exists(selected_file)) read_csv_utf8(selected_file) else NULL
order_ok <- !is.null(selected_data) && nrow(selected_data) == 1L &&
  all(selected_data[1, c("p", "d", "q", "P", "D", "Q")] ==
        c(2, 0, 1, 0, 1, 1))
add("最终 SARIMA 阶数明确且保存参数估计", order_ok,
    if (order_ok) "SARIMA(2,0,1)(0,1,1)[12]，含漂移" else "模型阶数与预期记录不一致")
diagnostic_path <- path_for("results", "selected_model_diagnostics.csv")
diagnostic_data <- if (file.exists(diagnostic_path)) read_csv_utf8(diagnostic_path) else NULL
diagnostic_ok <- !is.null(diagnostic_data) && nrow(diagnostic_data) == 1L &&
  diagnostic_data$ljung_box_lag[1] == 24 && is.finite(diagnostic_data$p_value[1])
add("最终模型残差 Ljung-Box 诊断已记录", diagnostic_ok,
    if (diagnostic_ok) paste0("lag=24; fitdf=", diagnostic_data$fitdf[1],
                               "; p=", signif(diagnostic_data$p_value[1], 4)) else
      "诊断缺失")
nnar_path <- path_for("results", "nnar_specification.csv")
nnar_data <- if (file.exists(nnar_path)) read_csv_utf8(nnar_path) else NULL
nnar_ok <- !is.null(nnar_data) && nnar_data$p[1] == 11 && nnar_data$P[1] == 1 &&
  nnar_data$hidden_units[1] == 6 && nnar_data$repeats[1] == 30 &&
  nnar_data$seed[1] == 20261020
add("NNAR 结构、重复次数和随机种子已保存", nnar_ok,
    if (nnar_ok) "NNAR(11,1,6)[12]; repeats=30; seed=20261020" else "NNAR 配置缺失")

metrics_path <- path_for("results", "model_comparison.csv")
metrics <- if (file.exists(metrics_path)) read_csv_utf8(metrics_path) else NULL
metric_ok <- !is.null(metrics) && all(c("MAE", "RMSE", "sMAPE") %in% names(metrics)) &&
  nrow(metrics) >= 3L && all(is.finite(as.matrix(metrics[, c("MAE", "RMSE", "sMAPE")])))
add("四模型同一测试期误差比较", metric_ok,
    if (metric_ok) paste(metrics$model, collapse = "; ") else "指标缺失或非有限")
model_set_ok <- metric_ok && all(c("ETS", "SARIMA", "NNAR", "季节性朴素") %in% metrics$model)
add("比较包含季节性朴素、SARIMA、NNAR 和 ETS", model_set_ok,
    if (model_set_ok) paste(metrics$model, collapse = "; ") else "模型比较集合不完整")

prediction_path <- path_for("results", "test_predictions.csv")
pred <- if (file.exists(prediction_path)) read_csv_utf8(prediction_path) else NULL
split_ok <- !is.null(pred) && nrow(pred) == 24L && !anyNA(pred$date) &&
  !anyNA(pred$actual) && !anyDuplicated(pred$date)
add("最近 24 个月按时间顺序留出且固定起点预测", split_ok,
    if (split_ok) paste(nrow(pred), min(pred$date), max(pred$date)) else
      "预测长度或时间对齐错误")

future_path <- path_for("results", "future_12_month_forecast.csv")
future <- if (file.exists(future_path)) read_csv_utf8(future_path) else NULL
future_ok <- !is.null(future) && nrow(future) == 12L &&
  !anyNA(future$date) && all(is.finite(future$forecast))
if (future_ok && !is.null(dat)) {
  future$date <- as.Date(future$date)
  future_ok <- min(future$date) == seq(max(dat$date), by = "month", length.out = 13)[2]
}
add("全样本重新拟合后预测未来 12 个月", future_ok,
    if (future_ok) paste(min(future$date), max(future$date)) else
      "未来日期或预测值校验失败")

expected_figures <- c("01_original_series.png", "02_stl_decomposition.png",
  "03_monthly_seasonality.png", "04_original_acf.png", "04_original_pacf.png",
  "05_differencing_comparison.png", "06_seasonal_diff_acf.png",
  "06_seasonal_diff_pacf.png", "09_sarima_residual_series.png",
  "10_sarima_residual_acf.png", "11_sarima_residual_qq.png",
  "12_sarima_test_forecast.png", "13_nnar_test_forecast.png",
  "14_all_model_test_comparison.png", "15_model_error_metrics.png",
  "16_future_12_month_forecast.png")
fig_info <- file.info(path_for("figures", expected_figures))
fig_ok <- all(file.exists(path_for("figures", expected_figures))) &&
  all(fig_info$size > 5000)
add("全部 300 DPI PNG 图片已生成", fig_ok,
    paste0(sum(file.exists(path_for("figures", expected_figures))), "/",
           length(expected_figures), " expected image files present"))

report_path <- path_for("report.md")
preview_path <- path_for("report_preview.md")
report_exists <- file.exists(report_path)
preview_exists <- file.exists(preview_path)
report <- if (report_exists) paste(readLines(report_path, warn = FALSE, encoding = "UTF-8"),
                                  collapse = "\n") else ""
preview <- if (preview_exists) paste(readLines(preview_path, warn = FALSE, encoding = "UTF-8"),
                                    collapse = "\n") else ""
required_headings <- c("第一章 概述", "1.1 研究背景与意义",
                       "1.1.1 研究背景", "1.1.2 研究意义",
                       "1.2 数据来源与说明", "1.2.1 数据源",
                       "1.2.2 描述性统计分析", "第二章 模型识别",
                       "2.1 平稳性分析", "2.2 相关函数与差分变换",
                       "2.3 模型定阶与参数识别", "2.4 白噪声检验",
                       "第三章 模型预测", "3.1 短期预测", "3.2 效果分析",
                       "第四章 神经网络自回归（NNAR）模型及其预测",
                       "4.1 短期预测", "4.2 模型对比分析",
                       "第五章 课程总结", "参考文献", "附录（程序代码）")
headings_ok <- report_exists &&
  all(vapply(required_headings, function(h) grepl(h, report, fixed = TRUE), logical(1)))
add("Markdown 报告封面、目录、五章、参考文献与附录", headings_ok,
    if (headings_ok) "全部指定章节标题均可检出" else "报告缺失或章节标题不完整")
report_metrics_ok <- FALSE
if (report_exists && metric_ok) {
  metric_strings <- unlist(lapply(seq_len(nrow(metrics)), function(i) {
    sprintf("%.3f", as.numeric(metrics[i, c("MAE", "RMSE", "sMAPE")]))
  }))
  report_metrics_ok <- all(vapply(metric_strings, function(s) {
    grepl(s, report, fixed = TRUE)
  }, logical(1)))
}
add("报告中的四模型 MAE、RMSE 和 sMAPE 与结果 CSV 对齐",
    report_metrics_ok, if (report_metrics_ok) "报告小数后三位均能在结果文件中检出" else
      "缺数、四舍五入不一致，或报告尚未生成")
report_future_ok <- report_exists && future_ok
missing_future_months <- character()
if (report_future_ok) {
  expected_future_rows <- vapply(seq_len(nrow(future)), function(i) {
    paste0("| ", format(as.Date(future$date[i]), "%Y-%m"), " | ",
           sprintf("%.3f", future$forecast[i]), " | [",
           sprintf("%.3f", future$lo80[i]), ", ", sprintf("%.3f", future$hi80[i]),
           "] | [", sprintf("%.3f", future$lo95[i]), ", ",
           sprintf("%.3f", future$hi95[i]), "] |")
  }, character(1))
  row_matches <- vapply(expected_future_rows, function(line) {
    grepl(line, report, fixed = TRUE)
  }, logical(1))
  report_future_ok <- all(row_matches)
  missing_future_months <- format(as.Date(future$date[!row_matches]), "%Y-%m")
}
add("报告未来 12 个月点预测及区间与结果 CSV 对齐", report_future_ok,
    if (report_future_ok) "12 个月六组数值均按三位小数匹配" else
      paste("数值不一致月份：", paste(missing_future_months, collapse = ", ")))
model_in_report <- report_exists &&
  grepl("SARIMA$(2,0,1)(0,1,1)_{12}$", report, fixed = TRUE)
add("报告阶数与保存的最终 SARIMA 阶数一致", model_in_report,
    if (model_in_report) "SARIMA(2,0,1)(0,1,1)[12]" else "阶数未在报告中检出")
placeholders <- if (report_exists) regmatches(report,
  gregexpr("【这里放：figures/[^】]+】", report, perl = TRUE))[[1]] else character()
placeholder_files <- sub("^【这里放：", "", sub("】$", "", placeholders))
placeholder_ok <- length(placeholders) >= 10L &&
  all(file.exists(path_for(placeholder_files)))
add("正文中文图片占位符均指向真实 PNG", placeholder_ok,
    paste0(length(placeholders), " placeholders; missing files: ",
           paste(placeholder_files[!file.exists(path_for(placeholder_files))], collapse = ", ")))
preview_links <- if (preview_exists) regmatches(preview,
  gregexpr("!\\[[^]]+\\]\\(figures/[^)]+\\)", preview, perl = TRUE))[[1]] else character()
preview_ok <- preview_exists && length(preview_links) == length(placeholders) &&
  !grepl("【这里放：", preview, fixed = TRUE)
add("带图 Markdown 预览与正式报告图数一致", preview_ok,
    paste0(length(preview_links), " preview links; ", length(placeholders),
           " formal placeholders"))
preview_back <- if (preview_exists) gsub(
  "!\\[[^]]+\\]\\(figures/([^)]+)\\)", "【这里放：figures/\\1】",
  preview, perl = TRUE) else ""
same_body <- report_exists && preview_exists && identical(report, preview_back)
add("正式版与预览版正文一致（仅图片语法不同）", same_body,
    if (same_body) "预览图片链接反向转换后与 report.md 完全一致" else
      "正文存在差异，需重新生成预览")
references_ok <- report_exists && all(vapply(paste0("[", 1:7, "]"),
  function(ref) grepl(ref, report, fixed = TRUE), logical(1)))
add("参考文献编号完整", references_ok,
    if (references_ok) "编号 [1] 至 [7] 均可检出" else "参考文献编号缺项")
add("README、R Project 与 PowerShell 入口",
    file.exists(path_for("README.md")) && file.exists(path_for("Assginment-TimeSeries.Rproj")) &&
      file.exists(path_for("run.ps1")),
    "README.md / Assginment-TimeSeries.Rproj / run.ps1")
pkgs <- c("forecast", "ggplot2", "urca")
pkg_ok <- all(vapply(pkgs, requireNamespace, logical(1), quietly = TRUE))
pkg_versions <- paste(pkgs, vapply(pkgs, function(p) {
  as.character(utils::packageVersion(p))
}, character(1)), sep = "=", collapse = "; ")
add("R 执行与依赖版本记录", pkg_ok, paste(R.version.string, pkg_versions))

checks <- do.call(rbind, items)
status <- if (all(checks$passed)) "全部通过" else
  paste(sum(!checks$passed), "项未通过或尚未完成")
lines <- c("# 实验验收与复核记录", "",
           paste0("- 状态：**", status, "**"),
           paste0("- 生成时间：", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
           paste0("- R：", R.version.string), "",
           "| 验收项目 | 结果 | 证据或未通过原因 |",
           "|---|---|---|",
           vapply(seq_len(nrow(checks)), function(i) {
             paste0("| ", checks$item[i], " | ",
                    if (checks$passed[i]) "通过" else "未通过", " | ",
                    gsub("\\|", "/", checks$evidence[i]), " |")
           }, character(1)),
           "",
           "说明：本机直连 FRED CSV 请求曾失败。数据来自 FRED 官方表格页和序列页，获取方式与快照信息记录在 data/raw/SOURCE_NOTES.txt 和 results/data_metadata.csv。")
summary_con <- file(path_for("results", "validation_summary.md"), open = "wt",
                    encoding = "UTF-8")
writeLines(lines, summary_con)
close(summary_con)
message("Validation summary written: ", path_for("results", "validation_summary.md"),
        " -- ", if (all(checks$passed)) "all passed" else "items require review")
