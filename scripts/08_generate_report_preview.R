report_path <- path_for("report.md")
preview_path <- path_for("report_preview.md")
if (!file.exists(report_path)) {
  message("report.md is not present; report-dependent generation is skipped.")
} else {
  read_result <- function(name) {
    file <- path_for("results", name)
    if (!file.exists(file)) stop("Required report input is missing: ", file)
    utils::read.csv(file, fileEncoding = "UTF-8", stringsAsFactors = FALSE,
                    check.names = FALSE)
  }
  fmt <- function(x, digits = 3L) {
    ifelse(is.finite(as.numeric(x)), sprintf(paste0("%.", digits, "f"), as.numeric(x)), "—")
  }
  fmt_p <- function(x) {
    x <- as.numeric(x)
    ifelse(!is.finite(x), "—", ifelse(x < .001, "<0.001", sprintf("%.4f", x)))
  }
  month <- function(x) format(as.Date(x), "%Y-%m")
  render_table <- function(headers, rows, caption) {
    rows <- as.matrix(rows)
    c(paste0("| ", paste(headers, collapse = " | "), " |"),
      paste0("|", paste(rep("---", length(headers)), collapse = "|"), "|"),
      if (nrow(rows)) apply(rows, 1L, function(row) paste0("| ", paste(row, collapse = " | "), " |")),
      "", paste0("**", caption, "**"))
  }
  replace_block <- function(lines, begin, end, replacement) {
    first <- which(lines == begin)
    last <- which(lines == end)
    if (length(first) != 1L || length(last) != 1L || first >= last) {
      stop("Report template marker is missing or duplicated: ", begin)
    }
    c(lines[seq_len(first)], replacement, lines[last:length(lines)])
  }

  config <- .experiment_config
  descriptive <- read_result("descriptive_statistics.csv")
  stationarity <- read_result("stationarity_tests.csv")
  candidates <- read_result("candidate_models.csv")
  selected <- read_result("selected_model.csv")
  parameters <- read_result("selected_model_parameters.csv")
  diagnostics <- read_result("selected_model_diagnostics.csv")
  metrics <- read_result("model_comparison.csv")
  coverage <- read_result("prediction_interval_coverage.csv")
  predictions <- read_result("future_12_month_forecast.csv")
  nnar <- read_result("nnar_specification.csv")

  selected_order <- as.integer(unlist(selected[1, c("p", "d", "q", "P", "D", "Q")],
                                      use.names = FALSE))
  selected_latex <- sprintf("SARIMA$(%d,%d,%d)(%d,%d,%d)_{%d}$",
                            selected_order[1], selected_order[2], selected_order[3],
                            selected_order[4], selected_order[5], selected_order[6],
                            config$season_period)
  selected_plain <- sprintf("SARIMA(%d,%d,%d)(%d,%d,%d)[%d]",
                            selected_order[1], selected_order[2], selected_order[3],
                            selected_order[4], selected_order[5], selected_order[6],
                            config$season_period)
  sarima_label <- selected_latex

  report_lines <- readLines(report_path, warn = FALSE, encoding = "UTF-8")
  report_lines <- gsub("SARIMA\\$\\([0-9,]+\\)\\([0-9,]+\\)_\\{[0-9]+\\}\\$",
                       selected_latex, report_lines, perl = TRUE)
  report_lines <- gsub("SARIMA\\([0-9,]+\\)\\([0-9,]+\\)\\[[0-9]+\\]",
                       selected_plain, report_lines, perl = TRUE)

  stat_keys <- c(
    "观测数" = "n", "起止月份" = NA_character_, "缺失值" = "missing",
    "均值" = "mean", "标准差" = "sd", "中位数" = "median", "最小值" = "min",
    "第一四分位数" = "q1", "第三四分位数" = "q3", "最大值" = "max"
  )
  stat_rows <- lapply(names(stat_keys), function(label) {
    key <- unname(stat_keys[[label]])
    value <- if (label == "起止月份") paste0(month(descriptive$start[1]), " 至 ",
                                               month(descriptive$end[1])) else if (key %in% c("n", "missing")) {
      as.character(as.integer(descriptive[[key]][1]))
    } else fmt(descriptive[[key]][1])
    c(label, value)
  })
  report_lines <- replace_block(report_lines, "<!-- AUTO_TABLE_1_1_BEGIN -->",
    "<!-- AUTO_TABLE_1_1_END -->",
    render_table(c("统计量", "数值"), do.call(rbind, stat_rows), "表 1-1 完整样本描述性统计"))

  stationarity_rows <- do.call(rbind, lapply(seq_len(nrow(stationarity)), function(i) {
    row <- stationarity[i, ]
    c(row$series, as.character(row$n), fmt(row$adf_statistic), fmt(row$adf_cv_5pct),
      row$adf_p_approx, fmt(row$kpss_statistic), fmt(row$kpss_cv_5pct), row$kpss_p_approx)
  }))
  report_lines <- replace_block(report_lines, "<!-- AUTO_TABLE_2_1_BEGIN -->",
    "<!-- AUTO_TABLE_2_1_END -->",
    render_table(c("序列", "n", "ADF 统计量", "ADF 5% 临界值", "ADF 近似 p 值",
                   "KPSS 统计量", "KPSS 5% 临界值", "KPSS 近似 p 值"),
                 stationarity_rows, "表 2-1 训练段平稳性检验结果"))

  candidate_order <- order(candidates$AICc)
  selected_candidate <- which(candidates$name == selected$selected_name[1])
  candidate_ids <- unique(c(candidate_order[seq_len(min(5L, length(candidate_order)))],
                            selected_candidate))
  candidate_rows <- do.call(rbind, lapply(candidate_ids, function(i) {
    item <- candidates[i, ]
    label <- if (identical(item$name, selected$selected_name[1])) {
      label <- paste0(selected_latex, if ("drift" %in% parameters$parameter) "，含漂移" else "",
                      "（auto.arima）")
    } else {
      sprintf("SARIMA$(%d,%d,%d)(%d,%d,%d)_{%d}$", item$p, item$d, item$q,
              item$P, item$D, item$Q, config$season_period)
    }
    c(label, fmt(item$AIC), fmt(item$AICc), fmt(item$BIC), fmt_p(item$ljung_box_p_value))
  }))
  report_lines <- replace_block(report_lines, "<!-- AUTO_TABLE_2_2_BEGIN -->",
    "<!-- AUTO_TABLE_2_2_END -->",
    render_table(c("候选模型", "AIC", "AICc", "BIC", "Ljung-Box p 值"),
                 candidate_rows, "表 2-2 主要 SARIMA 候选模型比较（训练段）"))

  parameter_rows <- do.call(rbind, lapply(seq_len(nrow(parameters)), function(i) {
    item <- parameters[i, ]
    c(item$parameter, fmt(item$estimate, 4L), fmt(item$std_error, 4L), fmt_p(item$p_value))
  }))
  report_lines <- replace_block(report_lines, "<!-- AUTO_TABLE_2_3_BEGIN -->",
    "<!-- AUTO_TABLE_2_3_END -->",
    render_table(c("参数", "估计值", "标准误", "近似 p 值"), parameter_rows,
                 "表 2-3 最终 SARIMA 参数估计"))

  format_metric_row <- function(model) {
    item <- metrics[metrics$model == model, , drop = FALSE]
    if (nrow(item) != 1L) stop("Expected one metric row for model: ", model)
    c(if (model == "SARIMA") sarima_label else model,
      fmt(item$MAE), fmt(item$RMSE), fmt(item$sMAPE), fmt(item$MASE))
  }
  baseline_rows <- rbind(format_metric_row("SARIMA"), format_metric_row("季节性朴素"))
  report_lines <- replace_block(report_lines, "<!-- AUTO_TABLE_3_1_BEGIN -->",
    "<!-- AUTO_TABLE_3_1_END -->",
    render_table(c("模型", "MAE", "RMSE", "sMAPE（%）", "MASE"), baseline_rows,
                 "表 3-1 SARIMA 与季节性朴素模型测试集预测误差比较"))

  coverage_rows <- do.call(rbind, lapply(seq_len(nrow(coverage)), function(i) {
    item <- coverage[i, ]
    c(paste0(fmt(100 * item$nominal_coverage, 0L), "%"),
      paste0(as.integer(item$covered_n), " / ", as.integer(item$test_n)),
      as.character(as.integer(item$test_n)), paste0(fmt(100 * item$actual_coverage, 2L), "%"),
      fmt(item$mean_width))
  }))
  report_lines <- replace_block(report_lines, "<!-- AUTO_TABLE_3_2_BEGIN -->",
    "<!-- AUTO_TABLE_3_2_END -->",
    render_table(c("名义覆盖率", "覆盖数", "测试样本数", "实际覆盖率", "平均区间宽度"),
                 coverage_rows, "表 3-2 SARIMA 测试集预测区间实际覆盖率"))

  nnar_rows <- rbind(
    c("NNAR 结构", paste0("NNAR$", sprintf("(%d,%d,%d)_{%d}$", nnar$p[1], nnar$P[1],
                                             nnar$hidden_units[1], config$season_period))),
    c("非季节滞后阶数 $p$", as.character(nnar$p[1])),
    c("季节滞后阶数 $P$", as.character(nnar$P[1])),
    c("隐层节点数 $k$", as.character(nnar$hidden_units[1])),
    c("repeats", as.character(nnar$repeats[1])),
    c("随机种子", as.character(nnar$seed[1])),
    c("训练样本数", as.character(nnar$training_n[1])),
    c("测试样本数", as.character(nnar$test_n[1]))
  )
  report_lines <- replace_block(report_lines, "<!-- AUTO_TABLE_4_1_BEGIN -->",
    "<!-- AUTO_TABLE_4_1_END -->",
    render_table(c("配置项", "实际设置"), nnar_rows, "表 4-1 NNAR 模型结构与训练设置"))

  all_metric_rows <- do.call(rbind, lapply(seq_len(nrow(metrics)), function(i) {
    item <- metrics[i, ]
    c(if (item$model == "SARIMA") sarima_label else item$model,
      fmt(item$MAE), fmt(item$RMSE), fmt(item$sMAPE), fmt(item$MASE))
  }))
  report_lines <- replace_block(report_lines, "<!-- AUTO_TABLE_4_2_BEGIN -->",
    "<!-- AUTO_TABLE_4_2_END -->",
    render_table(c("模型", "MAE", "RMSE", "sMAPE（%）", "MASE"), all_metric_rows,
                 "表 4-2 相同测试期多模型预测误差比较"))

  future_rows <- do.call(rbind, lapply(seq_len(nrow(predictions)), function(i) {
    item <- predictions[i, ]
    c(month(item$date), fmt(item$forecast),
      paste0("[", fmt(item$lo80), ", ", fmt(item$hi80), "]"),
      paste0("[", fmt(item$lo95), ", ", fmt(item$hi95), "]"))
  }))
  report_lines <- replace_block(report_lines, "<!-- AUTO_TABLE_4_3_BEGIN -->",
    "<!-- AUTO_TABLE_4_3_END -->",
    render_table(c("预测月份", "点预测", "80% 区间", "95% 区间"), future_rows,
                 paste0("表 4-3 全窗口重估后的未来 ", nrow(predictions), " 个月预测")))

  # 将报告中的结果判断按 CSV 指标重新生成，避免重新运行后文本排名失配。
  baseline <- metrics[metrics$model == "季节性朴素", , drop = FALSE]
  sarima <- metrics[metrics$model == "SARIMA", , drop = FALSE]
  nnar_metric <- metrics[metrics$model == "NNAR", , drop = FALSE]
  metric_columns <- c("MAE", "RMSE", "sMAPE", "MASE")
  metric_names <- c("MAE", "RMSE", "sMAPE", "MASE")
  relative_clause <- function(x, y, x_name, y_name) {
    x_values <- as.numeric(unlist(x[1, metric_columns], use.names = FALSE))
    y_values <- as.numeric(unlist(y[1, metric_columns], use.names = FALSE))
    lower <- x_values < y_values
    higher <- x_values > y_values
    if (all(lower)) return(paste0(x_name, " 的四项误差均低于", y_name))
    if (all(higher)) return(paste0(x_name, " 的四项误差均高于", y_name))
    detail <- vapply(seq_along(metric_columns), function(j) {
      direction <- if (lower[j]) "低于" else if (higher[j]) "高于" else "等于"
      paste0(metric_names[j], " ", direction, "（", fmt(x_values[j]),
             " 对比 ", fmt(y_values[j]), "）")
    }, character(1))
    paste0(x_name, " 相对", y_name, "：", paste(detail, collapse = "；"))
  }
  best_models <- vapply(metric_columns, function(column) {
    metrics$model[which.min(metrics[[column]])][1]
  }, character(1))
  if (length(unique(best_models)) == 1L) {
    best_text <- paste0(best_models[1], " 在四项误差指标上均最低")
  } else {
    best_text <- paste0(paste(metric_names, best_models, sep = " 对应最低值为 ", collapse = "；"), "")
  }
  nnar_relative <- relative_clause(nnar_metric, baseline, "NNAR", "季节性朴素")
  if (as.numeric(nnar_metric$RMSE) < as.numeric(baseline$RMSE) &&
      (as.numeric(baseline$RMSE) - as.numeric(nnar_metric$RMSE)) /
        as.numeric(baseline$RMSE) < .01) {
    nnar_relative <- paste0(nnar_relative, "，其中 RMSE 差异较小（",
                            fmt(nnar_metric$RMSE), " 对比 ", fmt(baseline$RMSE), "）")
  } else {
    nnar_relative <- paste0(nnar_relative, "（RMSE ", fmt(nnar_metric$RMSE),
                            " 对比 ", fmt(baseline$RMSE), "）")
  }
  sarima_relative <- relative_clause(sarima, baseline, "SARIMA", "季节性朴素")
  comparison_core <- paste0(best_text, "；", sarima_relative, "；", nnar_relative,
    "。这些排序只描述本次独立测试段，不能据此推断模型在其他时段普遍最优。")
  report_lines <- replace_block(report_lines, "<!-- AUTO_ABSTRACT_COMPARISON_BEGIN -->",
    "<!-- AUTO_ABSTRACT_COMPARISON_END -->", comparison_core)
  report_lines <- replace_block(report_lines, "<!-- AUTO_SARIMA_BASELINE_SUMMARY_BEGIN -->",
    "<!-- AUTO_SARIMA_BASELINE_SUMMARY_END -->",
    paste0(sarima_relative, "，仅说明这段测试样本表现，不构成总体显著性检验，",
           "也不保证未来时期保持相同差距。"))
  report_lines <- replace_block(report_lines, "<!-- AUTO_MODEL_RANKING_SUMMARY_BEGIN -->",
    "<!-- AUTO_MODEL_RANKING_SUMMARY_END -->",
    paste0(comparison_core, " RMSE 对较大误差更敏感，少数未捕捉的高峰会影响排序。"))
  report_lines <- replace_block(report_lines, "<!-- AUTO_CONCLUSION_COMPARISON_BEGIN -->",
    "<!-- AUTO_CONCLUSION_COMPARISON_END -->",
    paste0(comparison_core, " 单次测试不能替代多起点验证，也不支持对某类模型作普遍排名。"))

  parameter_value <- function(name) {
    value <- parameters$estimate[parameters$parameter == name]
    if (length(value)) value[1] else 0
  }
  polynomial <- function(prefix, order, sign_factor, seasonal = FALSE) {
    terms <- "1"
    if (order > 0L) for (lag in seq_len(order)) {
      value <- parameter_value(paste0(prefix, lag))
      signed <- sign_factor * value
      exponent <- if (seasonal) lag * config$season_period else lag
      variable <- if (exponent == 1L) "B" else paste0("B^{", exponent, "}")
      terms <- paste0(terms, if (signed < 0) " - " else " + ", fmt(abs(signed), 4L), variable)
    }
    terms
  }
  drift <- parameters$estimate[parameters$parameter == "drift"]
  intercept <- parameters$estimate[parameters$parameter == "intercept"]
  trend_line <- if (length(drift)) {
    paste0("X_t = ", fmt(abs(drift[1]), 4L), "t ", if (drift[1] < 0) "-" else "+", " U_t,")
  } else if (length(intercept)) {
    paste0("X_t = ", fmt(abs(intercept[1]), 4L), " ", if (intercept[1] < 0) "-" else "+", " U_t,")
  } else "X_t = U_t,"
  differencing <- c(if (selected_order[2] > 0L) paste0("(1-B)^{", selected_order[2], "}") else NULL,
                    if (selected_order[5] > 0L) paste0("(1-B^{", config$season_period,
                                                       "})^{", selected_order[5], "}") else NULL)
  lhs <- paste0("(", polynomial("ar", selected_order[1], -1), ")(",
                polynomial("sar", selected_order[4], -1, TRUE), ")",
                if (length(differencing)) paste0(paste(differencing, collapse = "")) else "",
                "U_t")
  rhs <- paste0("(", polynomial("ma", selected_order[3], 1), ")(",
                polynomial("sma", selected_order[6], 1, TRUE), ")\\varepsilon_t .")
  equation_lines <- c("$$", trend_line, "$$", "$$", paste0(lhs, " = ", rhs), "$$",
    paste0("上述多项式系数为训练段 SARIMA 参数的最大似然估计；漂移项单独写作趋势部分，",
           "移动平均多项式符号遵循 forecast::Arima 的参数约定。该方程描述统计动态结构，",
           "不代表因果关系或实际产出单位变化。"))
  report_lines <- replace_block(report_lines, "<!-- AUTO_MODEL_EQUATION_BEGIN -->",
    "<!-- AUTO_MODEL_EQUATION_END -->", equation_lines)

  report_lines <- enc2utf8(report_lines)
  writeLines(report_lines, report_path, useBytes = TRUE)

  image_lines <- grep("^【这里放：figures/[^】]+】$", report_lines, value = TRUE)
  preview_lines <- report_lines
  for (placeholder in image_lines) {
    file <- sub("^【这里放：figures/([^】]+)】$", "\\1", placeholder)
    idx <- match(placeholder, preview_lines)
    prior <- if (idx > 1L) preview_lines[idx - 1L] else ""
    caption <- sub("^\\*\\*([^*]+)\\*\\*$", "\\1", prior)
    if (!nzchar(caption) || identical(caption, prior)) caption <- file
    preview_lines[idx] <- paste0("![", caption, "](figures/", file, ")")
  }
  writeLines(enc2utf8(preview_lines), preview_path, useBytes = TRUE)
  message("Updated report tables and result-dependent conclusions from current CSV outputs; ",
          "generated report_preview.md (", length(image_lines), " image links).")
}
