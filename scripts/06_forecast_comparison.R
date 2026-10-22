dat <- utils::read.csv(path_for("data", "processed", "IPG2211A2N_clean.csv"),
                       stringsAsFactors = FALSE)
dat$date <- as.Date(dat$date)
train <- get(".training_data", envir = .GlobalEnv)
test <- get(".test_data", envir = .GlobalEnv)
train_ts <- get(".training_ts", envir = .GlobalEnv)
selected_model <- get(".selected_model", envir = .GlobalEnv)
nnar_fit <- get(".nnar_fit", envir = .GlobalEnv)
h <- nrow(test)
test_ts <- stats::ts(test$value, start = c(test$year[1], test$month[1]), frequency = 12)

fc_snaive <- forecast::snaive(train_ts, h = h)
fc_arima <- forecast::forecast(selected_model, h = h, level = c(80, 95))
fc_nnar <- forecast::forecast(nnar_fit, h = h, PI = FALSE)
fc_ets_fit <- forecast::ets(train_ts)
fc_ets <- forecast::forecast(fc_ets_fit, h = h, level = c(80, 95))

pred <- data.frame(
  date = test$date, actual = test$value,
  seasonal_naive = as.numeric(fc_snaive$mean),
  sarima = as.numeric(fc_arima$mean),
  nnar = as.numeric(fc_nnar$mean),
  ets = as.numeric(fc_ets$mean),
  sarima_lo80 = as.numeric(fc_arima$lower[, "80%"]),
  sarima_hi80 = as.numeric(fc_arima$upper[, "80%"]),
  sarima_lo95 = as.numeric(fc_arima$lower[, "95%"]),
  sarima_hi95 = as.numeric(fc_arima$upper[, "95%"])
)
if (nrow(pred) != 24L || any(!is.finite(as.matrix(pred[, c("actual", "seasonal_naive",
                                                            "sarima", "nnar", "ets")])))) {
  stop("Invalid forecast length, alignment, or non-finite prediction.")
}
write_csv(pred, "results", "test_predictions.csv")

metric_fn <- function(actual, predicted, training) {
  error <- actual - predicted
  denominator <- abs(actual) + abs(predicted)
  smape_term <- ifelse(denominator == 0, ifelse(error == 0, 0, NA_real_),
                       200 * abs(error) / denominator)
  mase_scale <- mean(abs(training[(13):length(training)] -
                           training[1:(length(training) - 12)]))
  c(MAE = mean(abs(error)), RMSE = sqrt(mean(error^2)),
    sMAPE = mean(smape_term, na.rm = TRUE),
    MASE = mean(abs(error)) / mase_scale)
}
pred_columns <- c(seasonal_naive = "季节性朴素", sarima = "SARIMA", nnar = "NNAR", ets = "ETS")
metrics <- do.call(rbind, lapply(names(pred_columns), function(column) {
  vals <- metric_fn(pred$actual, pred[[column]], train$value)
  data.frame(model = unname(pred_columns[[column]]),
             MAE = vals["MAE"], RMSE = vals["RMSE"],
             sMAPE = vals["sMAPE"], MASE = vals["MASE"])
}))
metrics <- metrics[order(metrics$RMSE), ]
write_csv(metrics, "results", "model_comparison.csv")

split_info <- data.frame(
  training_start = min(train$date), training_end = max(train$date), training_n = nrow(train),
  test_start = min(test$date), test_end = max(test$date), test_n = nrow(test),
  split_rule = "Last 24 months held out; fixed-origin multi-step forecast",
  selected_sarima_by = get(".selection_rule", envir = .GlobalEnv),
  nnar_tuning = "One configuration set before test evaluation; no test-based tuning"
)
write_csv(split_info, "results", "split_information.csv")

pred_long <- rbind(
  data.frame(date = pred$date, actual = pred$actual, model = "实际观测", value = pred$actual),
  data.frame(date = pred$date, actual = pred$actual, model = "SARIMA", value = pred$sarima),
  data.frame(date = pred$date, actual = pred$actual, model = "NNAR", value = pred$nnar),
  data.frame(date = pred$date, actual = pred$actual, model = "ETS", value = pred$ets),
  data.frame(date = pred$date, actual = pred$actual, model = "季节性朴素", value = pred$seasonal_naive)
)
pred_long$model <- factor(pred_long$model,
                          levels = c("实际观测", "SARIMA", "NNAR", "ETS", "季节性朴素"))

test_plot <- ggplot2::ggplot(pred, ggplot2::aes(date)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = sarima_lo95, ymax = sarima_hi95),
                       fill = "#B7D6E8", alpha = .32) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = sarima_lo80, ymax = sarima_hi80),
                       fill = "#6CA6C1", alpha = .28) +
  ggplot2::geom_line(ggplot2::aes(y = actual, colour = "实际观测"), linewidth = .7) +
  ggplot2::geom_line(ggplot2::aes(y = sarima, colour = "SARIMA 预测"),
                     linewidth = .7, linetype = 2) +
  ggplot2::scale_colour_manual(values = c("实际观测" = "#173F5F", "SARIMA 预测" = "#E07A5F")) +
  ggplot2::labs(title = "SARIMA 固定起点 24 个月测试集预测",
                subtitle = "阴影表示模型给出的 80% 与 95% 预测区间",
                x = "月份", y = "工业生产指数", colour = "序列")
save_plot(test_plot, "12_sarima_test_forecast.png", 10, 5.5)

nnar_plot <- ggplot2::ggplot(pred, ggplot2::aes(date)) +
  ggplot2::geom_line(ggplot2::aes(y = actual, colour = "实际观测"), linewidth = .7) +
  ggplot2::geom_line(ggplot2::aes(y = nnar, colour = "NNAR 预测"),
                     linewidth = .7, linetype = 2) +
  ggplot2::scale_colour_manual(values = c("实际观测" = "#173F5F", "NNAR 预测" = "#E07A5F")) +
  ggplot2::labs(title = "NNAR 固定起点 24 个月测试集预测",
                x = "月份", y = "工业生产指数", colour = "序列")
save_plot(nnar_plot, "13_nnar_test_forecast.png", 10, 5.5)

overlay <- ggplot2::ggplot(pred_long, ggplot2::aes(date, value, colour = model,
                                                    linetype = model)) +
  ggplot2::geom_line(linewidth = .65) +
  ggplot2::scale_colour_manual(values = c("实际观测" = "#173F5F", "SARIMA" = "#E07A5F",
    "NNAR" = "#3CAEA3", "ETS" = "#8E6C8A", "季节性朴素" = "#F6AE2D")) +
  ggplot2::labs(title = "四种方法在相同测试期的预测对比",
                x = "月份", y = "工业生产指数", colour = "模型", linetype = "模型")
save_plot(overlay, "14_all_model_test_comparison.png", 10, 5.8)

metric_long <- rbind(
  data.frame(model = metrics$model, metric = "MAE", value = metrics$MAE),
  data.frame(model = metrics$model, metric = "RMSE", value = metrics$RMSE),
  data.frame(model = metrics$model, metric = "sMAPE（%）", value = metrics$sMAPE),
  data.frame(model = metrics$model, metric = "MASE", value = metrics$MASE)
)
metric_long$metric <- factor(metric_long$metric,
                             levels = c("MAE", "RMSE", "sMAPE（%）", "MASE"))
metric_plot <- ggplot2::ggplot(metric_long,
  ggplot2::aes(stats::reorder(model, value), value, fill = model)) +
  ggplot2::geom_col(show.legend = FALSE) + ggplot2::coord_flip() +
  ggplot2::facet_wrap(~metric, scales = "free_x") +
  ggplot2::labs(title = "测试集预测误差指标比较", x = "模型", y = "指标值")
save_plot(metric_plot, "15_model_error_metrics.png", 10, 6)

full_ts <- stats::ts(dat$value, start = c(dat$year[1], dat$month[1]), frequency = 12)
selected_order <- forecast::arimaorder(selected_model)
full_fit <- forecast::Arima(
  full_ts, order = selected_order[1:3],
  seasonal = list(order = selected_order[4:6], period = 12),
  include.drift = TRUE, method = "ML"
)
future_fc <- forecast::forecast(full_fit, h = 12, level = c(80, 95))
future_dates <- seq(max(dat$date), by = "month", length.out = 13)[-1]
future <- data.frame(
  date = future_dates, forecast = as.numeric(future_fc$mean),
  lo80 = as.numeric(future_fc$lower[, "80%"]), hi80 = as.numeric(future_fc$upper[, "80%"]),
  lo95 = as.numeric(future_fc$lower[, "95%"]), hi95 = as.numeric(future_fc$upper[, "95%"])
)
if (nrow(future) != 12L || min(future$date) != seq(max(dat$date), by = "month", length.out = 13)[2]) {
  stop("Future forecast dates are not aligned after the last observed month.")
}
write_csv(future, "results", "future_12_month_forecast.csv")
saveRDS(full_fit, path_for("results", "selected_sarima_full_sample_fit.rds"))
capture.output(print(full_fit), file = path_for("results", "selected_sarima_full_sample_summary.txt"))
history_start <- seq(max(dat$date), by = "-1 month", length.out = 49)[49]
history <- dat[dat$date >= history_start, ]
future_plot <- ggplot2::ggplot() +
  ggplot2::geom_ribbon(data = future, ggplot2::aes(date, ymin = lo95, ymax = hi95),
                       fill = "#B7D6E8", alpha = .32) +
  ggplot2::geom_ribbon(data = future, ggplot2::aes(date, ymin = lo80, ymax = hi80),
                       fill = "#6CA6C1", alpha = .28) +
  ggplot2::geom_line(data = history, ggplot2::aes(date, value), colour = "#173F5F",
                     linewidth = .75) +
  ggplot2::geom_line(data = future, ggplot2::aes(date, forecast), colour = "#E07A5F",
                     linewidth = .75, linetype = 2) +
  ggplot2::geom_vline(xintercept = max(dat$date), linetype = 3,
                      colour = "#555555") +
  ggplot2::labs(title = "全样本重估后的未来 12 个月预测",
                subtitle = "虚线右侧为尚未观测的预测期；阴影为 80% 与 95% 预测区间",
                x = "月份", y = "工业生产指数")
save_plot(future_plot, "16_future_12_month_forecast.png", 10, 5.7)
message("Forecasts complete. Held-out test metrics saved; selected SARIMA refitted to all ",
        length(full_ts), " observations for the future forecast.")
