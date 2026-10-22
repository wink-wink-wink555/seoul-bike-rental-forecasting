dat <- utils::read.csv(path_for("data", "processed", "IPG2211A2N_clean.csv"),
                       stringsAsFactors = FALSE)
dat$date <- as.Date(dat$date)
h <- 24L
train_n <- nrow(dat) - h
train <- dat[seq_len(train_n), ]
train_ts <- stats::ts(train$value, start = c(train$year[1], train$month[1]), frequency = 12)
assign(".training_ts", train_ts, envir = .GlobalEnv)
assign(".training_data", train, envir = .GlobalEnv)
assign(".test_data", dat[(train_n + 1L):nrow(dat), ], envir = .GlobalEnv)

critical_p_label <- function(stat, critical, tail = c("left", "right")) {
  tail <- match.arg(tail)
  cv <- as.numeric(critical[c("1pct", "5pct", "10pct")])
  if (tail == "left") {
    if (stat <= cv[1]) return("<0.01")
    if (stat <= cv[2]) return(sprintf("约%.3f", .01 + .04 * (cv[1] - stat) / (cv[1] - cv[2])))
    if (stat <= cv[3]) return(sprintf("约%.3f", .05 + .05 * (cv[2] - stat) / (cv[2] - cv[3])))
    return(">0.10")
  }
  if (stat >= cv[1]) return("<0.01")
  if (stat >= cv[2]) return(sprintf("约%.3f", .01 + .04 * (stat - cv[1]) / (cv[2] - cv[1])))
  if (stat >= cv[3]) return(sprintf("约%.3f", .05 + .05 * (stat - cv[2]) / (cv[3] - cv[2])))
  ">0.10"
}
run_tests <- function(x, label, trend = FALSE) {
  x <- as.numeric(stats::na.omit(x))
  adf_type <- if (trend) "trend" else "drift"
  adf <- urca::ur.df(x, type = adf_type,
                     lags = min(12L, floor(length(x) / 10)), selectlags = "AIC")
  adf_stat <- unname(adf@teststat[1, 1])
  adf_cv <- adf@cval[1, ]
  kpss_type <- if (trend) "tau" else "mu"
  kpss <- urca::ur.kpss(x, type = kpss_type, lags = "long")
  kpss_stat <- as.numeric(kpss@teststat[1])
  kpss_cv <- kpss@cval[1, ]
  data.frame(
    series = label, n = length(x), adf_deterministic = adf_type,
    adf_statistic = adf_stat, adf_cv_1pct = adf_cv["1pct"],
    adf_cv_5pct = adf_cv["5pct"], adf_cv_10pct = adf_cv["10pct"],
    adf_p_approx = critical_p_label(adf_stat, adf_cv, "left"),
    adf_decision_5pct = if (adf_stat < adf_cv["5pct"]) "拒绝单位根原假设" else "未拒绝单位根原假设",
    kpss_deterministic = kpss_type, kpss_statistic = kpss_stat,
    kpss_cv_1pct = kpss_cv["1pct"], kpss_cv_5pct = kpss_cv["5pct"],
    kpss_cv_10pct = kpss_cv["10pct"],
    kpss_p_approx = critical_p_label(kpss_stat, kpss_cv, "right"),
    kpss_decision_5pct = if (kpss_stat > kpss_cv["5pct"]) "拒绝平稳性原假设" else "未拒绝平稳性原假设",
    stringsAsFactors = FALSE
  )
}
seasonal_diff <- diff(train_ts, lag = 12, differences = 1)
first_diff <- diff(train_ts, differences = 1)
combined_diff <- diff(seasonal_diff, differences = 1)
stationarity <- rbind(
  run_tests(train_ts, "训练段原序列（含趋势项）", trend = TRUE),
  run_tests(first_diff, "训练段一阶非季节差分"),
  run_tests(seasonal_diff, "训练段一阶季节差分（12阶）"),
  run_tests(combined_diff, "训练段季节差分后再一阶差分")
)
write_csv(stationarity, "results", "stationarity_tests.csv")

acf_frame <- function(x, partial = FALSE, lag_max = 48L) {
  x <- as.numeric(x)
  obj <- if (partial) stats::pacf(x, lag.max = lag_max, plot = FALSE) else
    stats::acf(x, lag.max = lag_max, plot = FALSE)
  data.frame(lag = as.numeric(obj$lag)[-1], correlation = as.numeric(obj$acf)[-1])
}
plot_corr <- function(x, title, file) {
  a <- acf_frame(x, FALSE)
  p <- acf_frame(x, TRUE)
  bound <- 1.96 / sqrt(length(x))
  ac <- ggplot2::ggplot(a, ggplot2::aes(lag, correlation)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#555555") +
    ggplot2::geom_segment(ggplot2::aes(xend = lag, y = 0, yend = correlation),
                          colour = "#176B87") +
    ggplot2::geom_hline(yintercept = c(-bound, bound), linetype = 2,
                        colour = "#C44536") +
    ggplot2::scale_x_continuous(breaks = seq(0, 48, 6)) +
    ggplot2::labs(title = paste0(title, "：ACF"), x = "滞后（月）", y = "自相关")
  pc <- ggplot2::ggplot(p, ggplot2::aes(lag, correlation)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#555555") +
    ggplot2::geom_segment(ggplot2::aes(xend = lag, y = 0, yend = correlation),
                          colour = "#E07A5F") +
    ggplot2::geom_hline(yintercept = c(-bound, bound), linetype = 2,
                        colour = "#C44536") +
    ggplot2::scale_x_continuous(breaks = seq(0, 48, 6)) +
    ggplot2::labs(title = paste0(title, "：PACF"), x = "滞后（月）", y = "偏自相关")
  save_plot(ac, paste0(file, "_acf.png"), 8, 4.6)
  save_plot(pc, paste0(file, "_pacf.png"), 8, 4.6)
}
plot_corr(train_ts, "训练段原序列", "04_original")
plot_corr(seasonal_diff, "训练段季节差分序列", "06_seasonal_diff")
correlation_table <- function(x, label) {
  a <- acf_frame(x, FALSE)
  p <- acf_frame(x, TRUE)
  data.frame(series = label, lag = a$lag[seq_len(24)],
             acf = a$correlation[seq_len(24)],
             pacf = p$correlation[seq_len(24)])
}
correlation_results <- rbind(correlation_table(train_ts, "训练段原序列"),
                             correlation_table(seasonal_diff, "训练段季节差分"))
write_csv(correlation_results, "results", "acf_pacf_identification.csv")

diff_frame <- rbind(
  data.frame(date = train$date, series = "原序列", value = train$value),
  data.frame(date = train$date[-1], series = "一阶非季节差分", value = as.numeric(first_diff)),
  data.frame(date = train$date[-seq_len(12)], series = "一阶季节差分（12阶）",
             value = as.numeric(seasonal_diff))
)
diff_frame$series <- factor(diff_frame$series,
                            levels = c("原序列", "一阶非季节差分", "一阶季节差分（12阶）"))
diff_plot <- ggplot2::ggplot(diff_frame, ggplot2::aes(date, value)) +
  ggplot2::geom_line(linewidth = .45, colour = "#176B87") +
  ggplot2::facet_grid(series ~ ., scales = "free_y") +
  ggplot2::labs(title = "训练段原序列与差分序列", x = "月份", y = "序列值") +
  ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0, face = "bold"))
save_plot(diff_plot, "05_differencing_comparison.png", 10, 7)
message("Stationarity analysis complete. Test period: ", min(.test_data$date),
        " to ", max(.test_data$date), " (", nrow(.test_data), " months).")
