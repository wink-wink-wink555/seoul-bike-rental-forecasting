dat <- utils::read.csv(path_for("data", "processed", "IPG2211A2N_clean.csv"),
                       stringsAsFactors = FALSE)
dat$date <- as.Date(dat$date)
dat$month_name <- factor(month.abb[dat$month], levels = month.abb)
y <- dat$value
stats_row <- data.frame(
  n = length(y), start = as.character(min(dat$date)), end = as.character(max(dat$date)),
  missing = sum(is.na(y)), mean = mean(y), sd = stats::sd(y), median = stats::median(y),
  min = min(y), q1 = unname(stats::quantile(y, .25)),
  q3 = unname(stats::quantile(y, .75)), max = max(y)
)
write_csv(stats_row, "results", "descriptive_statistics.csv")
month_summary <- do.call(rbind, lapply(1:12, function(m) {
  z <- y[dat$month == m]
  data.frame(month = m, n = length(z), mean = mean(z), sd = stats::sd(z),
             median = stats::median(z), min = min(z), max = max(z))
}))
month_summary$month_name <- factor(month.abb[month_summary$month], levels = month.abb)
write_csv(month_summary, "results", "monthly_seasonality_summary.csv")

p1 <- ggplot2::ggplot(dat, ggplot2::aes(date, value)) +
  ggplot2::geom_line(linewidth = .65, colour = "#176B87") +
  ggplot2::labs(title = "美国电力与燃气公用事业工业生产指数",
                subtitle = "月度、未经季节性调整，基期 2017=100",
                x = "月份", y = "工业生产指数")
save_plot(p1, "01_original_series.png", 10, 5.3)

p2 <- ggplot2::ggplot(dat, ggplot2::aes(month_name, value)) +
  ggplot2::geom_boxplot(fill = "#A8DADC", colour = "#176B87",
                        outlier.alpha = .55, linewidth = .45) +
  ggplot2::labs(title = "工业生产指数的月度季节性分布",
                x = "月份", y = "指数值")
save_plot(p2, "03_monthly_seasonality.png", 9, 5.3)

series <- stats::ts(y, start = c(dat$year[1], dat$month[1]), frequency = 12)
stl_fit <- stats::stl(series, s.window = "periodic", robust = TRUE)
stl_df <- data.frame(date = dat$date, value = y,
                     seasonal = as.numeric(stl_fit$time.series[, "seasonal"]),
                     trend = as.numeric(stl_fit$time.series[, "trend"]),
                     remainder = as.numeric(stl_fit$time.series[, "remainder"]))
write_csv(stl_df, "results", "stl_components.csv")
stl_long <- rbind(
  data.frame(date = stl_df$date, component = "原序列", component_value = stl_df$value),
  data.frame(date = stl_df$date, component = "趋势", component_value = stl_df$trend),
  data.frame(date = stl_df$date, component = "季节项", component_value = stl_df$seasonal),
  data.frame(date = stl_df$date, component = "余项", component_value = stl_df$remainder)
)
stl_long$component <- factor(stl_long$component,
                             levels = c("原序列", "趋势", "季节项", "余项"))
p3 <- ggplot2::ggplot(stl_long, ggplot2::aes(date, component_value)) +
  ggplot2::geom_line(linewidth = .45, colour = "#176B87") +
  ggplot2::facet_grid(component ~ ., scales = "free_y") +
  ggplot2::labs(title = "STL 趋势与季节性分解", x = "月份", y = "分解分量") +
  ggplot2::theme(legend.position = "none",
                 strip.text.y = ggplot2::element_text(angle = 0, face = "bold"))
save_plot(p3, "02_stl_decomposition.png", 10, 7.2)
message("Exploratory analysis complete.")
