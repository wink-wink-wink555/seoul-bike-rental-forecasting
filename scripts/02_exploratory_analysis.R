d <- hourly(); train <- d[d$date <= config$initial_end, ]
desc <- do.call(rbind, lapply(c("all", "operating", "initial_training"), function(subset) {
  z <- if (subset == "all") d$count else if (subset == "operating") d$count[d$operating == 1] else train$count
  data.frame(subset = subset, n = length(z), mean = mean(z), sd = sd(z), min = min(z),
    q1 = unname(quantile(z, .25)), median = median(z), q3 = unname(quantile(z, .75)), max = max(z))
}))
write_csv(desc, "results", "descriptive_statistics.csv")
daily <- read_csv("data", "processed", "seoul_daily.csv"); daily$date <- as.Date(daily$date)
p <- ggplot2::ggplot(daily, ggplot2::aes(date, rentals)) +
  ggplot2::geom_line(colour = "#247BA0", linewidth = .55) +
  ggplot2::geom_vline(xintercept = c(config$validation_start, config$test_start),
    linetype = 2, colour = "#D75D43") +
  ggplot2::geom_point(data = daily[daily$operating_hours < 24, ], colour = "#D75D43", size = 1.6) +
  ggplot2::labs(title = "全年每日租借总量与实验时间划分", subtitle = "虚线：验证段与测试段起点；红点：存在非运营小时的日期。日汇总仅用于展示。",
    x = "日期", y = "每日租借总量（次）")
save_plot(p, "01_daily_overview.png", "全年租借量、停运与时间划分", 1)
week <- train[train$date >= as.Date("2018-06-04") & train$date <= as.Date("2018-06-10"), ]
save_plot(ggplot2::ggplot(week, ggplot2::aes(timestamp, count)) + ggplot2::geom_line(colour = "#247BA0") +
  ggplot2::labs(title = "训练段典型一周的小时租借量", subtitle = "2018-06-04 至 2018-06-10；示例日期预先固定", x = "首尔当地时间", y = "每小时租借量（次）"),
  "02_hourly_example.png", "训练段一周小时曲线", 1)
op <- train[train$operating == 1, ]; op$day_type <- ifelse(op$weekend == 1, "周末", "工作日")
profile <- aggregate(count ~ hour + day_type, op, mean)
write_csv(profile, "results", "hourly_profile_training.csv")
save_plot(ggplot2::ggplot(profile, ggplot2::aes(hour, count, colour = day_type)) +
  ggplot2::geom_line(linewidth = .9) + ggplot2::geom_point(size = 1.5) +
  ggplot2::scale_x_continuous(breaks = seq(0, 23, 3)) +
  ggplot2::labs(title = "工作日与周末的平均日内模式", subtitle = "仅初始训练段正常运营小时；不代表因果效应", x = "小时", y = "平均租借量（次/小时）"),
  "03_hourly_profiles.png", "训练段工作日与周末小时模式", 1)
heat <- aggregate(count ~ weekday + hour, op, mean)
save_plot(ggplot2::ggplot(heat, ggplot2::aes(hour, factor(weekday), fill = count)) +
  ggplot2::geom_tile() + ggplot2::scale_fill_gradient(low = "#EFF7FA", high = "#175E79") +
  ggplot2::labs(title = "星期与小时的租借量热图", subtitle = "初始训练段正常运营小时；星期 1 为周一", x = "小时", y = "星期"),
  "04_weekday_hour_heatmap.png", "训练段星期与小时热图", 1)
monthly <- do.call(rbind, lapply(split(d, d$month), function(g) data.frame(month = g$month[1],
  n = nrow(g), mean = mean(g$count), operating_mean = mean(g$count[g$operating == 1]),
  total = sum(g$count), closure_hours = sum(g$operating == 0))))
write_csv(monthly, "results", "monthly_summary.csv")
save_plot(ggplot2::ggplot(d[d$operating == 1, ], ggplot2::aes(month, count)) +
  ggplot2::geom_boxplot(fill = "#BBDDE6", outlier.alpha = .12, outlier.size = .6) +
  ggplot2::labs(title = "各月正常运营小时的租借量分布", subtitle = "全年描述性展示；每个月份仅观测一次，不能验证跨年季节性", x = "月份", y = "每小时租借量（次）") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)),
  "05_monthly_distribution.png", "各月正常运营小时分布", 1)
weather <- do.call(rbind, lapply(c("temperature", "humidity", "wind_speed", "rainfall", "snowfall"), function(v)
  data.frame(variable = v, pearson = cor(op[[v]], op$count), spearman = cor(op[[v]], op$count, method = "spearman"))))
write_csv(weather, "results", "weather_correlations_training.csv")
save_plot(ggplot2::ggplot(op, ggplot2::aes(temperature, count)) +
  ggplot2::geom_point(alpha = .08, size = .55, colour = "#247BA0") +
  ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "#D75D43") +
  ggplot2::labs(title = "训练段气温与租借量的描述性关系", subtitle = "仅正常运营小时；未控制日历、季节与其他因素，不能作因果解释", x = "气温（℃）", y = "每小时租借量（次）"),
  "06_temperature_relationship.png", "训练段气温与租借量", 1)
op$rain_group <- ifelse(op$rainfall > 0, "有降雨", "无降雨")
save_plot(ggplot2::ggplot(op, ggplot2::aes(rain_group, count, fill = rain_group)) +
  ggplot2::geom_boxplot(outlier.alpha = .18) + ggplot2::guides(fill = "none") +
  ggplot2::labs(title = "训练段降雨条件下的租借量分布", subtitle = "描述性比较；不等同于控制其他变量后的降雨效应", x = "记录的降雨状态", y = "每小时租借量（次）"),
  "07_rainfall_relationship.png", "训练段降雨分组分布", 1, width = 7)
save_plot(ggplot2::ggplot(d, ggplot2::aes(date, hour, fill = factor(operating))) +
  ggplot2::geom_tile() + ggplot2::scale_fill_manual(values = c("0" = "#D75D43", "1" = "#E4EDF0"), labels = c("非运营", "正常运营")) +
  ggplot2::labs(title = "全年运营状态与结构性零值", subtitle = "所有 295 个零值均出现在非运营小时；保留完整小时轴", x = "日期", y = "小时"),
  "08_operating_status.png", "全年小时运营状态", 1)
mt <- forecast::mstl(forecast::msts(log1p(train$count), seasonal.periods = c(24, 168)), robust = TRUE)
cmp <- as.data.frame(mt); cmp$timestamp <- train$timestamp
write_csv(cmp, "results", "mstl_training_components.csv")
long <- do.call(rbind, lapply(setdiff(names(cmp), "timestamp"), function(nm)
  data.frame(timestamp = cmp$timestamp, component = nm, value = cmp[[nm]])))
save_plot(ggplot2::ggplot(long, ggplot2::aes(timestamp, value)) + ggplot2::geom_line(linewidth = .25, colour = "#247BA0") +
  ggplot2::facet_grid(component ~ ., scales = "free_y") +
  ggplot2::labs(title = "训练段日、周多重季节分解", subtitle = "对 log(1+租借量) 作 MSTL；仅供结构识别，不将全样本分解输入预测模型", x = "时间", y = "分量"),
  "09_mstl_training.png", "训练段多重季节分解", 2, height = 9)
dist <- rbind(data.frame(value = train$count, transform = "原始租借量"),
  data.frame(value = log1p(train$count), transform = "log(1+租借量)"))
save_plot(ggplot2::ggplot(dist, ggplot2::aes(value)) + ggplot2::geom_histogram(bins = 50, fill = "#247BA0", colour = "white") +
  ggplot2::facet_wrap(~ transform, scales = "free") + ggplot2::labs(title = "训练段原值与对数变换分布", x = "数值", y = "小时数"),
  "10_training_distributions.png", "训练段原值与变换分布", 2)
