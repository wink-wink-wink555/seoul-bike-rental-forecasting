d <- hourly(); z <- d[d$date <= config$initial_end, ]; y <- z$count
series <- list(raw = y, log1p = log1p(y), diff1 = diff(log1p(y)),
  diff24 = diff(log1p(y), lag = 24), diff1_diff24 = diff(diff(log1p(y), lag = 24)))
tests <- do.call(rbind, lapply(names(series), function(nm) {
  v <- series[[nm]]; tr <- nm %in% c("raw", "log1p")
  adf <- urca::ur.df(v, type = if (tr) "trend" else "drift", lags = 48, selectlags = "BIC")
  kp <- urca::ur.kpss(v, type = if (tr) "tau" else "mu", lags = "long")
  data.frame(series = nm, n = length(v), adf_deterministic = if (tr) "trend" else "drift",
    adf_max_lag = 48, adf_selected_lag = sum(grepl("^z.diff.lag", rownames(adf@testreg$coefficients))),
    adf_statistic = adf@teststat[1, 1], adf_cv_5pct = adf@cval[1, "5pct"],
    adf_reject_unit_root_5pct = adf@teststat[1, 1] < adf@cval[1, "5pct"],
    kpss_statistic = kp@teststat[1], kpss_cv_5pct = kp@cval[1, "5pct"],
    kpss_reject_stationarity_5pct = kp@teststat[1] > kp@cval[1, "5pct"])
}))
write_csv(tests, "results", "stationarity_tests.csv")
sensitivity<-do.call(rbind,lapply(names(series),function(nm) {
  v<-series[[nm]];tr<-nm %in% c("raw","log1p")
  a<-urca::ur.df(v,type=if(tr)"trend" else "drift",lags=168,selectlags="Fixed")
  k<-urca::ur.kpss(v,type=if(tr)"tau" else "mu",use.lag=168)
  data.frame(series=nm,n=length(v),adf_deterministic=if(tr)"trend" else "drift",adf_max_lag=168,
    adf_selected_lag=168,adf_statistic=a@teststat[1,1],adf_cv_5pct=a@cval[1,"5pct"],
    adf_reject_unit_root_5pct=a@teststat[1,1]<a@cval[1,"5pct"],kpss_statistic=k@teststat[1],
    kpss_cv_5pct=k@cval[1,"5pct"],kpss_reject_stationarity_5pct=k@teststat[1]>k@cval[1,"5pct"])
}))
write_csv(sensitivity,"results","stationarity_sensitivity.csv")
write_csv(data.frame(method = c("ndiffs_KPSS_log1p", "nsdiffs_seasonal_strength_log1p"),
  recommendation = c(forecast::ndiffs(log1p(y), test = "kpss", max.d = 1),
    forecast::nsdiffs(ts(log1p(y), frequency = 24), test = "seas", max.D = 1))),
  "results", "differencing_recommendations.csv")
correlations <- function(v, nm, maxlag = 336) {
  a <- as.numeric(acf(as.numeric(v), lag.max = maxlag, plot = FALSE)$acf)[-1]
  p <- as.numeric(pacf(as.numeric(v), lag.max = maxlag, plot = FALSE)$acf)
  stopifnot(length(a) == maxlag, length(p) == maxlag, abs(a[1] - p[1]) < 1e-10)
  rbind(data.frame(series = nm, lag = seq_len(maxlag), value = a, function_type = "ACF"),
    data.frame(series = nm, lag = seq_len(maxlag), value = p, function_type = "PACF"))
}
corr <- do.call(rbind, lapply(c("log1p", "diff24", "diff1_diff24"), function(nm) correlations(series[[nm]], nm)))
write_csv(corr, "results", "acf_pacf_training.csv")
for (nm in c("log1p", "diff24", "diff1_diff24")) {
  cc <- corr[corr$series == nm, ]; bound <- 1.96 / sqrt(length(series[[nm]]))
  p <- ggplot2::ggplot(cc, ggplot2::aes(lag, value)) +
    ggplot2::geom_segment(ggplot2::aes(xend = lag, y = 0, yend = value), colour = "#247BA0", linewidth = .3) +
    ggplot2::geom_hline(yintercept = c(-bound, bound), linetype = 2, colour = "#D75D43") +
    ggplot2::geom_vline(xintercept = c(24, 168, 336), linetype = 3, colour = "#88949E") +
    ggplot2::facet_grid(function_type ~ .) + ggplot2::scale_x_continuous(breaks = c(1, 24, 48, 72, 168, 240, 336)) +
    ggplot2::labs(title = paste0("训练段相关函数：", nm), subtitle = "虚线标出日、周及两周滞后；置信界限为描述性近似", x = "滞后（小时）", y = "相关系数")
  no <- match(nm, c("log1p", "diff24", "diff1_diff24")) + 10
  save_plot(p, sprintf("%02d_acf_pacf_%s.png", no, nm), paste0("训练段 ", nm, " 的 ACF/PACF"), 2, height = 7)
}
dif <- do.call(rbind, lapply(c("log1p", "diff1", "diff24", "diff1_diff24"), function(nm) {
  v <- series[[nm]]; data.frame(timestamp = tail(z$timestamp, length(v)), value = v, transform = nm)
}))
save_plot(ggplot2::ggplot(dif, ggplot2::aes(timestamp, value)) + ggplot2::geom_line(linewidth = .25, colour = "#247BA0") +
  ggplot2::facet_grid(transform ~ ., scales = "free_y") + ggplot2::labs(title = "训练段对数变换与差分比较", x = "时间", y = "变换值"),
  "14_differencing_comparison.png", "训练段不同差分方案比较", 2, height = 8)
