train_ts <- get(".training_ts", envir = .GlobalEnv)
specs <- list(
  "SARIMA(0,0,0)(0,1,1)[12]" = list(order = c(0, 0, 0), seasonal = c(0, 1, 1)),
  "SARIMA(1,0,0)(0,1,1)[12]" = list(order = c(1, 0, 0), seasonal = c(0, 1, 1)),
  "SARIMA(0,0,1)(0,1,1)[12]" = list(order = c(0, 0, 1), seasonal = c(0, 1, 1)),
  "SARIMA(1,0,1)(0,1,1)[12]" = list(order = c(1, 0, 1), seasonal = c(0, 1, 1)),
  "SARIMA(0,0,0)(1,1,0)[12]" = list(order = c(0, 0, 0), seasonal = c(1, 1, 0)),
  "SARIMA(0,0,0)(1,1,1)[12]" = list(order = c(0, 0, 0), seasonal = c(1, 1, 1)),
  "SARIMA(1,0,1)(1,1,1)[12]" = list(order = c(1, 0, 1), seasonal = c(1, 1, 1))
)
models <- list()
for (nm in names(specs)) {
  sp <- specs[[nm]]
  fit <- tryCatch(forecast::Arima(
    train_ts, order = sp$order,
    seasonal = list(order = sp$seasonal, period = 12),
    include.drift = FALSE, method = "ML"
  ), error = function(e) e)
  if (!inherits(fit, "error")) {
    models[[nm]] <- fit
  } else {
    message("Candidate failed: ", nm, " -- ", conditionMessage(fit))
  }
}
auto_fit <- forecast::auto.arima(
  train_ts, seasonal = TRUE, stepwise = TRUE, approximation = FALSE,
  max.p = 3, max.q = 3, max.P = 2, max.Q = 2, max.d = 1, max.D = 1,
  allowmean = TRUE, allowdrift = TRUE, trace = FALSE
)
auto_name <- "auto.arima（训练段逐步搜索辅助候选）"
models[[auto_name]] <- auto_fit

model_rows <- lapply(names(models), function(nm) {
  fit <- models[[nm]]
  ord <- forecast::arimaorder(fit)
  residual <- as.numeric(stats::na.omit(stats::residuals(fit)))
  fitdf <- sum(ord[c(1, 3, 4, 6)])
  lb_lag <- min(24L, floor(length(residual) / 5))
  lb <- stats::Box.test(residual, lag = lb_lag, type = "Ljung-Box", fitdf = fitdf)
  code <- if (is.null(fit$code)) NA_integer_ else as.integer(fit$code)
  data.frame(
    name = nm, p = ord[1], d = ord[2], q = ord[3], P = ord[4], D = ord[5], Q = ord[6],
    AIC = stats::AIC(fit), AICc = fit$aicc, BIC = stats::BIC(fit),
    logLik = as.numeric(stats::logLik(fit)), residual_sd = stats::sd(residual),
    ljung_box_lag = lb_lag, ljung_box_fitdf = fitdf,
    ljung_box_statistic = unname(lb$statistic), ljung_box_p_value = lb$p.value,
    optimizer_code = code, stringsAsFactors = FALSE
  )
})
candidate_table <- do.call(rbind, model_rows)
candidate_table <- candidate_table[order(candidate_table$AICc), ]
write_csv(candidate_table, "results", "candidate_models.csv")

eligible <- candidate_table[is.finite(candidate_table$AICc) &
                              candidate_table$ljung_box_p_value > .05, ]
if (!nrow(eligible)) {
  selected_name <- candidate_table$name[which.min(candidate_table$AICc)]
  selection_rule <- "No candidate passed Ljung-Box p > 0.05; selected lowest training AICc."
} else {
  selected_name <- eligible$name[which.min(eligible$AICc)]
  selection_rule <- "Selected lowest training AICc among candidates with Ljung-Box p > 0.05."
}
selected_model <- models[[selected_name]]
assign(".selected_model", selected_model, envir = .GlobalEnv)
assign(".selected_model_name", selected_name, envir = .GlobalEnv)
assign(".selection_rule", selection_rule, envir = .GlobalEnv)
saveRDS(selected_model, path_for("results", "selected_sarima_train_fit.rds"))
ord <- forecast::arimaorder(selected_model)
utils::write.csv(data.frame(
  selected_name = selected_name, selection_rule = selection_rule,
  p = ord[1], d = ord[2], q = ord[3], P = ord[4], D = ord[5], Q = ord[6],
  period = 12, aicc = selected_model$aicc
), path_for("results", "selected_model.csv"), row.names = FALSE, fileEncoding = "UTF-8")

co <- stats::coef(selected_model)
se <- tryCatch(sqrt(diag(stats::vcov(selected_model))),
               error = function(e) rep(NA_real_, length(co)))
param <- data.frame(parameter = names(co), estimate = unname(co), std_error = unname(se),
                    z_value = unname(co / se),
                    p_value = 2 * stats::pnorm(-abs(co / se)))
write_csv(param, "results", "selected_model_parameters.csv")

resid <- as.numeric(stats::na.omit(stats::residuals(selected_model)))
fitdf <- sum(ord[c(1, 3, 4, 6)])
lb_lag <- min(24L, floor(length(resid) / 5))
lb <- stats::Box.test(resid, lag = lb_lag, type = "Ljung-Box", fitdf = fitdf)
diagnostics <- data.frame(
  model = selected_name, residual_n = length(resid), ljung_box_lag = lb_lag,
  fitdf = fitdf, statistic = unname(lb$statistic), p_value = lb$p.value,
  residual_mean = mean(resid), residual_sd = stats::sd(resid),
  residual_skewness = mean((resid - mean(resid))^3) / stats::sd(resid)^3,
  residual_kurtosis = mean((resid - mean(resid))^4) / stats::sd(resid)^4
)
write_csv(diagnostics, "results", "selected_model_diagnostics.csv")
saveRDS(models, path_for("results", "all_arima_candidates.rds"))

resid_df <- data.frame(index = seq_along(resid), residual = resid)
p_res <- ggplot2::ggplot(resid_df, ggplot2::aes(index, residual)) +
  ggplot2::geom_hline(yintercept = 0, colour = "#555555") +
  ggplot2::geom_line(linewidth = .4, colour = "#176B87") +
  ggplot2::labs(title = "最终 SARIMA 模型训练残差", x = "训练样本序号（月）", y = "残差")
save_plot(p_res, "09_sarima_residual_series.png", 9, 4.8)

acf_obj <- stats::acf(resid, lag.max = 36, plot = FALSE)
acf_df <- data.frame(lag = as.numeric(acf_obj$lag)[-1],
                     acf = as.numeric(acf_obj$acf)[-1])
p_acf <- ggplot2::ggplot(acf_df, ggplot2::aes(lag, acf)) +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::geom_segment(ggplot2::aes(xend = lag, y = 0, yend = acf), colour = "#176B87") +
  ggplot2::geom_hline(yintercept = c(-1, 1) * 1.96 / sqrt(length(resid)),
                      linetype = 2, colour = "#C44536") +
  ggplot2::labs(title = "最终 SARIMA 模型残差 ACF", x = "滞后（月）", y = "自相关")
save_plot(p_acf, "10_sarima_residual_acf.png", 9, 4.8)

qq <- stats::qqnorm(resid, plot.it = FALSE)
qq_df <- data.frame(theoretical = qq$x, sample = qq$y)
p_qq <- ggplot2::ggplot(qq_df, ggplot2::aes(theoretical, sample)) +
  ggplot2::geom_point(size = 1.4, alpha = .75) +
  ggplot2::geom_abline(colour = "#C44536", linetype = 2) +
  ggplot2::labs(title = "最终 SARIMA 模型残差正态 Q-Q 图",
                x = "正态理论分位数", y = "残差分位数")
save_plot(p_qq, "11_sarima_residual_qq.png", 8, 5)
message("Selected order: SARIMA(", paste(ord[1:3], collapse = ","), ")(",
        paste(ord[4:6], collapse = ","), ")[12] with drift. ", selection_rule)
