source(path_for("scripts","04_model_engine.R"),encoding="UTF-8")
source(path_for("scripts","03_baselines.R"),encoding="UTF-8")
d <- hourly(); selected <- readRDS(path_for("results", "selected_arima_specs.rds"))
validation_days <- seq(config$validation_start,config$validation_end,by="day")
test_days <- seq(config$test_start,config$test_end,by="day")
initial_end_index <- max(which(d$date<=config$initial_end))
pretest_end <- max(which(d$date<config$test_start))
scale_val <- scale_at(d$count,initial_end_index); scale_test <- scale_at(d$count,pretest_end)
write_csv(data.frame(evaluation=c("validation","test"),training_end_index=c(initial_end_index,pretest_end),
  lag=168,MASE_denominator=c(scale_val,scale_test)),"results","mase_scaling.csv")
common_columns <- c("model","candidate","index","origin_index","horizon","forecast_day","timestamp",
  "actual","operating","prediction","lo80","hi80","lo95","hi95")
align <- function(x) {
  for (nm in setdiff(common_columns,names(x))) x[[nm]]<-NA_real_
  x[,common_columns]
}
validation_arima<-read_csv("results","arima_validation_predictions.csv")
ids<-vapply(selected,`[[`,character(1),"id")
validation_arima<-validation_arima[validation_arima$candidate %in% ids,]
rf_validation<-read_csv("results","rf_validation_predictions.csv")
rf_metrics<-read_csv("results","rf_validation_metrics.csv")
best_rf<-rf_metrics$candidate[rf_metrics$subset=="all"][which.min(rf_metrics$RMSE[rf_metrics$subset=="all"])]
rf_validation<-rf_validation[rf_validation$candidate==best_rf,]
val_baselines<-baseline_predictions(validation_days)
validation_predictions<-do.call(rbind,lapply(list(val_baselines,validation_arima,rf_validation),align))
write_csv(validation_predictions,"results","validation_predictions_selected.csv")
validation_metrics<-score_predictions(validation_predictions,scale_val)
write_csv(validation_metrics,"results","validation_model_comparison.csv")
baseline_validation<-validation_metrics[validation_metrics$model%in%baseline_ids & validation_metrics$subset=="all",]
baseline_validation<-baseline_validation[order(baseline_validation$RMSE),]
strongest_baseline<-baseline_validation$model[1]
write_csv(baseline_validation,"results","baseline_validation_comparison.csv")
write_csv(data.frame(model=strongest_baseline,selection="Minimum all-hours full-validation RMSE; no test-based selection"),"results","strongest_baseline_selection.csv")
plot_models<-unique(c("SARIMA24","SARIMAX24","RandomForest",strongest_baseline))

# These specifications were locked using validation, before any test scoring below.
test_results<-lapply(selected,function(s) {
  message("Final test evaluation: ",s$id)
  r<-run_origins(s,test_days,"test")
  if (!r$complete) stop("Final evaluation incomplete: ",s$id," ",paste(r$errors,collapse="; "))
  r
})
test_baselines<-baseline_predictions(test_days)
ba<-rbind(data.frame(attr(val_baselines,"audit"),phase="validation"),data.frame(attr(test_baselines,"audit"),phase="test"))
write_csv(ba,"results","baseline_source_time_audit.csv")
test_predictions<-do.call(rbind,lapply(c(list(test_baselines),
  lapply(test_results,`[[`,"predictions"),list(read_csv("results","rf_test_predictions.csv"))),align))
test_predictions<-test_predictions[order(test_predictions$model,test_predictions$index),]
stopifnot(all(is.finite(test_predictions$prediction)),all(test_predictions$prediction>=0),
  all(test_predictions$origin_index<test_predictions$index),all(test_predictions$horizon==test_predictions$index-test_predictions$origin_index))
write_csv(test_predictions,"results","test_predictions.csv")
test_metrics<-score_predictions(test_predictions,scale_test)
base_rmse<-test_metrics$RMSE[test_metrics$model=="Naive168" & test_metrics$subset=="all"]
test_metrics$RMSE_improvement_over_Naive168_percent<-100*(1-test_metrics$RMSE/base_rmse)
# Improvements are only comparable to their matching subset.
op_base<-test_metrics$RMSE[test_metrics$model=="Naive168" & test_metrics$subset=="operating"]
test_metrics$RMSE_improvement_over_Naive168_percent[test_metrics$subset=="operating"]<-
  100*(1-test_metrics$RMSE[test_metrics$subset=="operating"]/op_base)
for(subset in c("all","operating")) {
 at<-test_metrics$subset==subset
 denominator<-test_metrics$RMSE[test_metrics$model==strongest_baseline & at]
 test_metrics$RMSE_improvement_over_strongest_baseline_percent[at]<-100*(1-test_metrics$RMSE[at]/denominator)
}
write_csv(test_metrics,"results","test_model_comparison.csv")
write_csv(do.call(rbind,lapply(test_results,`[[`,"refits")),"results","test_arima_refit_log.csv")

group_score <- function(pred, group, scale) {
  z<-pred; z$group<-group
  do.call(rbind,lapply(split(z,list(z$model,z$group),drop=TRUE),function(g)
    data.frame(model=g$model[1],group=g$group[1],as.list(metrics(g$actual,g$prediction,scale)),row.names=NULL)))
}
horizon_metrics<-group_score(test_predictions,test_predictions$horizon,scale_test)
write_csv(horizon_metrics,"results","test_errors_by_horizon.csv")
weekday<-d$weekday[test_predictions$index]
daytype<-ifelse(weekday>=6,"周末","工作日")
write_csv(group_score(test_predictions,daytype,scale_test),"results","test_errors_by_day_type.csv")
hour<-d$hour[test_predictions$index]
period<-ifelse(hour<=6,"00–06 夜间",ifelse(hour<=9,"07–09 早高峰",ifelse(hour<=16,"10–16 日间",ifelse(hour<=20,"17–20 晚间","21–23 夜间"))))
write_csv(group_score(test_predictions,period,scale_test),"results","test_errors_by_time_of_day.csv")
closure_error<-group_score(test_predictions,ifelse(test_predictions$operating==1,"operating","closed"),scale_test)
write_csv(closure_error,"results","test_errors_by_operating_status.csv")
intervals<-do.call(rbind,lapply(split(test_predictions[!is.na(test_predictions$lo80),],
  test_predictions$model[!is.na(test_predictions$lo80)]),function(g) do.call(rbind,lapply(c("all","operating"),function(subset) {
    z<-if(subset=="all") g else g[g$operating==1,]
    do.call(rbind,lapply(c(80,95),function(level) {
      low<-z[[paste0("lo",level)]]; high<-z[[paste0("hi",level)]]
      alpha<-1-level/100
      data.frame(model=g$model[1],subset=subset,level=level,n=nrow(z),
        covered_n=sum(z$actual>=low & z$actual<=high),coverage=mean(z$actual>=low & z$actual<=high),
        mean_width=mean(high-low),winkler_score=mean((high-low)+2/alpha*(low-z$actual)*(z$actual<low)+2/alpha*(z$actual-high)*(z$actual>high)))
    }))
  }))))
write_csv(intervals,"results","prediction_interval_coverage.csv")

parameters<-list(); diagnostics<-list(); residual_table<-list(); future<-list()
load_or_fit <- function(origin,spec,label) {
  model_path<-path_for("results","models",paste0(spec$family,"_",label,".rds"))
  if(file.exists(model_path)) {
    saved<-readRDS(model_path)
    if(identical(saved$hash,model_code_hash) && identical(saved$spec_id,spec$id) && saved$origin==origin) return(saved)
  }
  obj<-fit_arima(origin,spec);obj$hash<-model_code_hash;obj$spec_id<-spec$id
  saveRDS(obj,model_path);obj
}
for (nm in names(selected)) {
  s<-selected[[nm]]; pre<-load_or_fit(pretest_end,s,"pretest")
  co<-coef(pre$fit); se<-sqrt(diag(pre$fit$var.coef))
  parameters[[nm]]<-data.frame(model=nm,candidate=s$id,parameter=names(co),estimate=unname(co),
    std_error=unname(se),z=unname(co/se),p_asymptotic=2*pnorm(-abs(co/se)))
  rr<-as.numeric(residuals(pre$fit)); burn<-s$order[2]+24*s$seasonal[2]
  ii<-seq.int(pre$start,pretest_end)
  if(burn>0) {rr<-rr[-seq_len(burn)];ii<-ii[-seq_len(burn)]}
  fitdf<-sum(s$order[c(1,3)],s$seasonal[c(1,3)])
  diagnostics[[nm]]<-diagnose_bundle(pre,s)
  residual_table[[nm]]<-data.frame(model=nm,timestamp=d$timestamp[ii],residual=rr)
  full<-load_or_fit(nrow(d),s,"final")
  capture.output(print(full$fit),file=path_for("results",paste0(nm,"_final_summary.txt")))
  pp<-predict_arima(nrow(d),s,full);pp$model<-nm
  pp$timestamp<-format(seq(max(d$timestamp)+3600,by="hour",length.out=24),"%Y-%m-%d %H:%M:%S",tz=config$timezone)
  pp$horizon<-seq_len(24);future[[nm]]<-pp
}
params<-do.call(rbind,parameters);diag<-do.call(rbind,diagnostics);res<-do.call(rbind,residual_table)
write_csv(params,"results","selected_model_parameters.csv")
write_csv(diag,"results","selected_model_diagnostics.csv")
write_csv(res,"results","selected_model_residuals.csv")
future_rf<-read_csv("results","rf_future_24h.csv");future_rf$model<-"RandomForest"
future_predictions<-do.call(rbind,lapply(c(future,list(future_rf)),function(x) {
  for(nm in c("lo80","hi80","lo95","hi95")) if(!nm %in% names(x)) x[[nm]]<-NA_real_
  x[,c("model","timestamp","horizon","prediction","lo80","hi80","lo95","hi95")]
}))
write_csv(future_predictions,"results","future_24h_forecasts.csv")

plot_metrics<-function(tab,title,file,caption) {
  tab$subset<-ifelse(tab$subset=="all","全部小时（主要评价）","正常运营小时（事后补充）")
  long<-rbind(data.frame(tab[,c("model","subset")],metric="MAE",value=tab$MAE),
    data.frame(tab[,c("model","subset")],metric="RMSE",value=tab$RMSE))
  p<-ggplot2::ggplot(long,ggplot2::aes(model,value,fill=model)) + ggplot2::geom_col(show.legend=FALSE) +
    ggplot2::scale_fill_manual(values=palette) + ggplot2::facet_grid(metric~subset,scales="free_y") +
    ggplot2::labs(title=title,x="模型",y="误差（次/小时）") +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=35,hjust=1))
  save_plot(p,file,caption,4,width=13,height=8)
}
plot_metrics(validation_metrics,"验证段逐日次日预测误差","15_validation_comparison.png","验证段所选模型与基线误差")
plot_metrics(test_metrics,"最终测试段逐日次日预测误差","16_test_comparison.png","测试段全部小时及正常运营小时误差")
plot_predictions<-function(start,end,file,caption) {
  z<-test_predictions[test_predictions$model%in%plot_models & as.Date(test_predictions$forecast_day)>=as.Date(start)&as.Date(test_predictions$forecast_day)<=as.Date(end),]
  z$timestamp<-as.POSIXct(z$timestamp,tz=config$timezone)
  actual<-z[!duplicated(z$index),]
  p<-ggplot2::ggplot(z,ggplot2::aes(timestamp,prediction,colour=model)) + ggplot2::geom_line(linewidth=.6) +
    ggplot2::geom_line(data=actual,ggplot2::aes(timestamp,actual),inherit.aes=FALSE,colour="#172B3A",linewidth=.6) +
    ggplot2::scale_colour_manual(values=palette) + ggplot2::labs(title=caption,subtitle="黑线为实际值；各日预测均在前一天结束时生成",x="首尔当地时间",y="租借量（次/小时）")
  save_plot(p,file,caption,4,width=12,height=6)
}
plot_predictions("2018-10-15","2018-10-21","17_test_typical_week.png","测试段固定一周的模型预测对比")
plot_predictions("2018-11-02","2018-11-10","18_test_closures.png","停运附近的预测误差与模型局限")
for(nm in c("SARIMA24","DHR_ARIMA","SARIMAX24")) {
  z<-test_predictions[test_predictions$model==nm & as.Date(test_predictions$forecast_day)>=as.Date("2018-10-15") &
    as.Date(test_predictions$forecast_day)<=as.Date("2018-10-21"),]; z$timestamp<-as.POSIXct(z$timestamp,tz=config$timezone)
  p<-ggplot2::ggplot(z,ggplot2::aes(timestamp)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin=lo95,ymax=hi95),fill="#BDD9E8",alpha=.5) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin=lo80,ymax=hi80),fill="#7AB3CD",alpha=.5) +
    ggplot2::geom_line(ggplot2::aes(y=actual),colour="#172B3A",linewidth=.55) +
    ggplot2::geom_line(ggplot2::aes(y=prediction),colour=palette[nm],linewidth=.65) +
    ggplot2::labs(title=paste0(nm," 次日预测与 80%/95% 区间"),subtitle="展示日期预先固定；覆盖率另由完整测试段计算",x="时间",y="租借量（次/小时）")
  file<-switch(nm,SARIMA24="19_sarima_prediction_intervals.png",DHR_ARIMA="20_dhr_prediction_intervals.png",SARIMAX24="30_sarimax_prediction_intervals.png")
  save_plot(p,file,paste0(nm," 测试段预测区间示例"),3,width=12)
}
horizon_metrics$group<-as.integer(horizon_metrics$group)
save_plot(ggplot2::ggplot(horizon_metrics[horizon_metrics$model%in%plot_models,],ggplot2::aes(group,RMSE,colour=model)) + ggplot2::geom_line(linewidth=.8) +
  ggplot2::scale_colour_manual(values=palette) + ggplot2::scale_x_continuous(breaks=c(1,4,8,12,16,20,24)) +
  ggplot2::labs(title="预测步长与测试误差",subtitle="每天 23:00 为预测起点；h=1 对应次日 00:00，h=24 对应 23:00",x="步长 h（小时）",y="RMSE（次/小时）"),
  "21_errors_by_horizon.png","不同预测步长的测试误差",4)
day_metrics<-read_csv("results","test_errors_by_day_type.csv")
save_plot(ggplot2::ggplot(day_metrics,ggplot2::aes(model,RMSE,fill=model)) + ggplot2::geom_col(show.legend=FALSE) +
  ggplot2::scale_fill_manual(values=palette) + ggplot2::facet_wrap(~group) +
  ggplot2::labs(title="工作日与周末测试误差",x="模型",y="RMSE（次/小时）") +
  ggplot2::theme(axis.text.x=ggplot2::element_text(angle=35,hjust=1)),
  "22_errors_by_day_type.png","工作日与周末的测试误差",4)
save_plot(ggplot2::ggplot(res,ggplot2::aes(timestamp,residual)) + ggplot2::geom_line(linewidth=.3,colour="#247BA0") +
  ggplot2::facet_grid(model~.,scales="free_y") + ggplot2::labs(title="测试起点之前重估模型的残差",subtitle="只使用截至 2018-09-30 的历史；不同训练窗口长度依验证选择而定",x="时间",y="拟合尺度残差"),
  "23_selected_residual_series.png","所选 ARIMA 类模型的训练残差",2,height=8)
ra<-do.call(rbind,lapply(split(res,res$model),function(g) data.frame(model=g$model[1],lag=1:336,
  acf=as.numeric(acf(g$residual,lag.max=336,plot=FALSE)$acf)[-1],bound=1.96/sqrt(nrow(g)))))
save_plot(ggplot2::ggplot(ra,ggplot2::aes(lag,acf)) + ggplot2::geom_segment(ggplot2::aes(xend=lag,y=0,yend=acf),colour="#247BA0",linewidth=.3) +
  ggplot2::geom_line(ggplot2::aes(y=bound),linetype=2,colour="#D75D43") + ggplot2::geom_line(ggplot2::aes(y=-bound),linetype=2,colour="#D75D43") +
  ggplot2::facet_grid(model~.) + ggplot2::labs(title="所选模型残差 ACF",x="滞后（小时）",y="自相关"),
  "24_selected_residual_acf.png","所选模型的残差自相关",2,height=8)
save_plot(ggplot2::ggplot(res,ggplot2::aes(sample=residual)) + ggplot2::stat_qq(size=.5,alpha=.35,colour="#247BA0") +
  ggplot2::stat_qq_line(colour="#D75D43") + ggplot2::facet_wrap(~model,scales="free",ncol=2) +
  ggplot2::labs(title="所选模型残差正态 Q–Q 图",subtitle="正态性与无自相关是不同假设",x="理论分位数",y="残差分位数"),
  "25_selected_residual_qq.png","所选模型残差正态 Q–Q 图",2,width=12)
cv<-intervals[intervals$subset=="all",];cv$level_label<-paste0(cv$level,"% 名义区间")
save_plot(ggplot2::ggplot(cv,ggplot2::aes(model,coverage,fill=model)) + ggplot2::geom_col(show.legend=FALSE) +
  ggplot2::geom_hline(data=unique(cv[,c("level_label","level")]),ggplot2::aes(yintercept=level/100),linetype=2) +
  ggplot2::scale_fill_manual(values=palette) + ggplot2::facet_wrap(~level_label) +
  ggplot2::scale_y_continuous(labels=scales::percent,limits=c(0,1)) + ggplot2::labs(title="测试段预测区间实际覆盖率",subtitle="全部小时，包括非运营小时；虚线为名义水平",x="模型",y="实际覆盖率"),
  "26_interval_coverage.png","测试段 80%/95% 区间实际覆盖率",3)
imp<-head(read_csv("results","rf_training_feature_importance.csv"),15)
save_plot(ggplot2::ggplot(imp,ggplot2::aes(reorder(feature,impurity_importance),impurity_importance)) +
  ggplot2::geom_col(fill=palette["RandomForest"]) + ggplot2::coord_flip() +
  ggplot2::labs(title="随机森林训练特征重要性（前 15 项）",subtitle="测试起点前拟合的模型；基于不纯度下降，相关特征会分摊重要性，不作因果解释",x="特征",y="不纯度重要性"),
  "27_rf_feature_importance.png","随机森林训练特征重要性",4,height=7)
future_predictions$timestamp<-as.POSIXct(future_predictions$timestamp,tz=config$timezone)
save_plot(ggplot2::ggplot(future_predictions,ggplot2::aes(timestamp,prediction,colour=model)) +
  ggplot2::geom_line(linewidth=.85) + ggplot2::scale_colour_manual(values=palette) +
  ggplot2::labs(title="样本末端之后的 24 小时历史外推",subtitle="2018-12-01；普通非节假日情景，未输入未来实测天气或运营状态；无样本外真实值可核验",x="首尔当地时间",y="预测租借量（次/小时）"),
  "28_future_24h.png","2018 年 12 月 1 日历史外推",3)
daily_error<-group_score(test_predictions,test_predictions$forecast_day,scale_test);daily_error$date<-as.Date(daily_error$group)
write_csv(daily_error,"results","test_daily_errors.csv")
save_plot(ggplot2::ggplot(daily_error[daily_error$model%in%plot_models,],ggplot2::aes(date,RMSE,colour=model)) + ggplot2::geom_line(linewidth=.6) +
  ggplot2::scale_colour_manual(values=palette) + ggplot2::labs(title="测试段每天的次日预测误差",subtitle="每天 24 个预测点；停运等突发状态会形成误差峰值",x="预测日期",y="当日 RMSE（次/小时）"),
  "29_daily_test_errors.png","逐日次日预测误差",4,width=12)
message("Test metrics (all hours):")
print(test_metrics[test_metrics$subset=="all",c("model","MAE","RMSE","MASE168")],row.names=FALSE)

# Report-ready evidence for the three requested improvements.
weekday_coverage<-read_csv("results","arima_validation_weekday_coverage.csv")
weekday_coverage<-aggregate(forecast_days~family+weekday,weekday_coverage,mean)
save_plot(ggplot2::ggplot(weekday_coverage,ggplot2::aes(factor(weekday),forecast_days,fill=family))+
 ggplot2::geom_col(position="dodge")+ggplot2::scale_fill_manual(values=palette)+ggplot2::labs(title="完整验证覆盖每个星期位置",subtitle="每个完成候选均评价61天；起始拟合后固定每周一重估",x="星期（1=周一）",y="每个候选的平均预测日数"),
 "31_validation_weekday_coverage.png","完整验证的星期覆盖",2)
baseplot<-test_metrics[test_metrics$model%in%c(baseline_ids,"SARIMA24","SARIMAX24","RandomForest") & test_metrics$subset=="all",]
save_plot(ggplot2::ggplot(baseplot,ggplot2::aes(reorder(model,RMSE),RMSE,fill=model))+
 ggplot2::geom_col(show.legend=FALSE)+ggplot2::coord_flip()+ggplot2::scale_fill_manual(values=palette)+
 ggplot2::labs(title="强基线与所选模型的共同测试比较",subtitle=paste0("验证段最强基线：",strongest_baseline,"；测试为补充历史评价"),x="模型",y="RMSE（次/小时）"),
 "32_stronger_baselines.png","强基线与模型的测试误差",4,height=7)
oldres<-read_csv("results","reference_v1","selected_model_residuals.csv")
comparison<-rbind(data.frame(model="SARIMA24_v1",residual=oldres$residual[oldres$model=="SARIMA24"]),
 data.frame(model=res$model[res$model%in%c("SARIMA24","SARIMAX24")],residual=res$residual[res$model%in%c("SARIMA24","SARIMAX24")]))
structure<-do.call(rbind,lapply(split(comparison,comparison$model),function(g) {
 a<-as.numeric(acf(g$residual,lag.max=336,plot=FALSE)$acf)[-1]
 data.frame(model=g$model[1],lag=c(10,24,48,168,336),acf=a[c(10,24,48,168,336)],n=nrow(g))
}))
write_csv(structure,"results","residual_structure_comparison.csv")
save_plot(ggplot2::ggplot(structure,ggplot2::aes(factor(lag),acf,fill=model))+
 ggplot2::geom_col(position="dodge")+ggplot2::scale_fill_manual(values=c(palette,SARIMA24_v1="#88949E"))+ggplot2::labs(title="日周结构优化前后的残差相关",subtitle="各自选定模型的训练残差；窗口与样本数见结果表",x="滞后（小时）",y="残差 ACF"),
 "33_residual_structure_comparison.png","ARIMA结构优化的残差证据",2)
current<-test_metrics[test_metrics$subset=="all",c("model","MAE","RMSE")]
previous<-read_csv("results","reference_v1","test_model_comparison.csv")
previous<-previous[previous$subset=="all",c("model","MAE","RMSE")]
change<-merge(previous,current,by="model",suffixes=c("_v1","_v2"),all=TRUE)
change$interpretation<-"v1 used Wednesday-anchored validation; v2 complete validation and Monday refits. Previously inspected test; supplementary comparison."
write_csv(change,"results","v1_v2_test_reference_comparison.csv")
