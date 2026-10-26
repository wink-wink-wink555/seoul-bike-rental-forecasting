source(path_for("scripts","05_rf_engine.R"),encoding="UTF-8")
rf_specs<-list(
 list(id="RF_raw_84_leaf5_calendar",window=84L,leaf=5L,fraction=.8,weather=FALSE,transform="raw"),
 list(id="RF_raw_84_leaf5_weather",window=84L,leaf=5L,fraction=.8,weather=TRUE,transform="raw"),
 list(id="RF_raw_84_leaf15_weather",window=84L,leaf=15L,fraction=.6,weather=TRUE,transform="raw"),
 list(id="RF_raw_56_leaf5_weather",window=56L,leaf=5L,fraction=.8,weather=TRUE,transform="raw"),
 list(id="RF_raw_112_leaf5_weather",window=112L,leaf=5L,fraction=.8,weather=TRUE,transform="raw"),
 list(id="RF_raw_full_leaf15_weather",window=0L,leaf=15L,fraction=.8,weather=TRUE,transform="raw"),
 list(id="RF_log_84_leaf5_weather",window=84L,leaf=5L,fraction=.8,weather=TRUE,transform="log1p"))
names(rf_specs)<-vapply(rf_specs,`[[`,character(1),"id")
write_csv(do.call(rbind,lapply(rf_specs,as.data.frame)),"results","rf_candidate_specifications.csv")
write_csv(rf_frame[,c("index","origin_index","max_source_index","forecast_day","horizon")],"results","rf_feature_time_audit.csv")
val_days<-seq(config$validation_start,config$validation_end,by="day")
scale<-scale_at(rf_d$count,max(which(rf_d$date<config$validation_start)))
runs<-lapply(rf_specs,function(spec){message("R-native full validation: ",spec$id);rf_run(spec,val_days,"validation_full")})
validation_scores<-do.call(rbind,lapply(names(runs),function(id){
 x<-score_predictions(runs[[id]]$predictions,scale);x$candidate<-id;x
}))
write_csv(validation_scores,"results","rf_validation_metrics.csv")
write_csv(do.call(rbind,lapply(runs,`[[`,"predictions")),"results","rf_validation_predictions.csv")
write_csv(do.call(rbind,lapply(runs,`[[`,"refits")),"results","rf_validation_refit_log.csv")
coverage<-do.call(rbind,lapply(names(runs),function(id){
 p<-runs[[id]]$predictions
 do.call(rbind,lapply(split(p,as.integer(format(as.Date(p$forecast_day),"%u"))),function(g)
 data.frame(candidate=id,weekday=as.integer(format(as.Date(g$forecast_day[1]),"%u")),hours=nrow(g),forecast_days=length(unique(g$forecast_day)))))
}))
write_csv(coverage,"results","rf_validation_weekday_coverage.csv")
scores<-validation_scores[validation_scores$subset=="all",];best<-scores$candidate[order(scores$RMSE,scores$MAE)][1]
spec<-rf_specs[[best]]
selection<-c(spec,list(trees=400L,selection_metric="full_61day_validation_all_hours_RMSE",validation_end=as.character(config$validation_end),
 test_used_for_selection=FALSE,test_previously_inspected=TRUE,refit_weekday="Monday",signature=rf_hash,
 feature_columns=rf_cols_for(spec),backend="R ranger",ranger_version=as.character(utils::packageVersion("ranger"))))
jsonlite::write_json(selection,path_for("results","selected_rf_specification.json"),auto_unbox=TRUE,pretty=TRUE)
message("R-native selection locked: ",best)
pre<-rf_fit(spec,config$test_start);saveRDS(pre,path_for("results","models","random_forest_pretest.rds"))
imp<-pre$model$variable.importance;imp<-imp/sum(imp)
importance<-data.frame(feature=names(imp),impurity_importance=as.numeric(imp));importance<-importance[order(-importance$impurity_importance),]
write_csv(importance,"results","rf_training_feature_importance.csv")
test_days<-seq(config$test_start,config$test_end,by="day");test<-rf_run(spec,test_days,"test")
write_csv(test$predictions,"results","rf_test_predictions.csv");write_csv(test$refits,"results","rf_test_refit_log.csv")
last_day<-max(rf_d$date)+1;full<-rf_fit(spec,last_day);saveRDS(full,path_for("results","models","random_forest_final.rds"))
f<-rf_features(last_day);f$prediction<-rf_predict(full,f)
write_csv(f[,c("timestamp","horizon","prediction")],"results","rf_future_24h.csv")
write_csv(data.frame(item=c("implementation","R","ranger","seed","trees"),value=c("R ranger",R.version.string,
 as.character(utils::packageVersion("ranger")),config$seed,400)),"results","rf_environment.csv")
