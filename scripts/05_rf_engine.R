# R-native random forest features, fitting and daily forecasting.
rf_d<-hourly()
rf_cache_root<-path_for("results","cache","ranger_v2")
dir.create(file.path(rf_cache_root,"fits"),recursive=TRUE,showWarnings=FALSE)
rf_hash<-paste(unname(tools::md5sum(path_for("scripts",c("05_rf_engine.R","05_random_forest.R")))),
  unname(tools::md5sum(path_for("data","processed","seoul_hourly.csv"))),
  unname(tools::md5sum(path_for("results","experiment_protocol.csv"))),collapse="|")
rf_features<-function(day) {
  day<-as.Date(day);origin<-max(which(rf_d$date<day));targets<-origin+1:24
  stopifnot(origin>=336,origin<=nrow(rf_d))
  weekday<-as.integer(format(day,"%u"))
  z<-data.frame(index=targets,origin_index=origin,horizon=1:24,hour=0:23,weekday=weekday,
    weekend=as.integer(weekday>=6),holiday=if(day<=max(rf_d$date))rf_d$holiday_flag[match(day,rf_d$date)] else 0,
    hour_sin=sin(2*pi*(0:23)/24),hour_cos=cos(2*pi*(0:23)/24))
  for(lag in c(24L,48L,168L,336L)) {
    stopifnot(max(targets-lag)<=origin)
    z[[paste0("count_lag",lag)]]<-rf_d$count[targets-lag]
  }
  z$same_hour_7day_mean<-rowMeans(vapply(1:7,function(k)rf_d$count[targets-24L*k],numeric(24)))
  for(window in c(24L,168L)) {
    x<-rf_d$count[(origin-window+1):origin]
    z[[paste0("past",window,"_mean")]]<-mean(x)
    z[[paste0("past",window,"_sd")]]<-sqrt(mean((x-mean(x))^2))
  }
  z$last_observed_count<-rf_d$count[origin]
  for(hour in 0:23) z[[sprintf("yesterday_hour_%02d",hour)]]<-rf_d$count[origin-23+hour]
  for(col in c("temperature","humidity","rainfall","wind_speed","solar_radiation"))
    z[[paste0(col,"_lag24")]]<-rf_d[[col]][targets-24L]
  z$max_source_index<-origin;z$forecast_day<-as.character(day)
  z$timestamp<-paste(day,sprintf("%02d:00:00",0:23))
  z$actual<-rf_d$count[targets];z$operating<-rf_d$operating[targets]
  z
}
rf_all_days<-seq(min(rf_d$date)+14,max(rf_d$date),by="day")
rf_frame<-do.call(rbind,lapply(seq_along(rf_all_days),function(j)rf_features(rf_all_days[j])))
rf_excluded<-c("index","origin_index","horizon","max_source_index","forecast_day","timestamp","actual","operating")
rf_columns<-setdiff(names(rf_frame),rf_excluded)
stopifnot(!any(c("actual","count","operating","functioning_day","season")%in%rf_columns))
rf_cols_for<-function(spec) {
  if(spec$weather) rf_columns else rf_columns[!grepl("_lag24$",rf_columns)|grepl("^count_",rf_columns)]
}
rf_fit<-function(spec,day) {
  day<-as.Date(day);cache<-file.path(rf_cache_root,"fits",paste0(spec$id,"_",day,".rds"))
  if(file.exists(cache)) {z<-readRDS(cache);if(identical(z$hash,rf_hash))return(z)}
  training<-rf_frame[as.Date(rf_frame$forecast_day)<day,]
  if(spec$window>0)training<-training[as.Date(training$forecast_day)>=day-spec$window,]
  origin<-max(which(rf_d$date<day))
  stopifnot(max(training$index)<=origin)
  cols<-rf_cols_for(spec);target<-training$actual
  if(spec$transform=="log1p")target<-log1p(target)
  model<-ranger::ranger(x=training[,cols,drop=FALSE],y=target,num.trees=400L,
    min.node.size=as.integer(2*spec$leaf),min.bucket=as.integer(spec$leaf),
    mtry=max(1L,as.integer(floor(spec$fraction*length(cols)))),max.depth=20L,
    importance="impurity",write.forest=TRUE,replace=TRUE,sample.fraction=1,
    num.threads=4L,seed=config$seed)
  obj<-list(model=model,columns=cols,spec=spec,training_n=nrow(training),trained_before=as.character(day),
    hash=rf_hash,backend="R ranger",trees=400L)
  saveRDS(obj,cache);obj
}
rf_predict<-function(bundle,features) {
  p<-as.numeric(predict(bundle$model,data=features[,bundle$columns,drop=FALSE],num.threads=4L)$predictions)
  if(bundle$spec$transform=="log1p")p<-expm1(p)
  pmax(0,p)
}
rf_run<-function(spec,days,phase) {
  cache<-file.path(rf_cache_root,paste0(phase,"_",spec$id,".rds"))
  if(file.exists(cache)) {
    x<-readRDS(cache)
    if(identical(x$hash,rf_hash)&&identical(x$days,days))return(x)
  }
  records<-list();logs<-list();bundle<-NULL
  for(j in seq_along(days)) {
    day<-days[j]
    if(is.null(bundle)||as.integer(format(day,"%u"))==config$refit_weekday) {
      bundle<-rf_fit(spec,day)
      logs[[length(logs)+1L]]<-data.frame(candidate=spec$id,forecast_day=as.character(day),
        training_n=bundle$training_n,trees=400L,backend="R ranger")
    }
    z<-rf_features(day);p<-rf_predict(bundle,z)
    z<-z[,c("index","origin_index","horizon","forecast_day","timestamp","actual","operating")]
    z$prediction<-p;z$model<-"RandomForest";z$candidate<-spec$id;records[[j]]<-z
    if(j%%14==0)message("R ranger ",phase," ",spec$id," ",j,"/",length(days))
  }
  result<-list(hash=rf_hash,days=days,predictions=do.call(rbind,records),refits=do.call(rbind,logs))
  saveRDS(result,cache);result
}
