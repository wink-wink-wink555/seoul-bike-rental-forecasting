# Pure model definitions. Loading this file never writes candidate tables or selects models.
d <- hourly()
arima_cache_root <- path_for("results","cache","v2")
dir.create(file.path(arima_cache_root,"fits"),recursive=TRUE,showWarnings=FALSE)
model_code_hash <- paste(c(unname(tools::md5sum(path_for("scripts",c("04_model_engine.R","04_arima_experiments.R","04_refinement.R")))),
  unname(tools::md5sum(path_for("data","raw","SeoulBikeData.csv"))),
  paste(unlist(config[c("protocol_version","refit_weekday","horizon","initial_end","validation_start","validation_end","test_start","test_end")])),collapse="|"),collapse="|")
make_spec <- function(id,family,order,seasonal=c(0,0,0),transform="raw",window=0L,K=0L,weather=FALSE,
                      calendar="none",weekly_K=3L,weekend_interactions=FALSE) {
  if(family=="DHR_ARIMA" && calendar=="none") calendar<-"fourier"
  list(id=id,family=family,order=as.integer(order),seasonal=as.integer(seasonal),transform=transform,
    window=as.integer(window),K=as.integer(K),weather=weather,calendar=calendar,
    weekly_K=as.integer(weekly_K),weekend_interactions=weekend_interactions)
}
spec_to_row <- function(s) data.frame(id=s$id,family=s$family,p=s$order[1],d=s$order[2],q=s$order[3],
  P=s$seasonal[1],D=s$seasonal[2],Q=s$seasonal[3],period=ifelse(any(s$seasonal>0),24,1),
  transform=s$transform,window_days=s$window,daily_K=s$K,weekly_K=ifelse(s$calendar=="none",0,s$weekly_K),
  weather_lag24=s$weather,weekend_interactions=s$weekend_interactions,calendar=s$calendar)
calendar_for_indices <- function(indices) {
  times<-d$timestamp[1]+(indices-1L)*3600
  data.frame(weekday=as.integer(format(times,"%u")),weekend=as.integer(as.integer(format(times,"%u"))>=6),
    holiday=vapply(indices,function(i) if(i<=nrow(d)) d$holiday_flag[i] else 0,numeric(1)))
}
design_matrix <- function(indices,spec) {
  if(spec$calendar=="none") return(NULL)
  out<-matrix(numeric(),nrow=length(indices),ncol=0)
  add_fourier<-function(period,K) {
    if(K==0) return(NULL)
    z<-do.call(cbind,lapply(seq_len(K),function(k)cbind(sin(2*pi*k*indices/period),cos(2*pi*k*indices/period))))
    colnames(z)<-as.vector(rbind(paste0("sin",period,"_",seq_len(K)),paste0("cos",period,"_",seq_len(K))))
    z
  }
  if(spec$K>0) out<-cbind(out,add_fourier(24,spec$K))
  out<-cbind(out,add_fourier(168,spec$weekly_K))
  ca<-calendar_for_indices(indices)
  out<-cbind(out,holiday=ca$holiday)
  if(spec$weekend_interactions) {
    extra<-do.call(cbind,lapply(1:3,function(k)cbind(ca$weekend*sin(2*pi*k*indices/24),ca$weekend*cos(2*pi*k*indices/24))))
    colnames(extra)<-as.vector(rbind(paste0("weekend_sin24_",1:3),paste0("weekend_cos24_",1:3)))
    out<-cbind(out,weekend=ca$weekend,extra)
  }
  if(spec$weather) {
    stopifnot(min(indices-24)>=1,max(indices-24)<=nrow(d))
    out<-cbind(out,temperature_lag24=d$temperature[indices-24]/10,
      humidity_lag24=d$humidity[indices-24]/100,rainfall_lag24=as.numeric(d$rainfall[indices-24]>0))
  }
  out
}
training_indices <- function(origin,spec) {
  first<-if(spec$window>0) max(1L,origin-spec$window*24L+1L) else 1L
  if(spec$weather) first<-max(first,25L)
  seq.int(first,origin)
}
differenced_xreg <- function(x,spec) {
  if(is.null(x)) return(NULL)
  if(spec$seasonal[2]>0) for(j in seq_len(spec$seasonal[2])) x<-x[25:nrow(x),,drop=FALSE]-x[1:(nrow(x)-24),,drop=FALSE]
  if(spec$order[2]>0) for(j in seq_len(spec$order[2])) x<-x[-1,,drop=FALSE]-x[-nrow(x),,drop=FALSE]
  x
}
fit_arima <- function(origin,spec) {
  cache<-file.path(arima_cache_root,"fits",paste0(spec$id,"_",origin,".rds"))
  if(file.exists(cache)) {
    z<-readRDS(cache)
    if(identical(z$hash,model_code_hash)) return(z)
  }
  ii<-training_indices(origin,spec);y<-d$count[ii]
  if(spec$transform=="log1p") y<-log1p(y)
  xr<-design_matrix(ii,spec);dropped<-character();kept<-character()
  if(!is.null(xr)) {
    dx<-differenced_xreg(xr,spec)
    varying<-apply(dx,2,function(v) diff(range(v))>1e-10)
    dropped<-colnames(xr)[!varying]
    xr<-xr[,varying,drop=FALSE];dx<-dx[,varying,drop=FALSE]
    if(ncol(xr)>0) {
      qr_x<-qr(cbind(intercept=1,dx),tol=1e-8)
      retained<-sort(qr_x$pivot[seq_len(qr_x$rank)])
      keep<-seq_len(ncol(xr)) %in% (retained-1L)
      dropped<-c(dropped,colnames(xr)[!keep]);xr<-xr[,keep,drop=FALSE]
    }
    kept<-colnames(xr)
    if(!ncol(xr)) xr<-NULL
  }
  warnings<-character()
  fit_once<-function(method) withCallingHandlers(forecast::Arima(y,order=spec$order,
    seasonal=list(order=spec$seasonal,period=if(any(spec$seasonal>0)) 24 else 1),xreg=xr,
    include.mean=sum(spec$order[2],spec$seasonal[2])==0,include.drift=FALSE,method=method,
    optim.control=list(maxit=300)),warning=function(w) {warnings<<-c(warnings,conditionMessage(w));invokeRestart("muffleWarning")})
  setTimeLimit(elapsed=180,transient=TRUE)
  on.exit(setTimeLimit(cpu=Inf,elapsed=Inf,transient=FALSE),add=TRUE)
  fit<-tryCatch(fit_once("ML"),error=function(e)e)
  valid_fit<-function(z) !inherits(z,"error") && (is.null(z$code)||z$code==0) &&
    all(is.finite(coef(z))) && all(is.finite(diag(z$var.coef))) && all(diag(z$var.coef)>0)
  method<-"ML"
  if(!valid_fit(fit)) {fit<-fit_once("CSS-ML");method<-"CSS-ML retry"}
  if(!valid_fit(fit)) stop("Nonconverged or invalid covariance: ",spec$id)
  fit$call$xreg<-xr
  obj<-list(fit=fit,start=min(ii),origin=origin,warnings=warnings,kept_columns=kept,
    dropped_columns=dropped,hash=model_code_hash,spec_id=spec$id,estimation=method)
  saveRDS(obj,cache);obj
}
predict_arima <- function(origin,spec,fitted) {
  ii<-seq.int(fitted$start,origin)
  xr<-design_matrix(ii,spec)
  if(!is.null(xr)) xr<-if(length(fitted$kept_columns)) xr[,fitted$kept_columns,drop=FALSE] else NULL
  if(origin==fitted$origin) fit<-fitted$fit else {
    yy<-d$count[ii];if(spec$transform=="log1p") yy<-log1p(yy)
    fit<-forecast::Arima(yy,model=fitted$fit,xreg=xr)
  }
  fit$call$xreg<-xr
  future<-origin+seq_len(24L)
  if(spec$weather) stopifnot(max(future-24L)<=origin)
  fx<-design_matrix(future,spec)
  if(!is.null(fx)) fx<-if(length(fitted$kept_columns)) fx[,fitted$kept_columns,drop=FALSE] else NULL
  fc<-forecast::forecast(fit,h=24,xreg=fx,level=c(80,95))
  mu<-as.numeric(fc$mean);lo80<-as.numeric(fc$lower[,1]);hi80<-as.numeric(fc$upper[,1])
  lo95<-as.numeric(fc$lower[,2]);hi95<-as.numeric(fc$upper[,2])
  if(spec$transform=="log1p") {
    variance<-((hi95-lo95)/(2*qnorm(.975)))^2
    mu<-exp(mu+variance/2)-1
    lo80<-expm1(lo80);hi80<-expm1(hi80);lo95<-expm1(lo95);hi95<-expm1(hi95)
  }
  z<-data.frame(index=future,prediction=pmax(0,mu),lo80=pmax(0,lo80),hi80=pmax(0,hi80),lo95=pmax(0,lo95),hi95=pmax(0,hi95))
  stopifnot(all(is.finite(as.matrix(z))),all(z$lo95<=z$lo80),all(z$lo80<=z$hi80),all(z$hi80<=z$hi95))
  z
}
run_origins <- function(spec,days,phase) {
  cache<-file.path(arima_cache_root,paste0(phase,"_",spec$id,".rds"))
  if(file.exists(cache)) {
    a<-readRDS(cache)
    if(identical(a$hash,model_code_hash)&&identical(a$days,days)&&a$complete) return(a)
  }
  result<-list();refits<-list();errors<-character();fitted<-NULL
  for(j in seq_along(days)) {
    day<-days[j];origin<-max(which(d$date<day))
    if(is.null(fitted)||as.integer(format(day,"%u"))==config$refit_weekday) {
      fitted<-tryCatch(fit_arima(origin,spec),error=function(e)e)
      if(inherits(fitted,"error")) {errors<-paste(day,conditionMessage(fitted));break}
      refits[[length(refits)+1L]]<-data.frame(candidate=spec$id,forecast_day=as.character(day),
        training_start_index=fitted$start,training_end_index=origin,training_n=origin-fitted$start+1,
        AIC=AIC(fitted$fit),AICc=fitted$fit$aicc,BIC=BIC(fitted$fit),optimizer_code=0,
        warnings=paste(unique(fitted$warnings),collapse="; "),dropped_columns=paste(fitted$dropped_columns,collapse=";"),estimation=fitted$estimation)
    }
    pp<-tryCatch(predict_arima(origin,spec,fitted),error=function(e)e)
    if(inherits(pp,"error")) {errors<-paste(day,conditionMessage(pp));break}
    pp$model<-spec$family;pp$candidate<-spec$id;pp$origin_index<-origin;pp$horizon<-1:24
    pp$forecast_day<-as.character(day);pp$timestamp<-format(d$timestamp[pp$index],"%Y-%m-%d %H:%M:%S",tz=config$timezone)
    pp$actual<-d$count[pp$index];pp$operating<-d$operating[pp$index];result[[j]]<-pp
    if(j%%14==0) message(phase," ",spec$id," day ",j,"/",length(days))
  }
  ans<-list(hash=model_code_hash,days=days,predictions=if(length(result))do.call(rbind,result) else data.frame(),
    refits=if(length(refits))do.call(rbind,refits) else data.frame(),errors=errors,complete=length(result)==length(days))
  saveRDS(ans,cache);ans
}
worker_run <- function(spec,days,phase) {message("Start ",phase," ",spec$id);run_origins(spec,days,phase)}
run_candidate_batch <- function(specs,days,phase) {
  workers<-min(config$arima_workers,length(specs))
  if(workers<=1) return(lapply(specs,worker_run,days=days,phase=phase))
  cl<-parallel::makePSOCKcluster(workers,outfile=path_for("logs",paste0(phase,"_workers.log")))
  on.exit(parallel::stopCluster(cl),add=TRUE)
  parallel::clusterEvalQ(cl,{if(.Platform$OS.type=="windows")Sys.setlocale("LC_CTYPE",".UTF-8");NULL})
  exports<-c("d","config","project_root","path_for","arima_cache_root","model_code_hash","make_spec","spec_to_row",
    "calendar_for_indices","design_matrix","training_indices","differenced_xreg","fit_arima","predict_arima","run_origins","worker_run")
  parallel::clusterExport(cl,exports,envir=.GlobalEnv)
  out<-parallel::parLapplyLB(cl,specs,worker_run,days=days,phase=phase)
  names(out)<-names(specs);out
}
diagnose_bundle <- function(obj,spec,model=spec$family) {
  rr<-as.numeric(residuals(obj$fit));burn<-spec$order[2]+24*spec$seasonal[2]
  if(burn>0) rr<-rr[-seq_len(burn)]
  fitdf<-sum(spec$order[c(1,3)],spec$seasonal[c(1,3)])
  ac<-as.numeric(acf(rr,lag.max=336,plot=FALSE)$acf)[-1]
  do.call(rbind,lapply(c(24L,48L,168L,336L),function(lag) {
    lb<-Box.test(rr,lag=lag,type="Ljung-Box",fitdf=fitdf)
    data.frame(model=model,candidate=spec$id,training_n=length(rr),transform=spec$transform,lag=lag,fitdf=fitdf,
      statistic=unname(lb$statistic),p_value=pchisq(unname(lb$statistic),df=lag-fitdf,lower.tail=FALSE),
      residual_mean=mean(rr),residual_sd=sd(rr),AICc=obj$fit$aicc,
      acf10=ac[10],acf24=ac[24],acf168=ac[168],max_abs_acf=max(abs(ac)))
  }))
}
export_validation <- function(specs,runs,stage) {
  scale<-scale_at(d$count,max(which(d$date<=config$initial_end)))
  good<-runs[vapply(runs,`[[`,logical(1),"complete")]
  if(!length(good)) stop("No completed candidates in ",stage)
  score<-do.call(rbind,lapply(names(good),function(id) {z<-score_predictions(good[[id]]$predictions,scale);z$candidate<-id;z}))
  pred<-do.call(rbind,lapply(good,`[[`,"predictions"));refits<-do.call(rbind,lapply(runs,`[[`,"refits"))
  failures<-do.call(rbind,lapply(names(runs),function(id) if(!runs[[id]]$complete) data.frame(candidate=id,phase=stage,reason=paste(runs[[id]]$errors,collapse="; ")) else NULL))
  if(is.null(failures)) failures<-data.frame(candidate=character(),phase=character(),reason=character())
  list(metrics=score,predictions=pred,refits=refits,failures=failures)
}
