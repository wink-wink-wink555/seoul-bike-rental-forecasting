# Independent numerical and artifact checks, all in R.
audit_checks<-list()
check<-function(name,ok,evidence="") {
 audit_checks[[length(audit_checks)+1L]]<<-data.frame(check=name,passed=isTRUE(ok),evidence=as.character(evidence))
}
near<-function(x,y,tolerance=1e-8)isTRUE(all.equal(as.numeric(x),as.numeric(y),tolerance=tolerance,check.attributes=FALSE))
tryCatch({
 raw<-utils::read.csv(path_for("data","raw","SeoulBikeData.csv"),fileEncoding="latin1",check.names=FALSE)
 d<-hourly();y<-d$count
 raw_time<-as.POSIXct(paste(as.Date(raw$Date,"%d/%m/%Y"),sprintf("%02d:00:00",raw$Hour)),tz=config$timezone)
 check("官方CSV固定快照MD5一致",tolower(unname(tools::md5sum(path_for("data","raw","SeoulBikeData.csv"))))=="bf632307dcb419f67df589faaa0318e1")
 check("清洗时间及租借量逐行对应原始记录",nrow(d)==8760&&near(raw_time,d$timestamp)&&near(y,raw[["Rented Bike Count"]]))
 check("完整小时轴无缺失重复",!anyNA(d)&&!anyDuplicated(d$timestamp)&&all(diff(as.numeric(d$timestamp))==3600))
 check("295个结构零值保留且均为非运营",sum(y==0)==295&&all((y==0)==(d$operating==0)))
 check("训练验证测试长度正确",identical(as.integer(base::table(factor(d$split,levels=c("initial_training","validation","test")))),c(5832L,1464L,1464L)))
 expected_models<-c("Naive24","Naive168","Mean7","Median7","DayTypeMean28","ARIMA","SARIMA24","SARIMAX24","DHR_ARIMA","RandomForest")
 for(phase in c("validation","test")) {
  p<-read_csv("results",if(phase=="validation")"validation_predictions_selected.csv" else "test_predictions.csv")
  m<-read_csv("results",paste0(phase,"_model_comparison.csv"))
  start<-if(phase=="validation")config$validation_start else config$test_start
  end<-if(phase=="validation")config$validation_end else config$test_end
  expected<-which(d$date>=start&d$date<=end);training_end<-min(expected)-1L
  check(paste(phase,"十种方法覆盖共同1464小时"),setequal(unique(p$model),expected_models)&&all(vapply(split(p,p$model),function(g)identical(sort(g$index),expected),logical(1))))
  check(paste(phase,"目标值与原始数据逐点一致"),near(p$actual,y[p$index])&&near(p$operating,d$operating[p$index]))
  check(paste(phase,"预测方向步长及时间标签正确"),all(p$index-p$origin_index==p$horizon)&&all(p$horizon%in%1:24)&&all(p$origin_index%%24==0)&&all(p$timestamp==format(d$timestamp[p$index],"%Y-%m-%d %H:%M:%S",tz=config$timezone)))
  check(paste(phase,"点预测有限非负"),all(is.finite(p$prediction))&&all(p$prediction>=0))
  scale<-mean(abs(y[169:training_end]-y[1:(training_end-168)]));ok<-TRUE
  for(i in seq_len(nrow(m))) {
   q<-p[p$model==m$model[i],];if(m$subset[i]=="operating")q<-q[q$operating==1,]
   e<-q$actual-q$prediction;den<-abs(q$actual)+abs(q$prediction);sm<-ifelse(den==0,0,200*abs(e)/den)
   values<-c(nrow(q),mean(abs(e)),sqrt(mean(e^2)),mean(sm),mean(abs(e))/scale,mean(q$prediction-q$actual))
   ok<-ok&&near(unlist(m[i,c("n","MAE","RMSE","sMAPE","MASE168","bias")]),values)
  }
  check(paste(phase,"全部及运营子集指标独立重算"),ok,paste("MASE denominator",format(scale,digits=12)))
  for(lag in c(24L,168L)) {
   q<-p[p$model==paste0("Naive",lag),];check(paste(phase,"同期基线",lag),near(q$prediction,y[q$index-lag]))
  }
  for(model in c("Mean7","Median7","DayTypeMean28")) {
   q<-p[p$model==model,];pred<-vapply(q$index,function(target) {
    prior<-target-24L*seq_len(if(model=="DayTypeMean28")28L else 7L)
    if(model=="DayTypeMean28")prior<-prior[d$weekend[prior]==d$weekend[target]]
    if(model=="Median7")median(y[prior]) else mean(y[prior])
   },numeric(1))
   check(paste(phase,"强基线独立重算",model),near(pred,q$prediction))
  }
 }
 p<-read_csv("results","test_predictions.csv");ci<-read_csv("results","prediction_interval_coverage.csv");ok<-TRUE
 for(i in seq_len(nrow(ci))) {
  r<-ci[i,];q<-p[p$model==r$model,];if(r$subset=="operating")q<-q[q$operating==1,]
  low<-q[[paste0("lo",r$level)]];high<-q[[paste0("hi",r$level)]];alpha<-1-r$level/100
  covered<-q$actual>=low&q$actual<=high
  winkler<-(high-low)+2/alpha*(low-q$actual)*(q$actual<low)+2/alpha*(q$actual-high)*(q$actual>high)
  ok<-ok&&near(unlist(r[,c("n","covered_n","coverage","mean_width","winkler_score")]),c(nrow(q),sum(covered),mean(covered),mean(high-low),mean(winkler)))
 }
 check("区间覆盖宽度及Winkler评分独立重算",ok)
 q<-p[!is.na(p$lo80),];check("区间有限非负并按名义水平嵌套",all(is.finite(as.matrix(q[,c("lo80","hi80","lo95","hi95")])) )&&all(q$lo95>=0&q$lo95<=q$lo80&q$lo80<=q$hi80&q$hi80<=q$hi95))
 specs<-read_csv("results","selected_arima_specifications.csv");vm<-read_csv("results","arima_validation_metrics.csv");vm<-vm[vm$subset=="all",]
 check("ARIMA类按完整验证RMSE选择",all(vapply(seq_len(nrow(specs)),function(i){g<-vm[vm$model==specs$family[i],];specs$id[i]==g$candidate[order(g$RMSE,g$MAE)][1]},logical(1))))
 wc<-read_csv("results","arima_validation_weekday_coverage.csv")
 check("每个完成候选覆盖61天及全部星期",all(vapply(split(wc,wc$candidate),function(g)setequal(g$weekday,1:7)&&sum(g$forecast_days)==61&&sum(g$hours)==1464,logical(1))))
 registry<-read_csv("results","arima_candidate_specifications.csv");failed<-read_csv("results","candidate_failures.csv")
 check("全部登记候选有完整验证或明确失败记录",setequal(registry$id,union(vm$candidate,failed$candidate)))
 rf<-jsonlite::read_json(path_for("results","selected_rf_specification.json"),simplifyVector=TRUE)
 rm<-read_csv("results","rf_validation_metrics.csv");rm<-rm[rm$subset=="all",]
 check("R随机森林只按完整验证选择",rf$id==rm$candidate[order(rm$RMSE,rm$MAE)][1]&&!rf$test_used_for_selection&&rf$backend=="R ranger")
 rw<-read_csv("results","rf_validation_weekday_coverage.csv")
 check("七组R随机森林均完成完整61天",nrow(rm)==7&&all(vapply(split(rw,rw$candidate),function(g)setequal(g$weekday,1:7)&&sum(g$forecast_days)==61&&sum(g$hours)==1464,logical(1))))
 refit_r<-read_csv("results","validation_refit_log.csv");refit_rf<-read_csv("results","rf_validation_refit_log.csv")
 vd<-seq(config$validation_start,config$validation_end,by="day");wanted<-as.character(vd[vd==config$validation_start|as.integer(format(vd,"%u"))==1])
 schedules<-c(split(refit_r,refit_r$candidate),split(refit_rf,refit_rf$candidate))
 check("验证统一起始拟合后每周一重估",all(vapply(schedules,function(g)if(g$candidate[1]%in%failed$candidate)TRUE else setequal(g$forecast_day,wanted),logical(1))))
 check("全部R随机森林验证固定400棵树",all(refit_rf$trees==400))
 audit<-read_csv("results","rf_feature_time_audit.csv")
 check("随机森林特征源索引不晚于起点",all(audit$max_source_index<=audit$origin_index)&&all(audit$index-24<=audit$origin_index))
 check("随机森林无目标小时实测天气和运营标记",!any(c("actual","count","operating","functioning_day","temperature","rainfall","humidity")%in%rf$feature_columns))
 ba<-read_csv("results","baseline_source_time_audit.csv")
 check("五基线源时间边界正确",nrow(ba)==5*1464*2&&all(ba$max_source_index<=ba$origin_index)&&all(ba$min_source_index>=1))
 bs<-read_csv("results","baseline_validation_comparison.csv");selected_base<-read_csv("results","strongest_baseline_selection.csv")$model[1]
 check("最强基线由验证选择",selected_base==bs$model[which.min(bs$RMSE)])
 saved<-readRDS(path_for("results","models","random_forest_pretest.rds"));origin<-7296L;targets<-origin+1:24
 x<-data.frame(row.names=seq_len(24))
 for(col in saved$columns) {
  v<-switch(col,hour=0:23,weekday=rep(1,24),weekend=rep(0,24),holiday=d$holiday_flag[targets],
   hour_sin=sin(2*pi*(0:23)/24),hour_cos=cos(2*pi*(0:23)/24),last_observed_count=rep(y[origin],24),NULL)
  if(is.null(v)&&grepl("^count_lag",col))v<-y[targets-as.integer(sub("^count_lag","",col))]
  if(is.null(v)&&col=="same_hour_7day_mean")v<-rowMeans(vapply(1:7,function(k)y[targets-24*k],numeric(24)))
  if(is.null(v)&&grepl("^past",col)) {
   window<-as.integer(sub("^past([0-9]+)_.*$","\\1",col));z<-y[(origin-window+1):origin]
   v<-rep(if(grepl("_mean$",col))mean(z)else sqrt(mean((z-mean(z))^2)),24)
  }
  if(is.null(v)&&grepl("^yesterday_hour_",col))v<-rep(y[origin-23+as.integer(sub("^yesterday_hour_","",col))],24)
  if(is.null(v)&&grepl("_lag24$",col))v<-d[[sub("_lag24$","",col)]][targets-24]
  if(is.null(v))stop("Unknown R forest feature: ",col)
  x[[col]]<-v
 }
 rp<-as.numeric(predict(saved$model,data=x[,saved$columns,drop=FALSE],num.threads=1L)$predictions)
 if(saved$spec$transform=="log1p")rp<-expm1(rp)
 first<-p[p$model=="RandomForest"&p$forecast_day=="2018-10-01",];first<-first[order(first$index),]
 check("RDS随机森林独立重建首日24点一致",saved$backend=="R ranger"&&saved$trained_before=="2018-10-01"&&near(pmax(0,rp),first$prediction))
 tr<-read_csv("results","test_arima_refit_log.csv");rr<-read_csv("results","rf_test_refit_log.csv")
 check("测试R模型均固定每周一重估",all(vapply(c(split(tr,tr$candidate),split(rr,rr$candidate)),function(g)nrow(g)==9&&all(as.integer(format(as.Date(g$forecast_day),"%u"))==1),logical(1))))
 f<-read_csv("results","future_24h_forecasts.csv");expected_times<-format(seq(as.POSIXct("2018-12-01",tz=config$timezone),by="hour",length.out=24),"%Y-%m-%d %H:%M:%S",tz=config$timezone)
 check("历史末端五模型24小时外推对齐",nrow(f)==120&&length(unique(f$model))==5&&all(vapply(split(f,f$model),function(g)identical(g$timestamp,expected_times),logical(1))))
 check("外推值有限非负",all(is.finite(f$prediction))&&all(f$prediction>=0))
 ac<-read_csv("results","acf_pacf_training.csv")
 check("ACF与PACF真实滞后对齐",all(vapply(split(ac,ac$series),function(g){a<-g[g$function_type=="ACF",];b<-g[g$function_type=="PACF",];identical(a$lag,1:336)&&identical(b$lag,1:336)&&abs(a$value[1]-b$value[1])<1e-10},logical(1))))
 fm<-read_csv("results","report_figure_manifest.csv");png_ok<-TRUE
 for(i in seq_len(nrow(fm))) {
  con<-file(path_for("figures",fm$file[i]),"rb");bytes<-readBin(con,"raw",n=24);close(con)
  be<-function(ii)sum(as.integer(bytes[ii])*256^(3:0))
  png_ok<-png_ok&&identical(as.integer(bytes[1:8]),c(137L,80L,78L,71L,13L,10L,26L,10L))&&abs(be(17:20)-300*fm$width_inches[i])<=1&&abs(be(21:24)-300*fm$height_inches[i])<=1
 }
 check("全部PNG头部及300DPI尺寸正确",png_ok,paste(nrow(fm),"figures"))
 check("图号完整唯一且标题无编码损坏",!anyNA(fm$report_number)&&!anyDuplicated(fm$report_number)&&!any(grepl("\\?{3,}",fm$caption)))
 tm<-read_csv("results","table_manifest.csv")
 check("所有报告表格CSV与Markdown可读取",all(file.exists(path_for(tm$csv)))&&all(file.exists(path_for(tm$markdown))),paste(nrow(tm),"tables"))
 doc<-paste(readLines(path_for("report_materials.md"),encoding="UTF-8"),collapse="\n")
 html<-paste(readLines(path_for("report_ready.html"),encoding="UTF-8"),collapse="\n")
 check("HTML包含全部图片",length(regmatches(html,gregexpr("<img ",html,fixed=TRUE))[[1]])==nrow(fm))
 mt<-read_csv("results","test_model_comparison.csv")
 check("报告关键RMSE对应实际结果",all(vapply(mt$RMSE[mt$subset=="all"],function(v)grepl(sprintf("%.3f",v),doc,fixed=TRUE),logical(1))))
 check("报告五章与附录齐全",all(vapply(c("第一章","第二章","第三章","第四章","第五章","参考文献","附录"),function(t)grepl(t,doc,fixed=TRUE),logical(1))))
 check("报告披露停运残余相关及一年限制",all(vapply(c("10.31%","残余相关","仅一年","未作为目标日"),function(t)grepl(t,doc,fixed=TRUE),logical(1))))
 check("报告披露已查看测试属于补充评价",grepl("补充历史评价",doc,fixed=TRUE)&&grepl("不称为全新盲测",doc,fixed=TRUE)&&rf$test_previously_inspected)
 source_files<-list.files(path_for("scripts"),recursive=TRUE,full.names=TRUE)
 check("项目分析脚本不含其他语言源文件",!any(grepl("\\.py(c)?$",source_files,ignore.case=TRUE)))
 # Parse symbols, rather than matching quoted text in this audit itself.
 source_symbols<-unique(unlist(lapply(source_files[grepl("\\.R$",source_files,ignore.case=TRUE)],
  function(file)all.names(parse(file,encoding="UTF-8"),functions=TRUE))))
 check("R运行链不调用外部分析语言",!any(c("system","system2","shell","Sys.which","reticulate")%in%source_symbols))
 checks_r<-read_csv("results","r_model_validation.csv");check("RDS阶数参数及诊断复算一致",all(checks_r$passed),paste(nrow(checks_r),"R model checks"))
},error=function(e)check("完整执行独立R验收",FALSE,conditionMessage(e)))
audit_table<-do.call(rbind,audit_checks);write_csv(audit_table,"results","validation_checks.csv")
passed<-all(audit_table$passed)
lines<-c("# 纯R实验验收记录","",paste0("状态：",if(passed)"全部工程核验通过" else "存在失败，不能作为完成结果交付"),paste0("核验时间：",Sys.time()),"",
 "工程核验检查来源、时间边界、预测、指标与报告一致性，不表示所有模型白噪声或正态性假设成立。","",
 "| 核验项目 | 结果 | 证据 |","|---|---|---|")
for(i in seq_len(nrow(audit_table)))lines<-c(lines,paste0("| ",audit_table$check[i]," | ",if(audit_table$passed[i])"通过" else "失败"," | ",gsub("|","/",audit_table$evidence[i],fixed=TRUE)," |"))
writeLines(enc2utf8(lines),path_for("results","validation_summary.md"),useBytes=TRUE)
message("Pure-R independent validation: ",sum(audit_table$passed),"/",nrow(audit_table))
if(!passed)stop("R validation failed; see results/validation_summary.md")
