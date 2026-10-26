# Transparent forecasts from information available at yesterday's 23:00 origin.
baseline_ids<-c("Naive24","Naive168","Mean7","Median7","DayTypeMean28")
baseline_predictions<-function(days) {
  predictions<-list();audits<-list();counter<-0L
  for(j in seq_along(days)) {
    day<-days[j];origin<-max(which(d$date<day));targets<-origin+seq_len(24L)
    for(id in baseline_ids) {
      source_indices<-lapply(targets,function(target) {
        if(id=="Naive24") return(target-24L)
        if(id=="Naive168") return(target-168L)
        if(id%in%c("Mean7","Median7")) return(target-24L*(1:7))
        ii<-target-24L*(1:28)
        ii[d$weekend[ii]==d$weekend[target]]
      })
      stopifnot(all(vapply(source_indices,function(ii)length(ii)>0&&min(ii)>=1&&max(ii)<=origin,logical(1))))
      pp<-vapply(source_indices,function(ii) {
        if(id=="Median7") median(d$count[ii]) else mean(d$count[ii])
      },numeric(1))
      counter<-counter+1L
      predictions[[counter]]<-data.frame(model=id,candidate=id,index=targets,origin_index=origin,horizon=1:24,
        forecast_day=as.character(day),timestamp=format(d$timestamp[targets],"%Y-%m-%d %H:%M:%S",tz=config$timezone),
        actual=d$count[targets],operating=d$operating[targets],prediction=pp)
      audits[[counter]]<-data.frame(model=id,index=targets,origin_index=origin,
        min_source_index=vapply(source_indices,min,integer(1)),max_source_index=vapply(source_indices,max,integer(1)),
        history_n=vapply(source_indices,length,integer(1)))
    }
  }
  z<-do.call(rbind,predictions);attr(z,"audit")<-do.call(rbind,audits);z
}
