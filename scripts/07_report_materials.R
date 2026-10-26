specifications<-read_csv("results","arima_candidate_specifications.csv")
cache_files<-list.files(path_for("results","cache","v2"),pattern="^validation_(full|targeted)_.*\\.rds$",full.names=TRUE)
information<-lapply(cache_files,function(p) {
  obj<-readRDS(p)
  if(!nrow(obj$refits)) return(NULL)
  z<-obj$refits[1,,drop=FALSE];z$complete_validation<-obj$complete
  z
})
information<-do.call(rbind,information)
information<-merge(information,specifications[,c("id","family","transform","window_days","d","D")],
  by.x="candidate",by.y="id",all.x=TRUE)
information$comparison_group<-with(information,paste(transform,training_start_index,training_end_index,d,D,sep="_"))
information$rank_within_group<-ave(information$AICc,information$comparison_group,FUN=function(x) rank(x,ties.method="min"))
write_csv(information,"results","training_information_criteria.csv")
source(path_for("scripts","07_report_builder.R"),encoding="UTF-8")
