# Step 2. Additional structures motivated only by saved pre-test training residuals.
if(!exists("run_candidate_batch")) source(path_for("scripts","04_model_engine.R"),encoding="UTF-8")
base_specs<-readRDS(path_for("results","candidate_specs.rds"))
initial_selected<-readRDS(path_for("results","stage1_selected_arima_specs.rds"))
extra_specs<-list(
 make_spec("A_raw_212_84d_v2","ARIMA",c(2,1,2),window=84L),
 make_spec("S_raw_101_100_84d_v2","SARIMA24",c(1,0,1),c(1,0,0),window=84L),
 make_spec("C_raw_101_111_week_84d","SARIMAX24",c(1,0,1),c(1,1,1),window=84L,calendar="weekly",weekend_interactions=TRUE),
 make_spec("C_raw_301_111_week_84d","SARIMAX24",c(3,0,1),c(1,1,1),window=84L,calendar="weekly",weekend_interactions=TRUE),
 make_spec("C_raw_601_111_week_84d","SARIMAX24",c(6,0,1),c(1,1,1),window=84L,calendar="weekly",weekend_interactions=TRUE),
 make_spec("C_raw_1001_111_week_84d","SARIMAX24",c(10,0,1),c(1,1,1),window=84L,calendar="weekly",weekend_interactions=TRUE),
 make_spec("C_raw_302_011_week_84d","SARIMAX24",c(3,0,2),c(0,1,1),window=84L,calendar="weekly",weekend_interactions=TRUE),
 make_spec("C_raw_202_101_fullcalendar_84d","SARIMAX24",c(2,0,2),c(1,0,1),window=84L,K=5L,calendar="fourier",weekend_interactions=TRUE),
 make_spec("C_raw_302_100_fullcalendar_84d","SARIMAX24",c(3,0,2),c(1,0,0),window=84L,K=5L,calendar="fourier",weekend_interactions=TRUE))
names(extra_specs)<-vapply(extra_specs,`[[`,character(1),"id")
all_specs<-c(base_specs[!names(base_specs)%in%names(extra_specs)],extra_specs)
validation_days<-seq(config$validation_start,config$validation_end,by="day")
message("Step 2: complete daily validation for ",length(extra_specs)," targeted candidates.")
runs<-run_candidate_batch(extra_specs,validation_days,"validation_targeted")
new<-export_validation(extra_specs,runs,"validation_targeted")
combine_unique<-function(old,new,key) rbind(old[!old[[key]]%in%unique(new[[key]]),],new)
dense<-combine_unique(read_csv("results","arima_validation_metrics.csv"),new$metrics,"candidate")
pred<-combine_unique(read_csv("results","arima_validation_predictions.csv"),new$predictions,"candidate")
refits<-combine_unique(read_csv("results","validation_refit_log.csv"),new$refits,"candidate")
failures<-rbind(read_csv("results","candidate_failures.csv"),new$failures)
winners<-lapply(split(dense[dense$subset=="all",],dense$model[dense$subset=="all"]),function(g)g$candidate[order(g$RMSE,g$MAE)][1])
selected<-all_specs[unlist(winners,use.names=FALSE)];names(selected)<-names(winners)
write_csv(dense,"results","arima_validation_metrics.csv")
write_csv(pred,"results","arima_validation_predictions.csv")
write_csv(refits,"results","validation_refit_log.csv")
write_csv(failures,"results","candidate_failures.csv")
write_csv(do.call(rbind,lapply(all_specs,spec_to_row)),"results","arima_candidate_specifications.csv")
write_csv(do.call(rbind,lapply(selected,spec_to_row)),"results","selected_arima_specifications.csv")
saveRDS(all_specs,path_for("results","candidate_specs.rds"))
saveRDS(selected,path_for("results","selected_arima_specs.rds"))
progress<-do.call(rbind,lapply(names(selected),function(nm) {
 before<-if(nm%in%names(initial_selected)) initial_selected[[nm]]$id else initial_selected[["SARIMA24"]]$id
 after<-selected[[nm]]$id
 a<-dense$RMSE[dense$candidate==before & dense$subset=="all"];b<-dense$RMSE[dense$candidate==after & dense$subset=="all"]
 data.frame(family=nm,initial_candidate=before,final_candidate=after,initial_validation_RMSE=a,final_validation_RMSE=b,
  change_percent=100*(b/a-1),reason="Same full-daily Monday-refit protocol; selection uses validation RMSE only")
}))
write_csv(progress,"results","optimization_history.csv")
coverage<-do.call(rbind,lapply(split(pred,pred$candidate),function(g)do.call(rbind,lapply(split(g,as.integer(format(as.Date(g$forecast_day),"%u"))),function(z)
 data.frame(candidate=z$candidate[1],family=z$model[1],weekday=as.integer(format(as.Date(z$forecast_day[1]),"%u")),
 forecast_days=length(unique(z$forecast_day)),hours=nrow(z))))))
write_csv(coverage,"results","arima_validation_weekday_coverage.csv")
initial_origin<-max(which(d$date<=config$initial_end));pretest_origin<-max(which(d$date<config$test_start))
checks<-list()
for(nm in names(selected)) for(origin in c(initial_origin,pretest_origin)) {
 obj<-fit_arima(origin,selected[[nm]])
 z<-diagnose_bundle(obj,selected[[nm]]);z$diagnostic_origin<-as.character(d$date[origin]);checks[[length(checks)+1L]]<-z
}
write_csv(do.call(rbind,checks),"results","training_diagnostics_two_origins.csv")
message("Step 2 selected: ",paste(vapply(selected,`[[`,character(1),"id"),collapse=", "))
