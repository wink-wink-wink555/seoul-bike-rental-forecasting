selected<-readRDS(path_for("results","selected_arima_specs.rds"))
parameters<-read_csv("results","selected_model_parameters.csv")
diagnostics<-read_csv("results","selected_model_diagnostics.csv")
checks<-list()
for(nm in names(selected)) {
  s<-selected[[nm]];obj<-readRDS(path_for("results","models",paste0(nm,"_pretest.rds")))
  ord<-obj$fit$arma[c(1,6,2,3,7,4)]
  checks[[paste0(nm,"_order")]]<-data.frame(check=paste(nm,"order"),passed=identical(as.integer(ord[1:3]),as.integer(s$order)) &&
    identical(as.integer(ord[4:6]),as.integer(s$seasonal)))
  co<-coef(obj$fit);pp<-parameters[parameters$model==nm,]
  checks[[paste0(nm,"_parameters")]]<-data.frame(check=paste(nm,"parameters"),passed=
    identical(names(co),pp$parameter) && max(abs(as.numeric(co)-pp$estimate))<1e-9)
  rr<-as.numeric(residuals(obj$fit));burn<-s$order[2]+24*s$seasonal[2]
  if(burn>0) rr<-rr[-seq_len(burn)]
  dg<-diagnostics[diagnostics$model==nm,]
  ok<-all(vapply(seq_len(nrow(dg)),function(j) {
    lb<-Box.test(rr,lag=dg$lag[j],type="Ljung-Box",fitdf=dg$fitdf[j])
    expected_p<-pchisq(unname(lb$statistic),df=dg$lag[j]-dg$fitdf[j],lower.tail=FALSE)
    abs(unname(lb$statistic)-dg$statistic[j])<1e-7 && abs(expected_p-dg$p_value[j])<1e-10
  },logical(1)))
  checks[[paste0(nm,"_diagnostics")]]<-data.frame(check=paste(nm,"Ljung-Box"),passed=ok)
  checks[[paste0(nm,"_covariance")]]<-data.frame(check=paste(nm,"finite positive covariance"),passed=
    all(is.finite(diag(obj$fit$var.coef)))&&all(diag(obj$fit$var.coef)>0))
}
write_csv(do.call(rbind,checks),"results","r_model_validation.csv")
source(path_for("scripts","08_full_checks.R"),encoding="UTF-8")
