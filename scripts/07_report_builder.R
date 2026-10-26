# Offline report materials, tables and figure mapping. Entirely implemented in R.
metrics_table<-read_csv("results","test_model_comparison.csv")
validation<-read_csv("results","validation_model_comparison.csv")
selected<-read_csv("results","selected_arima_specifications.csv")
rf<-jsonlite::read_json(path_for("results","selected_rf_specification.json"),simplifyVector=TRUE)
diagnostic<-read_csv("results","selected_model_diagnostics.csv")
parameters<-read_csv("results","selected_model_parameters.csv")
coverage<-read_csv("results","prediction_interval_coverage.csv")
figs<-read_csv("results","figure_manifest.csv");figs<-figs[order(figs$file),];figs$report_number<-NA_character_
all_metrics<-metrics_table[metrics_table$subset=="all",];all_metrics<-all_metrics[order(all_metrics$RMSE),]
row_for<-function(id)all_metrics[all_metrics$model==id,,drop=FALSE]
strongest<-read_csv("results","strongest_baseline_selection.csv")$model[1]
baseline_validation<-read_csv("results","baseline_validation_comparison.csv")
candidate_specs<-read_csv("results","arima_candidate_specifications.csv")
candidate_metrics<-read_csv("results","arima_validation_metrics.csv");candidate_metrics<-candidate_metrics[candidate_metrics$subset=="all",]
failures<-read_csv("results","candidate_failures.csv")
parts<-character();html_parts<-character();tables<-list()
escape_html<-function(s) {
 s<-gsub("&","&amp;",as.character(s),fixed=TRUE);s<-gsub("<","&lt;",s,fixed=TRUE)
 s<-gsub(">","&gt;",s,fixed=TRUE);gsub('"',"&quot;",s,fixed=TRUE)
}
fmt<-function(v) {
 if(is.na(v))return("—")
 if(is.logical(v))return(if(v)"是" else "否")
 if(is.integer(v))return(as.character(v))
 if(is.numeric(v))return(if(v!=0&&abs(v)<.001)format(v,digits=3,scientific=TRUE) else sprintf("%.3f",v))
 as.character(v)
}
heading<-function(s,level=2L) {
 parts<<-c(parts,paste0(strrep("#",level)," ",s));html_parts<<-c(html_parts,sprintf("<h%d>%s</h%d>",level,escape_html(s),level))
}
text<-function(s) {parts<<-c(parts,s);html_parts<<-c(html_parts,paste0("<p>",escape_html(s),"</p>"))}
labels<-c(model="模型",subset="评价范围",n="小时数",candidate="候选编号",transform="变换",window_days="回看天数（0=扩展）",
 parameter="参数",estimate="估计值",std_error="标准误",p_asymptotic="渐近p值",lag="检验滞后",statistic="Ljung–Box统计量",
 p_value="p值",training_n="训练残差数",coverage="实际覆盖比例",level="名义覆盖率（%）",mean_width="平均区间宽度",
 winkler_score="Winkler评分",covered_n="覆盖小时数",group="分组",bias="平均预测偏差",MASE168="MASE（周周期缩放）",
 sMAPE="sMAPE（%）",series="序列",adf_statistic="ADF统计量",adf_cv_5pct="ADF 5%临界值",kpss_statistic="KPSS统计量",
 kpss_cv_5pct="KPSS 5%临界值",adf_reject_unit_root_5pct="ADF拒绝单位根",kpss_reject_stationarity_5pct="KPSS拒绝平稳")
table_dir<-path_for("results","report_tables");dir.create(table_dir,showWarnings=FALSE)
table<-function(number,title,frame,source,note="") {
 z<-frame
 for(nm in intersect(c("p_value","p_asymptotic"),names(z)))z[[nm]]<-vapply(z[[nm]],function(v)if(is.na(v))"—" else if(v==0)"<1e-15" else fmt(v),character(1))
 names(z)<-ifelse(names(z)%in%names(labels),labels[names(z)],names(z))
 display<-as.data.frame(lapply(z,function(v)vapply(v,fmt,character(1))),check.names=FALSE)
 title<-paste0("表 ",number," ",title)
 line<-function(v)paste0("| ",paste(gsub("|","/",v,fixed=TRUE),collapse=" | ")," |")
 md<-paste(c(line(names(display)),line(rep("---",ncol(display))),apply(display,1,line)),collapse="\n")
 parts<<-c(parts,paste(title,md,note,sep="\n\n"))
 rows<-apply(display,1,function(v)paste0("<tr>",paste0("<td>",escape_html(v),"</td>",collapse=""),"</tr>"))
 html_parts<<-c(html_parts,paste0("<figure><figcaption>",escape_html(title),"</figcaption><div class='scroll'><table><thead><tr>",
  paste0("<th>",escape_html(names(display)),"</th>",collapse=""),"</tr></thead><tbody>",paste(rows,collapse=""),"</tbody></table></div><p class='note'>",escape_html(note),"</p></figure>"))
 stem<-paste0("table_",gsub("-","_",number,fixed=TRUE))
 writeLines(enc2utf8(paste(title,md,note,sep="\n\n")),file.path(table_dir,paste0(stem,".md")),useBytes=TRUE)
 write_csv(z,"results","report_tables",paste0(stem,".csv"))
 tables[[length(tables)+1L]]<<-data.frame(number=number,title=title,source=source,
  markdown=paste0("results/report_tables/",stem,".md"),csv=paste0("results/report_tables/",stem,".csv"))
}
figure<-function(file,number,note="") {
 i<-match(file,figs$file);if(is.na(i))stop("Missing figure: ",file)
 caption<-paste0("图 ",number," ",figs$caption[i])
 parts<<-c(parts,paste0("![",caption,"](figures/",file,")\n\n",caption,"\n\n",note))
 html_parts<<-c(html_parts,paste0("<figure><a href='figures/",file,"'><img src='figures/",file,"' alt='",escape_html(caption),"' loading='lazy'></a><figcaption>",escape_html(caption),"</figcaption><p class='note'>",escape_html(note),"</p></figure>"))
 figs$report_number[i]<<-number
}
heading("首尔共享单车小时租借量预测研究",1)
text("纯R报告撰写材料：完整逐日验证、ARIMA日周结构补强和强基线。数据处理、模型训练、图表、报告生成和验收均由R执行，不调用其他分析语言。研究对象为每小时实际发生的租借量。")
heading("摘要建议")
text(sprintf("本文使用2017-12-01至2018-11-30的8,760个连续小时记录，保留295个非运营零值。初始训练为前八个月，8—9月完整逐日验证，10—11月固定历史测试比较。对%d个ARIMA类候选尝试完整验证，%d个完成、%d个记录失败；七组R ranger随机森林均以400棵树完成61个验证日。首次拟合后统一每周一重估，日内不读取真实租借量。",nrow(candidate_specs),nrow(candidate_metrics),nrow(failures)))
text(sprintf("全部测试小时中%s的RMSE最低，为%.3f；SARIMA、SARIMAX、R随机森林分别为%.3f、%.3f、%.3f。验证预先选出的最强基线为%s，其测试RMSE为%.3f。",all_metrics$model[1],all_metrics$RMSE[1],row_for("SARIMA24")$RMSE,row_for("SARIMAX24")$RMSE,row_for("RandomForest")$RMSE,strongest,row_for(strongest)$RMSE))
text("评价边界：原有10—11月测试结果已经查看。新配置仅按开发验证选择，新测试属于补充历史评价，不称为全新盲测。reference_v1/仅保留固定参照，最终R随机森林使用ranger重新训练，不能假定与早期其他实现得到相同数值。")
heading("第一章 概述与数据")
text("任务是在前一天23:00，利用起点前租借量、历史天气和可提前知道的日历信息，预测次日00:00—23:00。数据没有站点库存、容量或调度成本，不直接生成调度方案，也不还原停运期间未满足的潜在需求。")
text("数据来自UCI Seoul Bike Sharing Demand，DOI 10.24432/C5F62R，CC BY 4.0。官方页面：https://archive.ics.uci.edu/dataset/560/seoul+bike+sharing+demand 。原始ZIP、CSV与来源记录在data/raw/。日期按日/月/年解析，时区Asia/Seoul；保留完整小时轴，未插值、未删除停运零值。")
table("1-1","数据核验",read_csv("results","data_quality.csv"),"data_quality.csv")
table("1-2","描述统计",read_csv("results","descriptive_statistics.csv"),"descriptive_statistics.csv")
table("1-3","时间划分与停运分布",read_csv("results","split_information.csv"),"split_information.csv","测试非运营占10.31%，初始训练仅0.82%。运营标记未作为目标日输入。")
eda<-c("01_daily_overview.png","02_hourly_example.png","03_hourly_profiles.png","04_weekday_hour_heatmap.png","05_monthly_distribution.png","06_temperature_relationship.png","07_rainfall_relationship.png","08_operating_status.png")
for(i in seq_along(eda))figure(eda[i],paste0("1-",i),if(i%in%c(1,5,6,7))"全年展示用于描述；仅一年历史不能证明跨年周期，天气相关不能作因果解释。" else "")
heading("第二章 模型识别与完整验证")
text("ADF原假设为单位根，KPSS原假设为平稳。初始训练段检验存在分歧，使用168小时滞后或带宽后判断也会变化。结合差分、ACF/PACF、日周规律、训练窗口和完整验证选择模型，不能凭一个p值证明模型充分。")
sta<-read_csv("results","stationarity_tests.csv");sta$setting<-"ADF最大48/BIC；KPSS long"
sen<-read_csv("results","stationarity_sensitivity.csv");sen$setting<-"ADF固定168；KPSS带宽168"
table("2-1","平稳性检验与敏感性",rbind(sta,sen)[,c("series","setting","adf_statistic","adf_cv_5pct","adf_reject_unit_root_5pct","kpss_statistic","kpss_cv_5pct","kpss_reject_stationarity_5pct")],"stationarity_tests.csv;stationarity_sensitivity.csv")
identification<-c("09_mstl_training.png","10_training_distributions.png","11_acf_pacf_log1p.png","12_acf_pacf_diff24.png","13_acf_pacf_diff1_diff24.png","14_differencing_comparison.png")
for(i in seq_along(identification))figure(identification[i],paste0("2-",i))
text("取消仅覆盖星期三的初筛，有限候选均尝试完整61天验证。验证起始日先拟合，此后每周一重估；测试使用相同日历规则。周内ARIMA更新状态，随机森林使用最新历史特征，全部完成候选覆盖周一至周日。失败候选记录收敛、协方差或预设计算预算原因，不将超时等同于统计表现差。")
figure("31_validation_weekday_coverage.png","2-7","每个完成候选覆盖61日、1,464小时，各星期位置有8或9个预测日。")
table("2-2","全候选完整验证",candidate_metrics[order(candidate_metrics$model,candidate_metrics$RMSE),],"arima_validation_metrics.csv","仅用全部小时验证RMSE选择，MAE辅助；失败候选不参与排名。")
text("结构优化将周内日历及工作日类型交互与日周期随机误差结合，新增SARIMAX24。差分与回归项共同检查，避免24小时季节差分消除同周期Fourier项。窗口内全零、常量或差分后不可识别的回归列被删除，预测沿用相同列结构；无法估计的节假日效应不作额外解释。")
table("2-3","所选模型结构",selected,"selected_arima_specifications.csv","window_days=0表示扩展窗口。")
info<-read_csv("results","training_information_criteria.csv")
table("2-4","同口径信息准则",info[,c("candidate","comparison_group","training_n","AIC","AICc","BIC","rank_within_group","complete_validation")],"training_information_criteria.csv","只在变换、起止与差分相同的组内比较AICc。最终模型仍按完整验证误差选择。")
table("2-5","测试起点前完整参数",parameters[,setdiff(names(parameters),c("z","candidate"))],"selected_model_parameters.csv","标准误与p值为选模后条件渐近量，不作因果解释。完整参数可置于附录。")
equations<-character()
for(i in seq_len(nrow(selected))) {
 s<-selected[i,];co<-parameters$estimate[parameters$model==s$family];names(co)<-parameters$parameter[parameters$model==s$family]
 poly<-function(prefix,order,sign,period=1) {
  out<-"1";if(order>0)for(j in seq_len(order)){v<-sign*co[paste0(prefix,j)];out<-paste0(out,if(v>=0)" + " else " − ",sprintf("%.6f",abs(v))," B",if(j*period==1)"" else paste0("^",j*period))};paste0("(",out,")")
 }
 reg<-s$calendar!="none";target<-if(reg)"u_t" else "z_t"
 if(!reg&&"intercept"%in%names(co))target<-paste0("(z_t−",sprintf("%.6f",co["intercept"]),")")
 lhs<-paste0(poly("ar",s$p,-1),poly("sar",s$P,-1,24),if(s$d>0)paste0("(1−B)^",s$d)else"",if(s$D>0)paste0("(1−B^24)^",s$D)else"")
 scale<-if(s$transform=="log1p")"z_t=log(1+y_t)" else "z_t=y_t"
 if(reg)scale<-paste0(scale,"，z_t=",if("intercept"%in%names(co))sprintf("%.6f",co["intercept"])else"0","+x_t'β+u_t")
 line<-paste0(s$family,"：",scale,"；",lhs,target," = ",poly("ma",s$q,1),poly("sma",s$Q,1,24)," ε_t。B为一小时滞后，MA符号遵循forecast。")
 equations<-c(equations,line);text(line)
}
writeLines(enc2utf8(c("# 测试起点前估计方程","",paste(equations,collapse="\n\n"))),path_for("results","model_equations.md"),useBytes=TRUE)
table("2-6","残差诊断",diagnostic[,c("model","training_n","lag","statistic","p_value","acf10","acf24","acf168")],"selected_model_diagnostics.csv","p值使用卡方生存函数。小于数值精度而返回零的值展示为<1e-15。工程验收不能替代白噪声检验。")
table("2-7","残差结构的原版本参照",read_csv("results","residual_structure_comparison.csv"),"residual_structure_comparison.csv","各模型窗口与样本数可能不同，仅作诊断线索；两个开发起点的完整诊断另见training_diagnostics_two_origins.csv。")
table("2-8","同一新协议内的结构优化",read_csv("results","optimization_history.csv"),"optimization_history.csv","新SARIMAX与新协议下所选纯SARIMA比较，不将协议变化归因于单个特征。")
for(nm in unique(diagnostic$model)){q<-diagnostic[diagnostic$model==nm&diagnostic$lag==168,][1,];text(sprintf("%s：残差ACF(24)=%.3f、ACF(168)=%.3f；%s。",nm,q$acf24,q$acf168,if(q$p_value<.05)"168小时检验仍拒绝无自相关原假设，保留残余相关限制" else "该检验未拒绝无自相关，但不能证明独立正态"))}
for(i in 1:4)figure(c("23_selected_residual_series.png","24_selected_residual_acf.png","25_selected_residual_qq.png","33_residual_structure_comparison.png")[i],paste0("2-",i+7))
heading("第三章 短期预测与强基线")
text("Mean7使用过去七天同小时均值，Median7使用其中位数，DayTypeMean28使用过去28天同属工作日或周末的同小时均值，后者不单独分类节假日、不删除历史零值。全部源时间不超过预测起点，明细见baseline_source_time_audit.csv。")
defs<-data.frame(model=c("Naive24","Naive168","Mean7","Median7","DayTypeMean28"),definition=c("前一天同小时","前一周同小时","过去七天同小时均值","过去七天同小时中位数","过去28天同工作日/周末类型的同小时均值"),lookback_days=c(1,7,7,7,28))
table("3-1","五种透明基线",defs,"03_baselines.R")
table("3-2","基线与季节ARIMA测试比较",metrics_table[metrics_table$model%in%c(defs$model,"SARIMA24","SARIMAX24"),c("model","subset","n","MAE","RMSE","sMAPE","MASE168")],"test_model_comparison.csv")
text(sprintf("验证预先选择的最强基线为%s，验证RMSE=%.3f；SARIMAX相对该基线的测试RMSE变化为%+.2f%%。全部五个基线均报告，不按测试误差重新选参照。",strongest,baseline_validation$RMSE[1],100*(row_for("SARIMAX24")$RMSE/row_for(strongest)$RMSE-1)))
text("MAE/RMSE单位为次/小时，全部1,464小时是主要评价，正常运营1,313小时为事后补充。MASE分母仅取评价起点前168小时季节朴素历史误差，MASE大于1不等于比本测试段基线差。sMAPE对零值敏感，作为辅助。运营标记未作为目标日输入。")
text("点预测及区间下界统一截为非负。对数ARIMA使用高斯变换方差作均值调整；随机森林对数候选直接逆变换，不宣称原尺度条件均值。区间同时评价覆盖、宽度与Winkler评分。")
table("3-3","区间覆盖与宽度",coverage,"prediction_interval_coverage.csv")
for(i in 1:4)figure(c("19_sarima_prediction_intervals.png","20_dhr_prediction_intervals.png","30_sarimax_prediction_intervals.png","26_interval_coverage.png")[i],paste0("3-",i))
future<-read_csv("results","future_24h_forecasts.csv")
table("3-4","季节日历模型的末端24小时外推",future[future$model=="SARIMAX24",setdiff(names(future),"model")],"future_24h_forecasts.csv","日期2018-12-01，普通非节假日情景；没有未来真实值可核验，未输入未来实测天气或运营状态。")
figure("28_future_24h.png","3-5")
heading("第四章 R随机森林及模型比较")
text("随机森林是第四章可选的机器学习对照，课程没有点名要求它。本项目用R ranger实现，输入为24/48/168/336小时历史、过去七天同小时均值、起点前24/168小时统计、前一天曲线、日历及可选24小时前天气。七组配置在相同61天、相同400棵树下验证。")
text(sprintf("R随机森林选定%s：窗口%d天、最小叶样本数%d、候选特征比例%.1f、400棵树、最大深度20、种子%d；天气滞后使用情况为%s。模型保存在RDS中。",rf$id,rf$window,rf$leaf,rf$fraction,config$seed,rf$weather))
table("4-1","七组R随机森林完整验证",read_csv("results","rf_validation_metrics.csv"),"rf_validation_metrics.csv")
table("4-2","所选模型与基线验证比较",validation[,c("model","subset","n","MAE","RMSE","sMAPE","MASE168")],"validation_model_comparison.csv")
figure("15_validation_comparison.png","4-1")
table("4-3","固定历史测试完整比较",metrics_table[order(metrics_table$subset,metrics_table$RMSE),],"test_model_comparison.csv","最强基线先在验证段选定；新测试属于补充历史评价。")
for(i in 1:8)figure(c("16_test_comparison.png","32_stronger_baselines.png","17_test_typical_week.png","18_test_closures.png","21_errors_by_horizon.png","22_errors_by_day_type.png","27_rf_feature_importance.png","29_daily_test_errors.png")[i],paste0("4-",i+1),if(i==5)"固定起点使步长与目标小时对应，曲线同时反映日内时段难度。" else "")
table("4-4","不同时段误差",read_csv("results","test_errors_by_time_of_day.csv"),"test_errors_by_time_of_day.csv")
table("4-5","运营状态分组误差",read_csv("results","test_errors_by_operating_status.csv"),"test_errors_by_operating_status.csv","非运营小时占10.31%，事后分组不意味着能提前知道停运。")
reference<-read_csv("results","v1_v2_test_reference_comparison.csv")
table("4-6","原版本与新版本固定参照",reference[,c("model","RMSE_v1","RMSE_v2")],"v1_v2_test_reference_comparison.csv","验证更新日、候选覆盖和随机森林实现变化，差异不能归因于单一因素；不按测试结果保留方案。")
old_rmse<-function(id)reference$RMSE_v1[reference$model==id]
text(sprintf("本轮改善的具体范围：普通ARIMA测试RMSE由%.3f降至%.3f（下降%.2f%%），纯SARIMA由%.3f降至%.3f（下降%.2f%%），DHR未变化。这些是同一固定历史测试段的描述性差异，不能分离候选覆盖、重估日和模型配置各自的贡献，也没有据此证明在其他时期显著更优。",old_rmse("ARIMA"),row_for("ARIMA")$RMSE,100*(1-row_for("ARIMA")$RMSE/old_rmse("ARIMA")),old_rmse("SARIMA24"),row_for("SARIMA24")$RMSE,100*(1-row_for("SARIMA24")$RMSE/old_rmse("SARIMA24"))))
text(sprintf("结构补强没有带来全面提升：SARIMAX相对纯SARIMA的完整验证RMSE下降%.2f%%，但测试RMSE为%.3f，高于纯SARIMA的%.3f（增加%.2f%%）。因此保留SARIMAX作为有收益也有局限的实验，不能将其宣称为优于SARIMA的最终答案。",100*(1-validation$RMSE[validation$model=="SARIMAX24"&validation$subset=="all"]/validation$RMSE[validation$model=="SARIMA24"&validation$subset=="all"]),row_for("SARIMAX24")$RMSE,row_for("SARIMA24")$RMSE,100*(row_for("SARIMAX24")$RMSE/row_for("SARIMA24")$RMSE-1)))
text(sprintf("强基线改变了结论的力度：七日同小时均值的测试RMSE为%.3f，优于两个季节ARIMA；验证预选的%s在测试段MAE为%.3f，也低于R随机森林的%.3f。R随机森林虽然全小时RMSE最低，但只比验证预选基线降低%.2f%%、比七日均值降低%.2f%%，且其早期与本轮实现不同，不能把前后差值全部归因于模型优化。不同损失函数的排名应分别解释。",row_for("Mean7")$RMSE,strongest,row_for(strongest)$MAE,row_for("RandomForest")$MAE,100*(1-row_for("RandomForest")$RMSE/row_for(strongest)$RMSE),100*(1-row_for("RandomForest")$RMSE/row_for("Mean7")$RMSE)))
text(sprintf("点预测改善也不等于区间已校准：纯SARIMA与SARIMAX的名义95%%区间在全部测试小时仅覆盖%.2f%%和%.2f%%。结合残差相关，区间仍存在覆盖不足，不能只展示一周图形就宣称不确定性估计可靠。",100*coverage$coverage[coverage$model=="SARIMA24"&coverage$subset=="all"&coverage$level==95],100*coverage$coverage[coverage$model=="SARIMAX24"&coverage$subset=="all"&coverage$level==95]))
heading("第五章 课程总结与局限")
text("本轮完成完整逐日验证、ARIMA日周结构补强与更强基线；语言统一为R。仍需保留：仅一年历史、秋季测试的范围限制、未知停运、没有真实天气预报、时间相关误差、残余相关和重尾对推断与区间的影响。没有使用独立样本假设证明模型普遍更优。")
text("高阶候选因收敛、协方差或计算预算失败时记录为未完成，不证明统计表现差。当前仍是有限候选集合，后续研究需固定规则，避免反复依据已经查看的测试段调整。")
heading("参考文献")
for(s in c("[1] UCI. Seoul Bike Sharing Demand. DOI:10.24432/C5F62R. https://archive.ics.uci.edu/dataset/560/seoul+bike+sharing+demand","[2] Hyndman RJ, Athanasopoulos G. Forecasting: Principles and Practice. https://otexts.com/fpp3/","[3] forecast documentation. https://pkg.robjhyndman.com/forecast/","[4] Breiman L. Random Forests. Machine Learning. 2001;45:5–32.","[5] Wright MN, Ziegler A. ranger. Journal of Statistical Software. 2017;77(1). https://doi.org/10.18637/jss.v077.i01"))text(s)
heading("附录 纯R复现与材料使用")
text("运行Rscript --vanilla scripts/run_all.R或run.ps1。全部实验、报告和验收仅依赖R；主要包为forecast、ggplot2、urca、ranger和jsonlite。模型以RDS保存，软件版本见r_package_versions.csv及r_session_info.txt。候选起点拟合对象按代码、数据和协议签名缓存。")
text("report_ready.html离线查看，report_materials.md可编辑，results/report_tables/提供表格，report_figure_manifest.csv提供图号。原结果参照、协议、星期覆盖、删列记录和时间边界审计在results/。正文可精选图表，完整参数等放附录。")
write_csv(do.call(rbind,tables),"results","table_manifest.csv");write_csv(figs,"results","report_figure_manifest.csv")
writeLines(enc2utf8(paste(parts,collapse="\n\n")),path_for("report_materials.md"),useBytes=TRUE)
style<-"body{font-family:'Microsoft YaHei',sans-serif;color:#203340;line-height:1.75;background:#f2f5f7;margin:0}main{max-width:1140px;margin:30px auto;background:white;padding:45px 55px}h2{margin-top:45px;border-bottom:1px solid #d9e2e7;padding-bottom:8px}figure{margin:32px 0}img{width:100%;height:auto}figcaption{font-weight:bold;text-align:center;margin:10px}.scroll{overflow:auto}table{border-collapse:collapse;font-size:13px;white-space:nowrap;width:100%}th,td{border:1px solid #d9e2e7;padding:8px 10px;text-align:right}th{background:#edf3f7}td:first-child{text-align:left}.note{font-size:13px;color:#52616b}@media print{main{margin:0;padding:0}figure{break-inside:avoid}}"
html<-paste0("<!doctype html><html lang='zh-CN'><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'><title>纯R时间序列报告材料</title><style>",style,"</style><main>",paste(html_parts,collapse="\n"),"</main></html>")
writeLines(enc2utf8(html),path_for("report_ready.html"),useBytes=TRUE)
message("Pure-R report: ",nrow(figs)," figures, ",length(tables)," tables.")
