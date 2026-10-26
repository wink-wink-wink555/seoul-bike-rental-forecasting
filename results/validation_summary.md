# 实验验收与复核记录

- 状态：**全部通过**
- 生成时间：2026-10-26 13:38:56 CST
- R：R version 4.6.1 (2026-06-24 ucrt)

| 验收项目 | 结果 | 证据或未通过原因 |
|---|---|---|
| FRED 来源、序列元信息和快照版本均有记录 | 通过 | IPG2211A2N; Monthly; Index 2017=100; retrieved_on=2026-10-20; Mixed source components; same revision vintage not verified |
| 官方一致版本刷新状态或混合快照限制已如实记录 | 通过 | NON-FATAL refresh failure on 2026-10-26: cannot open URL 'https://fred.stlouisfed.org/graph/fredgraph.csv?id=IPG2211A2N'; retained the original local snapshot Mixed source components; same revision vintage not verified |
| 本次原始 FRED 快照文件真实存在且格式正确 | 通过 | IPG2211A2N_fred_snapshot_2000_2026-09.csv; bytes=6607 |
| 处理数据覆盖固定窗口且月份连续、无缺失和重复 | 通过 | 321 2000-01-01 2026-09-01 |
| 清洗数据逐月对应本地原始官方快照 | 通过 | 固定分析窗口的日期和值逐行一致 |
| 核心分析、模型、预测及预测区间结果均已保存 | 通过 |  |
| R 模块脚本和统一入口完整 | 通过 |  |
| 官方数据刷新固定窗口校验失败时保留本地快照继续运行 | 通过 | 仅接受 FRED 官方 CSV；完整性校验通过后存档输出；失败非致命 |
| 原序列和季节差分序列 PACF(1)=ACF(1)，滞后连续并按 lag 对齐 | 通过 | original PACF(1)-ACF(1)=1.11e-16; seasonal PACF(1)-ACF(1)=0; each saved table has lags 1-24 |
| 报告/结果模型阶数与训练 SARIMA 模型对象一致 | 通过 | SARIMA(2,0,1)(0,1,1)[12]; AICc=1440.642 |
| SARIMA 候选按训练段 AICc 与 Ljung-Box 规则可复算 | 通过 | selected=auto.arima（训练段逐步搜索辅助候选）; candidate count=8 |
| SARIMA 参数估计有限并与拟合对象一致 | 通过 | ar1, ar2, ma1, sma1, drift |
| 模型残差有限且 Ljung-Box 统计量由 RDS 残差重新计算相符 | 通过 | lag=24; fitdf=4; p=0.095398 |
| 訓練和測試時間不重疊，四模型預測對齊相同實際月份 | 通过 | train through 2024-09-01; test 2024-10-01 to 2026-09-01; n=24 |
| 四模型 MAE/RMSE/sMAPE/MASE 由测试实际值重算一致 | 通过 | ETS; SARIMA; NNAR; 季节性朴素 |
| SARIMA 预测区间上下界有限、合法且 95% 区间包含 80% 区间 | 通过 | 24 个预测点均满足 lo95 ≤ lo80 ≤ hi80 ≤ hi95 |
| 80%/95% 预测区间覆盖率按含边界定义重算一致 | 通过 | 80%: 16/24 = 66.667%；95%: 20/24 = 83.333% |
| 全样本模型的未来 12 个月日期连续、预测有效且阶数一致 | 通过 | 2026-10-01 2027-09-01 |
| 所有要求图片均为可读 PNG 且由 300 DPI 输出设置生成 | 通过 | 16/16 images have valid PNG headers and dimensions |
| 报告保留完整章节结构、参考文献和代码附录 | 通过 | 课程要求的章节标题均可检出 |
| 图表编号唯一连续，正文每个图表引用均有对应标题 | 通过 | figures=1-1,1-2,1-3,2-1,2-2,2-3,2-4,2-5,2-6,2-7,2-8,3-1,4-1,4-2,4-3,4-4; tables=1-1,2-1,2-2,2-3,3-1,3-2,4-1,4-2,4-3 |
| 报告图号和图像文件逐一对应 | 通过 | 16 formal figure references; missing:  |
| report_preview 正文与 report.md 完全一致且图片链接有效 | 通过 | 16 图片链接；去除图片语法后正文逐行一致 |
| 报告摘要、参数识别和预测中的 SARIMA 阶数与模型结果一致 | 通过 | SARIMA(2,0,1)(0,1,1)[12] |
| 报告描述统计、检验、候选模型、参数、识别和 NNAR 配置表与结果 CSV 一致 | 通过 | 各自动生成表格的数值与对应结果文件逐行匹配 |
| 报告四模型误差表和模型优劣结论与 CSV 重算结果一致 | 通过 | 四项指标逐模型精确核对，排名按误差值重算 |
| 报告预测区间覆盖率表与覆盖率 CSV 一致 | 通过 | 名义水平、覆盖数、样本数、覆盖率和平均宽度均匹配 |
| 报告未来 12 个月点预测与预测区间逐月对应 CSV | 通过 | 12 个月全部数值一致 |
| NNAR 实际设置和训练/测试样本数已保存 | 通过 | NNAR(11,1,6)[12]; train=297; test=24; repeats=30; seed=20261020 |
| 参考文献编号完整且序列数据来源可追溯 | 通过 | [1] 至 [7] 可检出 |
| README、R Project 和 PowerShell 运行入口存在 | 通过 | README.md / Assginment-TimeSeries.Rproj / run.ps1 |
| R 及分析依赖可用并记录版本 | 通过 | R version 4.6.1 (2026-06-24 ucrt) forecast=9.0.2; ggplot2=4.0.3; urca=1.3.4 |

说明：全部通过仅在所有必需计算型检查均完成时写出；存在关键失败时脚本会在保存此摘要后返回非零状态。
