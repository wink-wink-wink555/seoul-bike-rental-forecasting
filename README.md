# 首尔共享单车小时租借量预测

使用 UCI Seoul Bike Sharing Demand 的 8,760 条连续小时记录，预测次日 24 小时实际租借量。数据处理、建模、图表、报告材料和独立核验全部使用 R。

## 报告材料

- `report_ready.html`：离线查看五章材料、实际结果表和全部图表。
- `report_materials.md`：可编辑的报告撰写材料。
- [实验过程详解.md](实验过程详解.md)：逐步解释实验设计、模型、困难处理、结果解读与报告写法。
- `results/report_tables/`：中文 CSV 与 Markdown 表格。
- `figures/`：300 DPI PNG 图表；`results/report_figure_manifest.csv` 提供报告图号。
- `results/validation_summary.md`：独立工程核验结果。核验通过不代表所有统计假设成立。

材料如实报告强基线、模型失败和残差相关，不保证复杂模型优于简单方法。已有测试段结果已经查看，本轮属于补充历史评价，不能称作全新盲测。

## 实验口径

初始训练为 2017-12-01 至 2018-07-31（5,832 小时），验证为 2018-08-01 至 2018-09-30（1,464 小时），固定历史测试为 2018-10-01 至 2018-11-30（1,464 小时）。

每次在前一天 23:00 预测次日全部 24 小时。全部候选尝试完整 61 天验证，取消只覆盖某一星期位置的初筛。起始拟合后统一每周一重估，周内 ARIMA 更新状态，随机森林使用最新已知历史特征。模型仅按全部小时的验证 RMSE 选择。失败候选单列原因，不把超时当作预测表现差。

模型包括普通 ARIMA、24 小时 SARIMA、Fourier 日周结构加 ARIMA 误差、日周日历加季节 ARIMA 误差，以及 R `ranger` 随机森林。随机森林是第四章的可选机器学习比较，课程没有点名要求它。五个简单基线为前日同期、前周同期、过去七日同小时均值、中位数、过去 28 日同工作日/周末类型均值。最强基线先由验证段选定，测试段报告全部基线。

保留全部 295 个非运营零值与连续小时轴。主要评价覆盖全部小时，正常运营子集仅作事后诊断。模型不输入目标日实测天气和运营状态。天气候选只使用目标小时前 24 小时已观测天气；所有输入源时间均不晚于预测起点。MASE 的周周期缩放分母仅由相应评价开始前历史计算。

末端外推是 **2018-12-01** 的 24 小时，并设定普通非节假日情景。只有一年数据，不能据此确认跨年周期稳定。

## 运行

使用 R，主要包为 `forecast`、`ggplot2`、`urca`、`ranger`、`jsonlite`。实际版本见 `results/r_package_versions.csv` 和 `results/r_session_info.txt`。项目 `.Rlib/` 中提供已安装的新增包；`data/r_packages/` 留存适用于本机 R 4.6 的 Windows 包归档，初始化时可按需安装。其他 R 版本或操作系统应安装相应版本的这五个包。

```powershell
Rscript --vanilla scripts/run_all.R
```

也可运行 `run.ps1`。分步运行示例：

```powershell
Rscript --vanilla scripts/run_all.R --steps=6,7,8
```

步骤依次为数据准备、探索图表、平稳性、ARIMA 完整验证及结构补强、R 随机森林、最终评价、报告材料、独立核验。后续步骤要求已有前置结果。首次完整建模耗时较长；相同代码、数据与协议签名复用 RDS 缓存。未完成候选再次运行会重试。模型 RDS 在 `results/models/`，运行日志在 `logs/`。

普通复现使用本地固定官方快照并校验 MD5，不联网覆盖数据。若本地 CSV 缺失，`fetch_data.R` 优先从项目原始 ZIP 恢复，再按需从 UCI 下载。

## R 代码

- `00_setup.R`：协议、软件依赖、工具与绘图规范。
- `01_data_preparation.R`：官方快照核验、小时轴、字段和停运记录。
- `02_exploratory_analysis.R`：描述统计及日内、周内、天气、运营图表。
- `03_stationarity_analysis.R`：训练段 ADF/KPSS、敏感性、差分和 ACF/PACF。
- `03_baselines.R`：五个基线及源时间审计。
- `04_model_engine.R`：拟合、状态更新、预测、缓存、并行候选及诊断。
- `04_arima_experiments.R`、`04_refinement.R`：完整验证、结构补强、选模和失败记录。
- `05_rf_engine.R`、`05_random_forest.R`：R ranger 特征、完整验证、预测与 RDS 保存。
- `06_final_evaluation.R`：共同测试段、区间、诊断、强基线与末端外推。
- `07_report_materials.R`、`07_report_builder.R`：从实际结果生成报告材料、表格与离线 HTML。
- `08_validate_outputs.R`、`08_full_checks.R`：独立重算指标、基线、时间边界、RDS 及产物一致性。
- `fetch_data.R`、`run_all.R`：数据恢复与完整入口。

以上脚本均在 `scripts/` 中。

## 数据来源

UCI Machine Learning Repository. *Seoul Bike Sharing Demand*. DOI: [10.24432/C5F62R](https://doi.org/10.24432/C5F62R)，CC BY 4.0。[官方页面](https://archive.ics.uci.edu/dataset/560/seoul+bike+sharing+demand)。官方 ZIP、原始 CSV、获取时间与哈希均在 `data/raw/`。
