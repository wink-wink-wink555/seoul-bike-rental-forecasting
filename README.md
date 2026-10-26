# 时间序列分析课程大作业

本项目研究美国电力与燃气公用事业工业生产指数，围绕季节性 ARIMA 建模完成数据描述、平稳性判断、模型识别、参数估计、残差诊断、样本外预测和预测评估，并以 ETS、NNAR 和季节性朴素方法作对照。研究对象是工业生产指数，不是实际发电量、用电量或能源消费量。

## 数据与研究窗口

- FRED 序列：`IPG2211A2N`
- 频率：月度；单位：Index 2017=100；季节调整：未经季节性调整
- 固定分析窗口：2000-01 至 2026-09，共 321 个观测
- 原始本地快照：`data/raw/IPG2211A2N_fred_snapshot_2000_2026-09.csv`
- 官方页面：[FRED 序列说明](https://fred.stlouisfed.org/series/IPG2211A2N)；[FRED 官方数据表](https://fred.stlouisfed.org/data/IPG2211A2N)

现存原始快照由 FRED 官方表格页截至 2026-08 的记录和官方序列页显示的 2026-09 观测值组成。两者来源均为 FRED，但未能证明属于同一修订时点。2026-10-26 对官方完整 CSV 的下载尝试因当前网络无法连接而失败；因此本次保留原始数据及其限制说明，未使用第三方镜像或人工补值。详细记录见 `data/raw/SOURCE_NOTES.txt` 和 `results/data_metadata.csv`。

## 运行

需要 R 和 `forecast`、`ggplot2`、`urca` 包。先确认 `Rscript` 可在当前 PowerShell 中找到：

```powershell
Get-Command Rscript
Rscript scripts/run_all.R
```

也可以使用项目入口：

```powershell
.\run.ps1
```

如果当前 PowerShell 执行策略阻止未签名脚本，可用一次性进程级设置运行入口，不会更改系统执行策略：

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File .\run.ps1
```

普通运行使用已保存的本地快照，不联网刷新。显式请求 FRED 官方 CSV 刷新：

```powershell
Rscript scripts/run_all.R --refresh-data
# 或
.\run.ps1 -RefreshData
# 若执行策略阻止脚本，可用进程级设置：
powershell -NoProfile -ExecutionPolicy Bypass -File .\run.ps1 -RefreshData
```

刷新只会接受一次官方 CSV 响应中完整覆盖固定分析窗口、日期连续且数值有限的数据。成功时保存新的时间戳原始快照并归档旧结果；若官方连接失败或校验不通过，则记录原因、保留原始快照并继续运行。分析窗口在 `scripts/00_setup.R` 集中配置，不会因新增月份自动延长。

## 实验流程

统一入口按以下次序运行：

1. `scripts/00_setup.R`：集中设置研究窗口、训练测试期、季节周期、NNAR 配置、依赖包和绘图输出。
2. `scripts/01_data_preparation.R`：校验 FRED CSV，生成清洗数据、数据质量检查和快照元信息。
3. `scripts/02_exploratory_analysis.R`：描述性统计、月份特征、原序列与 STL 分解。
4. `scripts/03_stationarity_analysis.R`：ADF/KPSS、差分、ACF/PACF 和相关表。ACF、PACF 分别按实际 lag 值绘图并显式校验 lag 1 与连续滞后。
5. `scripts/04_arima_modeling.R`：训练段 SARIMA 候选、AICc 比较、参数估计和残差诊断。
6. `scripts/05_neural_network_model.R`：按固定种子训练 NNAR。
7. `scripts/06_forecast_comparison.R`：四模型在共同测试期比较、预测区间覆盖率、全样本 SARIMA 未来 12 个月预测。
8. `scripts/08_generate_report_preview.R`：从最新 CSV 更新报告表格和模型排名文字，再生成带图预览。
9. `scripts/09_validate_outputs.R`：重新计算关键统计量、误差、区间覆盖率、图表编号及报告对应关系；关键检查失败时返回非零状态。

最近 24 个月作为按时间顺序留出的独立测试集。SARIMA 候选识别和 NNAR 拟合只使用训练段；各对照模型使用相同测试月份。误差指标包括 MAE、RMSE、sMAPE、MASE。SARIMA 测试区间采用包含上下界的定义计算 80%/95% 实际覆盖率。

## 输出

- 清洗数据：`data/processed/IPG2211A2N_clean.csv`
- 统计表、模型对象、预测与诊断：`results/`
- 80%/95% 预测区间覆盖率：`results/prediction_interval_coverage.csv`
- 图片：`figures/`（由统一绘图函数以 300 DPI PNG 输出）
- 正式报告：`report.md`
- 带图预览：`report_preview.md`
- 验收摘要：`results/validation_summary.md`
- 每次运行日志：`logs/`

## 目录

```text
data/raw/          官方来源快照与版本说明
data/processed/    校验后的固定分析窗口
scripts/           实验模块、统一入口和验收
figures/           300 DPI PNG 图表
results/           CSV、RDS、TXT 结果和验收摘要
logs/              每次流水线运行记录
```
