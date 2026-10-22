# 时间序列分析课程大作业

本项目基于美国联邦储备委员会的电力与燃气公用事业工业生产指数，完成描述性分析、平稳性检验、SARIMA 定阶与诊断、NNAR 和 ETS 预测比较，以及全样本未来 12 个月预测。研究对象是**工业生产指数**，不是实际发电量、用电量或能源消费量。

## 模板依据

报告封面和章节结构依据项目根目录的《时间序列分析-课程报告（本科）.docx》。实际可见封面包含上海电力大学、课程报告、题目、院系/专业、课程名称、课程编号、姓名、学号、任课教师和报告日期；模板原题为“基于美国月度失业率数据的时间序列分析报告”，姓名为 XXX、学号空白，课程编号为 2800251.01，任课教师为朱威，报告日期为 2026 年 11 月 1 日。报告按用户指定研究对象更新题目，并用用户给出的姓名孙宜飞、学号 20231822 填入封面，保留模板日期供核对。章节沿用概述、模型识别、模型预测、NNAR 模型及其预测、课程总结、参考文献和附录。

## 数据

- FRED 序列：IPG2211A2N，月度、未经季节性调整，指数基期 2017=100。
- 来源：Board of Governors of the Federal Reserve System (US)，经 Federal Reserve Bank of St. Louis 的 FRED 提供。
- 官方页面：[序列说明](https://fred.stlouisfed.org/series/IPG2211A2N)；[表格数据](https://fred.stlouisfed.org/data/IPG2211A2N)。
- 本地快照覆盖 2000-01 至 2026-09，共 321 个观测。2026-09 的 114.8603 来自 FRED 序列页面于 2026-10-16 显示的最新观测；表格数据页快照可见至 2026-08。
- 本机 PowerShell 直连 fredgraph.csv 返回“无法连接到远程服务器”。因此 data/raw/ 保存的是从 FRED 官方表格页提取的 2000-01 至 2026-08 记录，加上官方序列页显示的 2026-09 观测，不是臆造或第三方镜像。来源和限制记录在 data/raw/SOURCE_NOTES.txt 与 results/data_metadata.csv。网络直连刷新仍受当前环境限制。
- 原始文件：data/raw/IPG2211A2N_fred_snapshot_2000_2026-09.csv。后续默认读取本地快照；不会在普通运行时联网更新。
- 清洗脚本检查重复日期、非数值、缺失值、月度连续性，不做插值或异常值删除；处理结果保存到 data/processed/。

显式刷新命令为：

    .\run.ps1 -RefreshData
    Rscript scripts/run_all.R --refresh-data

程序会尝试从 FRED CSV 下载到带时间戳的新快照，并在覆盖既有结果前将旧结果、图片、处理数据及报告复制到 archive/before_refresh_时间戳/。刷新依赖本机网络可访问 FRED；下载或校验失败时会停止并保留原快照。

## 环境与依赖

- Windows PowerShell；R 4.6.1 (2026-06-24 ucrt)。
- 项目运行所需主要 R 包：forecast、ggplot2、urca。R 4.6.1 环境中已检查到 forecast 9.0.2、ggplot2 4.0.3、urca 1.3-4。
- 程序发现缺失包时会尝试安装至项目内 .Rlib/。如果 CRAN 不可访问，请在 RStudio 的 Packages 面板或 R 控制台安装上述包，再运行入口。
- 统计检验由 urca 的 ADF/KPSS 实现；报告中以 urca 给出的统计量和临界值为准，p 值按相邻临界值线性插值并明确标为近似区间。tseries 不属于本项目依赖。
- 图形以 300 DPI PNG 输出，字体指定为系统 Microsoft YaHei；交互式 RStudio 运行时会将图同时送到 Plots 面板。

## 目录结构

    .
    ├── 时间序列分析-课程报告（本科）.docx
    ├── Assginment-TimeSeries.Rproj
    ├── README.md
    ├── report.md
    ├── report_preview.md
    ├── run.ps1
    ├── data/raw/                 # 固定原始快照与来源说明
    ├── data/processed/           # 校验后的月度数据
    ├── scripts/                  # 清洗、探索、检验、建模、验证
    ├── figures/                  # 运行生成的 PNG 图片
    ├── results/                  # CSV、RDS、TXT 结果及验收摘要
    └── logs/                     # 每次运行日志

## 一键运行与 RStudio

在 PowerShell 中进入项目目录后运行：

    .\run.ps1

如果 PowerShell 执行策略阻止未签名的本地脚本，可用仅对该次子进程生效的方式启动，不会修改系统策略：

    powershell.exe -NoProfile -ExecutionPolicy Bypass -File .\run.ps1

也可直接使用已经定位的 Rscript：

    & 'D:\R\R-4.6.1\bin\Rscript.exe' --vanilla scripts/run_all.R

在 RStudio 中双击 Assginment-TimeSeries.Rproj 打开项目，打开 scripts/run_all.R，点击 Source（或在 Console 输入 source("scripts/run_all.R", encoding="UTF-8")）。项目脚本使用项目根路径拼接绝对文件路径，不依赖其他会话的 setwd()。交互运行时可在 Plots 面板查看图，也可从 figures/ 打开 PNG。

要复现模型比较，先保留固定本地快照并运行统一入口。训练集为 2000-01 至 2024-09，最近 24 个月（2024-10 至 2026-09）用于严格按时间顺序的固定起点测试。SARIMA 候选阶数与 auto.arima() 只使用训练数据；auto.arima() 使用逐步搜索，NNAR 只拟合训练集且设置固定随机种子；四模型均对齐同一测试月份。

## 输出位置

- 统计表格和模型比较：results/。
- 预测值和未来 12 个月预测区间：results/test_predictions.csv、results/future_12_month_forecast.csv。
- 模型对象：results/*.rds。
- PNG 图表：figures/。
- 正式中文占位符报告：report.md。
- 支持 Markdown 预览的版本：report_preview.md。
- 验收逐项结果：results/validation_summary.md。
- 程序标准输出、警告和错误记录：logs/。

## 已知运行环境问题

本机 Rscript 可执行文件位于 D:\R\R-4.6.1\bin\Rscript.exe，无需修改系统环境变量。当前 PowerShell 策略会阻止直接运行未签名的 .\run.ps1；本项目包装脚本已通过 `powershell.exe -NoProfile -ExecutionPolicy Bypass -File .\run.ps1` 在进程级临时策略下验证，未修改系统策略。也可直接调用上文的 Rscript 命令。PowerShell 无法直连 FRED CSV 下载地址，已通过 FRED 官方页面构成本地可核验快照，并在数据元信息中说明。首次对 tseries 的安装尝试因该包不在系统库且 CRAN 索引无法访问而失败；该包并非依赖，当前分析改用已经安装的 urca。R 启动时报告 C.UTF-8 locale 不可用；为保证 Windows 下中文 R 源码可执行，脚本尝试切换到简体中文 Windows locale，R 会提示当前代码页不是 UTF-8。CSV 与验收摘要通过显式 UTF-8 编码读写；生成的中文 PNG 已目视检查，字符正常。该 locale 提示未影响统计计算或图表输出，运行日志保留警告供检查。
