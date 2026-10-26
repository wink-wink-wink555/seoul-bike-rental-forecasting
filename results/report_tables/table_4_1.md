表 4-1 七组R随机森林完整验证

| 模型 | 评价范围 | 小时数 | MAE | RMSE | sMAPE（%） | MASE（周周期缩放） | 平均预测偏差 | 候选编号 |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| RandomForest | all | 1464 | 292.376 | 471.183 | 41.149 | 1.165 | 7.114 | RF_raw_84_leaf5_calendar |
| RandomForest | operating | 1368 | 246.545 | 393.287 | 30.002 | 0.982 | -58.736 | RF_raw_84_leaf5_calendar |
| RandomForest | all | 1464 | 291.322 | 470.005 | 41.005 | 1.161 | 9.320 | RF_raw_84_leaf5_weather |
| RandomForest | operating | 1368 | 245.888 | 393.740 | 29.848 | 0.980 | -55.903 | RF_raw_84_leaf5_weather |
| RandomForest | all | 1464 | 298.119 | 475.639 | 41.251 | 1.188 | 8.504 | RF_raw_84_leaf15_weather |
| RandomForest | operating | 1368 | 250.735 | 395.755 | 30.111 | 0.999 | -59.204 | RF_raw_84_leaf15_weather |
| RandomForest | all | 1464 | 305.903 | 496.271 | 41.853 | 1.219 | 4.003 | RF_raw_56_leaf5_weather |
| RandomForest | operating | 1368 | 256.163 | 407.218 | 30.755 | 1.021 | -66.923 | RF_raw_56_leaf5_weather |
| RandomForest | all | 1464 | 290.878 | 482.829 | 40.425 | 1.159 | 26.981 | RF_raw_112_leaf5_weather |
| RandomForest | operating | 1368 | 242.402 | 398.070 | 29.227 | 0.966 | -40.014 | RF_raw_112_leaf5_weather |
| RandomForest | all | 1464 | 289.156 | 473.033 | 39.364 | 1.152 | 44.308 | RF_raw_full_leaf15_weather |
| RandomForest | operating | 1368 | 235.982 | 378.599 | 28.091 | 0.940 | -26.049 | RF_raw_full_leaf15_weather |
| RandomForest | all | 1464 | 314.273 | 506.834 | 45.763 | 1.252 | -75.501 | RF_log_84_leaf5_weather |
| RandomForest | operating | 1368 | 283.020 | 462.639 | 34.939 | 1.128 | -134.106 | RF_log_84_leaf5_weather |


