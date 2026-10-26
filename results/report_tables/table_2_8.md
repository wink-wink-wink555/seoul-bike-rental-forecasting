表 2-8 同一新协议内的结构优化

| family | initial_candidate | final_candidate | initial_validation_RMSE | final_validation_RMSE | change_percent | reason |
| --- | --- | --- | --- | --- | --- | --- |
| ARIMA | A_raw_312_84d | A_raw_312_84d | 667.475 | 667.475 | 0.000 | Same full-daily Monday-refit protocol; selection uses validation RMSE only |
| DHR_ARIMA | D_raw_202_K5_weekend_84d_refined | D_raw_202_K5_weekend_84d_refined | 500.223 | 500.223 | 0.000 | Same full-daily Monday-refit protocol; selection uses validation RMSE only |
| SARIMA24 | S_raw_101_011_full | S_raw_101_011_full | 486.807 | 486.807 | 0.000 | Same full-daily Monday-refit protocol; selection uses validation RMSE only |
| SARIMAX24 | S_raw_101_011_full | C_raw_202_101_fullcalendar_84d | 486.807 | 478.759 | -1.653 | Same full-daily Monday-refit protocol; selection uses validation RMSE only |

新SARIMAX与新协议下所选纯SARIMA比较，不将协议变化归因于单个特征。
