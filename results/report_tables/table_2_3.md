表 2-3 所选模型结构

| id | family | p | d | q | P | D | Q | period | 变换 | 回看天数（0=扩展） | daily_K | weekly_K | weather_lag24 | weekend_interactions | calendar |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| A_raw_312_84d | ARIMA | 3 | 1 | 2 | 0 | 0 | 0 | 1 | raw | 84 | 0 | 0 | 否 | 否 | none |
| D_raw_202_K5_weekend_84d_refined | DHR_ARIMA | 2 | 0 | 2 | 0 | 0 | 0 | 1 | raw | 84 | 5 | 3 | 否 | 是 | fourier |
| S_raw_101_011_full | SARIMA24 | 1 | 0 | 1 | 0 | 1 | 1 | 24 | raw | 0 | 0 | 0 | 否 | 否 | none |
| C_raw_202_101_fullcalendar_84d | SARIMAX24 | 2 | 0 | 2 | 1 | 0 | 1 | 24 | raw | 84 | 5 | 3 | 否 | 是 | fourier |

window_days=0表示扩展窗口。
