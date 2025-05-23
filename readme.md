# London Housing Price Forecast

## Overview
This project presents a time series analysis and forecasting of average housing prices in London using the ARIMA and seasonal ARIMA (SARIMA) models. The analysis covers pre-processing, stationarity testing, model identification, estimation, validation, and forecasting for the next 12 months.


## 📊 Data
The dataset is sourced from the UK House Price Index and loaded from an Excel file (https://data.london.gov.uk/dataset/uk-house-price-index). The project focuses specifically on London’s monthly average housing prices from 1995 onward.

## Methodology
1. Pre-processing
- Log transformation for variance stabilization
- Differencing to achieve stationarity

2. Modeling
- Evaluated ARIMA models using ACF/PACF
- Validation
- Residual checks
- Causality & invertibility

3. Forecasting
- Train/test split for forecast accuracy
- 12-month forecast with 80% & 95% confidence intervals

Best model: ARIMA(2,2,1)(1,0,1)[12]



