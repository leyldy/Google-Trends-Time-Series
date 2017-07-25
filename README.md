# Google-Trends-Time-Series
- Time Series Modeling and Prediction for Various Google Trends
- Author: Jong Ha Lee & Binyi Cai
- [Detailed Report on Sample Dataset (Q5)](https://github.com/leyldy/Google-Trends-Time-Series/blob/master/report/STAT153_MT2_Report.pdf)

## 0. Project Description
Utilizing various time series modeling techniques to predict Google Trends weekly time series data. R was the primary method of analysis and forecasting via `forecast`, `tseries` and `stats` packages.
<br />
<br />


## 1. Data and Corresponding Google Trends Searches
The data in question were made up of averaging weekly data points from multiple Google Trend Searches, as outlined below.

1. Q1: Jambalaya + Gumbo
2. Q2: Hipster + Williamsburg, Brooklyn
3. Q3: Yoga + Meditation
4. Q4: Tabby Cat + Calico Cat
5. Q5: Artificial Intelligence + Machine Learning
<br />
<br />


## 2. Detrending and Stationarity
In order to fit a time series model such as ARIMA or SARIMA models, we utilized various methods of detrending to reduce the data into (at least) weak stationarity, where mean and covariance would be invariant of time (or time difference).

### 2.0 Transformations
- Transformed data via applying log or square-root to stabilize variability increases over time

### 2.1 Differencing
- Utilized first & 2nd-order differencing of lag-1 to remove overall mean trend
- Applied seasonal differencing by lag-s to remove seasonality

### 2.2 Smoothing + Parametric Fitting
- Used LOESS (locally weighted scatterplot smoothing) to both 1) fit and capture local trends by smoothing, and 2) fit an overall mean trend via parametric fitting) to remove overall mean trend and reduce large local fluctuations
- Fit sinusodial functions via Discrete Fourier Transform (DFT) and examining which frequencies are most important to fit periodic trends

### 2.3 Testing for Stationarity
- Utilized Augmented Dickey-Fuller Test to test and reject our null hypothesis that our data is not stationary.
<br />
<br /> 


## 3. Model Diagnostics
After detrending and making sure our data is stationary, we needed to determine which models were the best for forecasting and capturing the remaining fluctuations.

### 3.0 Looking at ACF and PACF
- Looked at ACF and PACF graphs to determine reasonable range of q and p MA(q) & AR(p) in ARIMA models, as well as SAR(P) and SAR(Q) in Seasonal ARIMA (SARIMA) models

### 3.1 Local External Validity: AIC & BIC
- Local External Validity indicate how well we fit signal and ignore noise within our local training dataset.
- Generated a p X q matrix to determine which combination of p and q in AR(p) and MA(q) gave lowest AIC and BIC scores,

### 3.2 Local Internal Validity: Ljung-Box-Pierce Test
- Local Internal Validity indicates how well we fit our model to completely detrend our data into white noise.
- Utilized Ljung-Box-Pierce test to see whether our residuals after fitting the best ARIMA or SARIMA model resembles white noise.

### 3.3 General External Validity: Time Series Cross Validation
- General External Validity measures predictiveness of the model by using testing MSE. This is perhaps the most important metric of all.
- Implemented time series cross validation function which increasingly uses more time series data to predict the same number of future time points (i.e. using 3 years worth of data to predict 4th year, all the way to using 10 years worth of data to predict 11th year).

<br />
<br /> 

## 4. Forecasting
- Finally, we chose the best model according to the three metrics and forecasted the next 2 year's worth of data, by using `predict`.
