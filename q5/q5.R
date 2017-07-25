#q5.R

setwd("~/Desktop/School/STAT153/midterm2")


##### 1. EXPLORATORY DATA ANALYSIS #####
data <- read.csv("q5/q5_train.csv", stringsAsFactors = F)
data["Xt"] <- seq(1:nrow(data))
plot(data$Xt, data$activity, main = "Initial Plot", type = "l")

data["log.activity"] <- log(data$activity + 2)
plot(data$Xt, data$log.activity, main = "Initial Plot", type = "l")

##### 2. MEAN TREND REDUCTION #####
acf(diff(diff(data$log.activity, lag = 52)), lag.max = 500)
diff.data <- diff(diff(data$log.activity), lag = 52)

pacf(diff.data, lag.max = 200)
acf(diff.data, lag.max = 500)

####Quick Break: Is model stationary?
adf.test(diff(diff(data$activity, lag = 52)), alternative = c('stationary'), k = 104)

##### 3. FITTING ARIMA MODELS #####
icTest = function(ts, p, d, q, P=0, D = 0, Q=0){
  aicmatrix = matrix(NA, (p+1), (q+1))
  bicmatrix = matrix(NA, (p+1), (q+1))
  for(a in 0:p){
    for(b in 0:q){
      model <- tryCatch(arima(ts, order = c(a,d,b),
                              seasonal = list(order = c(P,D,Q), period = 52)),
                        simpleError = function(e) e)
      if(inherits(model, "simpleError")){
        model <- tryCatch(arima(ts, order = c(a,d,b),
                                seasonal = list(order = c(P,D,Q), period = 52),
                                method = "ML"),
                          simpleError = function(e) e)
      }
      if(inherits(model, "simpleError")){
        next
      }
      aicmatrix[a+1, b+1] = model$aic
      bicmatrix[a+1, b+1] = BIC(model)
    }
    
  }
  return(list(aic = aicmatrix, bic = bicmatrix))
}

load("q5/DiffSAR011.RData")
#ic.matrix <- icTest(data$log.activity, p = 3, d = 1, q = 3, P = 0, D = 1, Q = 1)

### Best Models based on AIC
#### 1. ARIMA(1,1,1) X SARIMA(0,1,1)[52]
#### 2. ARIMA(2,1,1) X SARIMA(0,1,1)[52]
#### 3. (Simple Model to check) ARIMA(0,1,3) X SARIMA(0,1,1)[52]

### Best Models based on BIC
#### 1. ARIMA(1,1,1) X SARIMA(0,1,1)[52]
#### 2. ARIMA(2,1,1) X SARIMA(0,1,1)[52]
#### 3. (Simple Model to check) ARIMA(0,1,3) X SARIMA(0,1,1)[52]


#ARIMA(1,1,1) X SARIMA(0,1,1)[52]
sarima1 <- arima(data$log.activity, order = c(1,1,1),
                 seasonal = list(order = c(0,1,1), period = 52))
acf(sarima1$residuals, lag.max = 500,
    main = "ACF of Residuals of ARIMA(1,1,1) X ARIMA(0,0,1)[52]")

#ARIMA(2,1,1) X SARIMA(0,1,1)[52]
sarima2 <- arima(data$log.activity, order = c(2,1,1),
                 seasonal = list(order = c(0,1,1), period = 52))
acf(sarima2$residuals, lag.max = 500,
    main = "ACF of Residuals of ARIMA(3,2,1) X ARIMA(1,0,1)[52]")

#ARIMA(0,1,3) X SARIMA(0,1,1)[52]
sarima3 <- arima(data$log.activity, order = c(0,1,3),
                 seasonal = list(order = c(0,1,1), period = 52))
acf(sarima3$residuals, lag.max = 500,
    main = "ACF of Residuals of ARIMA(0,2,2) X ARIMA(1,0,1)[52]")



##### 5. MODEL DIAGNOSTICS #####
all.models <- list("sarima1" = sarima1, "sarima2" = sarima2, 
                   "sarima3" = sarima3)
all.iv <- 
  lapply(all.models, function(model){
    return(Box.test(model$residuals, lag = 104, fitdf = length(model$coef)))
  })

all.iv

tsCV <- function(ts, order, sorder = c(0,0,0), type, k){
  all_mses <- c()
  for(i in (10-k-1):8){
    train <- ts[1:(i*52), ]
    test <-  ts[(i*52+1):((i+2)*52), ]
    if(type == "resids"){
      param.fit <- loess(log.activity ~ Xt, data = train,
                         control = loess.control(surface = "direct"),
                         span = 0.75, degree = 1)
      
      periodos <- 
        abs(fft(param.fit$residuals)/sqrt(length(param.fit$residuals)))^2
      names(periodos) <- (1:length(periodos) - 1) / length(param.fit$residuals)
      end.periodos <- ceiling(nrow(train) / 2)
      num.periods <- floor(0.08*nrow(train))
      coeffs <- sort(periodos[2:end.periodos], decreasing = T)[1:num.periods]
      freqs <- as.numeric(names(coeffs))
      season.fit.df <- sinusoid(start = 1, end = i*52,  freqs = freqs)
      season.fit.df["y"] <- param.fit$residuals
      season.fit <- lm(y ~., data = season.fit.df)
      
      sresids.model <- arima(season.fit$residuals, order = order,
                             seasonal = list(order = sorder, period  = 52))
      
      sresids.fcast <- predict(sresids.model, n.ahead = 104)
      s.fitted <- predict(season.fit, 
                          newdata = sinusoid(start = (i*52+1), end = (i+2)*52,
                                             freqs = freqs))
      p.fitted <- predict(param.fit,
                          newdata = data.frame(Xt = test$Xt))
      predicted.val <- p.fitted + s.fitted + sresids.fcast$pred
    }
    if(type == "diff"){
      arima.model <- arima(train$log.activity, order = order,
                           seasonal = list(order = sorder, period = 52),
                           method = "CSS-ML")
      arima.fcast <- predict(arima.model, n.ahead = 104)
      predicted.val <- arima.fcast$pred
    }
    mses <- (test$activity - (exp(predicted.val) - 2))^2
    all_mses <- c(all_mses, mean(mses))
  }
  return(all_mses)
}

diff.log.all_mses <- tsCV(ts = data, order = c(2,1,1), sorder = c(0,1,1),
                          type = "diff", k  = 4)

#################################################################################

###### NOW WORKING WITH SMOOTHING FITS #########

## Parametric Fit
plot(data$log.activity, type = "l")
param.fit <- loess(log.activity ~ Xt, data = data,
                   control = loess.control(surface = "direct"),
                   span = 0.75, degree = 1)
lines(param.fit$fitted, col = "red")
acf(param.fit$residuals, lag.max = 500)

## Sinusodial Fit
sinusoid <- function(start, end, freqs){
  t <- start:end
  df <- matrix(NA, nrow = length(t), ncol = length(freqs) * 2)
  for(i in 1:length(freqs)){
    df[ , 2*i] <- cos(2 * pi * freqs[i] * t)
    df[ , (2*i-1)] <- sin(2*pi*freqs[i]*t)
  }
  return(as.data.frame(df))
}
periodos <- 
  abs(fft(param.fit$residuals)/sqrt(length(param.fit$residuals)))^2
names(periodos) <- (1:length(periodos) - 1) / length(param.fit$residuals)
end.periodos <- ceiling(nrow(data) / 2)
num.periods <- floor(0.08*nrow(data))
coeffs <- sort(periodos[2:end.periodos], decreasing = T)[1:num.periods]
freqs <- as.numeric(names(coeffs))
season.fit.df <- sinusoid(start = 1, end = nrow(data),  freqs = freqs)
season.fit.df["y"] <- param.fit$residuals
season.fit <- lm(y ~., data = season.fit.df)

#Checking Staitonarity of Seasional Fit Residuals
adf.test(season.fit$residuals, alternative = "s", k = 52)

# ACF and PACF of Seasonal Fit Residuals
acf(season.fit$residuals, lag.max = 200)
pacf(season.fit$residuals, lag.max = 200)

#AIC, BIC Matrix
sfit.ic.matrix <- icTest(season.fit$residuals, p = 3, d = 0, q = 3, P = 0, D = 0, Q = 0)

### BEST MODELS: ARIMA(2,0,2), ARIMA(3,0,1), ARIMA(1,0,3)

#ARIMA(2,0,2)
arima1 <- arima(season.fit$residuals, order = c(2,0,2),
                 seasonal = list(order = c(0,0,0), period = 52))
acf(arima1$residuals, lag.max = 500,
    main = "ACF of Residuals of ARIMA(2,0,2)")

#ARIMA(3,0,1)
arima2 <- arima(season.fit$residuals, order = c(3,0,1),
                 seasonal = list(order = c(0,0,0), period = 52))
acf(arima2$residuals, lag.max = 500,
    main = "ACF of Residuals of ARIMA(3,0,1)")

#ARIMA(1,0,3)
arima3 <- arima(season.fit$residuals, order = c(1,0,3),
                 seasonal = list(order = c(0,0,0), period = 52))
acf(sarima3$residuals, lag.max = 500,
    main = "ACF of Residuals of ARIMA(0,0,2)")


#Internal Validity Box.Test
all.models <- list("arima1" = arima1, "arima2" = arima2, 
                   "arima3" = arima3)
all.iv <- 
  lapply(all.models, function(model){
    return(Box.test(model$residuals, lag = 104, fitdf = length(model$coef)))
  })

sfit.all_mses <- tsCV(ts = data, order = c(0,0,2), sorder = c(0,0,0),
                      type = "resids", k= 4)
