### Q3.R
### Author: Jong Ha Lee
### Last Updated: 04/12/2017
setwd("~/Desktop/School/STAT153/midterm2")

### 1. Exploratory Data Analysis
data <- read.csv("q3/q3_train.csv", stringsAsFactors = F) #Note it is weekly data
data["Xt"] <- seq(1:nrow(data))

# Initial Plotting
plot(data$Xt, data$activity, main = "Initial Plot", type = "l")

#Log-Transformed Plot
data["log.activity"] <- log(2 + data$activity)
plot(data$log.activity, type = "l", main = "Log+2-Transformed Activity")

#Square-root transformation
data['sqrt.activity'] <- sqrt(data$activity + 2)
plot(data$sqrt.activity, type = "l")


#Based on plot, dividing into different sections to find local maximas
local_maxes <- sapply(seq(1, nrow(data), by = 50), function(x){
  if(x + 50 > nrow(data)){
    return(which.max(data$activity[x:nrow(data)]) + x - 1)
  }
  else{
    return(which.max(data$activity[x:(x+50)]) + x - 1)
  }
})

#What's the average indice difference until local max happens again?
mean(diff(local_maxes)) 



##### 2. Transformation of Data and Removing Mean Structure #####
param.fit <- loess(log.activity ~ Xt, data = data,
                   span = 0.5, degree = 2, 
                   control = loess.control(surface = "direct"))

plot(data$log.activity, type = "l")
lines(param.fit$fitted, col = "red")
plot(param.fit$residuals, type = 'l')
acf(param.fit$residuals, lag.max = 500)

#As expected, see a lot of seasonality. Let's remove seasonality by sinusodial fitting.
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
num.periods <- floor(0.3*nrow(data))
coeffs <- sort(periodos[2:end.periodos], decreasing = T)[1:num.periods]
freqs <- as.numeric(names(coeffs))
season.fit.df <- sinusoid(start = 1, end = nrow(data),  freqs = freqs)
season.fit.df["y"] <- param.fit$residuals
season.fit <- lm(y ~., data = season.fit.df)

plot(season.fit$fitted.values + param.fit$fitted, type = "l")
lines(data$log.activity, col = "red")
acf(season.fit$residuals, lag.max = 500)

#Take Differencing
acf(diff(season.fit$residuals, lag = 52),lag.max=500)

#Checking Staitonarity of Seasional Fit Residuals
adf.test(diff(season.fit$residuals, lag = 52), alternative = "s", k = 52) #ok
adf.test(season.fit$residuals, alternative = "s", k = 52)

# ACF and PACF of Seasonal Fit Residuals
acf(diff(season.fit$residuals, lag = 52), lag.max = 200)
pacf(diff(season.fit$residuals, lag = 52), lag.max = 200)

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

sfit.ic.matrix <- icTest(season.fit$residuals, p = 3, d = 0, q = 3, P = 1, D = 1, Q = 0)


sfit.ic2.matrix <- icTest(season.fit$residuals, p = 3, d = 0, q = 3, P = 0, D = 0, Q = 0)
arima4 <- arima(season.fit$residuals, order = c(2,0,2),
                seasonal = list(order = c(0,0,0), period = 52))
arima5 <- arima(season.fit$residuals, order = c(1,0,3),
                seasonal = list(order = c(0,0,0), period = 52))
arima6 <- arima(season.fit$residuals, order = c(0,0,1),
                seasonal = list(order = c(0,0,0), period = 52))
new.all.models <- list(arima4, arima5, arima6)
new.all.iv <- 
  lapply(new.all.models, function(model){
    return(Box.test(model$residuals, lag = 104, fitdf = length(model$coef)))
  })

new.all.iv
#Best Models for only seasonal differencing: ARIMA(1,0,2), ARIMA(2,0,3), ARIMA(0,0,1)
#Best Models for both non-seasonal and seasonal differencing: ARIMA(0,1,1), ARIMA(0,1,2), ARIMA(1,1,1)

#ARIMA(2,0,3)
arima1 <- arima(season.fit$residuals, order = c(0,1,2),
                seasonal = list(order = c(1,1,0), period = 52))
acf(arima1$residuals, lag.max = 500,
    main = "ACF of Residuals of ARIMA(2,0,3)")

#ARIMA(2,0,1)
arima2 <- arima(season.fit$residuals, order = c(0,1,1),
                seasonal = list(order = c(1,1,0), period = 52))
acf(arima2$residuals, lag.max = 500,
    main = "ACF of Residuals of ARIMA(2,0,1)")

#ARIMA(1,0,2)
arima3 <- arima(season.fit$residuals, order = c(2,1,3),
                seasonal = list(order = c(1,1,0), period = 52))
acf(arima3$residuals, lag.max = 500,
    main = "ACF of Residuals of ARIMA(1,0,2)")

#Internal Validity Box.Test
all.models <- list("arima1" = arima1, "arima2" = arima2, 
                   "arima3" = arima3)
all.iv <- 
  lapply(all.models, function(model){
    return(Box.test(model$residuals, lag = 104, fitdf = length(model$coef)))
  })

all.iv

plot(data$log.activity, type = "l")
lines(param.fit$fitted, col = "red")
lines(417:520, p.fitted, col = "blue")

tsCV <- function(ts, order, sorder = c(0,0,0), type, k){
  all_mses <- c()
  for(i in (10-k-1):8){
    train <- ts[1:(i*52), ]
    test <-  ts[(i*52+1):((i+2)*52), ]
    if(type == "resids"){
      param.fit <- loess(log.activity ~ Xt, data = train,
                         control = loess.control(surface = "direct"),
                         span = 0.6, degree = 1)
      
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
                             seasonal = list(order = sorder, period = 52))
      
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

sfit.all_mses <- tsCV(ts = data, order = c(1,1,3), sorder = c(1, 1 ,0),
                      type = "resids", k= 4)


final.arima <- arima(season.fit$residuals, order = c(1,1,3),
                     seasonal = list(order = c(1,1,0), period = 52))

arima.fcast <- predict(final.arima, n.ahead = 104)

final.s.fitted <- predict(season.fit, 
                          newdata = sinusoid(start = (526), end = 629,
                                             freqs = freqs))
final.p.fitted <- predict(param.fit, newdata = data.frame(Xt = 526:629))
final.predicted.val <- 
  as.numeric(arima.fcast$pred + final.s.fitted + final.p.fitted)

final.predicted.val <- exp(final.predicted.val) - 2

plot(data$activity, type = "l", col = "green",
     main = "Predicted Time Series Data", xlim = c(0, 650))
lines(526:629, final.predicted.val, col = "red")
write(final.predicted.val, file = "q3/Q3_Jong_Lee_25344865.txt", sep = ",",
      ncol = 1) 


#####*** OUTPUT OF PREDICTION RESULTS ***#####
#Assume predictions are in a vector called preds
preds <- sample(1:12312, 100, replace = F) #Random for now
write(preds, file = "Q1_Jong_Lee_25344865.txt", sep = ",",
      ncol = length(preds)) #As required format

