### Q1.R
### Author: Jong Ha Lee
### Last Updated: 04/05/2017
setwd("~/Desktop/School/STAT153/midterm2")

### 1. Exploratory Data Analysis
data <- read.csv("q1_train.csv", stringsAsFactors = F) #Note it is weekly data
data["Xt"] <- seq(1:nrow(data))
head(data)

# Initial Plotting
plot(data$Xt, data$activity, main = "Initial Plot")
#Looks like there is some sort of seasionality going on. 
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

#We see that around per 51.4 time points, there's seasonality. We also note that there are roughly 52 weeks in a year, so there is seasonality on an annual basis.




### 2. Transformation of Data
#Let's difference the data to remove the seasonality.


### 3. Removing mean structure

### 4. Fitting Time series models to residuals after removing mean structure. Finding Covariance structure.
# BLP
# CSS
# MLE


### 5. Diagnostics to check model accuracy.
# Standardized residuals
# Ljung-Box-Pierce Test


### 6. Forecasting.



#####*** OUTPUT OF PREDICTION RESULTS ***#####
#Assume predictions are in a vector called preds
preds <- sample(1:12312, 100, replace = F) #Random for now
write(preds, file = "Q1_Jong_Lee_25344865.txt", sep = ",",
      ncol = length(preds)) #As required format

