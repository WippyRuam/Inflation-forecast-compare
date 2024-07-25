library(tidyverse)
library(forecast)
Data <- read.csv("C:/Users/Windy/Desktop/Coding/R/Research/m2e.csv")


data_ts <- na.omit(ts(Data[,2:8],frequency = 12))
data <- diff(data_ts,differences = 1)
colnames(data) <- c("x1","x2","x3","x4","x5","y","x6")

epx <- y ~ x1 + x6

model  <- lm(epx ,data = data)

al <- model$coefficients[1]
b1 <- model$coefficients[2]
b2 <- model$coefficients[3]
summary(model) 

n <- 100
fcst_value <- matrix(NA,nrow(data)-n,1)
actual <- matrix(NA,nrow(data)-n,1)


for (i in n:(nrow(data)-1)) {
  datab1 <- data[i,"x1"]
  datab2 <- data[i,"x6"]
  fcst_value[(i-n)+1,] <- al+ (datab1*b1) + (datab2*b2) + model$residuals[(i-n)+1]
  actual[(i-n)+1,] <- data[i+1,6] 
}

mse <- mean(fcst_value - actual)^2

#---------------------

