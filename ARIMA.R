library(forecast)
library(vars)
library(readr)
library(tseries)

Data <- read.csv("C:/Users/Windy/Desktop/Coding/R/Research/m2.csv")
Data <- read.csv("C:/Users/Windy/Desktop/Coding/R/Research/m5.csv")

x <- as.matrix(Data$Equity.Market.Index..Month.End..SET)
po <- hp_filter(x,1600)

data <- ts(Data[,2],frequency = 12)
data <- ts(Data,frequency = 12)
data <- ts(Data[,7],frequency = 12)
data_diff <-(na.omit(data))

adf.test(data)

step <- 5
loop <- 100
nn <- length(data_diff)

fcst_save <- matrix(NA, nn-loop-step+1, step) 
actual_save <- matrix(NA,nn-loop-step+1, step) 
for (i in loop:nn - step) {
  data_in <- data_diff[1:i]
  fit <- Arima(data_in, order=c(2,0,0)) 
  fcst <- forecast(fit, step) 
  fcst_save[(i-(loop)+1),] <- as.numeric(fcst$mean)
  actual_save[(i-(loop)+1),] <- data[(i+1):(i+step)]
}


mse <- matrix(NA,1,step)
for (l in 1:step) {
  mse[1,l] <- mean(actual_save[,l] - fcst_save[,l])^2

}
mse
write.csv(mse, file = "mse_arima.csv", row.names = FALSE)
write.csv(fcst_save, file = "arima.csv", row.names = FALSE)
