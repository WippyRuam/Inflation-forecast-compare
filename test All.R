library(forecast)
library(vars)
library(readr)
library(tseries)
library(dplyr)




Data <- read.csv("C:/Users/Windy/Desktop/Coding/R/Research/m1.csv")
data_ts <- na.omit(ts(Data[,2:20],start = c(2000),frequency = 12))
data <- diff(data_ts,differences = 1)
colnames(data) <- c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","x14","x15","x16","x17","x18","y")



  
ll <- y ~ x1 +x17 +x4 +x5 +x6
li <- x2 +x9 +x10+x11 +x12 +x13 + x14  
#--------------Johansen Cointegration---------------------
library(urca)
library(vars)
library(tseries)
datas <- ts(data[,c("y","x1","x15","x4","x5","x6")],frequency = 12)
VARselect(data, lag.max = 100,type = "const")

coin <- ca.jo(datas,type = "trace",ecdet ="const", K = 12)
coin <- ca.jo(datas,type = "eigen",ecdet ="const", K = 12)
summary(coin)

#________________MICE_____________________________________
library(mice)
x <- mice(data)

#________________Heteroscedasticity______________
library(olsrr)
model <- lm(ll, data = data)
ols_test_breusch_pagan(model)
         # white test 
library(lmtest)
bptest(model, ~ fitted(model) + I(fitted(model)))
#_______________________Autocorrelation________________________________
library(car)
ll <- y ~ x1 +x16 +x4 +x5 +x6
model <- lm(ll, data = data)
durbinWatsonTest(model)
#_________________________Hausman Test Endogeneity !!!ได้ ___________________
library(AER)
#insert data from NK reg.R
mlr1 <- lm(ll, data = data)
mlr2 <- ivreg(formula = y ~ x1 + x6 | x3 + x4,data = data)
mlr2 <- ivreg(formula = y ~ x6 + x5 | x3 + x4,data = data)


summary(mlr2 , diagnostics = TRUE)
#___________________________Multicolinearity test___________________
library(car)
model <- lm(ll, data = data)
vif(model)
summary(vif(model))



