library(forecast)
library(vars)
library(tseries)

Data <- read.csv("C:/Users/Windy/Desktop/Coding/R/Research/m2.csv")

data <- ts(Data[,2:7],frequency = 12)
data_diff <-diff(na.omit(data))
#__________________________unit root test ____________________________________
at <- matrix(NA,ncol(data_diff),1)
kt <- matrix(NA,ncol(data_diff),1)
for (i in 1:ncol(data)){
    
  a <- adf.test(ts(data_diff[,i]))
  k <- kpss.test(ts(data_diff[,i]))
  at[i] <- a$p.value
  kt[i] <- k$p.value
}
#combine
cbind(at < 0.05 , kt > 0.05)

VARselect(data_diff,lag.max = 15)
#---------------------------VAR loop---------------------------------------
steps <- 5
num <- 100

fcst_save <- array(NA, dim = c(nrow(data_diff) - num - steps + 1, steps, ncol(data_diff)))
actual_save <- array(NA, dim = c(nrow(data_diff) - num - steps + 1, steps, ncol(data_diff)))
# Loop
for (i in (num:(nrow(data_diff) - steps))) {
  
  data_in <- data_diff[1:i, ] 
  
  # Fit the model
  fit <- VAR(data_in, p = 12)
  
  # Predict
  fcst <- predict(fit, n.ahead = steps)
  
  for (j in 1:ncol(data_diff)) {
    fcst_save[i - num+1 , , j] <- fcst$fcst[[j]][, 1]
    actual_save[i - num +1 , , j] <- data_diff[i:(i + steps - 1), j]
    
  }
}

##
mse <- array(NA, dim = c(steps,ncol(data_diff)))

for (k in 1:steps) {
  for (j in 1:ncol(data_diff)) {
    mse[k, j] <- mean((actual_save[, k, j] - fcst_save[, k, j])^2, na.rm = TRUE)
  }
}

##
sqrt(mean((actual_save - fcst_save)^2))

se1 <- (actual_save[,,1] - fcst_save[,,1])^2
mse_1 <- mean(se1[,1])
#____________________________________just try ______________________
write.csv(mse, file = "mse_var.csv", row.names = FALSE)
write.csv(fcst_save, file = "var.csv", row.names = FALSE)



fit <- VAR(data_diff[1:200,1:4], p = 3)

# Predict
fcst <- predict(fit, n.ahead = 4)

fcst
