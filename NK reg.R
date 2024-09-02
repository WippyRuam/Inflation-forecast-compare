# Load and preprocess the data
Data <- read.csv("m2e.csv")
data_ts <- na.omit(ts(Data[, 2:8], frequency = 12))
data <- diff(data_ts, differences = 1)
colnames(data) <- c("x1", "x2", "x3", "x4", "x5", "y", "x6")

# Initialize variables
n <- 100
fcst_value <- matrix(NA, nrow(data) - n, 1)
actual <- matrix(NA, nrow(data) - n, 1)
nk <- y ~ x6 + x2

# Loop through the data to fit the model
for (i in n:(nrow(data) - 1)) {
  
  subset_data <- as.data.frame(data[(i-n+1):i, ])
  
  # Fit the model using the previous n rows
  model <- lm(nk, data = subset_data)
  
  # Extract the coefficients
  al <- model$coefficients[1]
  b1 <- model$coefficients[2]
  b2 <- model$coefficients[3]
  
  # Get the values of the predictors for the current row
  datab1 <- data[i, "x2"]
  datab2 <- data[i, "x6"]
  
  # Calculate the forecasted value
  fcst_value[(i-n)+1, ] <- al + (datab1 * b1) + (datab2 * b2)
  
  # Store the actual value for the next time step
  actual[(i-n)+1, ] <- data[i+1, "y"]
}


# Calculate the Mean Squared Error
mse <- mean((fcst_value - actual)^2)
print(mse)
