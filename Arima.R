# Tải các thư viện
library(quantmod)
library(ggplot2)
library(tseries)
library(forecast)
library(rstudioapi)

run <- function() {
  eval(parse(text = rstudioapi::primary_selection(
    rstudioapi::getSourceEditorContext())$text))
}

# Step 1: Collect and Convert Data to Time Series
df <- read.csv("C:/Users/Dell/OneDrive/Documents/403 Phan Tich Du Lieu Kinh Doanh/PET_PRI_GND_DCUS_NUS_W.csv")
df$Date <- as.Date(df$Date, format="%m/%d/%Y")
str(df)

# REGULAR GAS PRICE
print('-------------------------------------------------------------')
print('REGULAR GAS PRICE')

# Step 2: Plot the gas Prices
ggplot(data = as.data.frame(df$R2), aes(x =df$Date, y = df$R2)) +
  geom_line() +
  labs(title = "Regular gas price", x = "Date", y = "Regular Gas Price") +
  theme_minimal()


# Step 3: Check for Stationarity using ADF Test
# Check data stionary status
adf_test <- adf.test(df$R2, 
                     alternative = "stationary",
                     k = 1)  # lag order = 1 to match Python
print(adf_test)


# Compare p_value (alpha = 5%)
if (adf_test$p.value > 0.05) {
  conclusion <- "Data's non stationary"
  cat("Conclusion:", conclusion, "\n")
  cat("Use the differencing method.\n")
  
  # Use fist differencing
  diff_data <- diff(df$R2)
  diff_data <- na.omit(diff_data)  # Remove null value
  
  # Check stationary again after differencing
  adf_test_diff <- adf.test(diff_data)
  print(adf_test_diff)
  
} else {
  conclusion <- "Data is stationary"
  cat("Conclusion:", conclusion, "\n")
}

#Conclusion
cat("ADF Statistic:", adf_test$statistic, "\n")
cat("p-value:", adf_test$p.value, "\n")
if (adf_test$p.value > 0.05) {
  cat("Because p-value > 5%, data is not stationary and need to use differencing.\n")
} else {
  cat("Because p-value <= 5%, data is tationary.\n")
}



# Step 5: Train, Test, Validate, and Future Forecast using Auto ARIMA

# Calculate split points
total_length <- length(df$R2)
train_size <- floor(0.7 * total_length)
test_size <- floor(0.15 * total_length)
validate_size <- total_length - train_size - test_size

# Split data
train <- df$R2[1:train_size]
test <- df$R2[(train_size + 1):(train_size + test_size)]
validate <- df$R2[(train_size + test_size + 1):total_length]

# Train auto.arima model using all data (for future forecast)
final_model <- auto.arima(df$R2)
print(final_model)  # Print model parameters

# Train model on training data (for historical evaluation)
model <- auto.arima(train)

# Generate forecasts
forecast_test <- forecast(model, h = test_size)
forecast_validate <- forecast(model, h = validate_size)
future_forecast <- forecast(final_model, h = 8)  # 8 weeks forecast

# Create date sequence for future forecast
last_date <- max(df$Date)
future_dates <- seq(last_date + 7, by = "7 days", length.out = 8)  # Weekly data

# Create data frames for plotting
train_df <- data.frame(
  Date = df$Date[1:train_size],
  Value = train,
  Type = "Training"
)

test_df <- data.frame(
  Date = df$Date[(train_size + 1):(train_size + test_size)],
  Value = test,
  Type = "Test"
)

validate_df <- data.frame(
  Date = df$Date[(train_size + test_size + 1):total_length],
  Value = validate,
  Type = "Validation"
)

forecast_test_df <- data.frame(
  Date = df$Date[(train_size + 1):(train_size + test_size)],
  Value = as.numeric(forecast_test$mean),
  Type = "Forecast_Test"
)

forecast_validate_df <- data.frame(
  Date = df$Date[(train_size + test_size + 1):total_length],
  Value = as.numeric(forecast_validate$mean),
  Type = "Forecast_Validation"
)

future_forecast_df <- data.frame(
  Date = future_dates,
  Value = as.numeric(future_forecast$mean),
  Type = "Future_Forecast"
)

# Add confidence intervals for future forecast
future_forecast_lower <- data.frame(
  Date = future_dates,
  Value = as.numeric(future_forecast$lower[,2]),  # 95% confidence interval
  Type = "Future_CI"
)

future_forecast_upper <- data.frame(
  Date = future_dates,
  Value = as.numeric(future_forecast$upper[,2]),  # 95% confidence interval
  Type = "Future_CI"
)

# Plot results
ggplot() +
  # Historical data and forecasts
  geom_line(data = train_df, aes(x = Date, y = Value, color = Type)) +
  geom_line(data = test_df, aes(x = Date, y = Value, color = Type)) +
  geom_line(data = validate_df, aes(x = Date, y = Value, color = Type)) +
  geom_line(data = forecast_test_df, aes(x = Date, y = Value, color = Type), linetype = "dashed") +
  geom_line(data = forecast_validate_df, aes(x = Date, y = Value, color = Type), linetype = "dashed") +
  # Future forecast with confidence interval
  geom_ribbon(data = data.frame(
    Date = future_dates,
    lower = future_forecast_lower$Value,
    upper = future_forecast_upper$Value
  ), aes(x = Date, ymin = lower, ymax = upper), fill = "grey70", alpha = 0.3) +
  geom_line(data = future_forecast_df, aes(x = Date, y = Value, color = Type), size = 1) +
  scale_color_manual(values = c(
    "Training" = "blue",
    "Test" = "red",
    "Validation" = "purple",
    "Forecast_Test" = "green",
    "Forecast_Validation" = "orange",
    "Future_Forecast" = "darkred"
  )) +
  labs(title = "Auto ARIMA Forecast with Train/Test/Validation Split and Future Prediction For R2",
       subtitle = "Including 8-week forecast with 95% confidence interval",
       x = "Date",
       y = "Value") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Step 6: Calculate MAE and RMSES
test_rmse <- sqrt(mean((test - forecast_test$mean)^2))
test_mae <- mean(abs(test - forecast_test$mean))

# Calculate RMSE and MAE for validation set
validate_rmse <- sqrt(mean((validate - forecast_validate$mean)^2))
validate_mae <- mean(abs(validate - forecast_validate$mean))

# Tính toán MAE và RMSE
# Print metrics
cat("\nTest Set Metrics:\n")
cat("RMSE:", round(test_rmse, 4), "\n")
cat("MAE:", round(test_mae, 4), "\n")

cat("\nValidation Set Metrics:\n")
cat("RMSE:", round(validate_rmse, 4), "\n")
cat("MAE:", round(validate_mae, 4), "\n")





# Midrange GAS PRICE
print('-------------------------------------------------------------')
print('Midrange GAS PRICE')

# Step 2: Plot the gas Prices
ggplot(data = as.data.frame(df$M2), aes(x = df$Date, y = df$M2)) +
  geom_line() +
  labs(title = "Midrange gas price", x = "Date", y = "Midrange Gas Price") +
  theme_minimal()


# Step 3: Check for Stationarity using ADF Test
# Check data staionary
adf_test <- adf.test(df$M2, 
                     alternative = "stationary",
                     k = 1)  # lag order = 1 to match Python
print(adf_test)


# Compare p_value (alpha = 5%)
if (adf_test$p.value > 0.05) {
  conclusion <- "Data's non stationary"
  cat("Conclusion:", conclusion, "\n")
  cat("Use the differencing method.\n")
  
  # Use fist differencing
  diff_data <- diff(df$M2)
  diff_data <- na.omit(diff_data)  # Remove null value
  
  # Check stationary again after differencing
  adf_test_diff <- adf.test(diff_data)
  print(adf_test_diff)
  
} else {
  conclusion <- "Data is stationary"
  cat("Conclusion:", conclusion, "\n")
}

# Conclusion
cat("ADF Statistic:", adf_test$statistic, "\n")
cat("p-value:", adf_test$p.value, "\n")
if (adf_test$p.value > 0.05) {
  cat("Because p-value > 5%, data is not stationary and need to use differencing.\n")
} else {
  cat("Because p-value <= 5%, data is tationary.\n")
}



# Step 5: Train, Test, Validate, and Future Forecast using Auto ARIMA

# Calculate split points
total_length <- length(df$M2)
train_size <- floor(0.7 * total_length)
test_size <- floor(0.15 * total_length)
validate_size <- total_length - train_size - test_size

# Split data
train <- df$M2[1:train_size]
test <- df$M2[(train_size + 1):(train_size + test_size)]
validate <- df$M2[(train_size + test_size + 1):total_length]

# Train auto.arima model using all data (for future forecast)
final_model <- auto.arima(df$M2)
print(final_model)  # Print model parameters

# Train model on training data (for historical evaluation)
model <- auto.arima(train)

# Generate forecasts
forecast_test <- forecast(model, h = test_size)
forecast_validate <- forecast(model, h = validate_size)
future_forecast <- forecast(final_model, h = 8)  # 8 weeks forecast

# Create date sequence for future forecast
last_date <- max(df$Date)
future_dates <- seq(last_date + 7, by = "7 days", length.out = 8)  # Weekly data

# Create data frames for plotting
train_df <- data.frame(
  Date = df$Date[1:train_size],
  Value = train,
  Type = "Training"
)

test_df <- data.frame(
  Date = df$Date[(train_size + 1):(train_size + test_size)],
  Value = test,
  Type = "Test"
)

validate_df <- data.frame(
  Date = df$Date[(train_size + test_size + 1):total_length],
  Value = validate,
  Type = "Validation"
)

forecast_test_df <- data.frame(
  Date = df$Date[(train_size + 1):(train_size + test_size)],
  Value = as.numeric(forecast_test$mean),
  Type = "Forecast_Test"
)

forecast_validate_df <- data.frame(
  Date = df$Date[(train_size + test_size + 1):total_length],
  Value = as.numeric(forecast_validate$mean),
  Type = "Forecast_Validation"
)

future_forecast_df <- data.frame(
  Date = future_dates,
  Value = as.numeric(future_forecast$mean),
  Type = "Future_Forecast"
)

# Add confidence intervals for future forecast
future_forecast_lower <- data.frame(
  Date = future_dates,
  Value = as.numeric(future_forecast$lower[,2]),  # 95% confidence interval
  Type = "Future_CI"
)

future_forecast_upper <- data.frame(
  Date = future_dates,
  Value = as.numeric(future_forecast$upper[,2]),  # 95% confidence interval
  Type = "Future_CI"
)

# Plot results
ggplot() +
  # Historical data and forecasts
  geom_line(data = train_df, aes(x = Date, y = Value, color = Type)) +
  geom_line(data = test_df, aes(x = Date, y = Value, color = Type)) +
  geom_line(data = validate_df, aes(x = Date, y = Value, color = Type)) +
  geom_line(data = forecast_test_df, aes(x = Date, y = Value, color = Type), linetype = "dashed") +
  geom_line(data = forecast_validate_df, aes(x = Date, y = Value, color = Type), linetype = "dashed") +
  # Future forecast with confidence interval
  geom_ribbon(data = data.frame(
    Date = future_dates,
    lower = future_forecast_lower$Value,
    upper = future_forecast_upper$Value
  ), aes(x = Date, ymin = lower, ymax = upper), fill = "grey70", alpha = 0.3) +
  geom_line(data = future_forecast_df, aes(x = Date, y = Value, color = Type), size = 1) +
  scale_color_manual(values = c(
    "Training" = "blue",
    "Test" = "red",
    "Validation" = "purple",
    "Forecast_Test" = "green",
    "Forecast_Validation" = "orange",
    "Future_Forecast" = "darkred"
  )) +
  labs(title = "Auto ARIMA Forecast with Train/Test/Validation Split and Future Prediction for M2",
       subtitle = "Including 8-week forecast with 95% confidence interval",
       x = "Date",
       y = "Value") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Step 6: Calculate MAE and RMSES
test_rmse <- sqrt(mean((test - forecast_test$mean)^2))
test_mae <- mean(abs(test - forecast_test$mean))

# Calculate RMSE and MAE for validation set
validate_rmse <- sqrt(mean((validate - forecast_validate$mean)^2))
validate_mae <- mean(abs(validate - forecast_validate$mean))

# Calculate MAE và RMSE
# Print metrics
cat("\nTest Set Metrics:\n")
cat("RMSE:", round(test_rmse, 4), "\n")
cat("MAE:", round(test_mae, 4), "\n")

cat("\nValidation Set Metrics:\n")
cat("RMSE:", round(validate_rmse, 4), "\n")
cat("MAE:", round(validate_mae, 4), "\n")






##PREMIUM GAS PRICE
print('-------------------------------------------------------------')
print('Premium GAS PRICE')

# Step 2: Plot the gas Prices
ggplot(data = as.data.frame(df$P2), aes(x = df$Date, y = df$P2)) +
  geom_line() +
  labs(title = "Premium gas price", x = "Date", y = "Premium Gas Price") +
  theme_minimal()


# Step 3: Check for Stationarity using ADF Test
# Check data stionary status
adf_test <- adf.test(df$P2, 
                     alternative = "stationary",
                     k = 1)  # lag order = 1 to match Python
print(adf_test)


# Compare p_value (alpha = 5%)
if (adf_test$p.value > 0.05) {
  conclusion <- "Data's non stationary"
  cat("Conclusion:", conclusion, "\n")
  cat("Use the differencing method.\n")
  
  # Use fist differencing
  diff_data <- diff(df$P2)
  diff_data <- na.omit(diff_data)  # Remove null value
  
  # Check stationary again after differencing
  adf_test_diff <- adf.test(diff_data)
  print(adf_test_diff)
  
} else {
  conclusion <- "Data is stationary"
  cat("Conclusion:", conclusion, "\n")
}

#Conclusion
cat("ADF Statistic:", adf_test$statistic, "\n")
cat("p-value:", adf_test$p.value, "\n")
if (adf_test$p.value > 0.05) {
  cat("Because p-value > 5%, data is not stationary and need to use differencing.\n")
} else {
  cat("Because p-value <= 5%, data is tationary.\n")
}



# Step 5: Train, Test, Validate, and Future Forecast using Auto ARIMA

# Calculate split points
total_length <- length(df$P2)
train_size <- floor(0.7 * total_length)
test_size <- floor(0.15 * total_length)
validate_size <- total_length - train_size - test_size

# Split data
train <- df$P2[1:train_size]
test <- df$P2[(train_size + 1):(train_size + test_size)]
validate <- df$P2[(train_size + test_size + 1):total_length]

# Train auto.arima model using all data (for future forecast)
final_model <- auto.arima(df$P2)
print(final_model)  # Print model parameters

# Train model on training data (for historical evaluation)
model <- auto.arima(train)

# Generate forecasts
forecast_test <- forecast(model, h = test_size)
forecast_validate <- forecast(model, h = validate_size)
future_forecast <- forecast(final_model, h = 8)  # 8 weeks forecast

# Create date sequence for future forecast
last_date <- max(df$Date)
future_dates <- seq(last_date + 7, by = "7 days", length.out = 8)  # Weekly data

# Create data frames for plotting
train_df <- data.frame(
  Date = df$Date[1:train_size],
  Value = train,
  Type = "Training"
)

test_df <- data.frame(
  Date = df$Date[(train_size + 1):(train_size + test_size)],
  Value = test,
  Type = "Test"
)

validate_df <- data.frame(
  Date = df$Date[(train_size + test_size + 1):total_length],
  Value = validate,
  Type = "Validation"
)

forecast_test_df <- data.frame(
  Date = df$Date[(train_size + 1):(train_size + test_size)],
  Value = as.numeric(forecast_test$mean),
  Type = "Forecast_Test"
)

forecast_validate_df <- data.frame(
  Date = df$Date[(train_size + test_size + 1):total_length],
  Value = as.numeric(forecast_validate$mean),
  Type = "Forecast_Validation"
)

future_forecast_df <- data.frame(
  Date = future_dates,
  Value = as.numeric(future_forecast$mean),
  Type = "Future_Forecast"
)

# Add confidence intervals for future forecast
future_forecast_lower <- data.frame(
  Date = future_dates,
  Value = as.numeric(future_forecast$lower[,2]),  # 95% confidence interval
  Type = "Future_CI"
)

future_forecast_upper <- data.frame(
  Date = future_dates,
  Value = as.numeric(future_forecast$upper[,2]),  # 95% confidence interval
  Type = "Future_CI"
)

# Plot results
ggplot() +
  # Historical data and forecasts
  geom_line(data = train_df, aes(x = Date, y = Value, color = Type)) +
  geom_line(data = test_df, aes(x = Date, y = Value, color = Type)) +
  geom_line(data = validate_df, aes(x = Date, y = Value, color = Type)) +
  geom_line(data = forecast_test_df, aes(x = Date, y = Value, color = Type), linetype = "dashed") +
  geom_line(data = forecast_validate_df, aes(x = Date, y = Value, color = Type), linetype = "dashed") +
  # Future forecast with confidence interval
  geom_ribbon(data = data.frame(
    Date = future_dates,
    lower = future_forecast_lower$Value,
    upper = future_forecast_upper$Value
  ), aes(x = Date, ymin = lower, ymax = upper), fill = "grey70", alpha = 0.3) +
  geom_line(data = future_forecast_df, aes(x = Date, y = Value, color = Type), size = 1) +
  scale_color_manual(values = c(
    "Training" = "blue",
    "Test" = "red",
    "Validation" = "purple",
    "Forecast_Test" = "green",
    "Forecast_Validation" = "orange",
    "Future_Forecast" = "darkred"
  )) +
  labs(title = "Auto ARIMA Forecast with Train/Test/Validation Split and Future Prediction For P2",
       subtitle = "Including 8-week forecast with 95% confidence interval",
       x = "Date",
       y = "Value") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Step 6: Calculate MAE and RMSES
test_rmse <- sqrt(mean((test - forecast_test$mean)^2))
test_mae <- mean(abs(test - forecast_test$mean))

# Calculate RMSE and MAE for validation set
validate_rmse <- sqrt(mean((validate - forecast_validate$mean)^2))
validate_mae <- mean(abs(validate - forecast_validate$mean))

# Tính toán MAE và RMSE
# Print metrics
cat("\nTest Set Metrics:\n")
cat("RMSE:", round(test_rmse, 4), "\n")
cat("MAE:", round(test_mae, 4), "\n")

cat("\nValidation Set Metrics:\n")
cat("RMSE:", round(validate_rmse, 4), "\n")
cat("MAE:", round(validate_mae, 4), "\n")

