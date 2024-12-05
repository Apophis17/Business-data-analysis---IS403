# Load required libraries
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

# Function to forecast next n weeks
forecast_next_weeks <- function(model, last_data, n_weeks = 8) {
  forecasted_prices <- numeric(n_weeks)
  current_data <- last_data
  
  for(i in 1:n_weeks) {
    # Make prediction for next week
    next_pred <- predict(model, current_data)
    forecasted_prices[i] <- next_pred
    
    # Update the data for next prediction
    current_data <- current_data
    current_data[nrow(current_data), names(current_data) != "Date"] <- next_pred
  }
  
  return(forecasted_prices)
}

# Step 1: Read data from CSV file
df <- read.csv("C:/Users/Dell/OneDrive/Documents/403 Phan Tich Du Lieu Kinh Doanh/PET_PRI_GND_DCUS_NUS_W.csv")
df$Date <- as.Date(df$Date, format="%m/%d/%Y")
print('-------------------------------------------------------------')
print('REGULAR GAS PRICE')

# Split data into training (1995-2015) and testing (2016-2021) sets
train_data <- df %>% 
  filter(year(Date) >= 1995 & year(Date) <= 2015)

test_data <- df %>% 
  filter(year(Date) > 2015 & year(Date) <= 2021)

# # Convert Date to numeric format (days since origin) for linear regression
# # Linear regression cannot directly handle datetime data
# subdataset_for_train$Date <- as.numeric(subdataset_for_train$Date)
# subdataset_for_test$Date <- as.numeric(subdataset_for_test$Date)

#Step 2: Create training and testing sets
# Prepare training and test sets
X_train <- train_data[, !(names(train_data) %in% c("R2"))]
y_train <- train_data$R2
X_test <- test_data[, !(names(test_data) %in% c("R2"))]
y_test <- test_data$R2

# Train linear regression model
lm_model <- lm(R2 ~ ., data = train_data[, !(names(train_data) %in% "Date")])

# Predict on test set
y_pred <- predict(lm_model, X_test)

# Step 4: Forecast next 8 weeks
last_date <- max(df$Date)
future_dates <- seq(last_date + 7, by = "week", length.out = 8)
future_prices <- forecast_next_weeks(lm_model, X_test[nrow(X_test),])

# Create future predictions dataframe
future_predictions <- data.frame(
  Date = future_dates,
  Price = future_prices,
  Type = "Future Forecast"
)

# Step 5: Compare forecast results with actual values and future predictions
# Create a plotting dataframe
plot_data <- rbind(
  data.frame(
    Date = c(train_data$Date, test_data$Date, test_data$Date),
    Price = c(y_train, y_test, y_pred),
    Type = c(
      rep("Train", length(y_train)),
      rep("Actual Price (2015-2021)", length(y_test)),
      rep("Forecasted Price (2015-2021)", length(y_pred))
    )
  ),
  future_predictions
)

# Create the plot
ggplot(plot_data, aes(x = Date, y = Price, color = Type)) +
  geom_line() +
  labs(
    x = "Year",
    y = "Regular Gas Price",
    title = "Regular Gas Price Prediction with 8-Week Forecast Using Linear Regression"
  ) +
  theme_minimal()

# Step 6:  Calculate RMSE and MAE
# Step 6: Calculate RMSE and MAE
rmse <- sqrt(mean((y_test - y_pred) ^ 2))
mae <- mean(abs(y_test - y_pred))

print(paste("Root Mean Squared Error is", round(rmse, 3)))
print(paste("Mean Absolute Error is", round(mae, 3)))

#MIDRANGE GAS PRICE
print('-------------------------------------------------------------')
print('MIDRANGE GAS PRICE')

#Step 2: Create training and testing sets
# Prepare training and test sets
X_train <- train_data[, !(names(train_data) %in% c("M2"))]
y_train <- train_data$M2
X_test <- test_data[, !(names(test_data) %in% c("M2"))]
y_test <- test_data$M2

# Train linear regression model
lm_model <- lm(M2 ~ ., data = train_data[, !(names(train_data) %in% "Date")])

# Predict on test set
y_pred <- predict(lm_model, X_test)

# Step 4: Forecast next 8 weeks
last_date <- max(df$Date)
future_dates <- seq(last_date + 7, by = "week", length.out = 8)
future_prices <- forecast_next_weeks(lm_model, X_test[nrow(X_test),])

# Create future predictions dataframe
future_predictions <- data.frame(
  Date = future_dates,
  Price = future_prices,
  Type = "Future Forecast"
)

# Step 5: Compare forecast results with actual values and future predictions
# Create a plotting dataframe
plot_data <- rbind(
  data.frame(
    Date = c(train_data$Date, test_data$Date, test_data$Date),
    Price = c(y_train, y_test, y_pred),
    Type = c(
      rep("Train", length(y_train)),
      rep("Actual Price (2015-2021)", length(y_test)),
      rep("Forecasted Price (2015-2021)", length(y_pred))
    )
  ),
  future_predictions
)

# Create the plot
ggplot(plot_data, aes(x = Date, y = Price, color = Type)) +
  geom_line() +
  labs(
    x = "Year",
    y = "Midrange Gas Price",
    title = "Midrange Gas Price Prediction with 8-Week Forecast Using Linear Regression"
  ) +
  theme_minimal()

# Step 6:  Calculate RMSE and MAE
# Step 6: Calculate RMSE and MAE
rmse <- sqrt(mean((y_test - y_pred) ^ 2))
mae <- mean(abs(y_test - y_pred))

print(paste("Root Mean Squared Error is", round(rmse, 3)))
print(paste("Mean Absolute Error is", round(mae, 3)))

# PREMIUM GAS PRICE 
print('-------------------------------------------------------------')
print('PREMIUM GAS PRICE')

#Step 2: Create training and testing sets
# Prepare training and test sets
X_train <- train_data[, !(names(train_data) %in% c("P2"))]
y_train <- train_data$P2
X_test <- test_data[, !(names(test_data) %in% c("P2"))]
y_test <- test_data$P2

# Train linear regression model
lm_model <- lm(P2 ~ ., data = train_data[, !(names(train_data) %in% "Date")])

# Predict on test set
y_pred <- predict(lm_model, X_test)

# Step 4: Forecast next 8 weeks
last_date <- max(df$Date)
future_dates <- seq(last_date + 7, by = "week", length.out = 8)
future_prices <- forecast_next_weeks(lm_model, X_test[nrow(X_test),])

# Create future predictions dataframe
future_predictions <- data.frame(
  Date = future_dates,
  Price = future_prices,
  Type = "Future Forecast"
)

# Step 5: Compare forecast results with actual values and future predictions
# Create a plotting dataframe
plot_data <- rbind(
  data.frame(
    Date = c(train_data$Date, test_data$Date, test_data$Date),
    Price = c(y_train, y_test, y_pred),
    Type = c(
      rep("Train", length(y_train)),
      rep("Actual Price (2015-2021)", length(y_test)),
      rep("Forecasted Price (2015-2021)", length(y_pred))
    )
  ),
  future_predictions
)

# Create the plot
ggplot(plot_data, aes(x = Date, y = Price, color = Type)) +
  geom_line() +
  labs(
    x = "Year",
    y = "Premium Gas Price",
    title = "Premium Gas Price Prediction with 8-Week Forecast Using Linear Regression"
  ) +
  theme_minimal()

# Step 6:  Calculate RMSE and MAE
# Step 6: Calculate RMSE and MAE
rmse <- sqrt(mean((y_test - y_pred) ^ 2))
mae <- mean(abs(y_test - y_pred))

print(paste("Root Mean Squared Error is", round(rmse, 3)))
print(paste("Mean Absolute Error is", round(mae, 3)))