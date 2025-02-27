property <- read.csv("propertysales(1).csv",header = TRUE)
emit <- read.csv("emissions(1).csv",header = TRUE)
prius <- read.csv("prius(1).csv",header = TRUE)
psts <- ts(property$Total_Sales, frequency = 12)
ets <- ts(emit$emissions, frequency = 1)
pts <- ts(prius$Num, frequency = 4)
plot(psts)
plot(ets)
plot(pts)
decompose(psts)
decompose(pts)



forecast_methods_with_performance <- function(ts_object, h = 4) {
  n <- length(ts_object)
  train_data <- ts_object[1:(n - 4)]
  test_data <- ts_object[(n - 3):n]
  
  # Forecast using the Average method
  avg_forecast <- meanf(train_data, h = h)
  
  # Forecast using the Naive method
  naive_forecast <- naive(train_data, h = h)
  
  # Forecast using the Seasonal Naive method
  seasonal_naive_forecast <- snaive(train_data, h = h)
  
  # Forecast using the Drift method
  drift_forecast <- rwf(train_data, h = h, drift = TRUE)
  
  # Calculate the accuracy for each method
  accuracy_avg <- accuracy(avg_forecast, test_data)
  accuracy_naive <- accuracy(naive_forecast, test_data)
  accuracy_seasonal_naive <- accuracy(seasonal_naive_forecast, test_data)
  accuracy_drift <- accuracy(drift_forecast, test_data)
  
  # Return the forecasts and accuracy metrics for each method
  return(list(
    average_forecast = avg_forecast$mean,
    naive_forecast = naive_forecast$mean,
    seasonal_naive_forecast = seasonal_naive_forecast$mean,
    drift_forecast = drift_forecast$mean,
    performance = list(
      average = accuracy_avg,
      naive = accuracy_naive,
      seasonal_naive = accuracy_seasonal_naive,
      drift = accuracy_drift
    )
  ))
}

forecast_results <- forecast_methods_with_performance(psts, h = 4)

# Print the forecasts
print(forecast_results$average_forecast)
print(forecast_results$naive_forecast)
print(forecast_results$seasonal_naive_forecast)
print(forecast_results$drift_forecast)

# Print the performance measures
print(forecast_results$performance$average)
print(forecast_results$performance$naive)
print(forecast_results$performance$seasonal_naive)
print(forecast_results$performance$drift)



forecast_ets_with_performance <- function(ts_object, h = 4) {

  n <- length(ts_object)
  train_data <- ts_object[1:(n - 4)]
  test_data <- ts_object[(n - 3):n]
  
  ets_forecast <- ets(train_data) 
  ets_fcst <- forecast(ets_forecast, h = h)
  accuracy_ets <- accuracy(ets_fcst, test_data)

  return(list(
    ets_forecast = ets_fcst$mean,       
    performance = accuracy_ets          
  ))
}


forecast_results_ets <- forecast_ets_with_performance(psts, h = 4)

print(forecast_results_ets$ets_forecast)

print(forecast_results_ets$performance)


forecast_autoarima_with_performance <- function(ts_object, h = 4) {
  
  n <- length(ts_object)
  train_data <- ts_object[1:(n - 4)]
  test_data <- ts_object[(n - 3):n]
  
  arima_model <- auto.arima(train_data)
  
  arima_fcst <- forecast(arima_model, h = h)
  
  accuracy_arima <- accuracy(arima_fcst, test_data)
  
  # Return the forecast and performance metrics
  return(list(
    arima_forecast = arima_fcst$mean,       
    performance = accuracy_arima            
  ))
}


# Call the function to get Auto ARIMA forecasts and performance
forecast_results_arima <- forecast_autoarima_with_performance(psts, h = 4)

# Print the Auto ARIMA forecasted values
print(forecast_results_arima$arima_forecast)

# Print the performance measures
print(forecast_results_arima$performance)
print(forecast_results_ets$performance)
print(forecast_results$performance$average)
print(forecast_results$performance$naive)
print(forecast_results$performance$seasonal_naive)
print(forecast_results$performance$drift)

forecast_results <- forecast_methods_with_performance(pts, h = 4)

print(forecast_results$average_forecast)
print(forecast_results$naive_forecast)
print(forecast_results$seasonal_naive_forecast)
print(forecast_results$drift_forecast)

print(forecast_results$performance$average)
print(forecast_results$performance$naive)
print(forecast_results$performance$seasonal_naive)
print(forecast_results$performance$drift)

forecast_results_ets <- forecast_ets_with_performance(pts, h = 4)
print(forecast_results_ets$ets_forecast)
print(forecast_results_ets$performance)


forecast_results_arima <- forecast_autoarima_with_performance(pts, h = 4)
print(forecast_results_arima$arima_forecast)
print(forecast_results_arima$performance)


forecast_results <- forecast_methods_with_performance(ets, h = 4)

print(forecast_results$average_forecast)
print(forecast_results$naive_forecast)
print(forecast_results$seasonal_naive_forecast)
print(forecast_results$drift_forecast)

print(forecast_results$performance$average)
print(forecast_results$performance$naive)
print(forecast_results$performance$seasonal_naive)
print(forecast_results$performance$drift)

forecast_results_ets <- forecast_ets_with_performance(ets, h = 4)
print(forecast_results_ets$ets_forecast)
print(forecast_results_ets$performance)


forecast_results_arima <- forecast_autoarima_with_performance(ets, h = 4)
print(forecast_results_arima$arima_forecast)
print(forecast_results_arima$performance)
