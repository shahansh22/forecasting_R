data <- read.csv("iphonesales.csv")
ipts <- ts(data$Sales,start=c(2007, 3), end=c(2016, 4), frequency=4)
ipts
plot(ipts)
masales<-ma(ipts, 4)
plot(ipts)
lines(masales, col="red", lwd=3)
de <- decompose(ipts, type="additive")
plot(de)
plot(ipts)
lines(de$trend, col=2)
lines(de$seasonal, col=3)
de <- decompose(ipts, type="multiplicative")
plot(de)
trend_values <- na.omit(de$trend)  
time_index <- 1:length(trend_values)
trend_model <- lm(trend_values ~ time_index)
future_time <- (length(trend_values) + 1):(length(trend_values) + 8)
future_trend <- predict(trend_model, newdata = data.frame(time_index = future_time))
seasonal_pattern <- tail(de$seasonal, frequency(ipts))  # Get last seasonal cycle
future_seasonal <- rep(seasonal_pattern, length.out = length(future_time))
future_forecast <- future_trend * future_seasonal
future_ts <- ts(future_forecast, start=c(2017, 1), frequency=4)
plot(ipts, xlim=c(2007, 2020), ylim=range(ipts, future_forecast), 
     main="iPhone Sales Forecast", ylab="Sales", xlab="Year")
lines(de$trend, col="blue", lwd=2)  # Existing trend
lines(future_ts, col="red", lwd=2, lty=2)  # Forecasted values

library("quantmod")
getSymbols('AAPL', src="yahoo", from=as.Date("2018-01-01"), to=as.Date("2019-01-01"))
candleChart(AAPL, up.col = "blue", dn.col = "red", theme = "white")

s10 <- SMA(AAPL[, "AAPL.Close"], n=10)
plot(AAPL[, "AAPL.Close"], main = "Apple Stock")  
lines(s10, col=2)
w10 <- WMA(AAPL[, "AAPL.Close"], n=10, w= c(0.4,0.3,0.1,0.07,0.05,0.03,0.02,0.01,0.01,0.01))
plot(AAPL[, "AAPL.Close"], main = "Apple Stock") 
lines(w10, col=2)
candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white")
addBBands(sd = 2, maType = "SMA", draw = 'bands')
candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white")
addBBands(sd = 2, maType = "WMA", draw = 'bands')
