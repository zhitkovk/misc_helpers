library(zoo)
library(xts)
library(forecast)
library(lubridate)

# Read data
home_work <- read.delim("~/Documents/repo/daily_tasks/home_work.txt")

# Select training points
train <- home_work[1:21,]

# Calculate conversion
train$conv <- train$Заказы/as.numeric(as.character(train$Рассылки))


train$conv_m <- 
# Convert it to TS object
train$conv <- xts(train$conv, order.by = dmy(train$Неделя), frequency = 7)

# Look at data

# Looks more like white noise. So the process is described by normal distribution
tsdisplay(train$conv, main = "Conversion")

# Get trend and seasonality
train$conv_d <- ts(train$conv, start = as.Date("2016-01-25"), frequency = 7)

# Probably they're present
plot(decompose(train$conv_d))

ws <- train$conv_d - decompose(train$conv_d)$seasonal

# Prediction

# Exp smoothing with trend and seasonality. So we make no assumptions about correlations between data points
hw <- HoltWinters(train$conv_d)

# Exclude seasonality
ws_pred <- HoltWinters(ws)
plot(ws_pred)

# Forecast on train data
plot(hw)

hw_f <- forecast.HoltWinters(hw, h = 10)

# Exclude seasonality
ws_f <- forecast.HoltWinters(ws_pred, h = 10)

# Forecast itself
hw_f

# Pic with it
plot(hw_f)
plot(ws_f)

# Function to plot forecast errors from little book of r for time series
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

# Check prediction errors: not that normal...
plotForecastErrors(hw_f$residuals) 
plotForecastErrors(ws_f$residuals)

# ARIMA just in case -- > White Noise: predictions are useless. No correlation between values
arima_hw <- auto.arima(train$conv)

# Results indicate white noise
arima_hw
