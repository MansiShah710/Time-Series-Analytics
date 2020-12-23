## USE FORECAST LIBRARY.

library(forecast)
library(ggplot2)

## Set working directory for locating files.
setwd("/Users/adi/Downloads")


## 1. GET DATA

## Create data frame.
AirPassenger.data <- read.csv("AirPassengers.csv")

## See the first 6 records of the file.
head(AirPassenger.data)

## Create time series data set in R using the ts() function.

AirPassenger.ts <- ts(AirPassenger.data$Passengers, 
                      start = c(1949, 1), end = c(1960, 12), freq = 12)


## 2. TEST PREDICTABILITY

## Approach 1 

## 2-a: Use Arima() function to fit AR(1) model 
AirPassenger.ar1<- Arima(AirPassenger.ts, order = c(1,0,0))
summary(AirPassenger.ar1)

## Approach 2 

## 2-b: Create differenced Airpassenger data using (lag-1).
diff.AirPassenger <- diff(AirPassenger.ts, lag = 1)
diff.AirPassenger

## 2-c: Use Acf() function to identify autocorrealtion for differenced Airpassenger, and plot autocorrelation for different lags 

Acf(diff.AirPassenger, lag.max = 12, 
    main = "Autocorrelation for Differenced Air Passenger Data")


## 3. EXPLORE AND VISUALIZE SERIES

## 3-a: Apply the plot() function to create a data plot with the historical data
plot(AirPassenger.ts, 
     xlab = "Time", ylab = "AirPassenger (in $M)", 
     ylim = c(0, 700),main = "AirPassenger", col = "blue")

## 3-b: Auto correlation on time series data
Acf(AirPassenger.ts, lag.max = 12, main = "Autocorrelation for AirPassenger")

## 3-c: Use stl() function to plot times series components of the original data.
AirPassenger.stl <- stl(AirPassenger.ts, s.window = "periodic")

autoplot(AirPassenger.stl, main = "Airpassenger Time Series Components")

## 4-d: Box plot
boxplot(AirPassenger.ts~cycle(AirPassenger.ts), xlab="Passenger Numbers ('000)", ylab="Months", col=rgb(0.1,0.9,0.3,0.4), 
        main="Monthly Air Passengers Boxplot from 1950 to 1960", notch=FALSE)


## 4. DATA PREPROCESSING

## 4-a: Check for missing values
sum(is.na(AirPassenger.data))

## 4-b: Test frequency
frequency(AirPassenger.ts)

## 4-c: Test cycle 
cycle(AirPassenger.ts)

## 4-d: Dataset summary
summary(AirPassenger.ts)


## 5. DATA PARTITION

## 5-a: Develop data partition with the validation partition of 25 periods and the rest for the training partition.

nValid <- 25
nTrain <- length(AirPassenger.ts) - nValid
train.ts <- window(AirPassenger.ts, start = c(1949, 1), end = c(1949, nTrain))
valid.ts <- window(AirPassenger.ts, start = c(1949, nTrain + 1), 
                   end = c(1949, nTrain + nValid))

# Plot the time series data and visualize partitions. 
plot(train.ts, 
     xlab = "Time", ylab = "Air Passengers", ylim = c(50, 720), bty = "l",
     xaxt = "n", xlim = c(1949, 1962.25), main = "Air Passenger data partition", lwd = 2) 
axis(1, at = seq(1949, 1962.25, 1), labels = format(seq(1949, 1962.25, 1)))
lines(valid.ts, col = "black", lty = 1, lwd = 2)

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(1958.9, 1958.9), c(0, 720))
lines(c(1960.95, 1960.9), c(0, 720))
text(1954, 720, "Training", cex = 0.75)
text(1959.9, 720, "Validation", cex = 0.75)
text(1961.8, 720, "Future", cex = 0.75)
arrows(1958.7, 670, 1948.7, 670, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1959.1, 670, 1960.8, 670, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1961.1, 670, 1962.5, 670, code = 3, length = 0.1,
       lwd = 1, angle = 30)

## 6. APPLY FORECASTING METHODS

## 6-a: REGRESSION BASED MODELS 

## Apply Regression Models with trend and seasonality and apply forecast() for validataion period.

## i.  Regression model with linear trend and seasonality

train.lin.season <- tslm(train.ts ~ trend + season)
summary(train.lin.season)
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)

# Plot predictions for linear trend and seasonality forecast.
plot(train.lin.season.pred$mean, 
     xlab = "Time", ylab = "Air Passengers", ylim = c(50, 720), bty = "l",
     xlim = c(1949, 1962.25), main = "Linear Trend and Seasonality Forecast", 
     col = "blue", lwd =2) 
lines(train.lin.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(1958.9, 1958.9), c(0, 720))
lines(c(1960.95, 1960.9), c(0, 720))
text(1954, 720, "Training", cex = 0.75)
text(1959.9, 720, "Validation", cex = 0.75)
#text(1961.8, 720, "Future", cex = 0.75)
arrows(1958.7, 670, 1948.7, 670, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1959.1, 670, 1960.8, 670, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#arrows(1961.1, 670, 1962.5, 670, code = 3, length = 0.1,
 #      lwd = 1, angle = 30)

## ii. Regression model with quadratic trend and seasonality 
    
train.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(train.trend.season)
train.trend.season.pred <- forecast(train.trend.season, h = nValid, level = 0)

# Plot predictions for quadratic trend and seasonality forecast.
plot(train.trend.season.pred$mean, 
     xlab = "Time", ylab = "Air Passengers", ylim = c(50, 720), bty = "l",
     xlim = c(1949, 1962.25), main = "Quadratic Trend and Seasonality Forecast", 
     col = "blue", lwd =2) 
lines(train.trend.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(1958.9, 1958.9), c(0, 720))
lines(c(1960.95, 1960.9), c(0, 720))
text(1954, 720, "Training", cex = 0.75)
text(1959.9, 720, "Validation", cex = 0.75)
#text(1961.8, 720, "Future", cex = 0.75)
arrows(1958.7, 670, 1948.7, 670, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1959.1, 670, 1960.8, 670, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#arrows(1961.1, 670, 1962.5, 670, code = 3, length = 0.1,
#       lwd = 1, angle = 30)

## 6-b HOLT-WINTER'S MODEL
    
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ 

hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

# Plot predictions for HOLT-WINTER'S model forecast
plot(hw.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Air Passengers", ylim = c(50, 720), bty = "l",
     xlim = c(1949, 1962.25), main = "Holt-winter's model Forecast", 
     col = "blue", lwd =2) 
lines(hw.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(1958.9, 1958.9), c(0, 720))
lines(c(1960.95, 1960.9), c(0, 720))
text(1954, 720, "Training", cex = 0.75)
text(1959.9, 720, "Validation", cex = 0.75)
#text(1961.8, 720, "Future", cex = 0.75)
arrows(1958.7, 670, 1948.7, 670, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1959.1, 670, 1960.8, 670, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#arrows(1961.1, 670, 1962.5, 670, code = 3, length = 0.1,
#       lwd = 1, angle = 30)

## 7. Apply the accuracy() function to compare performance measure of the 3 forecasts developed above. 

round(accuracy(train.lin.season.pred, valid.ts), 3)
round(accuracy(train.trend.season.pred, valid.ts), 3)
round(accuracy(hw.ZZZ.pred, valid.ts), 3)


## 8. CALCULATE THE RESIDUAL OF BEST MODEL TO IMPLEMENT 2-LEVEL FORECASTING 

## 8-a: Level 1: For Quad trend+Seasonality
train.trend.season.pred$residuals
Acf(train.trend.season.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for Training Residuals for Quadratic regression model")


## 8-b: Level 2: Apply AR(1),AR(2),AR(3) and AR(12) on Quad trend+seasonality residual
res.ar1 <- Arima(train.trend.season.pred$residuals, order = c(1,0,0))
summary(res.ar1)
res.ar1$fitted

res.ar2 <- Arima(train.trend.season.pred$residuals, order = c(2,0,0))
summary(res.ar2)
res.ar2$fitted

res.ar3 <- Arima(train.trend.season.pred$residuals, order = c(3,0,0))
summary(res.ar3)
res.ar3$fitted

res.ar12 <- Arima(train.trend.season.pred$residuals, order = c(12,0,0))
summary(res.ar12)
res.ar12$fitted

## 8-c Autocorrelation for Residual of residuals
Acf(res.ar12$residuals, lag.max = 12, 
    main = "AR(12) Autocorrelation for Quadratic Training model Residuals of Residuals")


## 8-d Forecasting residuals using AR(12)
res.ar12.pred <- forecast(res.ar12, h = nValid, level = 0)
res.ar12.pred

## 8-e Combined forecasting: lm predictions and AR(12) predictions
#For training period
two.level.pred.train <- train.trend.season.pred$fitted + res.ar12.pred$fitted
two.level.pred.train

#For validation period
two.level.pred <- train.trend.season.pred$mean + res.ar12.pred$mean
two.level.pred

## 8-f Create table
table.df <- data.frame(valid.ts,train.trend.season.pred$mean, 
                       res.ar12.pred$mean, two.level.pred)
names(table.df) <- c("Validation data", "Reg.Forecast", 
                     "AR(12)Forecast", "Combined.Forecast")
table.df

## 9. APPLY AUTO-ARIMA AND FORECAST() FUNCTION TO MAKE PREDICTIONS
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

# Plot predictions for Auto ARIMA model forecast
plot(train.auto.arima.pred$mean, 
     xlab = "Time", ylab = "Air Passengers", ylim = c(50, 720), bty = "l",
     xlim = c(1949, 1962.25), main = "12 Year Auto ARIMA Model (Train/Val)", 
     col = "blue", lwd =2) 
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(1949,650, legend = c("AirPassenger Time Series", 
                            "Auto ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n", cex=0.8)

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(1958.9, 1958.9), c(0, 720))
text(1954, 720, "Training", cex = 0.75)
text(1959.9, 720, "Validation", cex = 0.75)
arrows(1958.7, 670, 1948.7, 670, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1959.1, 670, 1960.8, 670, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## 10. CHECK ACCURACY FOR TRAINING DATASET MODEL
## (1) Linear trend with seasonality
## (2) Quadratic trend with seasonality
## (3) Holt's winter
## (4) Two-level modeling (Quad trend with seas+ AR(12)) - training
## (5) Two-level modeling (Quad trend with seas + AR(12)) - validation
## (6) Auto-ARIMA

round(accuracy(train.lin.season.pred, valid.ts), 3)
round(accuracy(train.trend.season.pred, valid.ts), 3)
round(accuracy(hw.ZZZ.pred, valid.ts), 3)
round(accuracy(two.level.pred.train, train.ts), 3)
round(accuracy(two.level.pred, valid.ts), 3)
round(accuracy(train.auto.arima.pred, valid.ts), 3)

## 11. FIT AUTO ARIMA MODELS FOR ENTIRE DATA SET. 

# Use auto.arima() function to fit ARIMA model for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
auto.arima <- auto.arima(AirPassenger.ts)
summary(auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 12 periods. 
auto.arima.pred <- forecast(auto.arima, h = 12, level = 0)
auto.arima.pred

# Plot predictions for Auto ARIMA model forecast for entire dataset
plot(auto.arima.pred$mean, 
     xlab = "Time", ylab = "Air Passengers", ylim = c(50, 720), bty = "l",
     xlim = c(1949, 1962.25), main = "Auto ARIMA Forecast for entire dataset", 
     col = "blue", lwd =2, lty=5) 
lines(auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(1949,650, legend = c("AirPassenger Time Series", 
                            "Auto Arima Forecast",
                            "Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n", cex=0.8)

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
#lines(c(1958.9, 1958.9), c(0, 720))
lines(c(1960.95, 1960.9), c(0, 720))
text(1955, 720, "Training", cex = 0.75)
#text(1959.9, 720, "Validation", cex = 0.75)
text(1961.8, 720, "Future", cex = 0.75)
arrows(1960.7, 670, 1948.7, 670, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#arrows(1959.1, 670, 1960.8, 670, code = 3, length = 0.1,
#       lwd = 1, angle = 30)
arrows(1961.1, 670, 1962.5, 670, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## 12. TWO-LEVEL FORECASTING ON ENTIRE DATASET
# Level 1: Use tslm() function to create quadratic trend and seasonality model.
trend.season <- tslm(AirPassenger.ts ~ trend + I(trend^2) + season)

# See summary of linear trend equation and associated parameters.
summary(trend.season)

# Apply forecast() function to make predictions with quadratic trend and seasonal 
# model into the future 12 months.  
trend.season.pred <- forecast(trend.season, h = 12, level = 0)
trend.season.pred

# plot autocorrelation for regression residuals (up to maximum of 12).
Acf(trend.season.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for Airpassenger Regression Residuals for Entire Data Set")


# Level 2: Use Arima() function to fit AR(12) model for regression residuals.
# The ARIMA model of order = c(12,0,0) gives an AR(12) model.
residual.ar12 <- Arima(trend.season$residuals, order = c(12,0,0))

# Use summary() to identify parameters of AR(12) model.
summary(residual.ar12)

# Use forecast() function to make prediction of residuals into the future 12 months.
residual.ar12.pred <- forecast(residual.ar12, h = 12, level = 0)
residual.ar12.pred


# Use Acf() function to identify autocorrelation for the residual of residuals 
# and plot autocorrelation for different lags (up to maximum of 12).
Acf(residual.ar12$residuals, lag.max = 12, 
    main = "Autocorrelation for Airpassenger Residuals of Residuals for Entire Data Set")


# Identify forecast for the future 12 periods as sum of quadratic trend and seasonal model
# and AR(12) model for residuals.
trend.season.ar12.pred <- trend.season.pred$mean + residual.ar12.pred$mean
trend.season.ar12.pred


# plot historical data, predictions for historical data, and forecast for 12 future periods.
plot(AirPassenger.ts, 
     xlab = "Time", ylab = "Air Passengers", ylim = c(50, 720), bty = "l",
     xaxt = "n", xlim = c(1949, 1962.25), lwd = 2,
     main = "Two-Level Forecast: Regression with Quadratic Trend and Seasonlity + AR(12)
     for Residuals for Entire Data Set") 
axis(1, at = seq(1949, 1962.25, 1), labels = format(seq(1949, 1962.25, 1)))
lines(trend.season$fitted + residual.ar12$fitted, col = "blue", lwd = 2)
lines(trend.season.ar12.pred, col = "blue", lty = 5, lwd = 2)
legend(1950,650, legend = c("Air Passengers Series (Training Data)", 
                             "Two-Level Forecast for Training Data", "Two-Level Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.

lines(c(1961, 1961), c(0, 720))
text(1955, 720, "Training")
text(1961.8, 720, "Future")
arrows(1948.7, 670, 1960.8, 670, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1961.1, 670, 1962.5, 670, code = 3, length = 0.1,
       lwd = 1, angle = 30)



# Create a data table with quadratic trend and seasonal forecast for 12 future periods,
# AR(12) model for residuals for 12 future periods, and combined two-level forecast for
# 12 future periods. 
table.df <- data.frame(trend.season.pred$mean, 
                       residual.ar12.pred$mean, trend.season.ar12.pred)
names(table.df) <- c("Reg.Forecast", "AR(12)Forecast","Combined.Forecast")
table.df

##12 c : HW model on entire dataset

hw_full.ZZZ <- ets(AirPassenger.ts, model = "ZZZ")
hw_full.ZZZ 

hw_full.ZZZ.pred <- forecast(hw_full.ZZZ, h = 12, level = 0)
hw_full.ZZZ.pred

plot(AirPassenger.ts, 
     xlab = "Time", ylab = "Air Passengers", ylim = c(50, 720), bty = "l",
     xaxt = "n", xlim = c(1949, 1962.25), lwd = 2,
     main = "HW model for Entire Data Set") 
axis(1, at = seq(1949, 1962.25, 1), labels = format(seq(1949, 1962.25, 1)))
lines(hw_full.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(hw_full.ZZZ.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(1950,650, legend = c("Air Passengers Series (Training Data)", 
                            "HW model for Training Data", "HW Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.

lines(c(1961, 1961), c(0, 720))
text(1955, 720, "Training")
text(1961.8, 720, "Future")
arrows(1948.7, 670, 1960.8, 670, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1961.1, 670, 1962.5, 670, code = 3, length = 0.1,
       lwd = 1, angle = 30)

## 13. MEASURE FORECAST ACCURACY FOR ENTIRE DATA SET.

# Use accuracy() function to identify common accuracy measures for:
# (1) Two-level model (regression model with quadratic trend and seasonality + AR(12) model for residuals),
# (2) Auto ARIMA Model
# (3) HW model
# (4) Seasonal naive forecast, and
# (5) Naive forecast.

# Accuracy measures for 2 level model (regression model with quadratic trend and seasonality + AR(12) for residuals)
round(accuracy(trend.season$fitted + residual.ar12$fitted, AirPassenger.ts), 3)

# Accuracy measures for Auto ARIMA model
round(accuracy(auto.arima.pred$fitted, AirPassenger.ts), 3)

#Accuracy measures for HW model
round(accuracy(hw_full.ZZZ.pred$fitted,AirPassenger.ts),3)

# Accuracy measures for seasonal naive model
round(accuracy((snaive(AirPassenger.ts))$fitted, AirPassenger.ts), 3)

# Accuracy measures for naive model
round(accuracy((naive(AirPassenger.ts))$fitted, AirPassenger.ts), 3)


