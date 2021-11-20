# Introduction to Time Series Econometrics - Chapter 4 
# Author: Daniele Ballinari
# R version 4.1.0 (2021-05-18)

rm(list = ls())

# In not already installed, you have to install the package of the book "Time Series Analysis and Its Applications: With R Examples",
# many data sets used in the following code come from this package.
# install.packages("astsa")
library(astsa)

# We are going to investigate if the time series of the annual unemployment rate 
# in the UK is a random walk. You can obtain this data from https://fred.stlouisfed.org/series/UNRTUKA
# Set your working directory (tell R from which folder you are working from):
setwd("C:/Users/danie/OneDrive/Work/PostDoc Basel/Teaching/Introduction to Time Series Econometrics/Codes/ITSE")
# load the data (note that the csv file should be in the folder you are working from):
unemp_rate_raw <- read.csv(file = "unemp_rate_uk.csv")
# define the time series:
unemp_rate <- ts(unemp_rate_raw$UNRTUKA, start = 1760, frequency = 1)
# plot the data
plot(unemp_rate, type='l', main='', ylab='Annual unemployment rate in %')
# add grid lines for better readability
grid()
# estimate AR(1) process:
ar <- arima(unemp_rate, order = c(1,0,0))
print(ar)
# define the number of observations:
n <- length(unemp_rate)
# define the differenced time series and the lagged time series
delta_x <- diff(unemp_rate)
x_lag <- unemp_rate[-n]
# we can check if this time series is stationary by running the following regression:
reg_df <- lm(delta_x ~ x_lag)
summary(reg_df)
# load the package:
# install.packages("urca")
library(urca)
# compute the ADF test:
test <- ur.df(unemp_rate, type = "drift", selectlags = "AIC")
summary(test)



# Forecasting ARMA models
# First we have to find an appropriate model (we use the BIC):
model_unemp_rate <- auto.arima(unemp_rate, d=0, D=0, seasonal=FALSE, ic = "bic")
summary(model_unemp_rate)
# We obtain forecasts with the function 'forecast' from the package 'forecast':
# install.packages("forecast")
library(forecast)
model_unemp_rate_forecast <- forecast(model_unemp_rate, h = 20)
# The function returns many information, we just need the 
# expected value:
unemp_rate_predicted <- model_unemp_rate_forecast$mean

# Plot the forecasting results
plot(unemp_rate, xlim=c(1760, 2016+20), type='l', main='', ylab='Annual unemployment rate in %')
grid()
lines(unemp_rate_predicted, col="red")
abline(h=mean(unemp_rate), lty=2)




# Forecast the log earnings of Johnson and Johnson
# get the log earnings of Johnson and Johnson up to the end of 1978
jj_log_earnings <- window(log(jj), end=c(1978,4))
# plot the data
plot(jj_log_earnings, type='o', main='', ylab='Quarterly log earnings per share')
# add grid lines for better readability
grid()

# Create a data frame that allows us to estimate trend and seasonality:
trendseason_df <- data.frame(earn=jj_log_earnings, 
                             s1 = rep(c(1,0,0,0), length(jj_log_earnings)/4), 
                             s2 = rep(c(0,1,0,0), length(jj_log_earnings)/4), 
                             s3 = rep(c(0,0,1,0), length(jj_log_earnings)/4), 
                             s4 = rep(c(0,0,0,1), length(jj_log_earnings)/4), 
                             tt = time(jj_log_earnings))
head(trendseason_df)
# Estimate the parameters: run without intercept
trendseason_reg <- lm(earn ~ tt + s1 + s2 + s3 + s4 -1, data = trendseason_df)

# Define a0:
a0 <- mean(coef(trendseason_reg)[c("s1", "s2", "s3", "s4")])
# Get a1:
a1 <- coef(trendseason_reg)["tt"]
# Define the seasonal components:
season_components <- coef(trendseason_reg)[c("s1", "s2", "s3", "s4")] - a0
# Get the residual component:
x <- jj_log_earnings - fitted(trendseason_reg)

# Check the reminder:
test_x <- ur.df(x, type = "drift", selectlags = "BIC")
summary(test_x)


# Forecasts of the seasonal pattern: the forecasts for the seasonal 
# component are just the seasonal coefficients estimated in the regression:
season_predicted <- ts(rep(season_components, 2), start = c(1979, 1), frequency = 4)

# Forecast of the trend: the trend can be forecasted by simply using the estimated
# parameters on future values of the time index:
tt_future <- seq(1979, 1980.75, by = 0.25)
trend_predicted <- ts(a0 + a1*tt_future, start = c(1979, 1), frequency = 4 )


# Find best model for the reminder term:
reminder_model <- auto.arima(x, d=0, D=0, seasonal=FALSE, ic = "bic")
# We obtain forecasts with the function 'forecast':
reminder_model_forecast <- forecast(reminder_model, h = 8)
# The function returns many information, we just need the expected
# value of the reminder term:
x_predicted <- reminder_model_forecast$mean

# Combine all forecasts:
jj_predicted <- trend_predicted + season_predicted + x_predicted 

# Plot the forecasts against the original time series
plot(log(jj),type='o', main='', xlim = c(1960, 1981), ylim=c(-1, 3), ylab = "Quarterly log earnings per share")
lines(jj_predicted, col="red", type='o')
grid()

