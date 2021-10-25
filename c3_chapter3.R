# Introduction to Time Series Econometrics - Chapter 3 
# Author: Daniele Ballinari
# R version 4.1.0 (2021-05-18)

rm(list = ls())

# In not already installed, you have to install the package of the book "Time Series Analysis and Its Applications: With R Examples",
# many data sets used in the following code come from this package.
# install.packages("astsa")
library(astsa)


# Plot of the ACF of the AR(1) process with positive coefficient
# Define the parameter of the AR(1) process
phi <- 0.5
# Define the lags for which we want to compute the ACF:
h <- 0:10
# Compute the ACF:
acf_values <- phi^h
# Plot the ACF:
plot(h, acf_values, type="b", ylim = c(-1, 1), main="ACF", xlab = "Lags", ylab = "")
grid()
abline(h=0, lty=2)

# Simulate observations of the AR(1) process with positive coefficient
# For reproducibility:
set.seed(123)
# Simulate 500 observations:
y_sim <- arima.sim(model = list(ar = 0.5), n = 100)
# Convert to a ts-object:
y_sim <- ts(y_sim, start=1, frequency = 1)
# Plot the simulated time series:
plot(y_sim, type="o")
grid()
abline(h=0.2)

# Plot of the ACF of the AR(1) process with negative coefficient
# Define the parameter of the AR(1) process
phi <- -0.5
# Define the lags for which we want to compute the ACF:
h <- 0:10
# Compute the ACF:
acf_values <- phi^h
# Plot the ACF:
plot(h, acf_values, type="b", ylim = c(-1, 1), main="ACF", xlab = "Lags", ylab = "")
grid()
abline(h=0, lty=2)

# Simulate observations of the AR(1) process with negative coefficient
# For reproducibility:
set.seed(123)
# Simulate 500 observations:
y_sim <- arima.sim(model = list(ar = -0.5), n = 100)
# Convert to a ts-object:
y_sim <- ts(y_sim, start=1, frequency = 1)
# Plot the simulated time series:
plot(y_sim, type="o")
grid()
abline(h=0.2/3)


# FIT ARMA(1,1) to JJ EARNINGS
# get the log earnings of Johnson and Johnson
jj_log_earnings <- log(jj)
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
# Plot the reminder component:
plot(x, main ="Remainder component")
grid()
# Fit an ARMA(1,1) model on the reminder term:
arma11 <- arima(x, order = c(1, 0, 1))
# Coefficients:
print(arma11$coef)
# Variance of Z:
print(arma11$sigma2)
# Number of observations:
nobs <- length(x)
# Number of parameters:
k_arama11 <- 2+1+1
# MSE:
mse_arma11 <- mean(arma11$residuals^2)
# Compute AIC:
aic_arma11 <- nobs*log(mse_arma11) + 2*k_arama11

# Fit an ARMA(2,1) model on the reminder term:
arma21 <- arima(x, order = c(2, 0, 1))
# Number of parameters:
k_arama21 <- 3+1+1
# MSE:
mse_arma21 <- mean(arma21$residuals^2)
# Compute AIC:
aic_arma21 <- nobs*log(mse_arma21) + 2*k_arama21

# Load the package:
library(forecast)
# Determine the best model in terms of AIC:
best_aic <- auto.arima(x, d=0, D=0, seasonal=FALSE, ic = "aic")
print(best_aic)
# Determine the best model in terms of BIC:
best_bic <- auto.arima(x, d=0, D=0, seasonal=FALSE, ic = "bic")
print(best_bic)
