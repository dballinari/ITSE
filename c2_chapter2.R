# Introduction to Time Series Econometrics - Chapter 2 
# Author: Daniele Ballinari
# R version 4.1.0 (2021-05-18)

rm(list = ls())

# In not already installed, you have to install the package of the book "Time Series Analysis and Its Applications: With R Examples",
# many data sets used in the following code come from this package.
# install.packages("astsa")
library(astsa)

# RANDOM WALK
# For reproducibility:
set.seed(123)
# Generate i.i.d. random variables:
z <- rnorm(1000)
# The random walk without drift is then simply the cumulative sum of these random variables
y <- cumsum(z)
# The random walk with drift is instead defined as the cumulative sum of the i.i.d.
# random variables plus the drift:
yt <- cumsum(z+0.2)
# Convert the data to time series:
y <- ts(y, start=1, frequency = 1)
yt <- ts(yt, start=1, frequency = 1)
# Plot the random walk with drift:
plot(yt, main="Random walk")
# Plot the random walk without drift in blue:
lines(y, col="blue")
# Add lines depicting the trend:
abline(a=0, b=0.2, lty=2)
abline(h=0, lty=2, col="blue")
grid()


# TIME SERIES WITH LINEAR TREND
# For reproducibility:
set.seed(123)
# Generate i.i.d. random variables:
z <- rnorm(1000)
# Generate the time series with a linear trend and white noise:
y <- 0.1 + 0.05*(1:1000) + z
# Transform 'y' to a ts-object:
y <- ts(y, start=1, frequency = 1)
# Plot the time series:
plot(y, main="Time series with linear trend")
grid()
# Remove the linear trend by taking the first difference:
Delta_y <- diff(y)
# Plot the new time series:
plot(Delta_y, main="First difference of the time series")
grid()
# Compute sample moments:
n <- length(Delta_y)
sample_mean <- sum(Delta_y)/n
sample_var <- sum((Delta_y - sample_mean)^2)/n
sample_gamma_1 <- sum((Delta_y[-1] - sample_mean)*(Delta_y[-n] - sample_mean))/n
sample_rho_1 <- sample_gamma_1/sample_var
# Plot the ACF:
acf(Delta_y)
grid() 
