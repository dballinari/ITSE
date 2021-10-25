# Introduction to Time Series Econometrics - Chapter 1 
# Author: Daniele Ballinari
# R version 4.1.0 (2021-05-18)

rm(list = ls())

# In not already installed, you have to install the package of the book "Time Series Analysis and Its Applications: With R Examples",
# many data sets used in the following code come from this package.
# install.packages("astsa")
library(astsa)

# get the earnings of Johnson and Johnson
jj_earnings <- jj
# plot the data
plot(jj_earnings, type='o', main='', ylab='Quarterly earnings per share')
# add grid lines for better readability
grid()


# get temperature data
temp_data <- tempr
# transform Fahrenheit to Celsius
temp_data <- (temp_data-32)*5/9
# plot the data
plot(temp_data, type='l', main='', ylab='Temperature in LA')
# add grid lines for better readability
grid()



# get weekly returns of the SP 500 index
sp500_data <- sp500w
# note that this data comes in the form of an "xts" object, which is a different
# class used in R to handle time series data; 
# we transform the xts object to a ts object for consistency with the other data
sp500_data <- ts(sp500_data, start=c(2003,1), frequency= 52)
# plot the data
plot(sp500_data, type='l', main='', ylab='Weekly returns')
# add grid lines for better readability
grid()


# Generate the example of a time series decomposition
# To reproduce the results, set the seed
set.seed(123)
# Define the number of observations in the time series
n <- 100
# Create the trend component
trend <- 10 + 0.1*1:n
# Create the seasonal component:
season <- rep(c(-1, 0, 1, 0), n/4)
# Generate the random component
x <- rnorm(n, mean = 0, sd = 1)
# Define the time series
y <- trend + season + x
# Lets convert all components into ts-objects and merge them
y <- ts(y, start = c(1, 1), frequency = 4)
trend <- ts(trend, start = c(1, 1), frequency = 4)
season <- ts(season, start = c(1, 1), frequency = 4)
x <- ts(x, start = c(1, 1), frequency = 4)
# combine the time series y and its components in one object:
ts_components <- ts.union(y, trend, season, x)
# plot the time series and its components
plot(ts_components, type ="l", main="")
grid()


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

# Plot the components:
jj_trend <- ts(a0 + a1*time(jj_log_earnings), start = start(jj_log_earnings), frequency = frequency(jj_log_earnings))
jj_season <- ts(rep(season_components, 21), start = start(jj_log_earnings), frequency = frequency(jj_log_earnings))
jj_log_components <- ts.union(jj_log_earnings, jj_trend, jj_season, x)
plot(jj_log_components, type ='o', main='Quarterly log earnings per share')
grid()