# Introduction to Time Series Econometrics - Preface
# Author: Daniele Ballinari
# R version 4.1.0 (2021-05-18)


rm(list=ls())


# In not already installed, you have to install the package of the book "Time Series Analysis and Its Applications: With R Examples",
# many data sets used in the following code come from this package.
install.packages("astsa")

# Set the seed for reproducibility of results that depend on random numbers
set.seed(398)

# Load the package:
library(astsa)

# See the data available in the package:
data(package="astsa")


# Example of how to assign a dataset such that it becomes visible in the working space:
gdp_data <- gdp


# Example of the use of ts-objects:
# define an array of 60 random numbers and create a cumulative sum
x <- cumsum(rnorm(n=60))
# create a time series: add time information
x_ts <- ts(data = x, c(2010, 1), end=c(2014, 12), frequency=12)
# plot the time series
plot(x_ts, type="o")
