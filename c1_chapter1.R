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
