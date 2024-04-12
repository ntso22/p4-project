library("tidyverse")
library("tidyquant")
library("quantmod")

#Importing the Data
library(readr)
prices <- read_csv("prices.csv")
returns <- read_csv("returns.csv")

#Calculates the number of assets
n = as.numeric(ncol(prices))

#Calculate the expected return of S&P100
sample_means = (1+apply(returns, 2, mean))^{252}-1

#Risk Free Asset
rf = 0.0446

#Range of beta
beta_theory = seq(0,2,0.01)

#Import Market Portfolio
market_portfolio <- read_csv("market_portfolio.csv")

#Daily returns of the market portfolio
market_daily = (as.matrix(returns)%*%as.matrix(market_portfolio[-1,2]))

#Calculates the expected return and variance of the market portfolio
rm = mean(market_daily)
market_var = var(market_daily)[1,1]

#Security market line function#
sml = function(b) {
  rf + beta*(as.numeric(rm) - as.numeric(rf))
}

#Calulates the covariance between asset i and the market portfolio
covar = function(i) {
  cov(market_daily, returns[,i])
}

#Calculates the Beta of all assets
beta = function(i) {
  covar(i) /market_var
}


#Plot
x = c()
for (i in 1:n) {
  x = c(x, beta(i))
}

y = c()
for (i in 1:n){
  y = c(y,sample_means[i])
}

plot(x,y)
plot(sml)

