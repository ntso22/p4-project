library("tidyverse")
library("tidyquant")
library("quantmod")

#Importing the Data
library(readr)
prices <- read_csv("prices.csv")
returns <- read_csv("returns.csv")

#Calculates the number of assets
n = as.numeric(ncol(prices))

#Calculate the expected return of all assets in the S&P100
sample_means = apply(returns, 2, mean)

#Risk Free Asset
rf = (1+0.0446)^{1/365}-1

#Range of beta
beta_theory = seq(0,2,0.01)

#Import the S&P 100 Index
market_portfolio <- tq_get('^SP100',
                    from = '2015-01-01',
                    to = '2020-01-01',
                    get = 'stock.prices')[,8]

#Daily returns of the S&P100 Index
market_daily <- na.omit(market_portfolio / lag(market_portfolio)) - 1


#Calculates the expected return and variance of the S&P100 Index
rm = as.numeric(apply(market_daily, 2, mean))
market_var = var(market_daily)[1,1]

#Calculates the covariance between asset i and the S&P100 Index
covar = function(i) {
  cov(market_daily, returns[,i])
}

#Calculates the Beta of all assets
beta = function(i) {
  covar(i)/market_var
}

#Security market line function#
sml = function(b) {
  rf + beta_theory*(rm - rf)
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

#Creates dataframes for plotting
df_data <- data.frame(x,y,colnames(returns))
df_sml <- data.frame(beta_theory = beta_theory, sml = sml(beta_theory))

SML <- ggplot(df_data , aes(x = x, y = y, color = colnames(returns))) + 
  geom_point(size = 2) + 
  geom_line (data = df_sml , aes(x = beta_theory, y = sml), color = "black") +
  geom_text(aes(label = colnames(returns)), vjust = -0.5, hjust = 1) +
  theme(legend.position = "none") +
  labs(x = "Beta", y = "Expected Daily Return")
SML



