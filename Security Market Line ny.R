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
rf = (1+0.0005351351)^{1/365}-1

#Range of beta
beta_theory = seq(0,2,0.01)

#Import the S&P 100 Index
market_portfolio <- tq_get('^SP100',
                           from = '2013-01-01',
                           to = '2016-01-01',
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
beta1 = function(i) {
  covar(i)/market_var
}

#Security market line function#
sml = function(b) {
  rf + beta_theory*(rm - rf)
}

#Plot
x = c()
for (i in 1:n) {
  x = c(x, beta1(i))
}

y = c()
for (i in 1:n){
  y = c(y,sample_means[i])
}

#Creates dataframes for plotting
df_data <- data.frame(x,y,colnames(returns))
df_sml <- data.frame(beta_theory = beta_theory, sml = sml(beta_theory))

#Plots the SML for all assets
SML <- ggplot(df_data , aes(x = x, y = y, color = colnames(returns))) + 
  geom_point(size = 1) + 
  geom_smooth(data = df_sml , aes(x = beta_theory, y = sml), color = "black") +
  geom_text(aes(label = colnames(returns)), vjust = -0.5, hjust = 1, size = 3) +
  theme(legend.position = "none") +
  labs(x = expression(beta), y = expression(E(r)))
SML


#Creates dataframes for plotting the assets with the highest and lowest weights
x_1 = c(beta1("WMT")[1,1], beta1("IBM")[1,1], beta1("LIN")[1,1], beta1("WFC")[1,1], beta("MO")[1,1], beta1("LMT")[1,1])
y_1 = c(sample_means["WMT"], sample_means["IBM"], sample_means["LIN"], sample_means["WFC"], sample_means["MO"], sample_means["LMT"])
asset_names = c("WMT", "IBM", "LIN", "WFC", "MO", "LMT")

df_data2 <- data.frame(x_1,y_1, asset_names)

# Plots the SML for selected assets
SML <- ggplot(df_data2 , aes(x = x_1, y = y_1, color = asset_names)) + 
  geom_point(size = 2) + 
  geom_line(data = df_sml, aes(x = beta_theory, y = sml), color = "black") +
  geom_text(aes(label = asset_names), vjust = -0.5, hjust = 1) +
  theme(legend.position = "none") +
  labs(x = expression(beta), y = expression(E(r))) +  ylim(-0.001,0.002)
SML

#Calculate the Daily Returns of the Tangency Portfolio and the expected return of the tagency portfolio
tangency_portfolio = read_csv("market_portfolio.csv")
tangency_expected = sum(tangency_portfolio$portfolio*tangency_portfolio$returns_vec)

#Calculates the beta of the Tangency Portfolio
tangency_beta = sum(x*tangency_portfolio$portfolio[2:98])


#Plots the tangency portfolio on the SML
df_data3 = data.frame(tangency_beta,tangency_expected)

SML <- ggplot(df_data3 , aes(x = tangency_beta, y = tangency_expected)) + 
  geom_point(size = 2) + 
  geom_line(data = df_sml, aes(x = beta_theory, y = sml), color = "black") +
  geom_text(label = "Tangency Portfolio", vjust = -0.5, hjust = 1) +
  theme(legend.position = "none") +
  labs(x = expression(beta), y = expression(E(r))) + ylim(0,0.025)
SML

#Calculates Jensen's Index
jensen_index = tangency_expected - rf - tangency_beta*(rm - rf)

#Sharpe Ratio of the tangency portfolio and the Market Portfolio
tangency_return = as.matrix(returns) %*% as.matrix(tangency_portfolio[2:98,2])
tangency_std = sqrt(var(tangency_return)[1,1])
tangency_sharpe = (tangency_expected - rf)/tangency_std
market_sharpe = (rm - rf)/sqrt(market_var)

#Calculates the correlation between the Tangency and Market Portfolio.
cor(tangency_return,market_daily)
