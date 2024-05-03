library("quantmod")
library("ggplot2")
library("moments")
library("tseries")

getSymbols("^SP100",from = '2015-01-01',
           to = '2020-01-01',) 
lukke = SP100[,6]
return = Delt(lukke,type = c("arithmetic"))

#Creates a plot of  the daily returns
plot(return)

#Creates a histogram of the returns
ggplot(data = return) +
  geom_histogram(aes(x = return), bins = 100)

#to Create a Q-Q plot
sorted_return <- sort(return)
qqnorm(sorted_return)
qqline(sorted_return, col = "red")

#The following code creates an autocorrelation plot for the returns
nona_return <- na.omit(return)
acf(nona_return, main = "Autocorrelation of Returns")

#The following code creates an autocorrelation plot for squared returns
sqrd_return <- nona_return^2
acf(sqrd_return, main = "Autocorrelation of Squared Returns")

