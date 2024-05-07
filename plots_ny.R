library("quantmod")
library("ggplot2")
library("moments")
library("tseries")
library("gridExtra")

getSymbols("WMT",from = '2013-01-01',
           to = '2016-01-01',) 
getSymbols("IBM",from = '2013-01-01',
           to = '2016-01-01',) 
getSymbols("LIN",from = '2013-01-01',
           to = '2016-01-01',) 
getSymbols("WFC",from = '2013-01-01',
           to = '2016-01-01',) 
getSymbols("MO",from = '2013-01-01',
           to = '2016-01-01',) 
getSymbols("LMT",from = '2013-01-01',
           to = '2016-01-01',) 
lukke_1 = WMT[,6]
lukke_2 = IBM[,6]
lukke_3 = LIN[,6]
lukke_4 = WFC[,6]
lukke_5 = MO[,6]
lukke_6 = LMT[,6]
return_1 = Delt(lukke_1,type = c("arithmetic"))
return_2 = Delt(lukke_2,type = c("arithmetic"))
return_3 = Delt(lukke_3,type = c("arithmetic"))
return_4 = Delt(lukke_4,type = c("arithmetic"))
return_5 = Delt(lukke_5,type = c("arithmetic"))
return_6 = Delt(lukke_6,type = c("arithmetic"))

#to Create a Q-Q plot
sorted_return_1 <- sort(return_1)
sorted_return_2 <- sort(return_2)
sorted_return_3 <- sort(return_3)
sorted_return_4 <- sort(return_4)
sorted_return_5 <- sort(return_5)
sorted_return_6 <- sort(return_6)

# Create a layout for the grid
layout(matrix(c(1,2,3,4,5,6), nrow=2))

# Plot each graph in the specified grid cell
par(mar=c(4, 4, 2, 2))
qqnorm(sorted_return_1, main = "WMT")
qqline(sorted_return_1, col = "red")
qqnorm(sorted_return_4, main = "WFC")
qqline(sorted_return_4, col = "red")

qqnorm(sorted_return_2, main = "IBM")
qqline(sorted_return_2, col = "red")
qqnorm(sorted_return_5, main = "MO")
qqline(sorted_return_5, col = "red")

qqnorm(sorted_return_3, main = "LIN")
qqline(sorted_return_3, col = "red")
qqnorm(sorted_return_6, main = "LMT")
qqline(sorted_return_6, col = "red")

#The following code creates an autocorrelation plot for the returns
#nona_return <- na.omit(return)
#acf(nona_return, main = "Autocorrelation of Returns")

#The following code creates an autocorrelation plot for squared returns
#sqrd_return <- nona_return^2
#acf(sqrd_return, main = "Autocorrelation of Squared Returns")

