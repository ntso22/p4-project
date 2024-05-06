library("quantmod")
library("ggplot2")
library("moments")
library("tseries")
library("gridExtra")

getSymbols("JPM",from = '2015-01-01',
           to = '2020-01-01',) 
getSymbols("NEE",from = '2015-01-01',
           to = '2020-01-01',) 
getSymbols("GOOGL",from = '2015-01-01',
           to = '2020-01-01',) 
getSymbols("GD",from = '2015-01-01',
           to = '2020-01-01',) 
getSymbols("DUK",from = '2015-01-01',
           to = '2020-01-01',) 
getSymbols("CL",from = '2015-01-01',
           to = '2020-01-01',) 
lukke_1 = JPM[,6]
lukke_2 = NEE[,6]
lukke_3 = GOOGL[,6]
lukke_4 = GD[,6]
lukke_5 = DUK[,6]
lukke_6 = CL[,6]
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
qqnorm(sorted_return_1, main = "JPM")
qqline(sorted_return_1, col = "red")
qqnorm(sorted_return_2, main = "NEE")
qqline(sorted_return_2, col = "red")
qqnorm(sorted_return_3, main = "GOOGL")
qqline(sorted_return_3, col = "red")
qqnorm(sorted_return_4, main = "GD")
qqline(sorted_return_4, col = "red")
qqnorm(sorted_return_5, main = "DUK")
qqline(sorted_return_5, col = "red")
qqnorm(sorted_return_6, main = "CL")
qqline(sorted_return_6, col = "red")

#The following code creates an autocorrelation plot for the returns
#nona_return <- na.omit(return)
#acf(nona_return, main = "Autocorrelation of Returns")

#The following code creates an autocorrelation plot for squared returns
#sqrd_return <- nona_return^2
#acf(sqrd_return, main = "Autocorrelation of Squared Returns")

