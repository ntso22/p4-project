
library("readr")
library("quantmod")
library("tidyverse")
library("tidyquant")
library("ggplot2")
library("rlist")


prices24 <- read_csv("prices_13_19.csv")
returns24 <- read_csv("returns_13_19.csv")
market_portfolio <- read.csv("market_portfolio.csv")[,2]

index <- tq_get('^SP100',
                from = '2013-1-1',
                to= '2019-1-11',
                get ='stock.prices')[,8]


Delt(as.matrix(index))

m <- apply(returns24,2,mean)

daily_returns <- sum(m * as.vector(market_portfolio)[2:98]) 

returns_sp100 <- na.omit(Delt(as.matrix(index)))


  
liste1 <- c()
liste2 <- c()

for (j in seq(1,length(returns_sp100), 1)){
  liste1 <- list.append(liste1,as.numeric(returns_sp100[j]))
  liste2 <- list.append(liste2,j)
}


returns_sp100_1 <- as.matrix(liste1)
sp100_x <- as.matrix(liste2)

colnames(returns_sp100_1) <- "returns"
colnames(sp100_x) <- "days"

matrix_sp100 <- cbind(returns_sp100_1, sp100_x)




liste3 <- c()
liste4 <- c()

for (j in seq(1,length(returns_sp100), 1)){
  liste3 <- list.append(liste3,sum(returns24[j,]*as.vector(market_portfolio)[2:98]))
  liste4 <- list.append(liste4,j)
}

returns_marketport <- as.matrix(liste3)
marketport_x <- as.matrix(liste4)

colnames(returns_marketport) <- "returns"
colnames(marketport_x) <- "days"

market_matrix <- cbind(returns_marketport, marketport_x)



ggplot(data=market_matrix, aes(x=marketport_x, y=returns_marketport))+labs(x='Days',y='Returns')+ geom_line()+geom_line(data = matrix_sp100, aes(days,returns),color="red")

#ggplot(data=market_matrix, aes(x=marketport_x, y=returns_marketport),color="red")+geom_line()

returns_marketport

cor(returns_sp100_1,returns_marketport)


liste5 <- c(1,1+(1*returns_sp100_1[1]))
liste6 <- c(1,1+(1*returns_marketport[1]))
liste7<- c(1,2)

for (j in seq(3,length(returns_sp100), 1)){
  liste5 <- list.append(liste5,liste5[j-1]*(1+returns_sp100_1[j-1]))
  liste6 <- list.append(liste6,liste6[j-1]*(1+returns_marketport[j-1]))
  liste7 <- list.append(liste7,j)
}

returns_marketport2 <- as.matrix(liste6)
returns_sp100_2 <- as.matrix(liste5)
x <- as.matrix(liste7)
colnames(returns_marketport2) <- "returns market"
colnames(returns_sp100_2) <- "returns sp100"
colnames(x) <- "days"

matrix_new <- cbind(returns_marketport2, returns_sp100_2, x)

ggplot(data=matrix_new, aes(x=x, y= returns_marketport2))+geom_line(size=1)+geom_line(size=1,data=matrix_new, aes(x=x,y=returns_sp100_2), color="red")+labs(x="Days",y="Index")


returns_sp100_2



cor(returns_sp100_1,returns_marketport)


