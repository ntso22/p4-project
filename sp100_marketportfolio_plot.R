
library("readr")
library("quantmod")
library("tidyverse")
library("ggplot2")

prices24 <- read_csv("prices_24.csv")
returns24 <- read_csv("returns_24.csv")
market_portfolio <- read.csv("market_portfolio.csv")[,2]

index <- tq_get('^SP100',
                from = '2019-12-31',
                to= '2024-4-17',
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



cor(returns_sp100_1,returns_marketport)


