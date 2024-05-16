library("readr")
library("MASS")
library("quantmod")
library('rlist')
library("IntroCompFinR")
library("ggplot2")
library("ggplotify")



# Import data 
df <- read_csv('returns.csv')

rownames(df)

#Expected return

m <- apply(df,2,mean) 

#m <- ((m1 + 1)^252 - 1)


variance <- apply(df,2,var)

# Define covariance matrix and expected return vector

V <- cov(df)

W <- solve(V)

risk_free <- (1+0.0005351351)^(1/365)-1

risk_free

e=rep(1,length(df))

#Definer "MAGIC" numbers


A = sum(e * (W %*% m))

B = sum(m * (W %*% m))

C = sum(e * (W %*% e))

D = B*C - A^2

H=risk_free^2*C - 2*risk_free*A+B


return_x <- function(x){
  ja = risk_free + (x)/2*H[1]
  return (ja)
} 



#Use formula for the Tangency Portfolio found in MFE page 47

tangency_portfolio <-as.vector(1/(A-risk_free*C)*(W%*%(m-risk_free*e)))

tangency_portfolio_return <- sum(tangency_portfolio * m)

tangency_portfolio_return


tangency_portfolio_variance <- sum(tangency_portfolio * (V %*% tangency_portfolio))

tangency_portfolio_std <- sqrt(tangency_portfolio_variance)


#Doublecheck result by applying IntroCompFinR function
tangency.portfolio(as.vector(matrix(m)),V,risk_free,shorts=TRUE)

#uden shorting

which.max(tangency.portfolio(as.vector(matrix(m)),V,risk_free,shorts=FALSE)$weights)

which.max(tangency.portfolio(as.vector(matrix(m)),V,risk_free,shorts=TRUE)$weights)



#Efficient frontier for risky assets


# (fejl. den er lineær i return funktion så denne kunne undlades med nye x værdier) 
# CML (efficient frontier i std-return planen med risk free) med værdier så tangentporteføljen er garanteret at være med
#x <- seq(0,0.5, .1)
#plot((return_x(x)-risk_free)/sqrt(H) ,return_x(x), type ="l",main ="Capital market line, tangent portfolio (red)", xlab=expression(sigma[x]),ylab=expression(r[x]))
#points(tangency_portfolio_std, tangency_portfolio_return, col = "red", pch = 19)
#points(0,risk_free, col="green",pch=19)
#text(0, 0.002, labels=expression(paste("(0,",r[f],")")))



#risky std som funktion af return (tau er udeladt)

risky_std <- function(x){
  sqrt((1/D)*(C*x^2-2*A*x+B))
}

#Efficient frontier only risky including tangent portfolio

x <- seq(A/C,tangency_portfolio_return*4, .001)
plot(risky_std(x),x,type='l', xlab = expression(r[x]), ylab = expression(sigma[x]), main = "Efficient frontier only risky assets and tangent portfolio (red)")
points(tangency_portfolio_std,tangency_portfolio_return, col = 'red', pch=19)


#Capital market line (efficient frontier med risk free)(sort) plottet med
#efficient frontier for risky assets (blå) og tangent portefølje (rød)





#x <- seq(0,0.8, .01)
#plot((return_x(x)-risk_free)/sqrt(H) ,return_x(x), type ="l",main="Tangency portfolio (red)", xlab = expression(sigma[x]),ylab=expression(r[x]), xlim = c(0,0.05), ylim = c(0,0.012))
#points(tangency_portfolio_std, tangency_portfolio_return, col = "red", pch = 19)
#points(liste1, liste2, col = "blue", pch = 19)
#lines(liste1, liste2, col = "blue", pch = 19)
#points(0,0,col="green",pch=19)
#text(0, 0.002, labels=expression(paste("(0,",r[f],")")))


liste3 <- c()
liste4 <- c()

for (j in seq(0,0.8*3, .01)){
  liste3 <- list.append(liste3,(return_x(j)-risk_free)/sqrt(H))
  liste4 <- list.append(liste4,return_x(j))
}




riskfree_liste <- as.matrix(liste3)
riskfree_x <- as.matrix(liste4)

colnames(riskfree_liste) <- "std"
colnames(riskfree_x) <- "x"

matrix_riskfree <- cbind(riskfree_liste, riskfree_x)



#ggplot(data=matrix_riskfree, mapping = aes(x=x,y=std))+
#  geom_line()

liste1 <- c()
liste2 <- c()

for (j in seq(A/C,tangency_portfolio_return*5, .0001)){
  liste1 <- list.append(liste1,risky_std(j))
  liste2 <- list.append(liste2,j)
}

risky_liste <- as.matrix(liste1)
risky_x <- as.matrix(liste2)

colnames(risky_liste) <- "std"
colnames(risky_x) <- "x"

matrix_risky <- cbind(risky_liste, risky_x)

ggplot(data=matrix_risky, mapping = aes(x=std,y=x))+ geom_line()+
  geom_line(data=matrix_riskfree, mapping = aes(x=std, y=x), color="red")+labs(x=expression(sigma[w]),y=expression(r[w]))+geom_point(aes(x=tangency_portfolio_std, y=tangency_portfolio_return))+xlim(0,0.08)+ylim(0,0.03)+coord_fixed(2)

ggsave(file="tangency_portfolio.png",width=9,height=9,dpi=300)




tangency_portfolio_std

tangency_tau <- 2/(A-risk_free*C)

tangency_tau


#+ylim(0,0.011)+xlim(0,0.04)

#plot
#Sharpe ratio function for risky efficient frontier
sharpe <- function(x){
  lol = (x-risk_free)/risky_std(x)
  return(lol)
}


#Tangent portefølje burde maksimere denne funktion bemærk hvordan den er en funktion udelukkende af return

#x <- seq(A/C,tangency_portfolio_return*8, .001)
#plot(x,(x-risk_free)/risky_std(x),type='l', xlab= expression(r[x]),ylab="Sharpe")


#Check that tangent portfolio gives highest sharpe ratio on the risky efficient frontier
optimise(sharpe, lower = A/C, upper = tangency_portfolio_return*2, maximum = TRUE)


#Values to check if the above is correct (it is close enough)
tangency_portfolio_return
sharpe(tangency_portfolio_return)

#estimeret annual return

tangency_portfolio_return
tangency_portfolio_annualreturn <- ((tangency_portfolio_return+1)^252-1)*100
tangency_portfolio_annualreturn 

#uden shorting tilladt er daglig expected return
uden_short_tan_port_daily_return <- 0.001325356 

#uden shorting, annual return
((uden_short_tan_port_daily_return+1)^252-1)*100

#indikerer at der er sket en fejl (måske covmat)


#indikerer at der er sket en fejl (måske covmat)


