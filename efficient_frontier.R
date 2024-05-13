
library(rlist)
library(ggplot2)

C1 <- as.matrix(cbind(c(1/4,0),c(0,1/3)))

C_inv <- solve(C1)

vec1 <- rep(1, ncol(C1))

m = c(4,3)

A = sum(vec1 * (C_inv%*%m))

B = sum( m * (C_inv %*% m))

C = sum(vec1 * (C_inv %*% vec1))

D = B*C-A^2


x <- seq(0,4, .001)
plot(1/C+(x/2)^2*D/C,A/C+x/2*D/C, main = "Efficient frontier", xlab = expression(sigma[x]^2),ylab=expression(r[x]), type ="l")

liste1 <- c()
liste2 <- c()

for (x in seq(0,4, .001)){
  liste1 <- list.append(liste1,1/C+(x/2)^2*D/C)
  liste2 <- list.append(liste2,A/C+x/2*D/C)
}

liste11 <- as.matrix(liste1)

liste21 <- as.matrix(liste2)
                     
colnames(liste11) <- "variance"
colnames(liste21) <- "return"
matrix <- cbind(liste11,liste21)

liste11[1]
liste21[1]

ggplot(data=matrix, aes(x=variance, y=return))+labs(x=expression(sigma[x]^2),y=expression(r[x]))+ geom_line(size=1.2)+geom_point(x=liste11[1001],y=liste21[1001],size=3)

ggsave(file="rplot_efficient_frontier.png",width=16,height=9,dpi=300)
seq(0,4, .001)[1001]

liste11[1001]
liste21[1001]


31/7
