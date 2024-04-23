library("readr")
library("MASS")
library("ggplot2")
library("quantmod")

# Metadata
RETURNS <- data.frame(read_csv("returns_15_24.csv"))
rf <- (1 + 0.0446)^(1/365) - 1
test <- data.frame(read_csv("market_portfolio.csv"))[2]

getSymbols("^GSPC", src="yahoo", from="2015-01-01", to="2024-01-01")
sp100_index <- Ad(GSPC)[-c(1:1000, nrow(GSPC))] * 100 / as.numeric(Ad(GSPC)[1001])
sp100_index
# Reevaluate markowitz efficient portfolio each time step
# Note that sufficient data berfore 'time' variable must exist. Check for that when calling function
evaluate_markowitz_rf <- function(time, tau) {
    # Evaluating expectation and covariance for time step
    scope <- RETURNS[(time - 1000):(time - 1), ]
    C <- cov(scope)
    r <- sapply(scope, FUN=mean)
    C_inv <- ginv(C)
    vec1 <- rep(1, ncol(scope))

    # Computing Markowitz efficient portfolio
    rfp <- append(rep(0, ncol(scope)), 1, 0) # rfp: Risk free portfolio
    y_star <- C_inv %*% (r  - rf * vec1)
    y_0 <- rf * sum(C_inv) - sum(C_inv %*% r)
    y <- append(y_star, y_0, 0)
    opt_portfolio <- rfp + tau * y / 2

    return (opt_portfolio)
}

# Compute return for active fund
create_index <- function(tau, price_index) {
    price_history <- c(price_index)
    for (t in 1001:nrow(RETURNS) - 1) {
        if (t > 1000) {
            portfolio <- evaluate_markowitz_rf(t, tau)
            price_index <- price_index * (1 + as.numeric(as.matrix(RETURNS[t, ]) %*% as.vector(portfolio[-1])) + rf * portfolio[1])
            price_history[t - 999] <- price_index
        } else {
            next
        }
    }
    return (price_history)
}

# Plotting data
index <- create_index(0.05, 100)
matrix <- cbind(sp100_index, index)
df <- data.frame(matrix)

ggplot(data = df, aes(x=index(df$index), y=index)) + 
    geom_line() + 
    geom_line(data = df, aes(x=index(df$GSPC.Adjusted), y=GSPC.Adjusted), color="red") +
    labs(x="days")


