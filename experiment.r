library("readr")
library("MASS")
library("ggplot2")
library("quantmod")

# Metadata
RETURNS <- data.frame(read_csv("returns_15_24.csv"))
rf <- (1 + 0.0005351351)^(1/365) -1
test <- data.frame(read_csv("market_portfolio.csv"))[2]

MEMORY <- 504 # Determines the amount of trading days, the historic returns are calculated with

# Reevaluate markowitz efficient portfolio each time step
# Note that sufficient data berfore 'time' variable must exist. Check for that when calling function
evaluate_markowitz_rf <- function(time, tau) {
    # Evaluating expectation and covariance for time step
    scope <- RETURNS[(time - MEMORY):(time - 1), ]
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
    getSymbols("^GSPC", src="yahoo", from="2013-01-01", to="2019-01-01")
    sp100_index <- Ad(GSPC)[-c(1:MEMORY, nrow(GSPC))] * price_index / as.numeric(Ad(GSPC)[MEMORY + 1])
    for (t in (MEMORY + 1):nrow(RETURNS) - 1) {
        if (t > MEMORY) {
            portfolio <- evaluate_markowitz_rf(t, tau)
            price_index <- price_index * (1 + as.numeric(as.matrix(RETURNS[t, ]) %*% as.vector(portfolio[-1])) + rf * portfolio[1])
            price_history[t - (MEMORY - 1)] <- price_index
            }
    }
    df <- data.frame(Markowitz=price_history, SP100=sp100_index)
    return (df)
}

# Plotting data
df <- create_index(0.2455255, 100)

ggplot() + 
    geom_line(data = df, aes(x=index(df$Markowitz), y=Markowitz), color=, size=1.5) +
    geom_line(data = df, aes(x=index(df$GSPC.Adjusted), y=GSPC.Adjusted), color="S&P100", size=1.5) +
    labs(x="Days", y="Index") +
    scale_color_manual(values = c("black" = "black", "red" = "red"))
ggsave(file="images/dynamic_portfolio.png", width=16, height=9, dpi=300)




