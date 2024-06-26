library("readr")
library("MASS")
library("ggplot2")

# Import data 
df <- read_csv('returns.csv')

# Define covariance matrix and expected return vector
C <- cov(df)
r <- sapply(df, FUN=mean)

C_inv <- ginv(C)

vec1 <- rep(1, ncol(df))

# Minimum variance point and optimal point
MIN_VARIANCE_POINT <- (C_inv %*% vec1) / sum(vec1 * (C_inv %*% vec1))
Z <- C_inv %*% r - (sum(vec1 * (C_inv %*% r)) / sum(vec1 * (C_inv %*% vec1))) * C_inv %*% vec1

Markowitz <- function(tau) {
    return (MIN_VARIANCE_POINT + tau * Z / 2)
}




# Market Portfolio data

rf <- (1 + 0.0005351351)^(1/365) -1

ZERO_n <- rep(0, ncol(df))

RISK_FREE_PORTFOLIO <- append(ZERO_n, 1, 0)

y_star <- C_inv %*% (r  - rf * vec1)

y_0 <- rf * sum(C_inv) - sum(C_inv %*% r)

Y <- append(y_star, y_0, 0)


# Finding market portfolio
tau_MARKET <- -(2 / (y_0))
tau_MARKET

Markowitz_risk_free <- function(rf, y, tau) {
    opt_portfolio <- RISK_FREE_PORTFOLIO + tau * y / 2
    return (opt_portfolio)
}
portfolio <- Markowitz_risk_free(rf, Y, tau_MARKET)
returns_vec <- append(r, rf, 0)

tocsv <- cbind(portfolio, returns_vec)

write.csv(tocsv, file="market_portfolio.csv")

Variance <- function(point, cov, riskfree=FALSE, tau) {
    if (riskfree) {
        alpha <- (tau_MARKET - tau) / tau_MARKET
        variance <- (1 - alpha) * sqrt(sum(point[-1] * (cov %*% point[-1])))
    } else {
        variance <- sum(point * (cov %*% point))
    }
    return (variance)
}

portfolio_mean <- function(point, returns, riskfree=FALSE) {
    if (riskfree) {
        mean <- sum(point * append(returns, rf, 0))
    } else {
        mean <- sum(point * returns)
    }
    return (mean)
}


sigma_mu <- function(tau, riskfree=FALSE) {
    if (riskfree) {
        point <- Markowitz_risk_free(rf, Y, tau)
    } else {
        point <- Markowitz(tau)
    }
    mu <- portfolio_mean(point, r, riskfree)
    sigma <- Variance(point, C, riskfree, tau)
    return (cbind(sigma, mu))
}

# Plot mean-variance diagrams
t = seq(0, 5, .01)
t_rf = seq(0, tau_MARKET * 2, tau_MARKET/250)

eff_frontier <- t(sapply(t, sigma_mu))
eff_frontier_rf <- t(sapply(t_rf, FUN = sigma_mu, riskfree=TRUE))

label <- c("Variance", "Mean")
colnames(eff_frontier_rf) <- label
colnames(eff_frontier) <- label

eff_frontier.df <- data.frame(eff_frontier)
eff_frontier_rf.df <- data.frame(eff_frontier_rf)

both_efficient <- rbind(eff_frontier.df, eff_frontier_rf.df)

plotrf <- ggplot(eff_frontier_rf.df, aes(x=Variance, y=Mean)) + geom_line()
plotrisk <- ggplot(eff_frontier.df, aes(x = Variance, y=Mean)) + geom_line()

plotrf
plotrisk

ggplot(both_efficient, aes(x=Variance, y=Mean)) + geom_point()
