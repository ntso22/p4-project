library("quantmod")
library("readr")
library("ggplot2")

TANGENCY_PORTFOLIO <- as.vector(sapply(as.matrix(read_csv("market_portfolio.csv"))[-1, 2], FUN = as.numeric))

PRICES <- as.matrix(read_csv("prices_13_19.csv")[-(1:756), ])

TANGENCY_UNITS <- TANGENCY_PORTFOLIO / PRICES[1, ]

PORTFOLIO_VALUE <- (PRICES %*% TANGENCY_UNITS) * 100

getSymbols("^GSPC", src = "yahoo", from = "2016-01-01", to = "2019-01-01")
sp100_index <- Ad(GSPC) * 100 / as.numeric(Ad(GSPC)[1])

df <- data.frame(Portfolio_value = PORTFOLIO_VALUE, SP100 = sp100_index)

str(as.Date(rownames(df)))

ggplot() +
    geom_line(data = df, aes(x = as.Date(rownames(df)), y = Portfolio_value), color = "black", size = 1.5) +
    geom_line(data = df, aes(x = as.Date(rownames(df)), y = GSPC.Adjusted), color = "red", size = 1.5) +
    labs(x = "days", y = "index")
ggsave(file = "images/stationary_portfolio.png", width = 16, height = 9, dpi = 300)
