library("readr")

MARKET_PORTFOLIO <- data.frame(read_csv("market_portfolio.csv"))

getExtremeWeights <- function(df) {
    weights <- df$portfolio
    n <- length(weights)
    indecies <- rev(order(weights)[c(1, 2, 3, n-2, n-1, n)])
    return (df[indecies, ])
}

extremeWeights <- getExtremeWeights(MARKET_PORTFOLIO)

ggplot(extremeWeights, aes(x=reorder(extremeWeights[, 1], +portfolio), y=portfolio, fill=extremeWeights[, 1])) + geom_bar(stat="identity") + labs(x="Asset ticker", y="Weight in tangency portfolio") + theme(legend.position="none")
