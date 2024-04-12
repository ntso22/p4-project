library("BatchGetSymbols")
library("quantmod")
library("matlib")

sp500 <- GetSP500Stocks()
tickers <- sp500$Tickers[-c(12, 63, 79, 90, 127, 212 , 215, 273, 359, 423, 468)] # Removes stocks (ABNB, BRK-B, BF.B, CARR, CEG, GEHC, GEV, KVUE, OTIS, SOLV, VLTO) that contain entries with no values

# Creates matrix with symbols as columns and corresponding daily returns as row
getpricematrix <- function(tickersymbols) {
    closingpricematrix <- matrix()
    returnsmatrix <- matrix()
    for (ticker in tickersymbols) {
        loadSymbols(ticker, src="yahoo", from="2020-01-01", to="2024-03-02", periodicity="monthly")
        returns <- Delt(Cl(get(ticker)))
        closingpricematrix <- cbind(closingpricematrix, returns)
        
        print(paste("Now loaded:    ", ticker))
    }
    cleanmatrix <- na.omit(closingpricematrix[,-1]) # Removes the first column 
    colnames(cleanmatrix) <- tickersymbols
    return (cleanmatrix)
}

expectedreturnsvec <- function(returns) {
    expectedreturns <- c()
    for (i in 1:ncol(returns)) {
        expectedreturns <- append(expectedreturns, mean(returns[, i]))
    }
    names(expectedreturns) <- colnames(returns)
    return (expectedreturns)
}

markowitzrisky <- function(mean, covariance, tau) {
    min_portfolio <- 1 / (sum())
}

returnsmatrix <- getpricematrix(tickers)
covmatrix <- cov(returnsmatrix)

expectedreturnsvec(returnsmatrix)
returnsmatrix

tickers
sp500$Tickers

