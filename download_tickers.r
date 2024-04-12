library("quantmod")
library("rvest")
library("MASS")

# Generate list of S&P 100 ticker symbols
getSP100tickers <- function() {
    url <- "https://en.wikipedia.org/wiki/S%26P_100"
    html <- html_table(html_nodes(read_html(url), "#constituents")) # Downloads specific table from Wikipedia page and stores as a dataframe
    return (html)
}

# Generate csv file with 
price.csv <- function(tickers) {
    price.matrix <- matrix()
    returns.matrix <- matrix()
    for (ticker in tickers) { # Generates matrix with historical prices for individual stocks as columns
        loadSymbols(ticker, src = "yahoo", from="2015-01-01", to="2020-01-01")
        price.matrix <- cbind(price.matrix, Ad(get(ticker)))
        returns.matrix <- cbind(returns.matrix, Delt(Ad(get(ticker))))
    }
    clean.matrix <- na.omit(price.matrix[, -1]) # Removes placeholder column and observations with no data
    clean.returns.matrix <- na.omit(returns.matrix[, -1])

    colnames(clean.matrix) <- tickers
    colnames(clean.returns.matrix) <- tickers

    df <- data.frame(clean.matrix)
    df.returns <- data.frame(clean.returns.matrix)
    write.matrix(df, file="prices.csv", sep=",")
    write.matrix(df.returns, file="returns.csv", sep=",")
}

# Create list of ticker symbols. BRK-B is excluded due to problems importing
tickers <- data.frame(getSP100tickers())$Symbol[-c(19, 35, 54, 80)]

# Write stock data to csv
price.csv(tickers)

