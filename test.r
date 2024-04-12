library(quantmod)

loadSymbols("^GSPC", src="yahoo", from="2014-01-01", to="2024-01-02", periodicity="monthly")
sd(Cl(GSPC[c(1, 13, 25, 37, 49, 61, 73, 85, 97, 109, 121),]))

loadSymbols("VFIAX", src="yahoo", from="2024-01-01", periodicity="daily")
sd(Ad(VFIAX))

test <- Cl(GSPC)
count(GSPC, 200)


loadSymbols("ULTA", src="yahoo", from="2020-01-01", to="2024-03-02", periodicity="monthly")
Cl(ULTA)
