# This script contains an exploration of stock data and saving the data to local files
library(quantmod)

# You can select different symbols and different time frames
# Download S$P 500 Index data
#getSymbols("^GSPC", src = "yahoo", from = "2009-01-01", to = "2023-10-31")

# FTSE 100 Index data
getSymbols("^FTSE", src = "yahoo", from = "2000-01-01")
write.zoo(FTSE, file = "data/FTSE_raw_data.csv", sep = ",")

# FTSE 200 Index data
getSymbols("^FTMC", src = "yahoo")
write.zoo(FTMC, file = "data/FTMC_raw_data.csv", sep = ",")

#FTSE 350  Index 
getSymbols("^FTLC", src = "yahoo")
write.zoo(FTLC, file = "data/FTLC_raw_data.csv", sep = ",")

#FTSE All share Index 
getSymbols("^FTAS", src = "yahoo")
write.zoo(FTAS, file = "data/FTAS_raw_data.csv", sep = ",")

#FTSE AIM All Share Index 
getSymbols("^FTAI", src="yahoo")
write.zoo(FTAI, file = "data/FTAI_raw_data.csv", sep = ",")

#FTSE SmallCap (^FTSC)
getSymbols("^FTSC", src="yahoo")
write.zoo(FTSC, file = "data/FTSC_raw_data.csv", sep = ",")

#Only the last plot is visible here. But use this for reference
plot_data <- function(symbol_price_data, index_name) {
  adjusted_close <- Ad(symbol_price_data)
  close_price <- Cl(symbol_price_data)
  open_price <- Op(symbol_price_data)
  volume <- Vo(symbol_price_data)
  max_price <- Hi(symbol_price_data)
  min_price <- Lo(symbol_price_data)
  
  plot(close_price, main = paste(index_name, " Closing Price"), col = "darkgreen", ylab = "Price (USD)", xlab = "Date")

  plot(adjusted_close, main = paste(index_name, " Adjusted Closing Price"), col = "darkgreen", ylab = "Price (USD)", xlab = "Date")

  plot(open_price, main = paste(index_name, " Open Price"), col = "darkgreen", ylab = "Price (USD)", xlab = "Date")

  plot(max_price, main = paste(index_name, " Max Price"), col = "darkgreen", ylab = "Price (USD)", xlab = "Date")

  plot(min_price, main = paste(index_name, " Min Price"), col = "darkgreen", ylab = "Price (USD)", xlab = "Date")

  plot(volume, main = paste(index_name, " Volume"), col = "darkgreen", xlab = "Date")
  
}

plot_data(GSPC, "GSPC")
plot_data(FTSE, "FTSE 100")
plot_data(FTMC, "FTSE 200")
plot_data(FTLC, "FTSE 350")
plot_data(FTAS, "FTSE All Share Index")
plot_data(FTAI, "FTSE AIM All Share Index")
plot_data(FTSC, "FTSE Small Cap Index")
# All the prices except the volume seems to have the same pattern.
