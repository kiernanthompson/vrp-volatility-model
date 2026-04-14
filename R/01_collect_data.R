# 01_collect_data.R
# VIX and SPY data collection

install.packages("quantmod")
library(quantmod)

# 1. Pull VIX
getSymbols("^VIX", from = "2000-01-01", to = Sys.Date())
# 2. Pull SPY
getSymbols("SPY", from = "2000-01-01", to = Sys.Date())

# Save raw data
saveRDS(VIX, "data/raw/VIX_raw.rds")
saveRDS(SPY, "data/raw/SPY_raw.rds")

# Check
cat("Data pulled successfully\n")
file.exists("data/raw/VIX_raw.rds")
file.exists("data/raw/SPY_raw.rds")
