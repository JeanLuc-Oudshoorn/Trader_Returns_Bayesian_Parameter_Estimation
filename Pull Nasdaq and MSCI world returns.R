library(quantmod)
library(xts)
library(readxl)

nasdaq <- getSymbols(Symbols = "^IXIC", src = "yahoo", auto.assign = FALSE)
world <- getSymbols(Symbols = "URTH", src = "yahoo", auto.assign = FALSE)
sp <- getSymbols(Symbols = "^GSPC", src = "yahoo", auto.assign = FALSE)

nasdaq <- to.period(nasdaq, period = "months", k = 1, OHLC = FALSE)
world <- to.period(world, period = "months", k = 1, OHLC = FALSE)
sp <- to.period(sp, period = "months", k = 1, OHLC = FALSE)

nasdaq <- nasdaq["2013-07/2021-08"]
world <- world["2013-07/2021-08"]
sp <- sp["2013-07/2021-08"]

idx <- index(nasdaq)

etorocopy <- read_excel("Etoro.xlsx", col_names = TRUE)
etorocopy <- xts(etorocopy, order.by = idx)

nasdaq$nasdaq <- monthlyReturn(nasdaq)*100
world$world <- monthlyReturn(world)*100
sp$sp <- monthlyReturn(sp)*100

etorocopy <- merge(etorocopy, nasdaq$nasdaq, world$world, sp$sp, join = "left")
etorocopy <- etorocopy[,-1]