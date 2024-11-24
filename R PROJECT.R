library(tidyquant)
library(ggplot2)
library(dplyr)
nvidiastock <- tq_get(c("NVDA"))
ggplot(data=nvidiastock , aes(x=date, color=symbol))+geom_line(aes(x=date,y=open, color=symbol))
library(quantmod)
getSymbols("NVDA")
getSymbols("NVDA" , from ="2024/08/01" , to = "2024/08/22" , periodicity ="daily")
head(NVDA)
chart_Series(NVDA , type = "candlesticks" )
add_EMA(n=7)
library(TTR) 
library(dplyr)
tq_mutate_xy(data=nvidiastock , x = close, y = volume, mutate_fun = EVWMA, col_rename = "EVWMA")
nvidia_date <- nvidiastock$date
nvidiayear2024 <- filter( nvidiastock ,date>= "2024-08-01" ) 
nvidiayear2024
tq_mutate_xy(data=nvidiayear2024 , x = close, y = volume, mutate_fun = EVWMA, col_rename = "EVWMA")
price1 <- nvidiayear2024[,"close"]
price1
rsinvidia <- RSI(price=price1, n= 4 ,maType = "WMA")
rsinvidia
nvidiayear2024$rs <- rsinvidia
nvidiayear2024
nvidiayear2024_buy <- filter(nvidiayear2024 , rsinvidia<70)
nvidiayear2024_buy
getSymbols("TXN", from = "2024-05-01" , to = "2024-08-22")
TXNstock <- tq_get("TXN" , from = "2024-05-01" , to = "2024-08-22")
TXNstock
TXNlastmonth <- filter( TXNstock ,date>= "2024-08-01" )
TXNstock
closeprice <- TXNstock[, "close"]
TXNrsi <- RSI(price = closeprice , n=4 , maType = "EMA" )
TXNstock$rsi <- TXNrsi
TXNstock
daystobuyTXN <- filter(TXNstock , rsi<70 , rsi>30)
daystobuyTXN
daystobuyTXN <- filter(daystobuyTXN , volume> 7000000)
daystobuyTXN
getSymbols("CVX" , from = " 2024-08-01" , to = "2024-08-30" , periodicity = "daily")
CVX
CVXopen <- CVX$CVX.Open
CVXclose <- CVX$CVX.Close
CVXdiffINTRADAY<- CVXclose - CVXopen
CVXdiffINTRADAY
CVX_intrday <- sum(CVXdiffINTRADAY)
CVXdiffMONTH <-145.44-158.71
CVXbestoption <- CVX_intrday - CVXdiffMONTH
if(CVXbestoption>0) {print("day trade")}
if(CVXbestoption<0) {print("month trade")}
listoften <- tq_get(c("NVDA" , "TSLA" , "META" , "INTC" , "TXN" , "CVX" , "SMC" , "MSFT") , get = "stock.prices" , from= "2024-01-01" , to  ="2024-08-01")
openingprice <- listoften$open
closingprice <-listoften$close
symbolic <-listoften$symbol

l <- sort_by(listoften , symbolic)
lpositve <-filter(l , openingprice -closingprice <0)
lrsi <- RSI(price = closingprice , n=4 , maType = "EMA" )
lrsi
