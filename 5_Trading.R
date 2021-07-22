library(quantmod)

getSymbols("BTC-USD", src="yahoo", 
           from="2019-01-01", to="2020-01-20")
# View(`BTC-USD`)
bitcoin <- `BTC-USD`
colnames(bitcoin) = c('open','high','low', 'close','volume','adjusted')

getSymbols("GOOG", src="yahoo", from="2013-01-01", to="2019-12-31")

chartSeries(GOOG, subset='2019-01-01::2019-12-31',
            theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),
                 addSMA(n=50,col="blue"),
                 addSMA(n=10,col="black"),
                 addRSI(n=14)))


getFX('USD/EUR', from="2020-05-31") # 180 days only

getMetals('gold') # Most recent 180 days only

getSymbols('GDPC1',src='FRED') # Federal Reserve Bank

rgdp <-  getSymbols('A191RO1Q156NBEA',src='FRED', auto.assign=F)


library(openxlsx)
urlfile1 <- "https://www.cad.gov.hk/english/pdf/Stat%20Webpage.xlsx"
air.travel <- read.xlsx(urlfile1,startRow=8)
colnames(air.travel) <- c("Year", "Month", "X3", "Landing",             
                          "Take-off", "Aircraft_Total","Aircraft_yoy",
                          "Arrival", "Departure", "PassengerTotal",                
                          "Passenger_yoy", "Unloaded",             
                          "Loaded", "Freight_Total",                
                          "Freight_yoy")

library(tidyverse) 
df1 <- as_tibble(air.travel[24:nrow(air.travel),])

df2 <- df1 %>%
  fill(Year) %>% 
  select(-X3)

rgdp <-  getSymbols('A191RO1Q156NBEA',src='FRED',auto.assign=F)
