#Garch modelling

library(xts)
library(rugarch)
library(aTSA)
library(tseries)
library(FinTS)
library(e1071)
library(readxl)


Dataset<- read_excel("R/LogR-HF-PO-NOOPEN.xlsx")
Dataset5s<- read_excel("R/Log-R-AAPL-5s-BID-NOOPEN.xlsx")
View(Dataset5s)
plot.ts(Dataset$`AXP-close`)
ArchTest(Dataset$`AXP-close`)
#Garch procedure
DatasetGARCH<-merge.zoo(Dataset[1],Dataset[5],Dataset[11],Dataset[21],Dataset[27],Dataset[29])
DatasetuGARCH<-merge.zoo(Dataset[1],Dataset[5])
DatasetuGARCH_xts <- xts(x = DatasetuGARCH[, -1], order.by = as.Date.factor(DatasetuGARCH$time))
garch(x =DatasetuGARCH_xts, grad=c("numerical"),trace=FALSE)

DatasetuGARCH_xts_zero<-DatasetuGARCH_xts[apply(DatasetuGARCH, 1, function(row) all(row !=0)),]
DatasetuGARCH_zero_xts <- xts(x = DatasetuGARCH_zero[, -1], order.by = as.Date.factor(DatasetuGARCH$time))

DatasetGARCH_vector=ugarchspec(variance.model = list(garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)))
DatasetGARCH_vector_fit=ugarchfit(DatasetGARCH_vector,data = DatasetuGARCH_xts_zero)
DatasetGARCH_vector_fit
news_garch=newsimpact(DatasetGARCH_vector_fit)
plot(news_garch$zx,news_garch$zy,ylab=news_garch$yexpr,news_garch$xexpr,main="News Impact Curve")
DatasetuGARCH_zero_xts_forecast=ugarchforecast(DatasetGARCH_vector_fit,n.ahead=30)
DatasetuGARCH_zero_xts_forecast

#LM-ARCH test
ArchTest(DatasetARCH)

#'AAPL','KO','DD','MSFT','VZ','WBA' have ARCH effect

#Stationary test ADF
adf.test(DatasetGARCH)

plot.ts(Dataset$`MMM-close`)
plot.ts(Dataset$`AXP-close`)
plot.ts(Dataset$`AMGN-close`)
plot.ts(Dataset$`AAPL-close`)
plot.ts(Dataset$`BA-close`)
plot.ts(Dataset$`CAT-close`)
plot.ts(Dataset$`CVX-close`)
plot.ts(Dataset$`CSCO-close`)
plot.ts(Dataset$`KO-close`)
plot.ts(Dataset$`DD-close`)
plot.ts(Dataset$`GS-close`)
plot.ts(Dataset$`HD-close`)
plot.ts(Dataset$`HON-close`)
plot.ts(Dataset$`IBM-close`)
plot.ts(Dataset$`INTC-close`)
plot.ts(Dataset$`JNJ-close`)
plot.ts(Dataset$`JPM-close`)
plot.ts(Dataset$`MCD-close`)
plot.ts(Dataset$`MRK-close`)
plot.ts(Dataset$`MSFT-close`)
plot.ts(Dataset$`NKE-close`)
plot.ts(Dataset$`PG-close`)
plot.ts(Dataset$`CRM-close`)
plot.ts(Dataset$`TRV-close`)
plot.ts(Dataset$`UNH-close`)
plot.ts(Dataset$`VZ-close`)
plot.ts(Dataset$`V-close`)
plot.ts(Dataset$`WBA-close`)
plot.ts(Dataset$`WMT-close`)
plot.ts(Dataset$`DIS-close`)

