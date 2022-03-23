############### Backtesting: R code

library(xts)
library(readxl) # per importare ed esportare i files
library(rugarch) # per i GARCH
library(RiskPortfolios) # per implementare velocemente il minimum-variance portfolio 
library(PerformanceAnalytics) # per la performance di portafoglio
library(foreach) # calcolo in parallelo (per velocizzare)
library(doParallel) # calcolo in parallelo (per velocizzare)

"Dataset<- read_excel("R/Log_returns_High_Frequency_Portfolio_Optimization_Data_close1.xlsx") # dataset con log-rendimenti (N.B.: no prezzi)
#View(Dataset)
Dataset_xts <- xts(x = Dataset[, -1], order.by = as.Date(Dataset$Date)) # use all columns except for first column (date) as data # Convert Date column from factor to Date and use as time index
#View(Dataset_xts)

cl<-makeCluster(detectCores()-1) # lascia 1 core del pc libero mentre fa i calcoli
registerDoParallel(cl)

n<-ncol(Dataset_xts) # no. Assets
t<-nrow(Dataset_xts) # lunghezza delle serie
M<-31 # estimation window (default 120, ma dobbiamo fare un giorno -- contare quante righe sono un giorno)

# Strategia statica -- no forecasts su covarianza e rebalance di portafoglio ogni 30 minuti

res_sc<-foreach(i=1:(t-M), .packages = c("RiskPortfolios"), .errorhandling = 'pass') %dopar%{
  w_sc<-optimalPortfolio(Sigma=covEstimation(as.matrix(Dataset_xts[i:(M+i-1),]), control=list(type="naive")), control=list(type="minvol"))
  result <- list()
  result$w<-w_sc
  result$rets<-weighted.mean(Dataset_xts[M+i,], w_sc)
  return(result)
}
rets_sc<-subset(unlist(res_sc), names(unlist(res_sc))=="rets", drop = FALSE)
W_sc<-matrix(NA, nrow=length(res_sc), ncol=n)
for (j in 1:length(res_sc)) {
  W_sc[j,]<-res_sc[[j]]$w
}
#View(rets_sc)
Dataset_time<-Dataset[1]
rets_sc_sharpe<-data.frame(rets_sc)
row.names(rets_sc_sharpe)<-LETTERS[1:3641]
View(rets_sc_sharpe)
Timeseries_rets<-merge(Dataset_time[1],rets_sc_sharpe[1], all  = FALSE)
View(Timeseries_rets)
View(Dataset_time)
SharpeRatio(rets_sc_sharpe) # Valutiamo le performance solo in termini di Sharpe ratio, per semplicità

# Strategia dinamica -- con forecasts su varianze (diagonale della covarianza, correlations
# le lasciamo statiche, altrimenti dovremmo utilizzare modelli multivariati come il DCC, vedi Engle 2002)

# DA INSERIRE

stopImplicitCluster() # per chiudere la sessione di calcolo in parallelo
""