#Code assembled for estimating and forecasting weights and covariances for the DCC GARCH model

library(xts)
library(rugarch)
library(aTSA)
library(tseries)
library(FinTS)
library(e1071)
library(readxl)
library(rmgarch)
library(RiskPortfolios)
library(PerformanceAnalytics)

Dataset3y<- read_excel("R/3y-log_returns.xlsx")
Dataset3y<-Dataset3y[,-1] #Eliminating time column if necessary. Check if Dataset3y has 30 variables, if yes, don't run this line

#dcca1 -> measures the short term spillover effect
#dccb1 -> measure the long term spillover effect
#Remember dcca1+dccb1 <=1 for dynamic relationship

#DCC-Garch 
model1=ugarchspec(mean.model=list(armaOrder=c(0,0)),variance.model=list(garchOrder=c(1,1),model="sGARCH"),distribution.model="norm")
modelspec=dccspec(uspec=multispec(replicate(30,model1)),dccOrder = c(1,1),distribution = "mvnorm")
modelfit=dccfit(modelspec,data=data.frame(Dataset3y))
modelfit

#Forecasting using DCC Garch model
Hfor=dccforecast(modelfit,n.ahead = 1)@mforecast$H[[1]][,,1]
Mu=modelfit@model$mu
M<-30
#Estimating weights for portfolio optimization using covariance matrixes
res_dcc<-foreach(i=1:(M), .packages = c("RiskPortfolios"), .errorhandling = 'pass') %dopar%{
  
  w_dcc<-optimalPortfolio(Sigma=Hfor, control=list(type="minvol"))
  result <- list()
  result$w<-w_dcc
  result$rets<-weighted.mean(Mu[8940,1:30], w_dcc)
  return(result)

}
modelfit@mfit$R
modelfit@model$mu
