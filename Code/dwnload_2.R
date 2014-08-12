#This downloads the OHLCV for many of the current R1000 stocks and the R1000 and the SP500
setwd("~/RTrading/Cooper")
#library(quantmod)
library(Quandl)
library(xts)
Quandl.auth("Enter your code here") 
load("r1kquandlcodes.Rdata")

qtick<-result$code
stock.data<-NULL
for (i in seq(from =1,to=961,by=10)){
  print(i)
  j<-seq(from=i,to=i+9,by=1)
  if (i==961){j<-seq(from=i,to=length(qtick),by=1)}
  curtick<-qtick[j]
  stock.data<-merge.xts(stock.data,Quandl(curtick,type="xts",start_date="2009-12-31",end_date="2014-04-30",collapse="weekly"))
}

qtick<-c("YAHOO/INDEX_RUI","YAHOO/INDEX_GSPC")
index.data<-Quandl(qtick,type="xts",start_date="2009-12-31",end_date="2014-04-30",collapse="weekly")

save(r1000.constituents,stock.data,index.data,result,file="cooper.rdata")


