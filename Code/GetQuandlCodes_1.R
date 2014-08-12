#Get the quandl codes for most of the Russell 1000
#Takes tickers in a file r1000list.xlsx (which I think I got from the internet) and finds the quandl codes
#also lists the companies not found.
#Results in a data.frame called result.  Output to file r1kquandlcodes.rdata
#6/9 note this not reading xlsx file because of a problem with Java on my new PC.  Workaround would be to save
#spreadsheet as CSV and use read.csv
setwd("~/RTrading/Cooper")
require(xlsx)
#http://www.quandl.com/resources/useful-lists is source of CSV files below
NYSE<-read.csv(file="NYSE.CSV",header=TRUE)
NASDAQ<-read.csv(file="NASDAQ.CSV",header=TRUE)
AMEX<-read.csv(file="AMEX.CSV",header=TRUE)
r1000.constituents<-read.xlsx2("r1000list.xlsx",sheetName="Sheet1",stringsAsFactors=FALSE)

result<-r1000.constituents
result$code<-rep(NA,length(result[,1]))
result$name<-rep(NA,length(result[,1]))
ntickers<-length(result[,1])
for (i in 1:ntickers){
  t<-r1000.constituents$Ticker[i]
  idxN<-(t==NYSE$Ticker)
  idxA<-(t==AMEX$Ticker)
  idxQ<-(t==NASDAQ$Ticker)
  if (sum(idxN)==1){
    result$code[i]<-as.character(NYSE$Code[idxN])
    result$name[i]<-as.character(NYSE$Name[idxN])   
  } else if (sum(idxA==1)) {
      result$code[i]<-as.character(AMEX$Code[idxA])
      result$name[i]<-as.character(AMEX$Name[idxA])
  } else if (sum(idxQ==1)) {
    result$code[i]<-as.character(NASDAQ$Code[idxQ])
    result$name[i]<-as.character(NASDAQ$Name[idxQ])
  } 
}
#remove companies for which codes were not found
idx<-!is.na(result$code)
#show names
result$Company[!idx]
result<-result[idx,]
save(result, idx,r1000.constituents,file=r1kquandlcodes.rdata)