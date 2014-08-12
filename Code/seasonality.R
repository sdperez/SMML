## Examine seasonality of S&P 500 data
require(xts)
data<-read.csv("SP500Daily.csv",header=T,row.names=1)
row.names(data)<-as.Date(row.names(data),"%m/%d/%Y")
data<-as.xts(data)
colnames(data)
## Fields:
## ADJCLOSE: Closing price of S&P 500
## PCTCHG: Percent change in price from previous day
## DOFW: Day of week
## CDFME: Calendar days from month end
## TDFME: Trading days from month end
## MOEND: True/False: True if last day of month
## TDUME: Trading days until month end
## CDUME: Calendar days until month end
## MONTH: 1-12 representing month
## MFELECTION: 0-47 representing #months from election 0=election month e.g. 11/2012

x<-data[,c(-1,-2)] # remove price level and pctchg from independent data
x$DOFW<-as.factor(x$DOFW)  #convert features to factors
x$CDFME<-as.factor(x$CDFME)
x$TDFME<-as.factor(x$TDFME)
x$MOEND<-as.factor(x$MOEND)
x$TDUME<-as.factor(x$TDUME)
x$CDUME<-as.factor(x$CDUME)
x$MONTH<-as.factor(x$MONTH)
x$MFELECTION<-as.factor(x$MFELECTION)
y<-data$PCTCHG

## examine data
ndays<-nrow(x)
cat("Number of days in data set:",ndays)
cat("Last day:",format(index(x)[ndays],"%m/%d/%Y"))
cat("First day:",format(index(x)[1],"%m/%d/%Y"))
plot(data$ADJCLOSE,main="Price level of S&P 500")
summary(y) #summary of all pctchg values
hist(y,main="Histogram of S&P 500 Daily Return",xlab="Daily % Price Change")
plot(y,main="Daily % Price Changes in the S&P 500")

## divide data into training and test sets
## test set starts with 1/1/1989
x.train<-x["/1988"] #everything thru 1988
y.train<-y["/1988"] #everything thru 1988
x.test<-x["1989/"] #everything from 1989
y.test<-y["1989/"] #everything from 1989
summary(x.train)
summary(y.train)
summary(x.test)
summary(y.test)
cat("#/pct of obs in test set:",nrow(x.train),"/",nrow(x.train)/nrow(x))
cat("#/pct of obs in test set:",nrow(x.test),"/",nrow(x.test)/nrow(x))

## Let's try svm (support vector machine)
require(e1071)
svm.model<-svm(x=x.train,y=y.train)
y.fitted<-predict(svm.model,x.train)
cat("Correlation of predicted and actual values (train)",cor(y.fitted,y.train))
plot(y.fitted,y.train,type="p",main="Predicted v Actual (Train)")

y.predict<-predict(svm.model,x.test)
cat("Correlation of predicted and actual values",cor(y.predict,y.test))
plot(y.predict,y.test,type="p",main="Predicted v Actual (Test)")

predict.order<-order(y.predict)
y.test.mat<-as.matrix(y.test)
y.test.mat<-y.test.mat[predict.order]
nr<-ceiling(length(y.test.mat)/10)
deciles<-matrix(NA,nrow=nr,ncol=10)
r<-1
c<-1
for (i in y.test.mat){
 deciles[r,c]<-i
 r<-r+1
 if (r>nr){
   r<-1
   c<-c+1
 }
}
summary(deciles)
summary(y.test)
boxplot(deciles)

geoavg<-vector(mode="numeric",length=10)
for (i in 1:10){
  r<-nrow(deciles)-sum(is.na(deciles[,i]))
  geoavg[i]<-prod(1+deciles[,i],na.rm=T)^(252/r)-1 #annualized based on 252 trading days per year
}
geoavg
# avg for entire test period
prod(1+y.test,na.rm=T)^(252/length(y.test))-1
plot(geoavg)
abline(h=prod(1+y.test,na.rm=T)^(252/length(y.test))-1)

## Try Random Forest
require(randomForest)

rf.model<-randomForest(x=x.train,y=y.train)
y.fitted<-predict(rf.model,x.train)
cat("Correlation of predicted and actual values (train)",cor(y.fitted,y.train))
plot(y.fitted,y.train,type="p",main="Predicted v Actual (Train)")

y.predict<-predict(rf.model,x.test)
cat("Correlation of predicted and actual values",cor(y.predict,y.test))
plot(y.predict,y.test,type="p",main="Predicted v Actual (Test)")

predict.order<-order(y.predict)
y.test.mat<-as.matrix(y.test)
y.test.mat<-y.test.mat[predict.order]
nr<-ceiling(length(y.test.mat)/10)
deciles<-matrix(NA,nrow=nr,ncol=10)
r<-1
c<-1
for (i in y.test.mat){
  deciles[r,c]<-i
  r<-r+1
  if (r>nr){
    r<-1
    c<-c+1
  }
}
summary(deciles)
summary(y.test)
boxplot(deciles)

geoavg<-vector(mode="numeric",length=10)
for (i in 1:10){
  r<-nrow(deciles)-sum(is.na(deciles[,i]))
  geoavg[i]<-prod(1+deciles[,i],na.rm=T)^(252/r)-1 #annualized based on 252 trading days per year
}
geoavg
# avg for entire test period
prod(1+y.test,na.rm=T)^(252/length(y.test))-1

plot(geoavg)
abline(h=prod(1+y.test,na.rm=T)^(252/length(y.test))-1)
