###########################################
#'Replicating Rex's Logistic Regression
#'
#'
#'#########################################

setwd("~/GitHub/SMML")
require(glmnet)
require(caret)
load("Data/smml.rdata")
#company.df<-read.csv("Data/companytbl.csv",header=T,row.names=1)  
#write.csv(smml.data, "Data.csv")

#Create Company Set#####
Ticker.List<-data.frame(TICKER=unique(smml.data$TICKER), TEST=0)
nrow(Ticker.List)
set.seed(101)
sampling.percent<-.2
Test.Ticker.idx<-sample(nrow(Ticker.List), nrow(Ticker.List)*sampling.percent)
Ticker.List$TEST[Test.Ticker.idx]<-1
mean(Ticker.List$TEST)

#lookup function for test set##########
istest<-function(ticker){
  Ticker.List[Ticker.List$TICKER==ticker,"TEST"]  
}
istest('ABG')

xvar=names(smml.data)[c(1,2,5,7,8,11,13,14,16,18,20,22,38,39,40,42,28)]
xvar #show names of features

retvar<-c("RET1W","RET4W","RET13W","RET26W","RET52W","RET78W","RET104W")

# limit data to complete cases
xy.df<-smml.data[,xvar]
cc.idx<-complete.cases(xy.df)
xy.df<-xy.df[cc.idx,]
ret.df<-smml.data[cc.idx,retvar]

xy.df$TICKER<-factor(xy.df$TICKER) #get rid of unused companies
nlevels(xy.df$TICKER)  #number of companies
## [1] 525
temp<-data.frame(table(xy.df$MONTH))  # determine number of observations each month
#test.idx<-sapply(xy.df$TICKER,istest) #takes forever to run
#train.idx<-!test.idx

xy.df2<-merge(xy.df,Ticker.List, by='TICKER' )
train.df<-xy.df2[xy.df2$TEST!=1,3:17]
logit1<-glm(WINNER~.,data=train.df,family=binomial())

summary(logit1)
confusionMatrix(logit1$fitted.values>=.5,train.df$WINNER, positive='TRUE')

#on the test set
test.x<-xy.df2[xy.df2$TEST==1,3:16]
logit1.predict<-predict(logit1,newdata=test.x,type="response")
pred.winners.idx<-logit1.predict>=.5
confusionMatrix(pred.winners.idx,xy.df2$WINNER[xy.df2$TEST==1], positive='TRUE')

#Change the probability cutoff value
pred.winners.idx<-logit1.predict>=.2
confusionMatrix(pred.winners.idx,xy.df2$WINNER[xy.df2$TEST==1], positive='TRUE')

#Sample all the winners and an equal number of non-winners to train on
mean(train.df$WINNER)
train.win.idx<-which(train.df$WINNER==1)
train.notwin.idx<-sample(which(train.df$WINNER==0),length(train.win.idx))
train.idx<-c(train.win.idx,train.notwin.idx)

#Rerun regression
logit1<-glm(WINNER~.,data=train.df, subset=train.idx,family=binomial())
summary(logit1)
confusionMatrix(logit1$fitted.values>=.5,train.df$WINNER[train.idx], positive='TRUE')
#test on test set
#on the test set
logit1.predict<-predict(logit1,newdata=test.x,type="response")
pred.winners.idx<-logit1.predict>=.5
confusionMatrix(pred.winners.idx,xy.df2$WINNER[xy.df2$TEST==1], positive='TRUE')
