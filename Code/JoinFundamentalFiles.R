###################################################
#Join Fundamental Ratio Files
#
#Code imports the fundamental ratio file for each
#stock ticker and joins them into one large csv
#
###################################################
library('plyr')
setwd("C:/Users/sdperez.EMORYUNIVAD/Documents/GitHub/SMML/Data/FundamentalData")
path="C:/Users/sdperez.EMORYUNIVAD/Documents/GitHub/SMML/Data/FundamentalData"
file.names <- dir(path, pattern =".csv")
out.file<-""
for(i in 1:length(file.names)){
  file <- read.csv(file.names[i],header=TRUE, stringsAsFactors=FALSE)
  file$ticker<-sub('_.+',"",file.names[i]) #Add ticker symbol to the data
  out.file <- rbind.fill(out.file, file) #merge with running file
}

out.file<-out.file[out.file$ticker!="",] #erase extra blank line 

write.csv(out.file,'C:/Users/sdperez.EMORYUNIVAD/Documents/GitHub/SMML/Data/CombinedFundamentals.csv')
