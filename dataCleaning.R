rm(list = ls(all = T))
source('./libraries.R')

FullData <- as.matrix(read.csv("./data.csv",header=TRUE,encoding = "UTF-8"))
#FullData[,8]<-gsub("???", '', FullData[,8]) 
#FullData[,8]<-gsub("K", '000', FullData[,8]) 
#write.csv(FullData, file="file.csv", row.names = FALSE)
for(i in seq(1, nrow(FullData))){
  if(substr(toString(FullData[i,7]), nchar(toString(FullData[i,7])), nchar(toString(FullData[i,7])))=='M'){
    FullData[i,7]<-gsub("???", '', FullData[i,7]) 
    FullData[i,7]<-gsub("M", '', FullData[i,7]) 
    FullData[i,7]<-as.numeric(FullData[i,7])*1000000
  }
}
for(i in seq(1, nrow(FullData))){
  if(substr(toString(FullData[i,7]), nchar(toString(FullData[i,7])), nchar(toString(FullData[i,7])))=='K'){
    FullData[i,7]<-gsub("???", '', FullData[i,7]) 
    FullData[i,7]<-gsub("K", '', FullData[i,7]) 
    FullData[i,7]<-as.numeric(FullData[i,7])*1000
  }
}
for(i in seq(1, nrow(FullData))){
  if(substr(toString(FullData[i,7]), nchar(toString(FullData[i,7])), nchar(toString(FullData[i,7])))=='0'){
    FullData[i,7]<-gsub("???", '', FullData[i,7]) 
  }
}
write.csv(FullData, file="file.csv", row.names = FALSE)
