rm(list = ls(all = T))
source('./libraries.R')

normalize_data <- function(data_matrix){
  # Input: data_matrix: type: matrix n_sentences x sentence_length, 
  # where n_sentences is total # sentences (155 in the dataset supplied to you) and
  # sentence_length is the total length of each sentence (200 in the dataset supplied to you).
  # output: a 155 x 200 matrix of type double, containing the normalized values in [0, 1] range per row.
  # Write code here to normalize data_matrix
  for(i in seq(1, ncol(data_matrix))){
    temp1<-as.vector(as.numeric(data_matrix[,i]))
    data_matrix[,i] <- (temp1- min(temp1,na.rm=TRUE))/(max(temp1,na.rm=TRUE)-min(temp1,na.rm=TRUE))
  }
  return(data_matrix)
}

FullData <- as.matrix(read.csv("./data_new.csv",header=TRUE,encoding = "UTF-8"))
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
temp<-FullData[,14:49]
temp=normalize_data(temp)
FullData[,14:49]<-temp

temp<-FullData[,4:5]
temp=normalize_data(temp)
FullData[,4:5]<-temp

temp<-FullData[,7:8]
temp=normalize_data(temp)
FullData[,7:8]<-temp

temp<-FullData[,2:3]
temp=normalize_data(temp)
FullData[,2:3]<-temp

temp<-FullData[,9:12]
temp=normalize_data(temp)
FullData[,9:12]<-temp

write.csv(FullData, file="file.csv", row.names = FALSE)
