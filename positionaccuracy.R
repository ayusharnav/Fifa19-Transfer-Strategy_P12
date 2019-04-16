rm(list = ls(all = T))
source('./libraries.R')
source('./fifaPositionMethods.R')
source('./methods.R')
library(hashmap)
comparePosition <- function(x,y){
  if(x==y){
  
    return(TRUE)
  }
  if(x=='CB'){
    if(y=='RCB' || y=='LCB')
      return(TRUE)
  }
  if(x=='LB'){
    if(y=='RB')
      return(TRUE)
  }
  if(x=='LWB'){
    if(y=='RWB')
      return(TRUE)
  }
  if(x=='CDM'){
    if(y=='LDM' || y=='RDM')
      return(TRUE)
  }
  if(x=='CAM'){
    if(y=='LAM' || y=='RAM')
      return(TRUE)
  }
  if(x=='LM'){
    if(y=='RM' || y=='RCM' || y=='LCM' || y=='CM')
      return(TRUE)
  }
  if(x=='LW'){
    if(y=='RW')
      return(TRUE)
  }
  if(x=='LF'){
    if(y=='RF' || y=='CF')
      return(TRUE)
  }
  if(x=='ST'){
    if(y=='RS' || y=='LS')
      return(TRUE)
  }
  return(FALSE)
}
calculate_overall_accuracy <- function() {
  FullData <- as.data.frame(read.csv("./data_new.csv",header=TRUE,encoding = "UTF-8"))
  FullData = FullData[FullData$Position=="CDM" | FullData$Position=="LDM" | FullData$Position=="RDM",]
  Positions <- c('GK','CB','LB','LWB','CDM', 'CAM','LM','LW','LF','ST')
  #Positions <- c('LB')
  overall_vector<-vector()
  count=0
  #t <- hashmap('GK',0)
  for(i in seq(1, nrow(FullData))){
    overall=0
     maxOverall = -1
     maxPositon =''
      for(k in 1:length(Positions)){
          vector<-return_list_for_position(Positions[k])
          overall=0
          for (j in 1:length(vector)) {
            overall=overall+(vector[[j]] * as.numeric(FullData[i,names(vector[j])]))
          }
    #overall_vector[i]<-overall
          if(overall > maxOverall){
            maxOverall = overall
            maxPositon = Positions[k]
          }
      }
     #print(maxPositon)
     #print(FullData[i,'Position'])
     #print(FullData[i,'Overall']) 
     #print(maxOverall)
     #print (i)
     #print (FullData[i,'Name'])
     #print("********************")
     if(comparePosition(maxPositon,FullData[i,'Position'])){
       count = count +1
     }
  }
  print(t)
  
  #FullData <- cbind(FullData, overall_vector) 
  #colnames(FullData)[ncol(FullData)] <- "CalculatedOverall"
  #write.csv(FullData, file="calc_overall.csv", row.names = FALSE)
  accuracy <- count/nrow(FullData)
  print(accuracy)

}

######## gaussian clustering

finalGaussianclustering <- function(){
  FullData <- as.data.frame(read.csv("./data_normalize.csv",header=TRUE,encoding = "UTF-8"))
  smp_size <- floor(0.7 * nrow(FullData))
  train_ind <- sample(seq_len(nrow(FullData)), size = smp_size)
  train.data <- FullData[train_ind, ] 
  test.data <- FullData[-train_ind, ]
  testvalue <- test.data[3400,] ## change it !
  train.data <- train.data[train.data$Position==testvalue$Position,]
  mb = Mclust(train.data[,c(4,7)])
  result <- mb$classification
  predTest <- predict(mb,testvalue[,c(4,7)])
  temp <- c()
  k<-1
  for (i in seq(length(result))) {
    if(result[i][[1]]==predTest$classification){
      temp[k] = as.numeric(names(result[i]))
      k <- k+1
    }
  }
  distance_matrix <- calculate_distance_matrix(FullData[temp,],testvalue,'calculate_euclidean')
  playerKdistances <- sort(distance_matrix,index.return=TRUE,decreasing = FALSE)$ix[1:4]
  print(FullData[temp[playerKdistances],])
  print("#########")
  print(testvalue)
}

