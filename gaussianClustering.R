rm(list = ls(all = T))
source('./libraries.R')
source('./fifaPositionMethods.R')
source('./methods.R')
library(hashmap)

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

