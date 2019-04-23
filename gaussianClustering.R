rm(list = ls(all = T))
source('./libraries.R')
source('./fifaPositionMethods.R')
source('./methods.R')


######## gaussian clustering
finalGaussianclustering <- function() {
  FullData <-
    as.data.frame(read.csv(
      "./data/data_normalize.csv",
      header = TRUE,
      encoding = "UTF-8"
    ))
  smp_size <- floor(0.8 * nrow(FullData))
  train_ind <- sample(seq_len(nrow(FullData)), size = smp_size)
  train.data <- FullData[train_ind,]
  test.data <- FullData[-train_ind,]
  
  mb = Mclust(train.data[, c(49, 7)])
  result <- mb$classification
  predictedPlayer <- c()
  g <- 1
  counter <- 0
  print(nrow(test.data))
  l1count <- 0
  l2count <- 0
  l3count <- 0
  l4count <- 0
  #test.data <- test.data[20:70,]
  for (z in seq(1:nrow(test.data))) {
    testvalue <- test.data[z, ]
    predTest <- predict(mb, testvalue[, c(49, 7)])
    temp <- c()
    k <- 1
    for (i in seq(length(result))) {
      if (result[i][[1]] == predTest$classification) {
        temp[k] = as.numeric(names(result[i]))
        k <- k + 1
      }
    }
    
    distance_matrix <-
      calculate_distance_matrix(FullData[temp, ], testvalue, 'calculate_euclidean')
    playerKdistances <-
      sort(distance_matrix,
           index.return = TRUE,
           decreasing = FALSE)$ix[1:8]
    #print(testvalue)
    clubmembers <- train.data[train.data$Club == testvalue$Club, ]
    ########################################## contribution to the team
    sum1 <- 0
    sum1 <-sum(clubmembers[, 'CalculatedOverall']) + testvalue['CalculatedOverall']
    avg1 <- sum1[[1]] / (nrow(clubmembers) + 1)
    cf1 <- 0
    sr1 <- 0
    for (i in nrow(clubmembers)) {
      if (clubmembers[i, "CalculatedOverall"] > avg1) {
        cf1 = cf1 + clubmembers[i, "CalculatedOverall"] - avg1
      }
    }
    
    if (testvalue['CalculatedOverall'] > avg1) {
      cf1 = cf1 + testvalue['CalculatedOverall'] - avg1
    }
    sr1 = (sum1 + cf1) / (nrow(clubmembers) + 1)
    
    indices = 0
    l1 = c()
    l2 = c()
    l3 = c()
    l4 = c()
    for (i in seq(1:length(temp[playerKdistances]))) {
      if (FullData[temp[playerKdistances][i], 'Club'] != testvalue$Club) {
        sum2 <-
          sum(clubmembers[, 'CalculatedOverall']) + FullData[temp[playerKdistances][i], 'CalculatedOverall']
        
        avg2 <- sum2[[1]] / (nrow(clubmembers) + 1)
        cf2 <- 0
        sr2 <- 0
        for (j in nrow(clubmembers)) {
          if (clubmembers[j, "CalculatedOverall"] > avg2) {
            cf2 = cf2 + clubmembers[j, "CalculatedOverall"] - avg2
          }
        }
        if (FullData[temp[playerKdistances][i], 'CalculatedOverall'] > avg2) {
          cf2 = cf2 + FullData[temp[playerKdistances][i], 'CalculatedOverall'] - avg2
        }
        
        sr2 = (sum2 + cf2) / (nrow(clubmembers) + 1)
        #if(minvalue>=FullData[temp[playerKdistances][i],'Value']){
        #minvalue = FullData[temp[playerKdistances][i],'Value']
        #indices <- temp[playerKdistances][i]
        #}
        
        if(testvalue$Value > FullData[temp[playerKdistances][i],'Value'] & 
           testvalue$CalculatedOverall < FullData[temp[playerKdistances][i],'CalculatedOverall']){ # to see in the graph about stats
          #pastsr2 = sr2
          l1 <- c(l1,temp[playerKdistances][i])
          #minvalue = FullData[temp[playerKdistances][i],'Value']
        } else if (testvalue$Value < FullData[temp[playerKdistances][i],'Value'] & 
                   testvalue$CalculatedOverall < FullData[temp[playerKdistances][i],'CalculatedOverall']) {
          l2 <- c(l2,temp[playerKdistances][i])
        } else if (testvalue$Value > FullData[temp[playerKdistances][i],'Value'] & 
                   testvalue$CalculatedOverall > FullData[temp[playerKdistances][i],'CalculatedOverall']) {
          l3 <- c(l3,temp[playerKdistances][i])
        } else {
          l4 <- c(l4,temp[playerKdistances][i])
        }
        #if (sr2 >= sr1) { # For accuracy computation
        #print(sr2)
        #print(sr1)
        #  counter <- counter + 1
        #break
        #print(FullData[temp[playerKdistances][i],])
        #}
      }
    }
    if(length(l1)>0) {
      minValue = 100
      player = l1[1]
      for (i in seq(1:length(l1))) {
        if(abs(testvalue['Value'] - FullData[l1[i],'Value']) < minValue) {
          minValue = abs(testvalue['Value'] - FullData[l1[i],'Value'])
          player <- l1[i]
        }
      }
      predictedPlayer[g] <- player
      counter <- counter + 1
      l1count <- l1count + 1
    } else if(length(l2)>0){
      minValue = 100
      player = l2[1]
      for (i in seq(1:length(l2))) {
        if(abs(testvalue['Value'] - FullData[l2[i],'Value']) < minValue) {
          minValue = abs(testvalue['Value'] - FullData[l2[i],'Value'])
          player <- l2[i]
        }
      }
      predictedPlayer[g] <- player
      counter <- counter + 0.5
      l2count <- l2count + 1
    } else if(length(l3)>0){
      minValue = 100
      player = l3[1]
      for (i in seq(1:length(l3))) {
        if(abs(testvalue['Value'] - FullData[l3[i],'Value']) < minValue) {
          minValue = abs(testvalue['Value'] - FullData[l3[i],'Value'])
          player <- l3[i]
        }
      }
      predictedPlayer[g] <- player
      counter <- counter + 0.0 
      l3count <- l3count + 1
    } else {
      minValue = 100
      player = l4[1]
      for (i in seq(1:length(l4))) {
        if(abs(testvalue['Value'] - FullData[l4[i],'Value']) < minValue) {
          minValue = abs(testvalue['Value'] - FullData[l4[i],'Value'])
          player <- l4[i]
        }
      }
      predictedPlayer[g] <- player
      counter <- counter + 0.0
      l4count <- l4count + 1
    }
    
    g <- g+1
  }
  
  # uncomment below lines to see the plot
  # plot(test.data[1:50,'CalculatedOverall'],test.data[1:50,'Value'],col="red",xlim = c(0.70,0.95),ylim = c(0,0.45))
  # points(FullData[predictedPlayer,'CalculatedOverall'],FullData[predictedPlayer,'Value'],col="blue")
  
  # for( i in seq(1:length(predictedPlayer))){
  #   if(test.data[i,'CalculatedOverall'] - FullData[predictedPlayer,'CalculatedOverall'][i]<=0 && test.data[i,'Value'] - FullData[predictedPlayer,'Value'][i] >=0){
  #     color<-'green'
  #   }
  #   else if(test.data[i,'CalculatedOverall'] - FullData[predictedPlayer,'CalculatedOverall'][i]<=0 && test.data[i,'Value'] - FullData[predictedPlayer,'Value'][i] < 0){
  #     color<-'blue'
  #   } 
  #   else if(test.data[i,'CalculatedOverall'] - FullData[predictedPlayer,'CalculatedOverall'][i] > 0 && test.data[i,'Value'] - FullData[predictedPlayer,'Value'][i] >= 0){
  #     color<-'yellow'
  #   }
  #   else{
  #     color<-'red'
  #   }
  #   arrows(test.data[i,'CalculatedOverall'],test.data[i,'Value'], FullData[predictedPlayer,'CalculatedOverall'][i],FullData[predictedPlayer,'Value'][i],col=color)
  # }
  
  print("accuracy")
  print(counter / nrow(test.data))
}


calculate_sse <- function(data_df, cluster_assignments){
  cluster_vector<-unique(cluster_assignments)
  sse=0
  for (cluster in cluster_vector) {
    mean_vector<-vector()
    index_vector<-which(cluster_assignments %in% c(cluster))
    subset<-data_df[c(index_vector),]
    for(col in 1:ncol(subset)) {
      mean_vector<-c(mean_vector,mean(subset[,col]))
    }
    s=0
    for(row in 1:nrow(subset)) {
      s<-s+ sum((subset[row,] - mean_vector)^2)
    }
    sse<-sse + s
  }
  return (sse)
}


kmeans_elbow_plot <- function(data_df, k_values){
  sse_vector<-vector()
  for (k in k_values) {
    kmeans_result <- player_cluster(data_df, n_clusters = k, clustering_type = "kmeans")
    sse_vector<-c(sse_vector,calculate_sse(data_df,kmeans_result))
  }
  png("10_elbow.png")
  plot(k_values,sse_vector,type="o", xlab="K Values", ylab="SSE", main="Elbow Plot")
  dev.off()
}


player_cluster <- function(data_df, n_clusters, clustering_type){
  set.seed(100)
  if(clustering_type == "kmeans"){
    return ((kmeans(data_df,centers = n_clusters, iter.max = 10, nstart = 1,algorithm ="Lloyd"))[["cluster"]])
    
    
  }else if(clustering_type == "single-link"){
    hc <- hclust(dist(data_df)^2, method ="single")
    plot(hc)
    memb <- cutree(hc, k = n_clusters)
    return (memb)
    
    
    
  }else{
    hc <- hclust(dist(data_df)^2, method ="complete")
    plot(hc)
    memb <- cutree(hc, k = n_clusters)
    return (memb)
    
    
  }
}

