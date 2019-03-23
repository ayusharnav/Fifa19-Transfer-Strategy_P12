rm(list = ls(all = T))
library(readr)
library(rpart)
library(data.table)
library(sqldf)
library(radarchart)
library(tidyr)
library(plyr)
library(dplyr)
library(dtplyr)
library(ggplot2)
library(modeest)
library(recommenderlab)
library(stringr)
library(DT)
library(knitr)
library(grid)
library(gridExtra)
library(corrplot)
library(qgraph)
library(methods)
library(Matrix)
library(ggthemes)
library(caret)
library(cluster)
library(e1071)
library(mclust)
library(fpc)

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

FullData <- as.data.frame(read.csv("./data_new.csv",header=TRUE,encoding = "UTF-8"))
ggplot(FullData,aes(x=FullData[,4], y=FullData[,7], color=FullData[,2]))+geom_point()+ geom_smooth(method="auto") + xlab("Overall")+ ylab("Value")
ggsave('G10_plot01.pdf',width=8,height=8)

source('./methods.R')


# function to load and process training and test data 
# Please note that TA may have a completely different dataset with the same dimensions as the one provided to you
load_train_and_test_data <- function(folder_path='./'){
  # Input: folder_path - points to the folder containing the hw2_training and hw2_test csv files
  # TA has different versions the same file, following the same properties of the data (# row, # columns and class values {1,2,3,4})
  data <- as.data.frame(read.csv("./data_new.csv",header=TRUE,encoding = "UTF-8"))
  smp_size <- floor(0.9996 * nrow(data))
  
  ## set the seed to make your partition reproducible
  set.seed(123)
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  
  train <- data[train_ind, ]
  test <- data[-train_ind, ]
  
  X_train <- train[, 14:47]
  X_test <- test[, 14:47]
  y_train <- train[, 7]
  y_test <- test[, 7]
  return(list(X_train, X_test, y_train, y_test))
}

# read data from disk, extract train test into separate variables 
all_data <- load_train_and_test_data('./')
X_train <- all_data[[1]]
X_test <- all_data[[2]]
y_train <- all_data[[3]]
y_test <- all_data[[4]]

# calculate classification outcomes using KNN with euclidean distance
euclidean_classification <- knn_classifier(X_train, y_train, X_test, 'calculate_euclidean', 5)
euclidean_result <- calculate_accuracy(euclidean_classification, y_test)

# calculate classification outcomes using KNN with cosine distance
cosine_classification <- knn_classifier(X_train, y_train, X_test, 'calculate_cosine', 5)
cosine_result <- calculate_accuracy(cosine_classification, y_test)

# calculate classification outcomes using KNN with Manhattan distance
manhattan_classification <- knn_classifier(X_train, y_train, X_test, 'calculate_manhattan', 5)
manhattan_result <- calculate_accuracy(manhattan_classification, y_test)

# calculate classification outcomes using KNN with chebyshev distance
chebyshev_classification <- knn_classifier(X_train, y_train, X_test, 'calculate_chebyshev', 5)
chebyshev_result <- calculate_accuracy(chebyshev_classification, y_test)

# calculate classification outcomes using KNN_V2 with cosine distance
knn_conf_classification <- knn_classifier_confidence(X_train, y_train, X_test,  'calculate_cosine', 5)
conf_result <- calculate_accuracy(knn_conf_classification, y_test)

# calculate classification outcomes using Decision Tree using rpart and gini index with default hyperparameters
dt_classification <- dtree(X_train, y_train, X_test)
dt_result <- calculate_accuracy(dt_classification, y_test)

# calculate classification outcomes using a tuned Decision Tree
dt_cv_classification <- dtree_cv(X_train, y_train, X_test, 5)
dt_cv_result <- calculate_accuracy(dt_cv_classification, y_test)

setDT(FullData)
names(FullData)

# Age of footballers
ggplot(FullData, aes(Age, fill = Age)) +
geom_density(position = "stack") 

# Player's Overall Distribution
FullData %>% 
ggplot(aes(x = Overall, fill = factor(Overall))) +
  geom_bar(color = "grey20") + guides(fill = FALSE)+
  labs(title="Player's Overall ")

#Rating vs Age
agerating <- FullData[Age<100,.("Overall"=mean(Overall)),by=Age][order(-Age)]
ggplot(data = agerating,aes(x=Age,y=Overall))+
  geom_line(color="red",size=2)+labs(title="Rating vs Age")+
  annotate("text", x = 30, y = max(agerating$overall),color="blue", label = "Max", parse = TRUE, size = 3)

# Best Clubs
TeamDF<-arrange(FullData[, list(Avg=mean(Overall)), by= "Club" ], desc(Avg) )
kable(head(TeamDF, 10))

#Number of clusters
cluster = FullData[,c(4,9,16:18,27:88)]
cluster[is.na(cluster)] = 0
wss <- (nrow(cluster[,c()])-1)*sum(apply(cluster[,1:ncol(cluster)],2,var))
print(wss)
for (i in 2:10) 
  wss[i] <- sum(kmeans(cluster[,1:ncol(cluster)], centers=i)$withinss)

plot(1:
       10, wss, type="b", xlab="Number of Cluster",  ylab="Squares Summatory")
