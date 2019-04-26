rm(list=ls(all=T))
cat('\014')

source('./libraries.R')
source('./gaussianClustering.R')

# set your working directory
# setwd()

set.seed(100)
############################################################################################################
load_data <- function(learning_type){
  FullData <- read.csv(paste0('./data/data_normalize.csv'), header=T)
  
  smp_size <- floor(0.7 * nrow(FullData))
  
  set.seed(123)
  train_ind <- sample(seq_len(nrow(FullData)), size = smp_size)
  
  train_df <- FullData[train_ind, ]
  test_df <- FullData[-train_ind, ]
  
  if(learning_type == 'classification'){
    train_df$Position <- as.factor(train_df$Position)
    test_df$Position <- as.factor(test_df$Position)
  }
  return(list(train_df, test_df))
}

##########################################################################################################
# Load data
# load data necessary for classification
clustering_data <- read.csv(paste0('./data/data_normalize.csv'), header=T)
clf_data <- load_data(learning_type='classification')
clf_train_df <- clf_data[[1]]
clf_test_df <- clf_data[[2]]

clustering_data %>% 
  ggpairs(columns = c(40:45,49),aes(col=SuperPosition))

fifa_pca <- clustering_data[14:47] %>% 
  prcomp(center=TRUE,scale.=TRUE)

print(fifa_pca)

tibble(sd = fifa_pca$sdev, 
       pc = 1:length(sd)) %>% 
  mutate(cumvar = cumsum((sd^2)/sum(sd^2))) %>% 
  ggplot(aes(pc,cumvar))+geom_line()+geom_point()+
  labs(x="Principal Component",y="Cummulative Proportion of Variance Explained")+
  theme_ipsum_rc()

ggbiplot(fifa_pca, obs.scale = 1, var.scale = 1, alpha = 0.01,
         groups = clustering_data$SuperPosition, varname.size = 4, varname.adjust = 3,
         ellipse = TRUE, circle = FALSE) +
  scale_color_discrete(name = '') +
  scale_x_continuous(limits = c(-20,20))+
  scale_y_continuous(limits = c(-10,10))+
  theme_ipsum_rc()+
  theme(legend.direction = 'horizontal', legend.position = 'bottom')


# KNN for positions
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, preProcOptions = list(pcaComp = 15))
grid_knn <- expand.grid(.k=seq(45,45,0))
position_knn <- train(clf_train_df[,c(14:47)], clf_train_df[,50], method = "knn",
                  trControl = train_control,preProcess = c("center","scale","pca"),
                  tuneGrid = grid_knn)

position_knn

position_knn_predict <- predict(position_knn,newdata = clf_test_df[,c(14:47)])
confusionMatrix(position_knn_predict,clf_test_df[,50])


# Random Forest for positions
train_control <- trainControl(method = "repeatedcv",number = 10, repeats = 3)
grid_rf <- expand.grid(.mtry=c(5))
position_rf <- train(clf_train_df[,c(14:47)], clf_train_df[,50], method = "rf",
                 trControl = train_control,preProcess = c("center","scale"),
                 tuneGrid = grid_rf)

position_rf

position_rf_predict <- predict(position_rf,newdata =  clf_test_df[,c(14:47)])
confusionMatrix(position_rf_predict,clf_test_df[,50])

#SVM Linear for Positions
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
grid_svm <- expand.grid(.cost=c(1.75))
position_svm_linear <- train(clf_train_df[,c(14:47)], clf_train_df[,13], method = "svmLinear2",
                         trControl=train_control,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid_svm)
plot(position_svm_linear) 
position_svm_linear_predict <- predict(position_svm_linear,newdata = clf_test_df[,c(14:47)])
confusionMatrix(position_svm_linear_predict,clf_test_df[,13])

#Neural Net for Positions
train_control <- trainControl(method = "repeatedcv",number = 10, repeats = 3)
grid_nnet <- expand.grid(.decay=c(0.5),.size=c(5))
position_nnet <- train(clf_train_df[,c(14:47)], clf_train_df[,50], method = "nnet",
                   trControl = train_control, preProcess = c("center","scale"),
                   tuneGrid = grid_nnet, maxit = 500, abstol=1e-2)
position_nnet
position_nnet_predict <- predict(position_nnet,newdata = clf_test_df[,c(14:47)])
confusionMatrix(position_nnet_predict,clf_test_df[,50])

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
  FullData <- as.data.frame(read.csv("./data/data_new.csv",header=TRUE,encoding = "UTF-8"))
  FullData = FullData[FullData$Position=="LB" | FullData$Position=="RB",]
  Positions <- c('GK','CB','LB','LWB','CDM', 'CAM','LM','LW','LF','ST')
  Position_Class<-vector()
  for (position in Positions) {
    Position_Class<-c(Position_Class,getPositionClass(position))
  }
  
  #Positions <- c('LB')
  overall_vector<-vector()
  count=0
  #t <- hashmap('GK',0)
  for(i in seq(1, nrow(FullData))){
    overall=0
    maxOverall = -1
    maxPositon =''
    position_overall_vector<-vector()
    for(k in 1:length(Positions)){
      vector<-return_list_for_position(Positions[k])
      overall=0
      for (j in 1:length(vector)) {
        overall=overall+(vector[[j]] * as.numeric(FullData[i,names(vector[j])]))
      }
      #overall_vector[i]<-overall
      position_overall_vector<-c(position_overall_vector,overall)
      if(overall > maxOverall){
        maxOverall = overall
        maxPositon = Positions[k]
      }
    }
    maxP<-max(position_overall_vector)
    indices<-which(maxP - position_overall_vector <=2)
    if(getPositionClass(FullData[i,'Position']) %in% Position_Class[indices]){
      count = count +1
    }
    #print(maxPositon)
    #print(FullData[i,'Position'])
    #print(FullData[i,'Overall']) 
    #print(maxOverall)
    #print (i)
    #print (FullData[i,'Name'])
    #print("********************")
    #if(comparePosition(maxPositon,FullData[i,'Position'])){
    # count = count +1
    #}
  }
  #print(t)
  
  #FullData <- cbind(FullData, overall_vector) 
  #colnames(FullData)[ncol(FullData)] <- "CalculatedOverall"
  #write.csv(FullData, file="calc_overall.csv", row.names = FALSE)
  accuracy <- count/nrow(FullData)
  print(accuracy)
  
}

calculate_overall_accuracy()


kmeans_result <- player_cluster(data_df = clustering_data[,c(14:47)], n_clusters = 10, clustering_type = "kmeans")
kmeans_sse <- calculate_sse(data_df = clustering_data[,c(14:47)], cluster_assignments = kmeans_result)

# Single link
single_link_result <- player_cluster(data_df = clustering_data[,c(14:47)], n_clusters = 7, clustering_type = "single-link")
single_link_sse <- calculate_sse(data_df = clustering_data[,c(14:47)], cluster_assignments = single_link_result)

# complete link
complete_link_result <- player_cluster(data_df = clustering_data[,c(14:47)], n_clusters = 7, clustering_type = "complete-link")
complete_link_sse <- calculate_sse(data_df = clustering_data[,c(14:47)], cluster_assignments = complete_link_result)

kmeans_elbow_plot(data_df = clustering_data[,c(14:47)], k_values = c(1:15))

print(paste("Kmeans SSE for given params = ", kmeans_sse))
print(paste("Single link SSE for given params = ", single_link_sse))
print(paste("Complete link SSE for given params = ", complete_link_sse))