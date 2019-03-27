###########################
# ALDA: hw2.R 
# Instructor: Dr. Thomas Price
# Mention your team details here
# Ayush Arnav (aarnav)
# Rashik Bhasin (rbhasin)
# Sagar Bajaj (sbajaj)
# Group 10
############################

require(caret)
require(rpart)
require(ggplot2)
require(reshape2)
require(data.table)
require(philentropy)
require(plyr)
require(utils)


calculate_distance_matrix <- function(train_matrix, test_matrix, method_name){
  # NOTE: This function has already been implemented for you.
  # DO NOT modifiy this function.
  
  # INPUT:
  # Input: train_matrix: type: matrix n_sentences x sentence_length,
  # where n_sentences is total # training rows (100 in the dataset supplied to you) and
  # sentence_length is the total # features (100 in the dataset supplied to you).
  # Input: test_matrix: type: matrix of size 50 x 100 (i.e, 50 rows, 100 features)
  # Input: method_name: type: string, can be one of the following values: ('calculate_euclidean', 'calculate_cosine')
  
  # OUTPUT:
  # output: a 50 x 100 matrix of type double, containing the distance/similarity calculation using method_name between 
  # every row in test to every row in train 
  # This function has already been implemented for you. It takes the data matrix and method name, outputs the distance
  # matrix based on the method name.
  
  distance_matrix = matrix(0L, nrow = nrow(test_matrix), ncol = nrow(train_matrix))
  # the looping logic for pairwise distances is already provided for you
  if(method_name %in% c("calculate_euclidean", "calculate_cosine", "calculate_manhattan")){
    # the looping logic for pairwise distances is already provided for you  
    for(i in seq(1, nrow(test_matrix))){
      print(i)
      for(j in seq(1, nrow(train_matrix))){
        distance_matrix[i,j] <- do.call(method_name, list(unlist(test_matrix[i,]), unlist(train_matrix[j,])))
      }
    }
  }else if(method_name == "calculate_chebyshev"){
    # for chebyshev, you have been tasked to use the library. You will not need to use loops here, since the 
    # library is already optimized. Read the documentation and figure out how to compute the distance matrix
    # without loops for chebyshev
    distance_matrix <- calculate_chebyshev(data_matrix)
  }
  return(distance_matrix)
}

calculate_euclidean <- function(p, q) {
  # Input: p, q are vectors of size 1 x 100, each representing a row (i.e., a sentence) from the original dataset.
  # output: a single value of type double, containing the euclidean distance between the vectors p and q
  # Write code here to calculate the euclidean distance between pair of vectors p and q
  
  return (sqrt(sum((p - q) ^ 2)))
}

calculate_cosine <- function(p, q) {
  # Input: p, q are vectors of size 1 x 100, each representing a row (i.e., a sentence) from the original dataset.
  # output: a single value of type double, containing the cosine distance between the vectors p and q
  # Write code here to calculate the cosine distance between pair of vectors p and q
  
  return (sum(p*q)/(sqrt(sum(p ^ 2)) * sqrt(sum(q ^ 2))))
}

calculate_manhattan <- function(p, q) {
  # Input: p, q are vectors of size 1 x 200, each representing a row (i.e., a sentence) from the original dataset.
  # output: a single value of type double, containing the manhattan distance between the vectors p and q
  # Write code here to calculate the manhattan distance between pair of vectors p and q
  
  return (sum(abs(p-q)))
}

calculate_chebyshev <- function(data_matrix){
  # Input: data_matrix: type: matrix n_sentences x sentence_length, 
  # where n_sentences is total # sentences (155 in the dataset supplied to you) and
  # sentence_length is the total length of each sentence (200 in the dataset supplied to you).
  # output: a 155 x 155 matrix of type double, containing the chebyshev distance between every pair of sentences
  # Write code here to calculate chebyshev distance given an original data matrix of size 155 x 200
  
  return (distance(data_matrix, method = "chebyshev"))
}

knn_classifier <- function(x_train, y_train, x_test, distance_method, k){
  # You will be IMPLEMENTING a KNN Classifier here
  
  # Build a distance matrix by computing the distance between every test sentence 
  # (row in training TF-IDF matrix) and training sentence (row in test TF-IDF matrix).
  # Use the above calculate_distance_matrix function to calculate this distance matrix (code already given to you).
  # You can re-use the calculate_euclidean and calculate_cosine methods from HW1 here.
  # Once the distance matrix is computed, for each row in the distance matrix, calculate the 'k' nearest neighbors
  # and return the most frequently occurring class from these 'k' nearest neighbors.
  
  # INPUT:
  # x_train: TF-IDF matrix with dimensions: (number_training_sentences x number_features)
  # y_train: Vector with length number_training_sentences of type factor - refers to the class labels
  # x_test: TF-IDF matrix with dimensions: (number_test_sentences x number_features)
  # k: integer, represents the 'k' to consider in the knn classifier
  # distance_method: String, can be of type ('calcualte_euclidean' or 'calculate_cosine')
  # OUTPUT:
  # A vector of predictions of length = number of sentences in y_test and of type factor.
  
  # NOTE 1: Don't normalize the data before calculating the distance matrix
  
  # NOTE 2: For cosine, remember, you are calculating similarity, not distance. As a result, K nearest neighbors 
  # k values with highest values from the distance_matrix, not lowest. 
  # For euclidean, you are calculating distance, so you need to consider the k lowest values. 
  
  # NOTE 3:
  # In case of conflicts, choose the class with lower numerical value
  # E.g.: in 5NN, if you have 2 NN of class 1, 2 NN of class 2, and 1 NN of class 3, there is a conflict b/w class 1 and class 2
  # In this case, you will choose class 1. 
  
  # NOTE 4:
  # You are not allowed to use predefined knn-based packages/functions. Using them will result in automatic zero.
  # Allowed packages: R base, utils
  
  distance_matrix <- calculate_distance_matrix(x_train,x_test, distance_method)
  result<- vector()
  for(i in seq(1, nrow(distance_matrix))){
    vec <- vector()
    temp<-distance_matrix[i,]
    if(distance_method %in% c("calculate_euclidean", "calculate_chebyshev", "calculate_manhattan")){
      vec<-(sort(temp, index.return=TRUE)$ix)[1:k]
    }
    else if(distance_method=='calculate_cosine'){
      vec<-(sort(temp, index.return=TRUE,decreasing=TRUE)$ix)[1:k]
    }
    vec<-y_train[vec]
    result[i]<-mean(vec)
  }
  print(result)
  return(result)
}


knn_classifier_confidence <- function(x_train, y_train, x_test, distance_method='calculate_cosine', k){
  # You will be trying to build a modified KNN classifier using the paper given in the HW
  # While most of the implementation is similar to the KNN classifier, there is one additional step you need to do.
  # Read the HW PDF for more details about this method
  
  # INPUT:
  # x_train: TF-IDF matrix with dimensions: (number_training_sentences x number_features)
  # y_train: Vector with length number_training_sentences of type factor - refers to the class labels
  # x_test: TF-IDF matrix with dimensions: (number_test_sentences x number_features)
  # k: integer, represents the 'k' to consider in the knn classifier
  # distance_method: String, can be of type ( 'calculate_cosine')
  
  # OUTPUT:
  # A vector of predictions of length = number of sentences in y_test and of type factor.
  
  # Read the NOTES from comments under knn_classifier.
  # Allowed packages: R base, utils
  
  distance_matrix <- calculate_distance_matrix(x_train,x_test, distance_method)
  result<- vector()
  for(i in seq(1, nrow(distance_matrix))){
    vec <- vector()
    temp<-distance_matrix[i,]
    ordered<-(sort(temp, index.return=TRUE,decreasing=TRUE))[1:k]
    confidence<-sum(ordered$x[1:k])
    for(j in seq(1, k)){
      if(is.na(vec[toString(y_train[ordered$ix[j]])])){
        vec[toString(y_train[ordered$ix[j]])]<-0
      }
      vec[toString(y_train[ordered$ix[j]])]<-vec[toString(y_train[ordered$ix[j]])]+ordered$x[j]
    }
    vec<-vec/confidence
    result[i]<-strtoi(names(sort(vec,decreasing=TRUE)[1]))
  }
  return(as.factor(result))
  
}

dtree <- function(x_train, y_train, x_test){
  set.seed(123)
  # You will build a CART decision tree, then use the tuned model to predict class values for a test dataset.
  
  # INPUT:
  # x_train: TF-IDF matrix with dimensions: (number_training_sentences x number_features)
  # y_train: Vector with length number_training_sentences of type factor - refers to the class labels
  # x_test: TF-IDF matrix with dimensions: (number_test_sentences x number_features)
  
  # OUTPUT:
  # A vector of predictions of length = number of sentences in y_test and of type factor.
  
  # Allowed packages: rpart, R Base, utils
  
  # HINT1: Make sure to read the documentation for the rpart package. Check out the 'rpart' and 'predict' functions.
  
  # HINT2: I've given you attributes and class labels as separate variables. Do you need to combine them 
  # into a data frame for rpart?
  x_train[,ncol(x_train)+1]<-y_train
  colnames(x_train)[ncol(x_train)] <- "Class"
  tree <- rpart(Class~.,
                data=x_train,
                method = "class",
                parms=list(split='gini'))
  plot(tree)
  text(tree)
  return(rpart_predict <- predict(tree,x_test,type="class"))
  
}


dtree_cv <- function(x_train, y_train, x_test, n_folds){
  set.seed(123)
  # You will build a decision tree and tune its parameters using n-fold crossvalidation on the *training* dataset,
  # then use the tuned model to predict class values for a test dataset.
  
  # INPUT:
  # x_train: TF-IDF matrix with dimensions: (number_training_sentences x number_features)
  # y_train: Vector with length number_training_sentences of type factor - refers to the class labels
  # x_test: TF-IDF matrix with dimensions: (number_test_sentences x number_features)
  # n_folds: integer, refers to the number of folds for n-fold cross validation
  
  # OUTPUT:
  # A vector of predictions of length = number of sentences in y_test and of type factor.
  
  # Allowed packages: rpart, caret, R Base, utils
  
  # HINT1: Make sure to read the documentation for the caret package. Check out the 'train' and 'trainControl' functions.
  x_train[,ncol(x_train)+1]<-y_train
  colnames(x_train)[ncol(x_train)] <- "Class"
  
  train_control<- trainControl(method="cv", number=n_folds, savePredictions = TRUE)
  
  model<- train(Class~., data=x_train, trControl=train_control, method="rpart")
  
  cv_predict<- predict(model,x_test)
  
  return(cv_predict)
  
}

regression <- function(k){
  FullData <- as.data.frame(read.csv("./data_normalize.csv",header=TRUE,encoding = "UTF-8"))
  var<-sd(FullData[,7])
  print(var)
  FullData<-FullData[FullData$Age <=0.25, ] 
  FullData<-FullData[FullData$Position == "CDM", ] 
  print(nrow(FullData))
  smp_size <- floor(0.7 * nrow(FullData))
  
  ## set the seed to make your partition reproducible
  set.seed(123)
  train_ind <- sample(seq_len(nrow(FullData)), size = smp_size)
  
  train.data <- FullData[train_ind, ]
  test.data <- FullData[-train_ind, ]
  
  ggplot(train.data, aes(CalculatedOverall, Value) ) +
    geom_point() +
    stat_smooth()
  
  # Build the model
  model <- lm(Value ~ CalculatedOverall, data = train.data)
  # Make predictions
  predictions <- model %>% predict(test.data)
  print (predictions)
  # Model performance
  data.frame(
    RMSE = RMSE(predictions, test.data$Value),
    R2 = R2(predictions, test.data$Value)
  )
  
  ggplot(train.data, aes(CalculatedOverall, Value) ) +
    geom_point() +
    stat_smooth(method = lm, formula = y ~ x)
  
  # Build the model
  model <- lm(Value ~ poly(CalculatedOverall, 5, raw = TRUE), data = train.data)
  # Make predictions
  predictions <- model %>% predict(test.data)
  print (predictions)
  # Model performance
  data.frame(
    RMSE = RMSE(predictions, test.data$Value),
    R2 = R2(predictions, test.data$Value)
  )
  
  ggplot(train.data, aes(CalculatedOverall, Value) ) +
    geom_point() +
    stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE))
}


calculate_accuracy <- function(y_pred, y_true){
  # Given the following:
  
  # INPUT:
  # y_pred: predicted class labels (vector, each value of type factor)
  # y_true: ground truth class labels (vector, each value of type factor)
  
  # OUTPUT:
  # a list in the following order: [confusion matrix, CalculatedOverall accuracy], where confusion matrix is of class "table"
  # (see Figure 2 in the PDF for an example Confusion Matrix)
  # and CalculatedOverall accuracy is on a scale of 0-1 of type double
  # CalculatedOverall class accuracy = accuracy of all the classes
  
  # confusion matrix should have Prediction to the left, and Reference on the top.
  print(as.table(setNames(y_pred, y_true)))
  return(as.table(setNames(y_pred, y_true)))
}

