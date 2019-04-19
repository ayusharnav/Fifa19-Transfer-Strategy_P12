rm(list = ls(all = T))

source('./libraries.R')
source('./methods.R')

load_train_and_test_data <- function(folder_path='./'){
  data <- as.data.frame(read.csv("./data/data_normalize.csv",header=TRUE,encoding = "UTF-8"))
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

regression_classification <- regression(4)