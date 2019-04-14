rm(list=ls(all=T))
cat('\014')

source('./libraries.R')
source('./regression_and_SVM_classifier.R')

# set your working directory
# setwd()

# install all necessary packages
required_packages = c('e1071', 'caret', 'glmnet')
for(package in required_packages){
  if(!(package %in% installed.packages())){
    install.packages(package, dependencies = T)
  }   
}

# set a seed for reproducibility
set.seed(100)
############################################################################################################
load_data <- function(data_folder='./', learning_type){
  FullData <- read.csv(paste0('./data_normalize.csv'), header=T)
  
  smp_size <- floor(0.7 * nrow(FullData))
  
  ## set the seed to make your partition reproducible
  set.seed(123)
  train_ind <- sample(seq_len(nrow(FullData)), size = smp_size)
  
  train_df <- FullData[train_ind, ]
  test_df <- FullData[-train_ind, ]
  
  
  # make sure dependent variable is of type factor if this is classification
  if(learning_type == 'classification'){
    train_df$Position <- as.factor(train_df$Position)
    test_df$Position <- as.factor(test_df$Position)
  }
  return(list(train_df, test_df))
}

##########################################################################################################
# Load data
# load data necessary for regression
reg_data <- load_data(data_folder='./', learning_type='regression')
reg_train_df <- reg_data[[1]]
reg_test_df <- reg_data[[2]]

# load data necessary for classification
clf_data <- load_data(data_folder='./', learning_type='classification')
clf_train_df <- clf_data[[1]]
clf_test_df <- clf_data[[2]]

############################################################################################################ 
# Learning and parameter tuning 

###############################################
# Regression

# slr
# simple_linear_regression_result: list, first argument is the model, second argument are the predicted values
simple_linear_regression_result <- alda_regression(x_train=as.matrix(reg_train_df[,c(2,14:47)]),x_test=as.matrix(reg_test_df[,c(2,14:47)]), 
                                      y_train=reg_train_df[,7], regression_type='linear') 

# ridge 
# ridge_regression_result: list, first argument is the model, second argument are the predicted values
ridge_regression_result <- alda_regression(x_train=as.matrix(reg_train_df[,c(2,14:47)]),x_test=as.matrix(reg_test_df[,c(2,14:47)]), 
                                              y_train=reg_train_df[,7], regression_type ='ridge')

# lasso
# lasso_regression_result: list, first argument is the model, second argument are the predicted values
lasso_regression_result <- alda_regression(x_train=as.matrix(reg_train_df[,c(2,14:47)]),x_test=as.matrix(reg_test_df[,c(2,14:47)]), 
                                              y_train=reg_train_df[,7], regression_type = 'lasso')

# compare rmse
all_regression_summary <- regression_compare_rmse(y_test=reg_test_df[,7], 
                                     linear_regression_prediction = simple_linear_regression_result[[2]], 
                                     ridge_prediction = ridge_regression_result[[2]], 
                                     lasso_prediction = lasso_regression_result[[2]])
print(paste('Best regression model =', all_regression_summary[[1]], 'RMSE =', all_regression_summary[[2]]))

########################################################
# Classification

# SVM

# linear kernel
linear_svm_result <- alda_svm(x_train = clf_train_df[,2,14:47], x_test = clf_test_df[,2,14:47], y_train = clf_train_df[,13], 
                              kernel_name = 'linear')

# radial kernel
radial_svm_result <- alda_svm(x_train = clf_train_df[,2,14:47], x_test = clf_test_df[,2,14:47], y_train = clf_train_df[,13], 
                              kernel_name = 'radial')

# sigmoid kernel
sigmoid_svm_result <- alda_svm(x_train = clf_train_df[,2,14:47], x_test = clf_test_df[,2,14:47], y_train = clf_train_df[,13], 
                              kernel_name = 'sigmoid')

# polynomial kernel
# linear svm
polynomial_svm_result <- alda_svm(x_train = clf_train_df[,2,14:47], x_test = clf_test_df[,2,14:47], y_train = clf_train_df[,13], 
                              kernel_name = 'polynomial')


# compare all classifiers
all_classifier_summary <- classification_compare_accuracy(y_test=clf_test_df[,13], 
                                                   linear_kernel_prediction = linear_svm_result[[2]], 
                                                   radial_kernel_prediction = radial_svm_result[[2]], 
                                                  polynomial_kernel_prediction = polynomial_svm_result[[2]], 
                                                  sigmoid_kernel_prediction = sigmoid_svm_result[[2]])

print(paste('Best classification model =', all_classifier_summary[[1]], 'Overall Accuracy =', all_classifier_summary[[2]]))







