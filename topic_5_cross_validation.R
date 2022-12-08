# Merle Crutchfield
# ISTA321
# 4/16/2022


# The goal of this homework is simple. You're going to take your knn model from last homework and perform k-fold cross-validation using a for loop.  

# Load packages
library(tidyverse)
library(caret)
set.seed(888)

# Bring in data
credit <- read_csv("https://docs.google.com/spreadsheets/d/1jFkOKgD5NGeD8mDj_42oBNJfFVK42-1cMKk0JxVFxeA/gviz/tq?tqx=out:csv")

# QUESTION - Perform your dummy variable conversion first.  Also remember to bind back on your target feature 'credit_score' and then convert that to a binary factor.  Look at the home solution if you're struggling.  Do you get 300 0's and 700 1's?

my_dummies <- dummyVars(credit_score ~ ., data = credit, fullRank = TRUE)
credit_score <- credit$credit_score
credit <- predict(my_dummies, newdata = credit)
credit <- data.frame(credit)
credit <- cbind(credit, credit_score)
credit$credit_score <-factor(ifelse(credit$credit_score == 'good', 1, 0))
summary(factor(credit$credit_score))


# QUESTION - Create your split index.  I want you do start with 10 folds and a training set size of 70% of the data.  

cs_index <- createDataPartition(credit$credit_score, p = 0.7, list = FALSE, times = 10)


# QUESTION - Initalize your empty data frame.  Make sure it has two columns...  one for error and the other for fold_number

credit2 <- data.frame(matrix(ncol = 2, nrow = ncol(cs_index)))
colnames(credit2) <- c('error', 'fold_number')


# QUESTION - create your for loop.  It should run for each column within split index.  For each run it should use the i'th entry of the index to split the data, then preprocess, fit a model, predict with that knn model (use k = 11), calculate the TEST ERROR RATE (see 2.2.3 in the book for a reminder), then add that error to the ith spot of the error column in the data frame.  Also add the fold number to the data frame.

for(i in 1:nrow(credit2)){
  features_train <- credit[ cs_index[,i], !(names(credit) %in% c('credit_score'))] 
  features_test  <- credit[-cs_index[,i], !(names(credit) %in% c('credit_score'))]
  target_train <- credit[ cs_index[,i], "credit_score"]
  target_test <- credit[-cs_index[,i], "credit_score"]
  preprocess_object <- preProcess(features_train, method = c('scale', 'center', 'knnImpute'))
  features_train <- predict(preprocess_object, features_train)
  features_test <- predict(preprocess_object, features_test)
  knn_fit <- knn3(features_train, target_train, k = 11)
  knn_pred <- predict(knn_fit, features_test, type = 'class' )
  error <- mean(ifelse(target_test != knn_pred, 1, 0))
  credit2[i,'error'] <- error
  credit2[i, 'fold_number'] <- i
}
mean(credit2$error)


# QUESTION - Make a figure using your data frame of errors and fold numbers. 

ggplot(credit2, aes(x = fold_number, y = error)) + geom_line()


# QUESTION - Now that your for loop is running play around with the k of the knn training or the split amount.  Can you get a lower mean standard error?  What value allows you to minimize error more? 

# The lowest error I was able to get was when the value of k = 42. However, this is close to the error that we got when we defaulted
# k to 11 above. All of the errors were in range of 0.256 to 0.327.

predictions <- cbind(data.frame(target_test, knn_pred))

k_seq <- seq(from = 1, to = 51, by = 1)

error_df <- data.frame(matrix(ncol = 2, nrow= 0))
x <- c('error', 'k')
colnames(error_df) <- x

for(i in k_seq) {
  knn_fit <- knn3(features_train, target_train, k = i)
  knn_pred <- predict(knn_fit, features_test, type = 'class')
  error <- mean(ifelse(predictions$target_test != knn_pred, 1, 0))
  error_df[i,'error'] <- error
  error_df[i, 'k'] <- i
}

ggplot(error_df, aes( x = k, y = error)) + geom_line() + geom_point() + labs(x = 'k', y = 'error')


rm(list = ls())
