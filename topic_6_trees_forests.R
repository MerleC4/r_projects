# Merle Crutchfield
# ISTA321
# 4/23/2022

# load libraries

library(rpart)
library(rpart.plot)
library(tidyverse)
library(caret)
library(randomForest)

# This week's homework will focus on evaluating a decision tree and random forest model to classify credit scores as 'good' or 'bad'.  

######### Import and datatype conversion - 0.5 points

# Load data
credit <- read_csv("https://docs.google.com/spreadsheets/d/1jFkOKgD5NGeD8mDj_42oBNJfFVK42-1cMKk0JxVFxeA/gviz/tq?tqx=out:csv")
set.seed(888)

# QUESTION - Convert all your character strings to factors.  I did this in the rpubs lesson using the mutate_if function

credit <- credit %>% mutate_if(is.character, as.factor)
credit$credit_score <-factor(ifelse(credit$credit_score == 'good', 1, 0))
summary(credit)


######### Simple decision tree - 1.5 points


# QUESTION - before we starting trying to predict anything, make a decision tree with all the features and credit_score as your target.  
# Also make a plot of this decision tree.

split_index <- createDataPartition(credit$credit_score, p = 0.8, list = F)
training <- credit[split_index,]
features_test <- credit[-split_index, !(colnames(credit) %in% c('credit_score'))]
target_test <- credit[-split_index, 'credit_score']
price_tree <- rpart(credit_score ~ . ,data = training)
price_tree$variable.importance
rpart.plot(price_tree)


# QUESTION - Are there too many terminal nodes?  Apply a method to reduce the complexity of the tree and plot the results.  Use a cost penalty of 0.05.

# The error went down from 1.322765 to 1.314029, which means there were too many terminal
# nodes and reducing the complexity worked.
tree_preds <- predict(price_tree, newdata = features_test)
target_test$credit_score <- as.numeric(target_test$credit_score)
tree_mse <- mean((tree_preds - target_test$credit_score)^2)
sqrt(tree_mse)
tree_pruned <- prune(price_tree, cp = 0.05)
rpart.plot(tree_pruned)
tree_preds_new <- predict(tree_pruned, newdata = features_test)
tree_mse_new <- mean((tree_preds_new - target_test$credit_score)^2)
sqrt(tree_mse_new)


# QUESTION - Given the above plot, what are the effects of the different levels of checking account status (amount of money in it)?

# We can tell from the plot that the lower the amount of money in it, such as <0
# or 0<=x<200 that you are more likely to have bad credit. If the checking account
# status is not in those, then they are more likely to have better credit.


######### Setting up your loop - 1 points


# QUESTION - Now let's split your data into training and test sets using an 80/20 train/test split.  Create 10 folds.

split_index <- createDataPartition(credit$credit_score, p = 0.8, list = FALSE, times = 10)


# QUESTION - Create an empty data frame with three columns, one for decision tree error rate, one for logistic regression error rate, and another for fold number.  
# The data frame should have as many rows as folds you created.  Don't forget to name the columns.

credit2 <- data.frame(matrix(ncol = 3, nrow = ncol(split_index)))
colnames(credit2) <- c('tree error rate', 'log regression error', 'fold_number')


######### Fitting & testing your tree - 2 points


# QUESTION - Create a for loop that splits your data, then both fits a decision tree and logistic regression model with credit_score as the target and the rest 
# of the columns as your features.  It should also generate predictions and then calculate and store the error rates as well as the fold number. 

for(i in 1:nrow(credit2)){
  features_train <- credit[ split_index[,i], !(names(credit) %in% c('credit_score'))] 
  features_test  <- credit[-split_index[,i], !(names(credit) %in% c('credit_score'))]
  target_train <- credit[ split_index[,i], "credit_score"]
  target_test <- credit[-split_index[,i], "credit_score"]
  full_train <- cbind(features_train, target_train)
  log_train <- glm(credit_score ~ ., family = 'binomial', data = full_train)
  log_pred <- predict(log_train, newdata = features_test, type = 'response')
  log_pred <- ifelse(log_pred >= 0.5, 1, 0)
  predictions <- cbind(data.frame(target_test, log_pred))
  predictions$log_pred <- factor(log_pred)
  predictions$log_error <- ifelse(predictions$credit_score != predictions$log_pred, 1, 0)
  target_test$credit_score <- as.numeric(target_test$credit_score)
  tree_preds <- predict(price_tree, newdata = features_test)
  tree_mse <- mean((tree_preds - target_test$credit_score)^2)
  credit2[i, 'fold_number'] <- i
  credit2[i, 'tree error rate'] <- sqrt(tree_mse)
  credit2[i, 'log regression error'] <- mean(predictions$log_error)
}
credit2


######### Fitting a random forest - 1 points


# QUESTION - Now fit a random forest model, generate predictions and mean error rate.  No need to do the for loop.  You can either generate a new single split 
# index or just use an entry from your previously created split index

rf_train <- randomForest(credit_score ~ ., data = credit, mtry = 6)
importance(rf_train)


######### Calculation error and wrap up - 1.5 points


# QUESTION - Calculate the mean error rate for your decision tree and logistic regression models.  How do those compare to the error rate in your random forest?  
# Include code to do both below.

# The error of the decision tree was the highest of 1.32, then error of the 
# log regression model was 0.24 and finally the random forest had an error
# rate of 0. This means the random forest was the most accurate.
mean(credit2$`tree error rate`)
mean(credit2$`log regression error`)

rf_preds <- predict(rf_train, newdata = features_test)
rf_preds <- as.numeric(rf_preds)
glimpse(target_test$credit_score)
rf_mse <- mean((rf_preds - target_test$credit_score)^2)
sqrt(rf_mse)


# QUESITON - You want to create a credit predictor for when someone applies for a credit card.  When doing so they input all the same feature data and then you 
# need the algorithm to return back 'good' or 'bad'.  Which model type would you use for this?

# The best model for this would be the random forest. This is because we saw that it was more
# accurate than both the log regression model as well as the decision tree. Along with this,
# random forest use lots of different trees with different features and then average them out,
# which makes them more useful, if you have the time since they can take a while for a computer
# to run.


# QUESTION - Which features were most important in your random forest model.  Give me the top three.  

# The top three were credit_amount, checking_status, and duration.
importance(rf_train)


rm(list = ls())

