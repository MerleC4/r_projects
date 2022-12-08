# Merle Crutchfield
# ISTA321
# 4/6/2022

# Classification homework - the goal of this assignment is to predict if someone has 'good' or 'bad' credit.  You're going to use both a logistic regression model as well as a kNN model to try and best predict credit class.  

# NOTE - People get most tripped up at the end in either making sure their predictions are outputted as classes (or converted to them) and comparing them to the true test targets. 

# Load packages
library(tidyverse)
library(caret)

#############  Import and Explore - 0.5 points

# Bring in data
credit <- read_csv("https://docs.google.com/spreadsheets/d/1jFkOKgD5NGeD8mDj_42oBNJfFVK42-1cMKk0JxVFxeA/gviz/tq?tqx=out:csv")


wine <- read_csv("https://docs.google.com/spreadsheets/d/13uZEUTfQg8TweeVWiYtF4BqCxNmRm9RpzC5cuetXw1s/gviz/tq?tqx=out:csv")


acid_mod <- lm(score ~ acidity, data = wine)
ph_mod <- lm(score ~ ph, data = wine)
alc_mod <- lm(score ~ alcohol_perc, data = wine)
summary(acid_mod)
summary(ph_mod)
summary(alc_mod)

summary(wine[,c('acidity', 'ph', 'alcohol_perc')])

final_mod <- lm(score ~ acidity + sugars + ph + alcohol_perc + cost_dollars, data = wine)
summary(final_mod)




rm(list = ls())


admit <- read_csv("https://docs.google.com/spreadsheets/d/10J_DXMXc-RAVqWV9FFLYD24ULHEeziPqATRTi3uYMTk/gviz/tq?tqx=out:csv")
admit_split <- createDataPartition(admit$chance_admit, p = 0.8, list = FALSE)
features_train <- admit[ admit_split, !(names(admit) %in% c('chance_admit'))]
features_test  <- admit[-admit_split, !(names(admit) %in% c('chance_admit'))]
target_train <- admit[ admit_split, "chance_admit"]
target_test <- admit[-admit_split, "chance_admit"]
preprocess_object <- preProcess(features_train, method = c('center', 'scale', 'knnImpute'))
features_train <- predict(preprocess_object, newdata = features_train)
features_test <- predict(preprocess_object, newdata = features_test)

target_train <- as.factor(target_train$chance_admit)
target_test <- as.factor(target_test$chance_admit)


knn_fit <- knn3(features_train, target_train, k = 5)
knn_pred <- predict(knn_fit, features_test, type = 'class' )
predictions <- cbind(data.frame(target_test, knn_pred))
summary(predictions)
full_train <- cbind(features_train, target_train)
full_train <- full_train %>% rename(chance_admit = target_train)
log_train <- glm(chance_admit ~ ., family = 'binomial', data = full_train)
log_pred <- predict(log_train, newdata = features_test, type = 'response')
log_pred <- ifelse(log_pred >= 0.5, 1, 0)
predictions$log_pred <- factor(log_pred)
predictions$knn_error <- ifelse(predictions$target_test != predictions$knn_pred, 1, 0)
predictions$log_error <- ifelse(predictions$target_test != predictions$log_pred, 1, 0)
summary(predictions)
knn_conf <- confusionMatrix(predictions$knn_pred, predictions$target_test)
knn_conf$table
log_conf <- confusionMatrix(predictions$log_pred, predictions$target_test)
log_conf$table


# QUESTION - As always, take some time to explore your data.  What levels are present in the character columns?  

# There are several important levels in each column, however the most important ones to me seemed liket the
# employment, which has different ranges of work years such as "<1", "1<=4" ... and even including unemployed.
# Along with this, there is personal status of "male single", "female div/dep/mar", and a few more, but what was
# interesting to me is that they didn't separate gender and marital status. Finally, there is job, which focuses
# on the skill level of the current job, such as "skilled", "unskilled resident", and a few more.
summary(credit)
glimpse(credit)
unique((credit$employment))
unique((credit$personal_status))
unique((credit$job))


# QUESTION - Based on your exploration, pick two columns and describe how you think they might be related to credit score.  What levels within these features will have what effect on the target

# I chose to look at employment and age. I believe that employment, having the levels of 
# ">=7'"       "1<=X<4'"    "4<=X<7'"    "unemployed" "<1'", will have a positive impact on
# credit score as you have been employed longer. In contrast, age, which has values ranging
# from 19-75 will have an interesting relationship with credit score. I believe being younger
# up to a certain range will lower your credit score but there is a range of ages that is good,
# then once you get to a high enough age it will be lowered again.
unique((credit$employment))
unique((credit$age))


############ Preprocessing - 1.5 points


# QUESTION - Start by creating your dummy variables.  
credit2 <- credit
credit_dummy <- dummyVars(credit_score ~ ., data = credit2, fullRank = TRUE)
credit2 <- predict(credit_dummy, newdata = credit2)
credit2 <- data.frame(credit2)
credit_vals <- credit %>% select(credit_score)
credit2 <- cbind(credit2, credit_vals)

# QUESTION - you need to convert your target 'credit_score' to a binary factor.  Make it so that if the value is 'good' that it's replaced with a 1, and if it's 'bad' it's replaced with a 0.  Then convert that to a factor. Do this all while overwriting the original 'credit_score' column so that you don't have two targets. 

credit2$credit_score <- factor(ifelse(credit$credit_score == 'good', 1, 0))


# QUESTION - Now split your data into train and test features and targets.  Use an 80/20 train/test split.

set.seed(888) # run this first and then put your answer below it. 
credit_split <- createDataPartition(credit2$credit_score, p = .8, list = FALSE)


# QUESTION - Take a second to verify that your targets and features contain the proper data.  Check the number of rows in them.  Check to make sure the proper columns are in them as well!

# 800 rows
head(credit_split, 10)
length(credit_split)
features_train <- credit2[ credit_split, !(names(credit2) %in% c('credit_score'))]
features_test  <- credit2[-credit_split, !(names(credit2) %in% c('credit_score'))]
target_train <- credit2[ credit_split, "credit_score"]
target_test <- credit2[-credit_split, "credit_score"]


# QUESTION - On to preprocessing.  Preprocess your data using centering, scaling, and the knnImpute within method.  

preprocess_object <- preProcess(features_train, 
                                method = c('center', 'scale', 'knnImpute'))
features_train <- predict(preprocess_object, newdata = features_train)
features_test <- predict(preprocess_object, newdata = features_test)


###############  KNN Model - 1.5 points


# QUESTION - Use the formula from the lesson/book to calculate your k value before we fit our kNN model.  Remember to round to an odd value.

knn_fit <- knn3(features_train, target_train, k = 5)


# QUESTION - Fit a kNN model on your training data 

knn_pred <- predict(knn_fit, features_test, type = 'class' )


# QUESTION - Now use that to predict the credit_scorees of your test data

predictions <- cbind(data.frame(target_test, knn_pred))
summary(predictions)


# QUESTION - Make a predictions data frame with your true target values and your knn_pred values

full_train <- cbind(features_train, target_train)


##################  Logistic Regression - 1.5 points

# QUESTION - Now fit a logistic regression.  Remember you have to join your features and target back together to train your model. You'll also have to rename your target back to 'credit_score'

full_train <- full_train %>%
  rename(credit_score = target_train)
log_train <- glm(credit_score ~ ., family = 'binomial', data = full_train)

# QUESTION - Check out a summary of your logistic regression model.  Are all features important?  Do the ones you made predictions about earlier pan out?

# Not all of the features are important. Both of the predictions I made earlier were correct
# according to our logistic regression model.
summary(log_train)

# QUESTION - Generate your predictions for your test data.  Be sure to look at the data and convert the values to classes if needed.

log_pred <- predict(log_train, newdata = features_test, type = 'response')
head(log_pred)
log_pred <- ifelse(log_pred >= 0.5, 1, 0)


# QUESTION - Add these logistic regression predictions to your predictions data frame as a new column

predictions$log_pred <- factor(log_pred)
summary(predictions)


#############  Error rates - 1 point

# QUESTION - Calculate error rates between our true test values and the predicted values from both models.  Which model did best?

# Our knn did a slightly better job at calculating the values.
predictions$knn_error <- ifelse(predictions$target_test != predictions$knn_pred, 1, 0)
predictions$log_error <- ifelse(predictions$target_test != predictions$log_pred, 1, 0)
summary(predictions)


# QUESTION - Make confusion matrices for both models.  Which model had more true positives?  Which had more true negatives?   

# True positives: log, true negatives: log
knn_conf <- confusionMatrix(predictions$knn_pred, predictions$target_test)
knn_conf$table
log_conf <- confusionMatrix(predictions$log_pred, predictions$target_test)
log_conf$table


########### Tweaks and summary - 1.5 points

# QUESTION - Go dial k back to 9 and then rerun the script.  Did that improve model fit?

# It improved the error by .06.
k_seq <- seq(from = 5, to = 9, by = 4)

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
error_df <- drop_na(error_df)
head(error_df)


# QUESTION - If we created a preprocessing object using our full dataset and then applied that to both the training and test data, 
# how would this impact our accuracy on the test data? Would this accuracy hold on truly new and unseen data?  
# Why or why not and what is this process called?

# It would increase our accuracy, BUT it would not hold on truly new and unseen data,
# as it will be feeding forward information to our test set, which is known as
# data leakage. The model will perform well but bad once it gets truly new data.


# QUESTION - If you just used a naive model, what would your error rate be if you just predicted every credit score to be 'good.'  Use code to calculate the naive error rate.

# When everything is set to good, we get an error of 0.3 when our k value is 9.

error_df <- data.frame(matrix(ncol = 2, nrow= 0))
x <- c('error', 'k')
colnames(error_df) <- x

knn_fit <- knn3(features_train, target_train, k = 9)
knn_pred <- predict(knn_fit, features_test, type = 'class')
predictions$knn_pred[predictions$knn_pred %in% c(0)] <- 1
error <- mean(ifelse(predictions$target_test != predictions$knn_pred, 1, 0))
error_df[9,'error'] <- error
error_df[9, 'k'] <- 9
error_df <- drop_na(error_df)
head(error_df)


# QUESTION - Based on the naive error rate, how much better are our models?  

# Our models are significantly better than the naive ones. They are better
# by an error rate of 0.05.


rm(list = ls())
