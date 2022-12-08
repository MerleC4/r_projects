# Merle Crutchfield
# 4/1/2022
# ISTA321

##### Background
#The goal of this assignment is to apply all your preprocessing skills to two new sets of data.  You'll need to explore the datasets, identify the issues, and then fix them. You'll also have to select out the columns and rows you need, drop those you don't.  Finally, there's some imputation, one hot encoding, and creating of dummy variables.  


# You'll need both tidyverse AND the caret package.  Load them up!
library(tidyverse)
library(caret)
library(stringr)

###### Dataset # 1 - LA parking tickets - 5 points

# This dataset is a slimmed down version of the 5 million or so parking tickets given in LA in 2018!  There a bunch of real-life errors within that happen when you have thousands of people writing tickets.  

# bring in the data
parking <- read_csv("https://docs.google.com/spreadsheets/d/11ahddH6snm10AuxF51MlOISx2yCsJ2WmPKFdfRFpInU/gviz/tq?tqx=out:csv")

# set a seed for reproducible results

set.seed(888)

# QUESTION - Explore the dataset at the whole dataset level.  What columns might not be useful based off your exploration?

# I think that the violation is not useful to us. This is because we are given the violation code already as a column, which
# makes it easier for us to look at and group together similar violations, making the violation one redundent. Along with this
# the issue time one can be helpful, but it is not formatted in a way that is very helpful for us, as it would be a lot more
# useful if we converted it into strings of times, which we could still implement logic on later.

glimpse(parking)
summary(parking)


# QUESTION - Explore the individual columns within the dataset.  Specifically what levels are present within make, body style, and color?  Any issues that need to be fixed?

# There are 9 makes, 19 body styles, and 7 colors. There is the issue of different levels
# that are meant to represent the same thing, like TOYO and TOYT for toyota, and BK and BL
# for the color black.

length(unique(parking$make))
length(unique(parking$body_style))
length(unique(parking$color))
summary(factor(parking$make))
summary(factor(parking$body_style))
summary(factor(parking$color))


# QUESTION - Explore the agency, plate and violation_code columns. Are they useful?  Should you drop them? Explain why or why not. And if not useful do it.

# There are 12 agencies, 61 states (which is not good), and 13 different violation codes.
# The plates is bad, since there should only be 50 at most, which makes it useless without
# going through to fix the acronyms and make sure they are correct. According to a google
# search, violation codes are different for each state, which makes this useless unless
# we are in California. Finally, the agencies have nothing wrong, but they are useless
# to me for exploring the data set. I will be removing the agencies and plate, but keep
# the violation codes since we can look at how many of each.

length(unique(parking$agency))
length(unique(parking$plate))
length(unique(parking$violation_code))
summary(factor(parking$agency))
summary(factor(parking$plate))
summary(factor(parking$violation_code))

parking$agency <- NULL
parking$plate <- NULL

# QUESTION - based on your earlier exploration there are too many body styles.  Enter summary(factor(parking$body_style)) into your column to get a list of how many observations there are for each style.  Below filter your dataset so it contains only the top four most common body styles.  Ideally you'll do this in a way so that even if the styles in the top four change the filter will still work (i.e. the styles are not hardcoded).

summary(factor(parking$body_style))
top_four <- parking %>% group_by(body_style) %>% summarize(total_obs = n()) %>% top_n(4)
parking <- parking %>% filter(body_style %in% top_four$body_style)


# QUESTION - When you looked at the unique values within the make column I hope you saw that there were two labels for two of the car brands (Toyota and Mercedes). Use the summary(factor()) method like you did above to figure out which of the two in each brand is the wrong label.  Then use ifelse to correct it in this data frame.

summary(factor(parking$make))
for (i in 1:length(parking$make)) {
  if (parking$make[i] == "TOYO") {
    parking$make[i] = "TOYT"
  } else if (parking$make[i] == "MERC") {
    parking$make[i] = "MERZ"
  }
}
summary(factor(parking$make))


# QUESTION - Colors have some similar errors in labels such as there being both WH and WT for white.  Do what you did above and correct the two errors in color that you find. 

summary(factor(parking$color))
for (i in 1:length(parking$color)) {
  if (parking$color[i] == "WH") {
    parking$color[i] = "WT"
  } else if (parking$color[i] == "SI") {
    parking$color[i] = "SL"
  }
}
summary(factor(parking$color))


# QUESTION - Our fine column has several issues.  First, there is the $ sign in the number which prevents us from using it as a numeric column.  Remove the $ from all numbers. After removal convert the column to numeric. Next, there are NA values in the data frame. Use whatever method you like to verify that there are NA values. Then use an ifelse statement to median impute these missing values. 

# Before there were 137 NA values, and now there are none

parking$fine <- parking$fine %>% str_replace('\\$', '')
parking$fine <- as.integer(parking$fine)
sum(is.na(parking$fine))
for (i in 1:length(parking$fine)) {
  if (is.na(parking$fine[i])) {
    parking$fine[i] = "WT"
  }
}
parking$fine <- ifelse(is.na(parking$fine), median(parking$fine, na.rm=TRUE), parking$fine)
sum(is.na(parking$fine))


# QUESTION - The various levels in the violation column are a mess.  Replace all spaces with an underscore '_'.  Replace all forward slashes with a '-'.  Remove all periods.

parking$violation <- parking$violation %>% str_replace('\\s', '_')
parking$violation <- parking$violation %>% str_replace('/', '-')
parking$violation <- parking$violation %>% str_replace('\\.', '')



###########################################################

# Now for part two of our assignment - preprocessing our insurance data. - 5 points

# In this dataset our ultimate goal is to predict insurance costs based on the other features.  Thus the target is the charges column.

# bring in data
costs <- read_csv("https://docs.google.com/spreadsheets/d/1WUD22BH836zFaNp7RM5YlNVgSLzo6E_t-sznxzjVf9E/gviz/tq?tqx=out:csv")

# QUESTION - Explore the whole dataset quickly

glimpse(costs)
summary(costs)


# QUESTION - Remember from our earlier lesson that bodyfat is highly colinear with BMI.  Make a plot that shows this colinear relationship between the two.  

ggplot(costs, aes(x = costs$bodyfat, y = costs$bmi)) + geom_point() + theme_classic() + theme(text = element_text(size=20))


# QUESTION - Given the above, drop the bodyfat column

costs <- costs %>% select(-bodyfat)


# QUESTION - How many levels are present in the region column?  After exploring that, use the dummyVars function to one hot encode everything.  Remember our target is charges and will be dropped after you create the dummy variables, so you'll have to remember to join that back on.  If you're struggling look back at the tutorial!  You should have 9 columns in your final encoded dataframe.

# There are 4 levels present in the region column

summary(factor(costs$region))
my_dummies <- dummyVars(~ region, data = costs, fullRank = TRUE)
my_dummies_pred <- predict(my_dummies, newdata = costs)
costs <- cbind(costs, my_dummies_pred)
costs <- costs %>% select(-region)


# QUESTION - Maybe all that matters is if the individual has kids or not, and not how many kids they have.  Make a binary feature for children and call it children_b in your dataframe.  Drop the original children column afterwards.

my_dummies_2 <- dummyVars(~ children, data = costs, fullRank = TRUE)
children_b <- predict(my_dummies_2, newdata = costs)
costs <- costs %>% select(-children)
costs <- cbind(costs, children_b)


# QUESTION - Both age and bmi need to be scaled and centered.  Use the scale function to do this to both and assign back to their existing columns.

costs$age <- scale(costs$age)
costs$bmi <- scale(costs$bmi)


# QUESTION - Make a linear regression model with charges as your target and all the other features as your predictors.  
# What region has the lowest healthcare costs?  How much does having children influence insurance costs?  
# Given we scaled and centered age and bmi, which one has a bigger effect on costs for a single SD increase in the respective feature?

# The northwest region has the lowest healthcare costs. Having a children increases insurance
# by around $475.50. Age has a bigger effect on costs for a signle SD increase.

summary(lm(costs$charges ~ costs$age + costs$sex + costs$bmi + costs$smoker + costs$regionnorthwest + costs$regionsoutheast + costs$regionsouthwest
   + costs$children, data=costs))




rm(list = ls())
