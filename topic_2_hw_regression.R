# Merle Crutchfield
# 3/26/2022
# ISTA321

#### BACKGROUND
# this week's assignment revolves around understanding how weather conditions influence the number of bike rides in a given day. This is real 2018 data from Chicago's DIVVY bikeshare system that I've connected to  2018 weather data.  

# you have the following relevant columns:
# number_rides - number of rides in a given day
# AWND - Average wind speed in the day
# TAVG - Average temperature in Fahrenheit
# SNOW - Total snowfall in the day
# PRCP - Total rainfall in the day

# load libraries 
library(tidyverse)
library(ggplot2)
library(reshape2)

# load data
bikes <- read_csv("https://docs.google.com/spreadsheets/d/1DK8ZSmIgvZ1eVVF33NCNLyLxvYFA8t1jeGrJppaBDpI/gviz/tq?tqx=out:csv")

############## EDA - 1.5 points

# QUESTION - Explore your data with a summary.  In a few sentences describe the data, if you think they make sense, and why that is.  

# The data shows information on every day in 2018, with the amount of rides ranging from 375-20585 in a day with a mean of 9847. 
# Along with this, it shows that the temperature is 51 degrees F, but it ranges from -9 degrees to 97 degrees throughout the year.
# It is a little confusing, since by having a TAVG, TMAX, and TMIN we have to look through each one to find specific points for
# highs and lows, but the data itself makes sense. What stands out the most to me is the mean_rider_age, which seems to be in the
# 32-38 range, whereas I would have assumed it would be closer to the 20's range.
summary(bikes)
glimpse(bikes)


# QUESTION - Make some histograms to understand the distribution of the columns mean_trip_time and mean_distance.  Describe a few features you see in each figure?

# For the mean_trip_time, it is skewed right, with the peak being in the 10-12 column, and the range going from 8-35.
# For the mean_distance, it looks more like a multimodal distribution, with the two main peaks at 1.1 and 1.4,
# but having a range between 0.8 and 1.8.
hist(bikes$mean_trip_time)
hist(bikes$mean_distance)


# QUESTION - Use a method to figure out the weather conditions on the day that had the most rides.  Also, what date did this occur on? 

# There was no rain or snow, and the temperature ranged between 61-82 with an average of 71.
# It was on 7-28-2018
bikes[bikes$number_rides == max(bikes$number_rides), ]


# QUESTION - Explore how at least two pairs of features are correlated visually.  In addition to the figures for each, provide a sentence describing the correlation of each. 

# I chose to look at the mean distance of bike rides when compared to the amount of snow,
# and the amount of rain in a day. In both instances, as the amount of snow and rain
# increase, the average distance ridden decreases, making them inversely correlated,
# which makes sense since no one wants to ride bikes in poor weather conditions.
plot(bikes$mean_distance, type="o", col = "red", ylim=c(0, 5))
lines(bikes$SNOW, col = "blue")

plot(bikes$mean_distance, type="o", col = "red", ylim=c(0, 3))
lines(bikes$PRCP, col = "blue")


############## Linear regression - 1.5 points

# QUESTION -  fit a linear regression model using TAVG as a feature to explain your target, number_rides.  Remember the format is lm(targer ~ feature, data = ...)

lm(bikes$number_rides ~ bikes$TAVG, data=bikes)


# QUESTION - How much variation is being explained in your target? Please use code to get the answer from the summary and store it in an object. So CAll that object vs. just writing out the answer from a summary.  

# .7859 is our R-squared value, which is the proportion of variation
model <- lm(bikes$number_rides ~ bikes$TAVG, data=bikes)
summary(model)
rval <- summary(model)$r.squared


# QUESTION - Calculate the confidence interval for B1 - Do it in a way that works if model structure or data gets added to or removed!  You can use the model coefficients from the model summary vs. calculating from scratch.

b1 <- summary(model)$coefficients[[2]]
SE_b1 <- summary(model)$coefficients[[4]]
b1_ci_upper <- round(b1 + 2*SE_b1, 2)
b1_ci_lower <- round(b1 - 2*SE_b1, 2)
paste('Confidence interval is ', b1_ci_lower, 'to', b1_ci_upper)


# QUESTION - Interpret your B1 coefficient in 'plain English.'

# The B1 coefficient is the slope of the graph, which means for every degree of temperature
# higher it goes, there will be 251 more bike rides that day.


# QUESTION - Calculate the predicted number of rides if the average temperature is 68 degrees

# 14106 rides (14105 if truncated)
summary(model)$coefficients[[1]] + summary(model)$coefficients[[2]]*68


# QUESTION - Make a figure showing the relationship between TAVG and number_rides. 
# This is two continuous variables which should tell you what type of plot you need.  
# You can then make a vector of x-values.  Then extract the coefficients from the model to predict y.  
# You then have all you need to add a geom_line() to your plot! Also, I know that ggplot can fit this line for you, 
# but please don't do that. The goal is to demonstrate you understand the different parts of a regression model.

vector <- c(bikes$TAVG)
plot(bikes$TAVG, bikes$number_rides)
df <- data.frame(TAVG = bikes$TAVG, number_rides = bikes$number_rides)
ggplot(df, aes(x = TAVG, y = number_rides)) + geom_point() + geom_line()


############## Comparing two linear regression models - 1.5 points

# QUESTION - Fit another regression model using AWND as a feature but leave number_rides as the target

model2 <- lm(bikes$number_rides ~ bikes$AWND, data=bikes)


# QUESTION - Which is a better model, this one or the first one you fit?  Use two pieces of evidence from the model summaries to justify your answer.

# The first model would be a better one. First, the r-squared value is significantly higher, as 0.09
# of the proportion of the variation being attributed to AWND is less significant than the .78
# value we got earlier. Second, the confidence interval has a much larger range of values than the
# first one, which indicates it is a worse model.
summary(model2)
b1_2 <- summary(model2)$coefficients[[2]]
SE_b1_2 <- summary(model2)$coefficients[[4]]
b1_ci_upper_2 <- round(b1_2 + 2*SE_b1_2, 2)
b1_ci_lower_2 <- round(b1_2 - 2*SE_b1_2, 2)
paste('Confidence interval is ', b1_ci_lower_2, 'to', b1_ci_upper_2)


############## Multiple regression - 1.5 points

# QUESTION -  fit a multiple regression model with number of rides as the target and then AWND, PRCP, SNOW, and TAVG as features.  Remember, multiple regression models have all these features in a single model.  

model3 <- lm(bikes$number_rides ~ bikes$AWND + bikes$PRCP + bikes$SNOW + bikes$TAVG, data=bikes)


# QUESTION - How much extra variation did you explain by including these other features compared to just the simple linear model with only TAVG as a feature? 

# Only an extra .067
rval3 <- summary(model3)$r.squared
rval3 - rval


# QUESTION - Were any of the additional features not important?  Use two pieces of evidence to justify your answer.

# First, the snow was not important. This is because the R-squared value only increased
# by .002 when taking into account TAVG as well. Second, conceptually, it will only be
# snowing when the average temperature is low, and so most of the data can be attributed
# to the temperature and not the snow itself. This differs from wind and rain because
# there does not have to be a set temperature for these to be occuring.
summary(lm(bikes$number_rides ~ bikes$AWND+bikes$TAVG, data=bikes))
summary(lm(bikes$number_rides ~ bikes$PRCP+bikes$TAVG, data=bikes))
summary(lm(bikes$number_rides ~ bikes$SNOW+bikes$TAVG, data=bikes))
summary(lm(bikes$number_rides ~ bikes$TAVG, data=bikes))


############# Making new features - 1.5 points

# Several of these features are correlated.  For example, snow is a form of precipitation, so they're inherently related. On the other hand, some might make more sense if they're aggregated in a unique way. For example, the difference in min and max temperature might be more informative than just the max alone.

# Let's make a temperature variability feature and see how that relates to the number of our rides

# QUESTION - Make a new feature that gets simply the difference between the max temp of the day and the min temp of the day. Call it TVAR. Fit that TVAR feature in addition to the features you fit in your last multiple regression model

TVAR <- bikes$TMAX - bikes$TMIN
bikes2 <- bikes
bikes2$TVAR <- TVAR
summary(lm(bikes2$number_rides ~ bikes2$AWND + bikes2$PRCP + bikes2$SNOW + bikes2$TAVG + bikes2$TVAR, data=bikes2))


# QUESTION - Was TVAR important?  If so, what does the effect of TVAR suggest on rider behavior?

# TVAR was not important. This is because the R-squared value only increased by 0.02. Along with
# this, it only has a R-squared value of .12
summary(lm(bikes2$number_rides ~ bikes2$TVAR, data=bikes2))


############ Age effects - 1.5 points

# QUESTION - Make a model to determine how average temperature influences the average age of the rider on a given day.

model4 <- lm(bikes$TAVG ~ bikes$mean_rider_age, data=bikes)
summary(model4)


# QUESTION - Given this model, what can you say about how temperature influences the age demographics of the bikeshare users? 

# I would say that temperature might have a slight influence on the age demographics of
# bikeshare users. This is because we get an R-squared value of .3551.


############# Interaction models   - 1.5 points

# Age seems to interact with temperature.  Let's make an interaction model to look at how this age x temperature interaction impacts the number of rides


# QUESTION - fit an interaction model between TAVG and mean_rider_age

model5 <- lm(bikes$TAVG ~ bikes$mean_rider_age, data=bikes)
summary(model5)


# QUESTION - what do your model's parameter estimates suggest about how these properties interact.  What happens when the average age of a day goes up or down?  Temperature?  What about when both go up or down? 

# They suggest that as there is an increase in temperature, the average age will decrease. As the
# average age of a day goes up the temperature goes down.



# QUESTION - Obviously it's hard to states all the different outcomes which is why it's generally better to plot different 
# 'scenarios' of interaction models. You're going to do that here by making one figure that plots number of rides ~ TAVG, 
# but you'll actually plot three different lines.  The first will be how when the average age is the lowest in the dataset 
# how rides responds to average temperature.  The second will be how rides responds to average temp when the rider age is at
# the average value in the dataset. The last is when the rider age is at the highest value in the dataset. 

# Note - the bookdown example is for a binary but the same general approach here works!  

mod <- lm(bikes$number_rides ~ bikes$TAVG, data=bikes)
df_n <- nrow(df)
ride_seq <- seq(min(bikes$number_rides), max(bikes$number_rides), length.out = df_n)
int_model <- summary(mod)
int_model$coefficients
ggplot(df, aes(x = TAVG, y = number_rides)) + geom_point() + geom_line(aes(x = TAVG, y = max(bikes$mean_rider_age))) + 
  geom_line(aes(x = TAVG, y = bikes$mean_rider_age)) + geom_line(aes(x = TAVG, y = min(bikes$mean_rider_age)))


# QUESTION -  Based on the plot you created, interpret how age  and temperature interact to influence the number of rides. In other words, how does a day of having higher or lower average age of riders influence the relationship between temperature and the number of rides in a day?

# The number of riders are influenced by age and temperature as both increase the number will decrease,
# and when both decrease the number will increase. When temp increases and age decreases the number
# increases, but when temp decreases and age increases the number decreases.



rm(list = ls())




