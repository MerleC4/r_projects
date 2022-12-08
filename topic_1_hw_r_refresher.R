# Merle Crutchfield
# 3/19/2022
# ISTA321

library(tidyverse)

bikes <- read_csv("https://docs.google.com/spreadsheets/d/1t4wJXlT2-P7djUjlK2FziedSrCsxtjTFHRD4mkqfvfo/gviz/tq?tqx=out:csv")


# For this assignment you're going to do some simple exploration and manipulation of the 2018 Chicago DIVVY bikeshare data. Each row represents a single bike rental.


# QUESTION - Use your data exploration techniques to look at both a summary AND a glimpse of bikes.

summary(bikes)
glimpse(bikes)


# QUESTION - What are the two data types present? How many rows and columns are present? Pick two columns and describe what you find in them (e.g. summary stats, character levels, etc. )

# The two data types present are character lists and number lists. There are 154966 rows and 12 columns. The first column I picked is
# gender, which is a list of either "Male" or "Female". In the data, there were 77338 Males, 25240 Females, and 52388 did not have a 
# specified gender. The next column I picked is birthyear. The mean birthyear was 1981 (1982 if we round up instead of trunkating),
# with a standard deviation of 11 years. The median year was 1985, and since it is greater than the mean, the distribution is
# positively skewed.
nrow(bikes)
ncol(bikes)
summary(bikes$gender == "Male")
mean(bikes$birthyear, na.rm = TRUE)
sd(bikes$birthyear, na.rm = TRUE)
median(bikes$birthyear, na.rm = TRUE)


# QUESTION - How many unique values are present in the column bikeid?

# 6104 unique values
length(unique(bikes$bikeid))


# QUESTION - Use some visual exploratory data analysis by making histograms of two of the numeric columns (your choice).  NOTE: Do one in base R and one using ggplot.

hist(bikes$from_station_id)
ggplot(bikes, aes(x = to_station_id)) + geom_histogram()



# QUESTION - Use base R to slice the data frame several different ways. Specifically, to do ALL OF the following operations: Grab just a single row, a single column, a series of rows, a series of columns, a list of rows, and a list of columns.

bikes[1, ]
bikes[, 1]
bikes[1:3, ]
bikes[, 1:3]
bikes[c(1, 3, 5), ]
bikes[, c(2, 4, 6)]


# QUESTION - Use base R to extract JUST the columns usertype, gender, and distance_miles to a dataframe called bikes_1.  Do this again using tidyverse.

bikes_1 <- subset(bikes, select=c("usertype", "gender", "distance_miles"))


# QUESTION - Do the same as above but using tidyverse.  Assign it to an object called bikes_2.

bikes_2 <- bikes %>% select(usertype, gender, distance_miles)


# QUESTION - Use base R to create a dataframe of just subscribers.  Call the resulting dataframe bikes_subs.

bikes_subs <- subset(bikes, usertype=="Subscriber")


# Question - Now use tidyverse to make a dataframe of just subscribers.  Name it whatever you want.

bikes_subs_2 <- filter(bikes, usertype=="Subscriber")


# QUESTION - Use tidyverse to create a dataframe of just Customers who are also Male  Name this data frame bikes_male_cust

bikes_male_cust <- filter(bikes, gender=="Male")


# QUESTION - What's the average distance ridden by male customers?

# 1.193351 miles
mean(bikes_male_cust$distance_miles, na.rm = TRUE)


# QUESTION - Birthyear isn't super useful right now.  Having actual rider age would be better.  Use either base R or tidyverse to create a new column that calculates the rider's age (these data were collected in 2018, btw).  After it's made explore your new column (using whatever method you'd like) to make sure it makes sense.

age_col <- 2018 - bikes$birthyear
bikes$ages <- age_col
min(age_col, na.rm = TRUE)
max(age_col, na.rm = TRUE)
mean(age_col, na.rm = TRUE)
median(age_col, na.rm = TRUE)


# QUESTION - I'm guessing you see some strange values in your newly created age column.  Why don't you create a new dataframe called bikes_realages where it only contains riders who ages are less than or equal to some age that seems realistic to you.

# Have to be between ages of 10-60
bikes_realages <- filter(bikes, ages < 60 & ages > 10)


# QUESTION - Make a histogram of rider ages using ggplot.  Based on this, what age range used the bikeshare the most?

# 25-35 yr olds use it the most, more specifically around 28-30
ggplot(bikes, aes(x = ages)) + geom_histogram() + xlim(10, 60)


# QUESTION - Some of these data types could or need to be changed.  There are three variables that are currently numeric but should be a factor.  What are they and why?

# Birthyear, bikeid, and ages. This is because we want to be
# able to see the distinct values so we don't have to focus on all of the
# duplicate values.


# QUESTION - Now change those three variables in the bikes dataframe to factors

bikes$birthyear <- factor(bikes$birthyear)
bikes$bikeid <- factor(bikes$bikeid)
bikes$ages <- factor(bikes$ages)


# QUESTION - Use some method to figure out what was the most frequently used bike

# Bike 6234
Mode <- function(x) {
  u <- unique(x)
  u[which.max(tabulate(match(x, u)))]
}
Mode(bikes$bikeid)


# QUESTION - How many miles in total was the most frequently used bike ridden.  You're going to need to filter and then do a sum on a column.

# 83.31822 miles
sum(filter(bikes, bikeid==6234)$distance_miles, na.rm = TRUE)


# QUESTION - What was the least used bike?  How many miles was it ridden?

# 0.3087573 miles
AntiMode <- function(x) {
  u <- unique(x)
  u[which.min(tabulate(match(x, u)))]
}
AntiMode(bikes$bikeid)

sum(filter(bikes, bikeid==AntiMode(bikes$bikeid))$distance_miles, na.rm = TRUE)


# QUESTION - How many rides in our data set were returned to the same station they were checked out from?  Remember that you can do a logical comparison between values using ==.  You can then sum your true/false values!

# 8000 rides
sum <- 0.0
for (x in 1:154966) {
  if (bikes$from_station_id[x]==bikes$to_station_id[x]) {
    sum <- sum + 1
  }
}
sum

# QUESTION - Use base R to select just the column distance_miles from bikes and assign it to an object called target_1

target_1 <- bikes$distance_miles


# QUESTION - Now do the same but use tidyverse's select() function.  Assign to target_2

target_2 <- bikes %>% select(distance_miles)


# QUESTION - Now get the mean of target_1 and target_2 using mean().  Don't forget you might need to specify na.rm = TRUE as an argument in mean().

mean(target_1, na.rm = TRUE)
mean(target_2, na.rm = TRUE)


# QUESTION - Did you get a return for both?  Why or why not?  What happens when you run is.data.frame() on target_1 and then target_2?  What about is.vector()?  What is this telling you about the way in which both methods extracted the column?

# I did not get a return for the mean of target_2 but I did for target_1 of 1.331433.
# This is because target_2 is a data frame, but target_1 is not. This means we have to 
# be careful when extracting columns since the return type might be different, such as now.
is.data.frame(target_1)
is.data.frame(target_2)


rm(list = ls())

