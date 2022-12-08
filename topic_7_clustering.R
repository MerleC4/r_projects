# Merle Crutchfield
# ISTA321
# 4/30/2022

# This week's assignment will focus on using k-means clustering to find out how many groups of wines exist based on a dataset of 178 wines that vary in 13 different features.  Each of these features relate to some aspect of the wine's chemical composition.  Given there are lots of different wine varieties, the question is if we can detect varietal differences based only on these chemical properties.

# Packages and data
library(tidyverse)
library(caret)
library(rpart)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(caret)
library(randomForest)
wine <- read_csv("https://docs.google.com/spreadsheets/d/1QA96h2A_i35FBKCrlmm8_QPgdcUYSZlTDeNFSUl8sEc/gviz/tq?tqx=out:csv")


# QUESTION - Scale your data

wine_scaled <- data.frame(scale(wine))


# QUESTION -  fit a k-means model with k= 5 and plot the resulting clusters with alcohol on the x and flavanoids on the y.  
# Be sure to color your points by cluster and make sure cluster isn't a continuous variable. Also, be sure to make a copy 
# of your original data frame to assign your clusters back to as we're going to use the scaled data again.

wine_kmeans_scaled <- kmeans(wine_scaled, centers = 5, nstart = 20)
wine_kmeans_scaled_clusters <- factor(wine_kmeans_scaled$cluster)
wine$cluster <- wine_kmeans_scaled_clusters
ggplot(wine, aes(x = alcohol, y = flavanoids, color = cluster)) + geom_point()


# QUESTION - What do you think of the results?  Do the clusters make sense?  Too many or too few?

# I don't think that the clusters make sense. This is because they are not very unique clusters,
# as they overlap heavily and aren't distinctly in an area. The only cluster that somewhat
# makes sense is cluster 5, but even that one has some of 1 and 2 points within it. I think that
# this means there are too many clusters right now.


# QUESTION - Calculate the total within-cluster sum of squares from your above model.

# 229.5932 166.0368 160.8527 293.2237 245.4466, with a total of
# 1095.153
print(wine_kmeans_scaled$withinss)
print(wine_kmeans_scaled$tot.withinss)


# QUESTION - We're now going to use the elbow method to figure out our optimal k.  First make your empty data frame with two columns and 10 rows.  
# One column should be named for the within column variation and the other for the number of k

k_means_df <- data.frame(matrix(ncol = 2, nrow = 10))
colnames(k_means_df) <- c('k', 'wi_ss')


# QUESTION - Now use a for loop to fit a model for k values ranging from 1 to 10.  You should fit the model for each value in i, 
# calculate the within-column variation, and then add that value and the k value into your empty data frame

for(i in 1:10) {
  km <- kmeans(wine_scaled, centers = i)
  wi_ss <- km$tot.withinss
  k_means_df[i, 'k'] <- i
  k_means_df[i, 'wi_ss']  <- wi_ss
}


# QUESTION - Now make your elbow plot

ggplot(k_means_df, aes(x = k, y = wi_ss)) + geom_line() + labs(x = 'number of clusters', y = 'total within-cluster sum of squares') + theme_classic()


# QUESTION - Based on this, what value k should you use?  Go and refit the single model with this value, assign clusters back to the data, and replot, 
# coloring by cluster

# The cluster showed that centers = 8 is the best option.
wine2 <- read_csv("https://docs.google.com/spreadsheets/d/1QA96h2A_i35FBKCrlmm8_QPgdcUYSZlTDeNFSUl8sEc/gviz/tq?tqx=out:csv")
wine_scaled2 <- data.frame(scale(wine2))
wine_kmeans_scaled2 <- kmeans(wine_scaled2, centers = 8, nstart = 20)
wine_kmeans_scaled_clusters2 <- factor(wine_kmeans_scaled2$cluster)
wine2$cluster <- wine_kmeans_scaled_clusters2
ggplot(wine2, aes(x = alcohol, y = flavanoids, color = cluster)) + geom_point()


# QUESTION - How good of a job did this do clustering?  Give an example of what you could use this model for.

# This did a better job of clustering that the centers = 5. This model could be used for
# advertising companies such as google and youtube to cluster groups of people based on
# factors like age and interests to give them ads that are more oriented to what the
# viewers are interested in so that they will buy the products.

rm(list = ls())


