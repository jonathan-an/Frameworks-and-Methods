# 1. An analyst is interested in making good predictions 
# but doesn't care much about inference or interpretability of the model. 
# Which of the following techniques should the analyst use?

# Linear Regression
# Lasso
# Trees
# Support Vector Machines

# Support Vector Machines has the lowest interpretability.

# ANS: SUPPORT VECTOR MACHINES


# 2. An analyst collects a set of data on the Top 500 firms in the US. 
# For each firm she records profit, number of employees, industry, and CEO salary. 
# She is interested in understanding which factors affect CEO salary. 
# Which of the two is the analyst most interested in? 

# Prediction
# Inference

# Goal of prediction is getting the number as accurate as possible
# Goal of Inference is to evaluate the impact of various factors.
# Since she is interested in factors that affect CEO salary and NOT accruately
# estimating the CEO salary, this is inference.

# Ans: INFERENCE


# 3. A researcher is using a set of price, and promotion variables 
# (e.g., size of discount, special) to predict whether a shopper buys 
# Citrus Hill or Minute Maid orange juice. 
# The data is contained in the dataset OJ which is included with the ISLR package. 
# Run the following code to inspect the data.

install.packages('ISLR')
library(ISLR)
str(OJ)

# Is this a regression problem or classification problem? 

# Regression - outcome is numeric (and on a scale)
# Classification - outcome is categorical
# Since the goal is to predict whether a shopper buys Citrus Hill 
# or Minute Maid, there are two outcomes to Predict (either MM or CH), so
# this is a classification problem.

# Ans: CLASSIFICATION


# 4. As model complexity goes up, prediction error on sample used to 
# estimate the model (i.e, training data) goes down. 

# As model complexity increases, the model will always do a 
# better job fitting the training data. No matter how unrelated the additional 
# factors are to a model, adding them will cause training error to decrease.

# Models perform better on the sample used to train the model
# However, it will perform worse on datasets not used to train the model

# prediction error decreases as model complexity increases

# Ans: TRUE


# 5. As model complexity goes up, prediction error on sample NOT used to 
# estimate the model (i.e, test data) first goes down, then goes up. 

# Prediction error decreases and then increases as complexitity increases (curves)
# Rationale: This is an outcome of overfitting.
# There is high bias initally as the model tracks closely to the test sample.
# As the model complexity increases, there is higher variance as the test sample
# is not tracking as closely to the training set anymore.

# Ans: TRUE


# 6. Use simple random sampling to split the mpg dataset into a train and 
# test sample with 80% of the data in the train sample. 
# Set the seed to 100. The mpg dataset comes with ggplot2 package, so 
# load the ggplot2 library first. 
# What is the difference in average highway gas mileage (hwy) between 
# train and test samples?

library(ggplot2)
View(mpg)

# set the seed to 100
set.seed(100)

# split vector contains a set of 80% of the numbers from 1:nrow(mpg)
split = sample(x = nrow(mpg), size = 0.8 * nrow(mpg))

# see the first 10 values of the split vector
split[1:10]

# use the numbers in split to subset the mpg data frame for the train set
train = mpg[split,]
# create the test sample by NOT including the rows in split
test = mpg[-split,]

# confirm by checking the number of rows in the train and test sets
# confirmed that 80% of data is in train
nrow(train)
nrow(test)
nrow(mpg)

# check the difference in average highway gas mileage (hwy) between 
# train and test samples
mean(train$hwy) - mean(test$hwy)
# 0.8969166

# 7. Use stratified sampling to split the mpg dataset into a train and test 
# sample with 80% of the data in the train sample. 
# Do the sampling in such a way that the distribution of hwy is approximately
# equal in both samples. Use a seed of 100. 
# Use createDataPartition for the split and set groups to 20.
# What is the difference in average highway gas mileage (hwy) between 
# train and test samples?

library(caret)

# set the seed to 100
set.seed(100)

# sample in such a way that the distribution of hwy is approximately
# equal in both samples
split_Stratified = createDataPartition(y = mpg$hwy, 
                                       # sample with 80% of the data in the 
                                       # train sample
                                       p = 0.8, 
                                       # returns a matrix of rows
                                       list = F, 
                                       # Use createDataPartition for the split 
                                       # and set groups to 20.
                                       groups = 20)

split_Stratified

# Use stratified sampling to split the mpg dataset into a train and test
train_Stratified = mpg[split_Stratified,]
test_Stratified = mpg[-split_Stratified,]

# confirm by checking the number of rows in the train and test sets
# training data is roughly 80%
nrow(train_Stratified)
nrow(test_Stratified)
nrow(mpg)

# check the difference in average highway gas mileage (hwy) between 
# train and test samples
mean(train_Stratified$hwy) - mean(test_Stratified$hwy)
# -0.08732466


# 8. Use stratified sampling to split the OJ dataset into a 
# train and test sample with 60% of the data in the train sample. 
# Ensure that the proportion of juices in the Purchase column is 
# approximately equal across train and test samples. 
# Use a seed of 100. 
# Utilize sample.split() from the caTools package for this problem. 
# How many minute maid (MM) purchases are in the train dataset? 

install.packages('caTools')
library(caTools)
str(OJ)
View(OJ)

# Use a seed of 100.
set.seed(100)

# Ensure that the proportion of juices in the Purchase column is 
# approximately equal across train and test samples.
# Utilize sample.split() from the caTools package for this problem.
# train and test sample with 60% of the data in the train sample.
split_OJ = sample.split(Y = OJ$Purchase, SplitRatio = 0.6)

# sample.split() will generate a logical, not a vector of numbers
table(split_OJ)

# Use stratified sampling to split the OJ dataset into a train and test
# train and test sample with 60% of the data in the train sample.
train_OJ = OJ[split_OJ,]

# the ! operator is required for the test sample
# subsetting is different for a logical
test_OJ = OJ[!split_OJ,]

# confirm by checking the number of rows in the train and test sets
# confirmed that 60% of the data is in the training set
nrow(train_OJ)
nrow(test_OJ)
nrow(OJ)

# How many minute maid (MM) purchases are in the train dataset? 
# print the results
table(train_OJ$Purchase)
# 250

# How many minute maid (MM) purchases are in the test dataset? 
table(test_OJ$Purchase)


# 9. If the probability associated with the test statistic (p-value) 
# is greater than the level of significance (alpha), 
# the null hypothesis (H0) is rejected. 

# if the chance (p = 0.05) is higher than our threshold (alpha = 0.01),
# one would accept the null hypothesis and conclude that coefficent is zero.
# in other words, Spend does NOT influence Sales.

# Ans: FALSE


# 10. Level of significance (alpha) should be set only after examining the data.

# Choose a level of significance to reflect tolerance for Type 1 error
# e.g. rejecting H0 when in fact it is true

# THEN you gather data and calculate value of test statistic

# Ans: FALSE

