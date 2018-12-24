### Assignment 8 Section 1: Advanced Decision Trees ###

# 1. Load the dataset OJ by calling the ISLR library.
# library(ISLR)
# head(OJ)

# Next, split the dataset OJ into train and test samples such that 
# 70% of data is placed in the train sample. 
# Use the caTools package to do the split and set seed to 1234.

# How many rows are in the train sample?

library(ISLR)
head(OJ)

# set seed to 1234
set.seed(1234)

# use the caTools package
library(caTools)

# 70% of data is place in the train sample
split_OJ = sample.split(Y = OJ$Purchase, SplitRatio = 0.7)

# sample.split() will generate a logical, not a vector of numbers
table(split_OJ)

# Use stratified sampling to split the OJ dataset into a train and test
# train and test sample with 70% of the data in the train sample.
train_OJ = OJ[split_OJ,]

# the ! operator is required for the test sample
# subsetting is different for a logical
test_OJ = OJ[!split_OJ,]

# confirm by checking the number of rows in the train and test sets
# confirmed that 70% of the data is in the training set
nrow(train_OJ)
nrow(test_OJ)
nrow(OJ)

### ANS: 749



# 2. In the train sample, how many Minute Maid purchases were made?
train_OJ$Purchase

# print the results
table(train_OJ$Purchase)

### 292



# 3. What is the average Price for Minute Maid (in the train sample)? 
# Do not round your answer and do not include units of currency.

train_OJ %>%
  summarize(mean(PriceMM))

### ANS: 2.087223

# 4. What is the average Discount for Minute Maid (in the train sample)? 
# Do not round your answer and do not include currency units.

train_OJ %>%
  summarize(mean(DiscMM))

### ANS: 0.1237116



# 5. How many purchases of Minute Maid were made in Week 275?

train_OJ %>%
  filter(WeekofPurchase == 275) %>%
  filter(Purchase == "MM") %>%
  summarize(count_MM_purchases = n())

### 17



### Assignment 8 Section 2: Advanced Decision Trees ###

# 1. Let us construct a classification tree model to predict "Purchase" 
# of Citrus Hill and MinuteMaid using the following variables:
# Price, Discount, Special and Percent Discount for each of Citrus Hill 
# and Minute Maid, Loyalty for Citrus Hill and difference in Sale Price

# Specifically, the variables are
# PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH

# What is the auc for the test sample? Do not round your answer.

# Tip: The predict function when applied to a classification tree 
# (rather than a regression tree) produces a matrix of probabilities of 
# 0 and probabilities of 1. When entering the predictions into the prediction
# function of the ROCR package, you need to use pred[,2] and not pred. 
# As an e.g.,
# pred = predict(treeModel,newdata=test)
# ROCRpred = prediction(pred[,2],test$dependentVariable)

library(rpart)
library(rpart.plot)

# construct classification tree

tree1 = rpart(Purchase ~ PriceCH +
                PriceMM + 
                DiscCH + 
                DiscMM + 
                SpecialCH + 
                SpecialMM + 
                LoyalCH + 
                PriceDiff + 
                PctDiscMM + 
                PctDiscCH,
              data = train_OJ, 
              method = 'class')
rpart.plot(tree1)
summary(tree1)

library(ROCR)

pred = predict(tree1,newdata=test_OJ)
ROCRpred = prediction(pred[,2],test_OJ$Purchase)

# auc measure
as.numeric(performance(ROCRpred,"auc")@y.values)

### 0.8628776



# 2. Now, tune the model to optimize complexity. 
# Use 10-fold cross-validation and test cp values ranging 
# from 0 to 0.1 in steps of 0.001. What is the optimal cp?

# Be sure to set seed to 100 just before running the train function.

library(caret)

# set up the parameters for cross validation
# use 10-fold cross validation
trControl = trainControl(method = "cv", number = 10)

# trying out 100 complexity parameter values to determine cp with lowest 
# cross-validation error
tuneGrid = expand.grid(.cp=seq(0,0.1,0.001))

# set seed to 100
set.seed(100)
trainCV = train(Purchase ~ PriceCH +
                  PriceMM + 
                  DiscCH + 
                  DiscMM + 
                  SpecialCH + 
                  SpecialMM + 
                  LoyalCH + 
                  PriceDiff + 
                  PctDiscMM + 
                  PctDiscCH,
                data = train_OJ,
                method = "rpart",
                trControl = trControl,
                tuneGrid = tuneGrid)

# examine the relationship between cp and accuracy by looking at the data
# and derived plot
head(trainCV$results)

plot(trainCV)  

# cp that yielded the lowest cross validation error
trainCV$bestTune


### 0.005



# 3. Rerun the tree model with the optimal cp value. 
# What is the auc for this model on the test sample?

tree1_withcp = rpart(Purchase ~ PriceCH +
                PriceMM + 
                DiscCH + 
                DiscMM + 
                SpecialCH + 
                SpecialMM + 
                LoyalCH + 
                PriceDiff + 
                PctDiscMM + 
                PctDiscCH,
              data = train_OJ, 
              method = 'class',
              cp = 0.005)

rpart.plot(tree1_withcp)
summary(tree1_withcp)

pred_cp = predict(tree1_withcp,newdata=test_OJ)
ROCRpred_cp = prediction(pred_cp[,2],test_OJ$Purchase)

# auc measure
as.numeric(performance(ROCRpred_cp,"auc")@y.values)



### Assignment 8 Section 3: Advanced Decision Trees ###

# 1. Using the same variables employed in the classification tree model, 
# let us construct a bag model. Use 1000 trees. 
# Remember, we are running a bag model here, so one has to set a value 
# for mtry. 
# (Hint: Think of the difference between a bag model and a random forest model.)

# Set seed to 100 just before the step where you run the bag model.

# In order to get an AUC, we need to get the prediction probability 
# for each prediction. For this purpose, in the predict function, 
# use argument type = "prob" and use the second column of the output.

# What is the auc for the test sample? 
# Round your answer to two decimal places. 
# If you are not sure how to round, use the round function in R: round(x,2)

library(randomForest)

# set seed to 100
set.seed(100)

bag = randomForest(Purchase ~ PriceCH +
                     PriceMM + 
                     DiscCH + 
                     DiscMM + 
                     SpecialCH + 
                     SpecialMM + 
                     LoyalCH + 
                     PriceDiff + 
                     PctDiscMM + 
                     PctDiscCH,
                   data = train_OJ, 
                   mtry = 9,
                   # use 1000 trees
                   ntree = 1000)

predBag = predict(bag, newdata = test_OJ, type = "prob")
ROCRpred_Bag = prediction(predBag[,2], test_OJ$Purchase)

# auc measure
round(as.numeric(performance(ROCRpred_Bag,"auc")@y.values),2)

### 0.87



# 2. Next, let us construct a random forest model. Use 1000 trees. 
# Do not set mtry as we will use the default. 
# Set seed to 100 just before the step where you run the forest model.

# As in the case of the bag model, for the predict function, 
# use argument type = "prob" and use the second column of the output.

# What is the auc for the test sample? 
# Round your answer to two decimal places. 
# If you are not sure how to round, use the round function in R: round(x,2)

# set seed to 100
set.seed(100)

# Do not set mtry as we will use the default
forest = randomForest(Purchase ~ PriceCH +
                     PriceMM + 
                     DiscCH + 
                     DiscMM + 
                     SpecialCH + 
                     SpecialMM + 
                     LoyalCH + 
                     PriceDiff + 
                     PctDiscMM + 
                     PctDiscCH,
                   data = train_OJ, 
                   # use 1000 trees
                   ntree = 1000)

# use argument type = "prob" and use the second column of the output.
predForest = predict(forest, newdata = test_OJ, type = "prob")
ROCRpred_Forest = prediction(predForest[,2], test_OJ$Purchase)

# auc measure
round(as.numeric(performance(ROCRpred_Forest,"auc")@y.values),2)

### ANS: 0.88



# 3. In this dataset, the levels of variable Purchase are 1 and 2. 
# But, in order to run a boosting model for a two-level classification model, 
# the dependent variable can only take  values 0 and 1. So, run the following 
# to create a new variable Purchase2 as follows:

# For the new variable, 0 represents CH and 1 represents MM
# train$Purchase2 = as.numeric(train$Purchase)-1
# test$Purchase2 = as.numeric(test$Purchase)-1

# Use this new variable Purchase2 and not Purchase. 
# Run a gradient boosting model (gbm) with 1000 trees using Purchase2. 
# Set distribution to "bernoulli", interaction depth to 1 
# and shrinkage parameter to 0.04. 
# Set seed to 100 just before the step where you run the 
# gradient boosting model. 
# Use the same independent variables used in above models.

# In the predict function, use argument type = "response" 
# and set n.trees = 100. 
# Unlike the randomForest package you do not need to 
# request the second column. Probabilities generated are the 
# chance of purchasing the juice labeled as 1 which is Minute Maid.

# What is the auc for this model on the test sample? 
#Round your answer to two decimal places. 
# If you are not sure how to round, use the round function in R: round(x,2)

install.packages('gbm')
library(gbm)

# For the new variable, 0 represents CH and 1 represents MM
train_OJ$Purchase2 = as.numeric(train_OJ$Purchase)-1
test_OJ$Purchase2 = as.numeric(test_OJ$Purchase)-1

# Set seed to 100
set.seed(100)

# Run a gradient boosting model (gbm) with 1000 trees using Purchase2
boost = gbm(Purchase2 ~ PriceCH +
              PriceMM + 
              DiscCH + 
              DiscMM + 
              SpecialCH + 
              SpecialMM + 
              LoyalCH + 
              PriceDiff + 
              PctDiscMM + 
              PctDiscCH,
            data = train_OJ,
            # set distribution to "bernoulli"
            distribution = "bernoulli",
            # use 1000 trees
            n.trees = 1000,
            # interaction depth to 1
            interaction.depth = 1,
            # shrinkage parameter to 0.04.
            shrinkage = 0.04)

# In the predict function, use argument type = "response" 
# and set n.trees = 100.
predBoostTrain = predict(boost, newdata = test_OJ, n.trees = 100, type = "response")

# Unlike the randomForest package you do not need to 
# request the second column. Probabilities generated are the 
# chance of purchasing the juice labeled as 1 which is Minute Maid.
ROCRpred_BoostTrain = prediction(predBoostTrain, test_OJ$Purchase2)

# auc measure
round(as.numeric(performance(ROCRpred_BoostTrain,"auc")@y.values),2)

### 0.88