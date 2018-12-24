### Assignment 5 Section 1: Logistic Regression ###

# Read the data
orig.ebayAssignment = read.csv(file = 
                                 "Desktop/Frameworks and Methods/Assignments/Assignment 5/eBayAssignment.csv")

# Explore the data

# 1. How many rows are in the data? 
str(orig.ebayAssignment)
# 1861 obs. of  14 variables

View(orig.ebayAssignment)
# shows 1861 rows

nrow(orig.ebayAssignment)
# 1861

### ANS: 1861



# 2. How many iPads are Black in color?

# load the dplyr library
library(dplyr)

orig.ebayAssignment %>%
  filter(color == "Black") %>%
  count()
# 425

# load ggplot2 library
library(ggplot2)
# graph it
ggplot(data = orig.ebayAssignment) +
  geom_bar(mapping = aes(x = color))

### ANS: 425



# 3. Which of the following iPads does this dataset contain? 
# (Select all that apply)

# iPad 1
# iPad 2
# iPad 3
# iPad 4
# iPad 5
# iPad mini 3
# iPad mini 4

orig.ebayAssignment %>%
  group_by(productline) %>%
  count()

# alternative method using ggplot
ggplot(data = orig.ebayAssignment) +
  geom_bar(mapping = aes(x = productline))
  
### ANS
### iPad 1
### iPad 2
### iPad 3
### iPad 4
### iPad mini 3

# 4.  What is the uniqueID of the iPad with the highest startprice? 
orig.ebayAssignment %>%
  group_by(UniqueID) %>%
  summarize(max_startprice = max(startprice)) %>%
  arrange(desc(max_startprice))

### ANS: 11397



### Assignment 5 Section 2: Logistic Regression ###

# 1. Split the Data into a train sample and a test sample using a seed 
# of 196 such that 80% of the data is in the train sample. 
# Use sample.split from library(caTools). 
# Hereafter, we will only use the train sample for exploring and 
# building the model. The test sample will only be utilized for 
# evaluating the model.

# load the caTools library
library(caTools)

# use a seed of 196
set.seed(196)
# 80% of the data is in the train sample
split = sample.split(orig.ebayAssignment$sold, SplitRatio = 0.8)
train = orig.ebayAssignment[split,]
test = orig.ebayAssignment[!split,]

# confirm by checking the number of rows in the train and test sets
# training data is roughly 80%
nrow(train)
nrow(test)
nrow(orig.ebayAssignment)

# How many rows are in the train sample?
str(train)
# 1489 obs. of  14 variables

nrow(train)
# 1489

### ANS: 1489



# 2. What is the median startprice of iPads that sold? 
# (Do not include dollar sign. Only include numbers. 
# E.g., 45.75 NOT $45.75) 
train %>%
  filter(sold == "1") %>%
  summarize(median(startprice))

### ANS: 99



# 3. What is the median startprice of iPads that did not sell? 
# (Do not include dollar sign. Only include numbers. 
# E.g., 45.75 NOT $45.75) 
train %>%
  filter(sold == "0") %>%
  summarize(median(startprice))
# 249.99



# 4. Now, let us run a model to predict the variables that influence 
# whether an iPad will be sold or not.  Since the variable to be 
# predicted only takes on two values, we will use a logistic regression 
# model. Use the 'glm' function to build a model with 'sold' as the 
# dependent variable and the following independent variables:

# biddable, startprice, condition, cellular, carrier, color, storage, 
# productline, noDescription, charCountDescription, upperCaseDescription, 
# startprice_99end.

# Be sure to set family as 'binomial'. 

model1 = glm(sold ~ biddable + startprice + condition + cellular +
               carrier + color + storage + productline + noDescription + 
               charCountDescription + upperCaseDescription + 
               startprice_99end, 
             data = train,
             family = "binomial")
summary(model1)
# AIC: 1454.5

# What is the AIC?
AIC(model1)
# 1454.497

### ANS: 1454.497



# 5. Now, let us examine individual variables. 
# Which of the following variables has a significant influence 
# on whether an iPad is sold or not? (Select all that apply). 
# Use a less conservative alpha of 0.10, i.e., compare p-value to 0.10.

# test whether the estimated coefficient is SIGNIFICANTLY different from zero. 

# Any estimated coefficient with an associated p-value that is sufficiently 
# small (e.g. less than 0.10) could be said to have some non-zero influence 
# on price. Then, it is simply a matter of looking at the summary 
# table of the coefficients.

summary(model1)
# The stars are only intended to flag levels of significance for three 
# of the most commonly used levels. 
# If a p-value is less than 0.05 it is flagged with one star (*). 
# If a p-value is less than 0.01 it is flagged with two stars (**). 
# If a p-value is less than 0.001 it is flagged with three stars (***). 
# Note that the stars are based on the full precision (15 digits) 
# of the p-values even though the p-values are only shown with 3 digits 
#after the decimal point. So, a p-value of 0.0504324531978422 will be shown 
# as 0.050 without a star and a p-value of 0.0495340384925377 will be shown 
# as 0.050 with one star.

### ANS:
### biddable
### startprice
### cellular 
### productline



# 6. Based on the results of the model, does a 99 ending for startprice 
# increase the chance of an iPad being sold?

# startprice_99endnot a 99 ending  4.125e-02  1.466e-01   0.281 0.778419 

# if the P-Value is less than 0.05, we reject the null hypothesis 
# rejecting the null means that the coefficient for 99ending is NOT ZERO
# and indicates a strong effect of this variable

# all color coefficients have very high p-values
# therefore, we accept the null
# coefficient for 99ending is Zero and does not impact whether an iPad is sold

### ANS: No



# 7. Based on the results of the model, does color of the iPad have an 
# impact on whether an iPad is sold? 

# colorGold                        7.717e-02  4.654e-01   0.166 0.868288    
# colorSpace Gray                 -1.299e-01  2.788e-01  -0.466 0.641226    
# colorUnknown                    -2.073e-03  1.863e-01  -0.011 0.991121    
# colorWhite                      -4.091e-02  2.040e-01  -0.201 0.841076  

# if the P-Value is less than 0.05, we reject the null hypothesis 
# rejecting the null means that the coefficient for color is NOT ZERO
# and indicates a strong effect of this variable

# all color coefficients have very high p-values
# therefore, we accept the null
# coefficient for color is Zero and does not impact whether an iPad is sold

### ANS: No



### Assignment 5 Section 3: Logistic Regression ###

# 1. Simpler models are generally preferred to more complex models because 
# they are less likely to overfit the data. So, let us drop out non-signficant 
# variables from model1 above but keep variables that previous research or 
# experience indicates should have an effect. 
# So, estimate generate model2 with the following variables:

# biddable+startprice+condition+storage+productline+
# upperCaseDescription+startprice_99end

# What is the AIC (round to one decimal place)?

model2 = glm(sold ~ biddable + startprice + condition + storage + 
               productline + upperCaseDescription + startprice_99end,
             data = train,
             family = "binomial")

summary(model2)
# AIC: 1448.5
  
AIC(model2)
# 1448.506

# If you are surprised by a drop in AIC from model1 to model2, 
# it is because AIC = 2k - 2LL = 2k + (-2LL). 
# Now, -2LL is a measure of error and is similar to SSE  (from linear 
# regression). Simpler models such as model2 will always have -2LL that 
# is larger than that for complex models like model1. 
# To address this, AIC applies a penalty with the term 2k 
# (where k is number of parameters or number of coefficients). 
# For model1, 2k is much larger than 2k for model2.) 

### ANS: 1448.5



# 2. Based on the coefficient of upperCaseDescription, 
# what advice would you give someone selling iPads on eBay? 

## Use a lot of upper case letters in the description
## Use very few upper case letters in the description

summary(model2)

# upperCaseDescription            -0.0191666  0.0088894  -2.156 0.031075

# coefficient for upperCaseDescription is negative
# therefore, additional multiples will only decrease the sell probability


### ANS: Use very few upper case letters in the description




# 3. You will note that the data contains a number of factor variables. 
# In order to model factor variables, they have to be dummy coded. 
# Fortunately, glm and lm functions automatically dummy code factor variables 
# and then run the dummy variables in the model. 
# The first level is usually selected to be the baseline or reference variable 
# to which each of the other levels is compared. 
# Review the results of model2 and the coefficients of the variables. 
# (After controlling for the effects of all other variables in the model), 
# what sells better iPad3 or iPad 1?

summary(model2)$coef[10]

# how much better is the chance of selling a iPad3 relative to iPad1?
exp(summary(model2)$coef[10])
# 1.911807
# relative to iPad1, iPad 3 is 1.9 times more likely to sell

# Now for yourself, after answering the above question, 
# see if you can also find out how much better one sells than the other! 

### ANS: iPad 3



# 4. If startprice goes up by $1, what will be the % reduction in the 
# chance of selling an iPad. To interpret coefficients in logistic regression, 
# you have to exponentiate the coefficient. E.g., exp(coefficient) 

# coeffieicent for startprice
summary(model2)$coef[3]

# exponentiate the coefficient
exp(summary(model2)$coef[3])
# 0.9910342

# relative to 1
1 -(exp(summary(model2)$coef[3]))
round(1 -(exp(summary(model2)$coef[3])), digits = 2)
# relative to 1, there's an 1% decrease

### ANS: 1%



# 5. Based on model2 (and controlling for the effects of all other 
# variables), how much more (or less) likely is an iPad Air 1/2 to 
# sell compared to an iPad 1?

# coeffieicent for iPad Air 1/2
summary(model2)$coef[12]

# exponentiate the coefficient
exp(summary(model2)$coef[12])
# 6.601546
# iPad Air 1/2 is 6.6 times more likely to sell than iPad 1

# percentage likelihood
100 * (exp(summary(model2)$coef[12])-1)
# 560.1546
# iPad Air 1/2 is 560% more likely to sell than iPad 1


### ANS: iPad Air 1/2 is 6.6 times (or 560%) more likely to sell than iPad 1



# 6. Now, let us run one more model called model_productline. 
# For this model, predict the variable 'sold' using only 'productline'. 
# Is the sign of the coefficient for iPad Air1/2 in this model the same 
# as that in model2?

# Your conclusion may make you feel uncomfortable. 
# The explanation lies in the fact that a multiple logistic regression 
# controls for the effects of all other variables such as startprice 
# but a simple logistic regression does not.)

model_productline = glm(sold ~  productline,
                            data = train,
                            family = "binomial")
summary(model_productline)
# productlineiPad Air 1/2 coefficient = -0.5207

summary(model2)
# productlineiPad Air 1/2 coefficient = 1.8873038

### ANS: No



### Assignment 5 Section 4: Logistic Regression ###

# 1. Make predictions on the test set using model2. 
# Place all the predictions in a variable "pred".
# Now, let us use the model to find out what is the probability of sale 
# for an iPad with UniqueID 10940?

# You could do this by running the following code 
# (round your answer to two decimals). pred[test$UniqueID==10940]

# use predict function to make predictions
# use model2
# make predictions on the test set
# type = response, to come up with the probabilities
pred = predict(model2, newdata = test, type = "response")

# find the probability of sale for an iPad with UniqueID 10940
# round answer to two decimals
round(pred[test$UniqueID == 10940], digits = 2)
# 0.03

### ANS: 0.03



# 2. What is the accuracy of model2 on the test set? 
# Use a threshold of 0.5. (Round your answer to two decimals)

# build the classification table
# use threshold of 0.5
ct = table(test$sold, pred>0.5)
ct
#   FALSE TRUE
# 0   172   28
# 1    44  128

# calculate the accuracy
# accuracy = (True Negative + True Positive) /
#           (True Negative + True Positive + False Negative + False Positive)

accuracy = sum(ct[1,1], ct[2,2]) / nrow(test)
round(accuracy, digits = 2)  
# 0.81
  
# calculate manually
round(((172 + 128) / (172 + 128 + 44 + 28)), digits = 2)
# 0.81

### ANS: 0.81



# 3. Let us see if there is any incremental benefit from using model2 over 
# the baseline. Note, if you examine 'sold' in the train sample, it would 
# be easy to see that most iPads don't sell. 
# If one did not have any information on the independent variables one 
# would predict an iPad will not sell. 

# Baseline is the percentage times one would be correct in the test sample 
# if one were to make this assumption. (Round your answer to two decimals) 

# what is the baseline prediction for the test sample
baseline = table(test$sold)[1] / nrow(test)
round(baseline, digits = 2)
# 0.54

### ANS: 0.54



# 4. Is model2 performing better than the baseline? 

accuracy > baseline

### ANS: Yes



# 5. The accuracy measure depends on the cut-value (or threshold) used. 
# Hence a more popular measure is area under the curve (or AUC). 
# AUC is computed by finding the area under the curve of a plot of 
# Senstivity vs. 1-Specificity. AUC is model performance measure that 
# is independent of any particular threshold. You can do this by running 
# the following code.

# Ensure that your set of predictions is called 'pred' and 
# your test sample is called test

# install.packages('ROCR')   
# if you have not installed ROCR, be sure to install it first.
# library(ROCR)
# ROCRpred = prediction(pred,test$sold)
# as.numeric(performance(ROCRpred,"auc")@y.values) # auc measure

## construct plot
# ROCRperf = performance(ROCRpred,"tpr","fpr")
# plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.2),
#text.adj=c(-0.3,2),xlab="1 - Specificity",ylab="Sensitivity") 
# color coded, annotated ROC curve

# What is the auc?

library(ROCR)
ROCRpred = prediction(pred,test$sold)
# auc measure
as.numeric(performance(ROCRpred,"auc")@y.values)
# 0.868968

# color coded, annotated ROC curve
ROCRperf = performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.2),text.adj=c(-0.3,2),xlab="1 - Specificity",ylab="Sensitivity") # color coded, annotated ROC curve

# basic plot
plot(ROCRperf)


