### Section 1: Feature Selection ###

# Read the data

orig.houses = read.csv(
  file = "houses.csv")

View(orig.houses)
summary(orig.houses)

# 1. Let us start by splitting the data into a train and test sample 
# such that 70% of the data is in the train sample. 
# Use createDataPartition from the caret package with groups = 100. 
# Set seed to 1031 and be sure to do this just before passing createDataPartition()

library(caret)

# set the seed to 1031
set.seed(1031)

split = createDataPartition(y = orig.houses$price, 
                            # sample with 70% of the data in the train sample
                            p = 0.7, 
                            # returns a matrix of rows
                            list = F, 
                            # and set groups to 100
                            groups = 100)

split

# split the orig.houses dataset into a train and test
train = orig.houses[split,]
test = orig.houses[-split,]

# confirm by checking the number of rows in the train and test sets
# training data is roughly 70%
nrow(train)
nrow(test)
nrow(orig.houses)

# What is the average house price in the train sample? 
# Do not round your answer.
mean(train$price)
# 540674.2

### ANS: 540674.2



# 2. Now, examine bivariate correlations with price to identify variables 
# that are weakly related to (or not relevant) for predicting price. 
# Which of the following variables has the weakest relationship with price?

# bedrooms
# bathrooms
# sqft_lot
# condition
# grade

# large numbers indicate high correlations
# a good predictor has a high correlation with the outcome, but 
# low correlations with other predictors

# compute manually
cor(train$bedrooms,train$price)
# 0.3147962

cor(train$bathrooms,train$price)
# 0.5304317

cor(train$sqft_lot,train$price)
# 0.09052413

cor(train$condition,train$price)
# 0.03790087

cor(train$grade,train$price)
# 0.6620404

# find the correlation of all variables
cor(train)
# bedroom = 0.31479624
# bathrooms = 0.53043170
# sqft_lot = 0.09052413
# condition = 0.03790087
# grade = 0.66204036

### ANS: Condition



# 3. Now, examine correlations amongst the predictors. 
# Which pair has the highest bivariate correlation? 
# You can visualize the relationship by running the following code. 
# This code assumes the train sample is called train.

# sqft_living and bathrooms
# sqft_living and sqft_lot
# sqft_basement and sqft_lot
# sqft_basement and age

# compute manually
cor(train$sqft_living,train$bathrooms)
# 0.7570853

cor(train$sqft_living,train$sqft_lot)
# 0.17369

cor(train$sqft_basement, train$sqft_lot)
# 0.01147958

cor(train$sqft_basement,train$age)
# 0.136069

# library(corrplot)
# corrplot(cor(train[,c(3:7, 10:13,16)]),method = 'square',type = 'lower',diag = F)

# However, to answer the question, you will have to examine bivariate 
# correlations and answer the question: which pair has the highest bivariate 
# correlation? 

library(corrplot)
corrplot(cor(train[,c(3:7, 10:13,16)]),method = 'square',type = 'lower',diag = F)

### ANS: sqft_living and bathrooms



# 4. Theory or domain knowledge can help identify sources of multicollinearity.
# The area of a house (sqft_living) is the sum of area above the basement 
# (sqft_above) and the basement (sqft_basement). 

# This is useful to know because multicollinearity can arise not only 
# from associations between a pair of predictors but also between a 
# linear combination of predictors. But, first let’s verify this by 
# computing the correlation between sqft_living and the sum of sqft_above 
# and sqft_basement. What is the correlation? 

sqft_combined = train$sqft_above + train$sqft_basement

cor(train$sqft_living, sqft_combined)

cor(train$sqft_living,train$sqft_above + train$sqft_basement)

### ANS: 1



# 5. As is apparent from the previous question, the threat of collinearity 
# can also come from linear relationships between sets of variables. 
# One way to assess the threat of multicollinearity in a linear regression 
# is to compute the Variance Inflating Factor (VIF). To do this, 
# run a multiple regression model with the following predictors:
# bedrooms, bathrooms, sqft_living, sqft_lot, floors, waterfront, 
# view, condition, grade, age

# Call this model1. Now, use vif() from library(car) as follows

# price = f(sqft_living)
# use the training data
model1 = lm(formula = price ~ bedrooms + bathrooms + sqft_living +
              sqft_lot + floors + waterfront + view + condition + 
              grade + age, data = train)

# install.packages(‘car’)
library(car)
vif(model1)

# Which predictor has the highest VIF?

# bedrooms = 1.625194
# bathrooms = 3.221352
# sqft_lot = 1.049030
# sqft_living = 4.183860
# grade = 2.944185

# 1 < VIF < INFINITY
# VIF > 10 indicates serious multicollinearity
# VIF > 5 may warrant examination

### ANS: sqft_living



### Section 2: Feature Selection ###

# 1. Let’s examine some algorithm driven ways of feature selection 
# predictors for price. We are going to select from the following predictors:
# bedrooms, bathrooms, sqft_living, sqft_lot, floors, waterfront, view, 
# condition, grade, age

# Evaluate all possible subsets to identify the best-six predictor model. 
# Which of the following variables are included in the best six-predictor
# model?(Select all that apply)

# bedrooms
# bathrooms
# sqft_living
# sqft_lot
# floors

# Best Subset Regression
library(leaps)
subsets = regsubsets(price ~ . ,data = train)
summary(subsets)

# bedrooms
# sqft_living
# waterfront
# view
# grade
# age

### ANS: bedrooms, sqft_living



# 2. What is the R2 for the best 6 predictor model?
# alternative method
summary(subsets)$rsq
# 0.6440608

subsets.summary = summary(subsets)
# summarize other statistics
summary(subsets.summary)
subsets.summary$rsq
# 0.6440608

# compute manually
model_subset = lm(price ~ bedrooms + sqft_living + waterfront +
                    view + grade + age, data = train)

summary(model_subset)
summary(model_subset)$r.squared
# 0.6440608



# 3. Next, run a forward stepwise regression model. 
# As we did when choosing from all possible subsets, we are going 
# to select from the following predictors:
# bedrooms, bathrooms, sqft_living, sqft_lot, floors, waterfront, 
# view, condition, grade, age

# Which of the following variables were included in the best model? 
# (Select all that apply) 

# Forward Stepwise Regression
start_mod = lm(price ~ 1, data = train)
# start and empty is the same for forward stepwise regression
empty_mod = lm(price ~ 1, data = train)

full_mod = lm(price ~ bedrooms + bathrooms + sqft_living +
                sqft_lot + floors + waterfront + view + condition + 
                grade + age, data = train)

forwardStepwise = step(start_mod, 
                       scope=list(upper=full_mod,lower=empty_mod),
                       direction='forward')

summary(forwardStepwise)

# best model = lowest AIC

### ANS: bedrooms, bathrooms, sqft_living, sqft_lot, floors



# 4. Now, run a backward stepwise regression model. 
# As we did when choosing from all possible subsets, we are going 
# to select from the following predictors.
# bedrooms, bathrooms, sqft_living, sqft_lot, floors, waterfront, 
# view, condition, grade, age

# Which of the following variables were included in the best model? 
# (Select all that apply) 

start_mod_back = lm(price ~ bedrooms + bathrooms + sqft_living +
                      sqft_lot + floors + waterfront + view + condition + 
                      grade + age, data = train)
# work backwards so starting point is all the variables
# and empty model is only the intercept
empty_mod_back = lm(price ~ 1, data = train)

# full model includes all variables
full_mod_back = lm(price ~ bedrooms + bathrooms + sqft_living +
                sqft_lot + floors + waterfront + view + condition + 
                grade + age, data = train)

backwardStepwise = step(start_mod_back, 
                       scope=list(upper=full_mod_back,lower=empty_mod_back),
                       direction='backward')

# only generated one step since AIC did not improve
summary(backwardStepwise)

### ANS: bedrooms, bathrooms, sqft_living, sqft_lot, floors



# 5. Next, run a hybrid stepwise regression model, where both 
# forward and backward algorithms operate simultaneously. 
# As we did when choosing from all possible subsets, we are going to 
# select from the following predictors.
# bedrooms, bathrooms, sqft_living, sqft_lot, floors, waterfront, 
# view, condition, grade, age

# Which of the following variables were included in the best model? 
# (Select all that apply) 

# starting model and ending model should both be just the intercept
start_mod_hybrid = lm(price~1, data = train)
empty_mod_hybrid = lm(price~1, data = train)

full_mod_hybrid = lm(price ~ bedrooms + bathrooms + sqft_living +
                       sqft_lot + floors + waterfront + view + condition + 
                       grade + age, data = train)

hybridStepwise = step(start_mod_hybrid, 
                      scope=list(upper=full_mod_hybrid,lower=empty_mod_hybrid),
                      direction='both')

summary(hybridStepwise)

### ANS: bedrooms, bathrooms, sqft_living, sqft_lot, floors



# 6. Now, use a Lasso model to select features. As we did above, 
# we are going to select from the following predictors:
# bedrooms, bathrooms, sqft_living, sqft_lot, floors, waterfront, 
# view, condition, grade, age

# Use a lasso model to select features. 
# Use cv.glmnet which finds the best lambda through 
# 10-fold crossvalidation. 

# Which of the following variables were included in the best model? 
# (Select all that apply)

library(glmnet)

x = model.matrix(price ~ bedrooms + bathrooms + sqft_living +
                   sqft_lot + floors + waterfront + view + condition + 
                   grade + age, data = train)

y = train$price

lassoModel = glmnet(x = x, y = y, alpha = 1)
lassoModel

plot(lassoModel,xvar = 'lambda', label = T)

# 10-fold cross-validation using cv.glmnet
cv.lasso = cv.glmnet(x, y, alpha = 1)
plot(cv.lasso)

coef(cv.lasso)

# bathrooms
# sqft_living
# waterfront
# view
# grade
# age

### ANS: bathrooms, sqft_living



# 7. What is the R2 for the model selected by lasso?

# manually through lm function
lassoModel_lm = lm(price ~ bathrooms + sqft_living + waterfront + 
                     view + grade + age, data = train)

summary(lassoModel_lm)
summary(lassoModel_lm)$r.squared
# 0.6435414

### ANS: 0.6435414



# 8. Dimension reduction: Now, rather than selecting individual variables,
# we will capture the essence in a few components so as to retain at least
# 90% of the information. Run the following code to reduce the predictors 
# to a few components.

# library(caret)
# trainPredictors = train[,c(3:11,16)]
# testPredictors = test[,c(3:11,16)]
# x = preProcess(x = trainPredictors,method = 'pca',thresh = 0.9)
# trainComponents = predict(x,newdata=trainPredictors)
# trainComponents$price = train$price

# How many components did we use to capture the information in the 
# predictors? 

# Principal Component Analysis

library(caret)
trainPredictors = train[,c(3:11,16)]
testPredictors = test[,c(3:11,16)]
x = preProcess(x = trainPredictors,method = 'pca',thresh = 0.9)
trainComponents = predict(x,newdata=trainPredictors)
trainComponents$price = train$price

head(trainComponents)

# construct a model using the components derived from the 
# original predictors
train_model = lm(price~., data = trainComponents)
summary(train_model)

### ANS: 7



# 9. Now, use only the components to predict price in the train sample. 
# What is the R2?
summary(train_model)$r.squared
# 0.5482695

### ANS: 0.5482695



# 10. Next, let us impose the trained component structure on the 
# test sample. Run the following code to do this.

# testComponents = predict(x,newdata=testPredictors)
# testComponents$price = test$price

# Next, apply the train model created with components to the 
# test-component dataset just created. 

# Compute R2 on the test set. Remember R2 = 1 – sse/sst.
# What is the R2 in the test sample?

testComponents = predict(x,newdata=testPredictors)
testComponents$price = test$price

# make sure train_components and test_components has the same structure
str(trainComponents)
str(testComponents)

# evaluate the estimated train model on the test data to access performance
pred = predict(train_model, newdata = testComponents)
sse = sum((pred-testComponents$price)^2)
sst = sum((mean(trainComponents$price) - testComponents$price)^2)
r2_test = 1 - sse/sst
r2_test
#  0.559012

### ANS:  0.559012
