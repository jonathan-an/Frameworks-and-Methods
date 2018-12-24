### Section 1: Linear Regression ###

# Read Data
# Set the working drive to import the file
getwd()
orig.houses = read.csv(file = 
                          'houses.csv')
View(orig.houses)

# 1. Let us start by splitting the data into a train and test sample 
# such that 70% of the data is in the train sample. 
# Use createDataPartition from the caret package with groups = 100. 
# Set seed to 1031 and be sure to do this just before passing createDataPartition()

library(caret)

# set the seed to 1031
set.seed(1031)

# split the data into a train and test sample 
# such that 70% of the data is in the train sample
# Do the sampling in such a way that the distribution of price is approximately
# equal in both samples.
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

# What is the average house price in the train sample? Do not round your answer.
mean(train$price)

### ANS: 540674.2



# 2. What is the average house price in the test sample? Do not round your answer

mean(test$price)

### ANS: 538707.6



# 3. It is best to explore the data to better understand the structure 
# and nature of the data and to spot unusual values. 
# But, such exploration must only be done with the train data. 
# Now, review the following code before running it to understand what it is doing.

# train %>%
#    select(id, price:sqft_lot , age) %>%
#    gather(key=numericVariable, value=value, price:age) %>%
#    ggplot(aes(x='',y=value)) +
#    geom_boxplot(outlier.color = 'red') +
#    facet_wrap(~numericVariable, scales='free_y')

# You have no doubt noticed a few outliers. 
# Inspect the outlier for bedrooms. 

library(dplyr)
library(ggplot2)
library(tidyr)

# take the training set
train %>%
  # select the id, columns price through sqft_lot, and age
  select(id, price:sqft_lot , age) %>%
  # gather to convert from wide to tall format
  # key - name of the variable whose values form the column names
  # value - name of the variable whose values are spread over the cells
  # return all columns from price to age
  gather(key = numericVariable, value = value, price:age) %>%
  # show distribution of a numeric variable
  # highlight the dispersion in the data as well as outliers
  # plot x (distribution) against value
  ggplot(aes(x='',y=value)) +
  # create a boxplot
  # highlight the outliers in red
  geom_boxplot(outlier.color = 'red') +
  # facet_wrap to organize the charts left to right and then next line
  # wrap by the numericVariable
  # By default, the same scales are used for all panels. You can allow
  # scales to vary across the panels with the `scales` argument.
  # Free scales make it easier to see patterns within each panel, but
  # harder to compare across panels.
  facet_wrap(~numericVariable, scales='free_y')

# What is the living area (sqft_living) for the house with the most bedrooms?
train %>%
  filter(bedrooms > 30) %>%
  summarize(max(sqft_living))

### ANS: 1620



# 4. It seems reasonable to expect or hypothesize that larger houses cost more. 
# Construct a scatterplot to examine the relationship between sqft_living and 
# price, placing sqft_living on the horizontal axis and price on the vertical axis.

# plot the data using the training set
# horizontal axis (x) = sqft_living
# vertical axis (y) = price
# use geom_point() for a scatter plot
ggplot(data = train, aes(x = sqft_living, y = price)) +
  geom_point()

# What is the direction of the points? 

### ANS: Bottom-left to top-right



# 5. What is the correlation between sqft_living and price? 
# Do you see a link between the scatterplot and the correlation measure?

cor(train$sqft_living, train$price)

# check
cor(train$sqft_living, train$price, use = "pairwise.complete.obs")
cor(train$price, train$sqft_living)

### ANS: 0.7060823



### Section 2: Linear Regression ### 

# 1. Construct a simple regression to predict house price from area (sqft_living).
# Call this model1. Let us examine how well the model is predicting price. 
# What is the p-value for the F-statistic? 

# 2.2
# Between 0.5 and 1
# Between 0.05 and 0.5
# Less than 0.05

# price = f(sqft_living)
# use the training data
model1 = lm(formula = price~sqft_living, data = train)
paste('price','=',round(coef(model1)[1],0),'+',round(coef(model1)[2],0),'sqft_living')

model1
summary(model1)

# p-value: < 2.2e-16

# check
2.2e-16 <.05

### ANS: Less than 0.05



# 2. What is the R2 for model1? Do not round your answer. 
# (Think about what this says about model performance).

# get the R^2 // 0.4985522
summary(model1)$r.squared

# check // 0.4986
summary(model1)

### ANS: 0.4985522

# R^2 explains how strong the relationship is
# R^2 is between 0 and 1
# R^2 of .49 indicates a moderately strong relationship


# 3. What is the rmse for model 1? Do not round your answer.

# direct method
# MSE // 69660399490
mean(model1$residuals^2)
# RMSE // 263932.6
sqrt(mean(model1$residuals^2))

# alternative method
# residual sum of squares or SSE (sum of squared errors) // 1.056888e+15
RSS_model1 <- c(crossprod(model1$residuals))
RSS_model1
# mean squared error // 69660399490
MSE_model1 <- RSS_model1 / length(model1$residuals)
MSE_model1
# root mean squared error // 263932.6
RMSE_model1 <- sqrt(MSE_model1)
RMSE_model1


### ANS: 263932.6



# 4. Since this model is built on sample data, it is important to see if 
# the coefficient estimates are non-zero in the population. Based on
# the model results, indicate your agreement with the following statement: 
# the coefficient of sqft_living is significantly different from zero 
# (Hint: Is p < 0.05?)

# True
# False

summary(model1)
paste('price','=',round(coef(model1)[1],0),'+',round(coef(model1)[2],0),'sqft_living')

# p value is < .00000000000000022
# this p value is much smaller than the conventional value of .05 that 
# is often used as a criterion for statistical significance.
# the P-Value is less than 0.05, we reject the null hypothesis 
# rejecting the null means that the coefficient for sqft_living is NOT ZERO

### ANS: TRUE



# 5. Based on this model, on average, what would a 1400 square foot house cost?

summary(model1)

# doing it manually
model1$coef[1]+ model1$coef[2]*1400

# use the predict function
predict(model1, newdata = data.frame(sqft_living = 1400)) 


### ANS: 346581



# 6. If a homeowner were to put in a 200 square foot addition on the house, 
# how much would the price be expected to go up by?

summary(model1)
paste('price','=',round(coef(model1)[1],0),'+',round(coef(model1)[2],0),'sqft_living')

# first method
model1$coef[2]*200

# alternative method
200 * coef(model1)[2]

### ANS: 56980.49 



# 7. Construct another simple regression to predict house price from waterfront. 
# Call this model2. Note, for the waterfront variable, 1 indicates the house 
# has a view to the waterfront. 

# simple regression with categorical predictor
# waterfront is a categorical variable with two levels
# 1 indicates the house has a view to the waterfront
# 0 indicates the house doesn't have a view to the waterfront

model2 = lm(price~waterfront, data = train)

# confirm the factors
class(train$waterfront)
# confirm the levels
levels(train$waterfront)

# 1 indicates the house has a view to the waterfront
# 0 indicates the house doesn't have a view to the waterfront
table(train$waterfront) 
nrow(train)

# What is the R2 for model2?
# For this question, an unrounded answer is acceptable.  
# Answers with three decimal place precision will also earn full credit. 
# E.g., if the answer is 1.123456, the result of round(x = 1.123456, digits = 3) 
# which is 1.123 will also earn full credit.

summary(model2)

# get the R^2
summary(model2)$r.squared



### ANS: 0.07406626



# 8. Does a waterfront view influence price of a house?

# Yes
# No

# Explore the relationship with a categorical variable, use a bar chart.
ggplot(data = model2,aes(x = waterfront,y = price, fill = waterfront)) +
  geom_bar(stat='summary',fun.y='mean',position='dodge')

summary(model2)
# p value for waterfront <2e-16
# given such a low p-value that is < 0.05, one would reject the null
# null is that waterfront DOES NOT influence price
# this indicates A STRONG EFFECT of this variable

### ANS: Yes



# 9. What is the impact of a waterfront view on the expected price? 
# Asked another way, how much more is the expected price of a house 
# with a waterfront view compared to one without a waterfront view?

# doing it manually
# predicted price without a waterfront view
model2$coef[1]+ model2$coef[2]*0

# predicted price with a waterfront view
model2$coef[1]+ model2$coef[2]*1

# check
(model2$coef[1]+ model2$coef[2]*1) - (model2$coef[1]+ model2$coef[2]*0)

# waterfront view adds 1130312
model2$coef[2]*1

### ANS: 1179766



# 10. Which of the two models has a lower rmse? 
# Remember, a lower rmse implies a better model.

# direct method
# model1 RMSE
sqrt(mean(model1$residuals^2))
# model2 RMSE
sqrt(mean(model2$residuals^2))

# alternative method
# residual sum of squares or SSE (sum of squared errors)
RSS_model2 <- c(crossprod(model2$residuals))
RSS_model2
# mean squared error
MSE_model2 <- RSS_model2 / length(model2$residuals)
MSE_model2
# root mean squared error
RMSE_model2 <- sqrt(MSE_model2)
RMSE_model2

# model1 RMSE // 263932.6
RMSE_model1
# model2 RMSE // 358649.4
RMSE_model2

# check
sqrt(mean(model1$residuals^2)) < sqrt(mean(model2$residuals^2))

### ANS: model1



### Section 3: Linear Regression ###

# 1. Now let us use both the predictors from model1 and model2 to predict price.
# Use sqft_living and  waterfront to predict price. 
# Do not model their interaction. Call this model3.

# Multiple regression
model3 = lm(price~sqft_living + waterfront, data = train)

summary(model3)

# How does R2 of model3 compare to model1 and model2?

# model1 R^2 // 0.4985522
summary(model1)$r.squared

# model2 R^2 // 0.07406626
summary(model2)$r.squared

# model3 R^2 // 0.5375464
summary(model3)$r.squared

# check
summary(model3)$r.squared > summary(model1)$r.squared
summary(model3)$r.squared > summary(model2)$r.squared

# model3 R2 is higher than model1 or model2
# model3 R2 is lower than model1 or model2

### ANS: model3 R2 is higher than model1 or model2



# 2. What is the impact of a waterfront view on the expected price 
# holding area constant (sqft_living)? This question is slightly different 
# from the question asked of model2 and so is the answer.

summary(model3)

# (1) indicates when there is a waterfront view
# waterfront view will correspond to a (1 * 829983.104) increase in price
# while holding sqft_living constant
model3$coef[3]*1

### ANS: 861002.4



# 3. Now, run a multiple regression model with the following predictors:
# bedrooms, bathrooms, sqft_living, sqft_lot, floors, waterfront, view, 
# condition, grade, age
# Call this model4. What is the R2 for model4?

# multiple regression - many variables
model4 = lm(price~
              bedrooms + bathrooms + sqft_living + 
              sqft_lot + floors + waterfront + view +
              condition + grade + age,
              data = train)

summary(model4)

# model4 R^2 // 0.6512827
summary(model4)$r.squared


### ANS: 0.6512827



# 4. What is the rmse for model4?

# direct method
# MSE // 48443310691
mean(model4$residuals^2)
# RMSE // 220098.4
sqrt(mean(model4$residuals^2))


# alternative method // 7.349819e+14
RSS_model4 <- c(crossprod(model4$residuals))
RSS_model4
# mean squared error // 48443310691
MSE_model4 <- RSS_model4 / length(model4$residuals)
MSE_model4
# root mean squared error // 220098.4
RMSE_model4 <- sqrt(MSE_model4)
RMSE_model4



### ANS: 220098.4



# 5. Which of the four models constructed so far has the lowest rmse? 

# RMSE (direct method)
sqrt(mean(model1$residuals^2))
sqrt(mean(model2$residuals^2))
sqrt(mean(model3$residuals^2))
sqrt(mean(model4$residuals^2))


# alternative method
# residual sum of squares or SSE (sum of squared errors) // 9.747006e+14
RSS_model3 <- c(crossprod(model3$residuals))
RSS_model3
# mean squared error // 64243381364
MSE_model3 <- RSS_model3 / length(model3$residuals)
MSE_model3
# root mean squared error // 253462.8
RMSE_model3 <- sqrt(MSE_model3)
RMSE_model3


# RMSE (alternative method)
RMSE_model1
RMSE_model2
RMSE_model3
RMSE_model4

# check
RMSE_model4 < RMSE_model1
RMSE_model4 < RMSE_model2
RMSE_model4 < RMSE_model3

# check
sqrt(mean(model4$residuals^2)) < sqrt(mean(model1$residuals^2))
sqrt(mean(model4$residuals^2)) < sqrt(mean(model2$residuals^2))
sqrt(mean(model4$residuals^2)) < sqrt(mean(model3$residuals^2))


### ANS: Model 4



# 6. Which of the following predictors have an influence 
# on price? (Select all that apply)

# bedrooms
# bathrooms  
# sqft_living
# floors     
# age


# The question didn't ask which predictors had a high enough influence
# just whether they had any influence at all
# From that point of view, it is sufficient to test whether the 
# estimated coefficient is SIGNIFICANTLY different from zero. 

# Any estimated coefficient with an associated p-value that is sufficiently 
# small (e.g. less than 0.05) could be said to have some non-zero influence 
# on price. Then, it is simply a matter of looking at the summary 
# table of the coefficients.

summary(model4)$coefficients

# based on table and p-values
# bedrooms = 3.904663e-56 // sufficiently small non-zero influence on price
3.904663e-56 < 0.05

# bathrooms = 2.661297e-34 // sufficiently small non-zero influence on price
2.661297e-34 < 0.05

# sqft_living = 0.000000e+00 // sufficiently small non-zero influence on price
0.000000e+00 < 0.05

# floors = 3.271174e-09 // sufficiently small non-zero influence on price
3.271174e-09 < 0.05

# age = 0.000000e+00 // sufficiently small non-zero influence on price
0.000000e+00 < 0.05

### ANS: ALL - bedrooms, bathrooms, sqft_living, floors, and age



# 7. If a person decides to add another bathroom, what would be 
# the increase in expected price, holding all other predictors constant?

summary(model4)
model4$coef[3] * 1

### ANS: 50744.76 



# 8. Of all the predictors in model4, 
# which exerts the strongest influence on price?
lm.beta(model4)

# bedrooms = -.097
# bathrooms = .105
# sqft_living = .437
# sqft_lot = -.029
# floors = .036
# waterfront = .143
# view = .085
# condition = .030
# grade = .380
# age = .293

### ANS: sqft_living



# 9. Finally, let us apply this model estimated on train data to test data. 
# What is the R2 for the test sample?
# For this question, an unrounded answer is acceptable.  
# Answers with three decimal place precision will also earn full credit. 
# E.g., if the answer is 1.123456, the result of round(x = 1.123456, digits = 3) 
# which is 1.123 will also earn full credit.

# in sample metrics
# R^2
summary(model4)$r.squared

# out of sample metrics
pred_test = predict(model4, newdata = test)
# sum of squared errors
sse_test = sum((pred_test - test$price)^2)
# total sum of squared
sst_test = sum((mean(train$price)-test$price)^2)
# R^2
r2_test = 1 - sse_test/sst_test; r2_test


### ANS: 0.6544908




# 10. What is rmse for model4 on the test sample?

# root mean squared error
rmse_test = sqrt(mean((pred_test-test$price)^2))
rmse_test


### ANS: 207835.2
