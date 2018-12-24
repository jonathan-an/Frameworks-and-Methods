### Section 1: Decision Trees ###

# Read Data
# Set the working drive to import the file
getwd()
orig.wages = read.csv(file = "wages.csv")
                    
View(orig.wages)
summary(orig.wages)

# 1. Which of the following variables are non-metric?
# non-metric = binary / nominal / ordinal

### sex (binary)
### race (nominal)



# 2. Some of the values of earn in this dataset are below 0. 
# Remove these data points by running the following code

# remove rows with negative earning
wages = orig.wages[orig.wages$earn>0,]

View(wages)

# What fraction (a number between 0 and 1) of the respondents are female? 
# (round to two decimal places)
library(dplyr)

wages %>%
  filter(sex == "female") %>%
  count()
# 859

count(wages)
# 1368

round(859/1368,2)
### 0.63



# 3. Which of following races earns the least?
wages %>%
  group_by(race) %>%
  summarize(average_wages = mean(earn)) %>%
  arrange(desc(average_wages))

# check manually
wages %>%
  filter(race == "white") %>%
  summarize(mean(earn))

wages %>%
  filter(race == "other") %>%
  summarize(mean(earn))

wages %>%
  filter(race == "black") %>%
  summarize(mean(earn))

wages %>%
  filter(race == "hispanic") %>%
  summarize(mean(earn))

# race     average_wages
# <fct>            <dbl>
# 1 white           33571.
# 2 other           33432.
# 3 black           28830.
# 4 hispanic        25821.

### hispanic



# 4. Now, split the data into a train and test sample;
# Use the following code for the split. 
# You will note that we are using the sample function that 
# is automatically loaded into R.

set.seed(100)
split = sample(1:nrow(wages), nrow(wages)*0.75)
train = wages[split,]
test = wages[-split,]

# Approximately, what percent of the data is placed in the training sample?

# confirm by checking the number of rows in the train and test sets
# training data is roughly 70%
nrow(train)
nrow(test)
nrow(wages)

nrow(train)/nrow(wages)

### 0.75



# 5. How many observations are in the training set?
nrow(train)

### 1026



### Section 2: Decision Trees ###

# 1. Now, construct a linear regression model to predict earn using 
# all other variables. Call this model1

#Which of the following variables are significant (p < 0.05)? 
# Select all that apply. 

# earn = f(all variables)
# use the training data
model1 = lm(formula = earn~., data = train)

model1
summary(model1)

# (Intercept)   8.70e-06 ***
# height        0.0213 *  
# sexmale       2.16e-12 ***
# racehispanic  0.5210    
# raceother     0.8634    
# racewhite     0.7482    
# ed            < 2e-16 ***
# age           1.52e-07 ***

1.52e-07 < 0.05
### age

2e-16 < 0.05
### education

2.16e-12 < 0.05
### sex

0.0213 < 0.05
### hieght



# 2. We will be comparing linear regression to a regression tree 
# model later.Since tree models don't generate an R2, let us compute 
# the root mean squared error on the training sample. 
# What is the root mean squared error for model1?

# MSE // 705821858
mean(model1$residuals^2)
# RMSE // 26567.31
sqrt(mean(model1$residuals^2))

### 26567.31



# 3. Sometimes variables in combination act differently than when 
# evaluated individually. model1 does not incorporate such combinations 
# called interactions. Let us try and understand the meaning 
# of an interaction using this dataset.

# One may speculate that education has a positive influence on earnings. 
# This is a main effect.

# Similarly one might expect that men on average earn more than women. 
# This is also a main effect.

# Now, let us propose an interaction hypothesis. 
# Education impacts earning differently for men than for women. 
# Specifically, education boosts earnings more for men than for women. 
# This is an interaction effect.

# Visualize this interaction effect by running the code below to 
# plot a bar chart.

library(ggplot2)
ggplot(data=train,aes(y=earn,x=sex,fill=factor(ed)))+ geom_bar(stat="summary",fun.y="mean",position="dodge")

# Next, run the following code that plots a regression between 
# ed and earn separately for men and women.

ggplot(data=train,aes(y=earn,x=ed,color=sex))+  geom_smooth(method="lm",se=F,size=1.2)+  scale_x_continuous(breaks=c(seq(2,20,2)))+  scale_y_continuous(breaks=c(seq(0,100000,20000)))

# What is the approximate difference in earn between 12 years and 
# 16 years of education for Males? 

### 20,000



# 4. What is the approximate difference in earn between 12 years and 
# 16 years of education for Females?

### 15,000



# 5. Now, construct a regression that only models effects of 
# ed and sex on earn.

model_sex_ed = lm(earn~sex + ed + sex*ed,data=train)

# Which of the following variables are significant?

model_sex_ed
summary(model_sex_ed)

# (Intercept)   8.49e-05 ***
# sexmale       0.9929    
# ed            4.33e-16 ***
# sexmale:ed    0.0269 *  

### ed 
4.33e-16 < 0.05

### sexmale:ed
0.0269 < 0.05



# 6. Based on model_sex_ed, sex and ed do not interact in influencing earn. 

# p value for sexmale:ed is < .0269
# this p value is much smaller than the conventional value of .05 that 
# is often used as a criterion for statistical significance.
# the P-Value is less than 0.05, we reject the null hypothesis 
# rejecting the null means that the coefficient for sexmale:ed is NOT ZERO

### FALSE



# 7. Now, that we understand what an interaction effect is, 
# construct a model that incorporates all the variables in model1 
# and the interaction between sex and ed. Call this model2.

# (To include the interaction effect, you need to add a variable called 
# sex*ed like you did in the model_sex_ed.

# What is the rmse for model2 (on train sample)?

model2 = lm(earn~height +
              sex + 
              race +
              ed + 
              age +
              sex*ed,
            data=train)

# MSE // 703099903
mean(model2$residuals^2)
# RMSE // 26516.03
sqrt(mean(model2$residuals^2))

### 26516.03



# 8. The rmse for model2 is lower than rmse for model1.

sqrt(mean(model1$residuals^2))
# 26567.31

sqrt(mean(model2$residuals^2))
# 26516.03

### TRUE



# 9. Now, construct another model called model3 by adding the 
# interaction between sex and age to model2.

# What is the rmse for model3 (on train sample)?

model3 = lm(earn~height +
              sex + 
              race +
              ed + 
              age +
              sex*ed +
              sex*age,
            data=train)

# MSE // 702914753
mean(model3$residuals^2)
# RMSE // 26512.54
sqrt(mean(model3$residuals^2))

### 26512.54



# 10. Now, construct another model called model4 by adding the 
# interaction between age and ed to model3. 
# What is the rmse for model4 (on train sample)

model4 = lm(earn~height +
              sex + 
              race +
              ed + 
              age +
              sex*ed +
              sex*age +
              age*ed,
            data=train)

# MSE // 702684489
mean(model4$residuals^2)
# RMSE // 26508.2
sqrt(mean(model4$residuals^2))

### ANS: 26508.2



# 11. Finally, construct a model called model5 that considers all 
# possible pairwise interactions. 
# Adding all possible interactions can be tedious, so we will use a shortcut. Review the following code before running it.

model5 = lm(earn~(height+sex+race+ed+age)^2,data=train)

# What is the rmse for this model (on train sample)?

# MSE // 689651257
mean(model5$residuals^2)
# RMSE // 26261.21
sqrt(mean(model5$residuals^2))

### ANS: 26261.21



# 12. Which of the following variables from model1 are significant 
# (p<0.05) for model5? Can you think of why the statistical significance 
# of variables has changed? 

model5
summary(model5)

### None of the above



### Section 3: Decision Trees ###

# 1. Develop a regression tree model to predict earn.
# Call the rpart and rpart.plot libraries and then construct a 
# regression tree model to predict earn using all other variables. 
# Tree models can be run using the rpart function. 
# The syntax for rpart is very similar to that for lm. 
# In this case, we will not add method="class" argument because we are 
# running a regression tree and not a clasification tree. 
# Call the model tree1. Visualize the tree. 
# For visualizing the tree, add an argument, digits=5, so that you can 
# see numbers to 3 decimals like this:
  
#  prp(tree1,digits=5)

# Which is the first variable to be used for the split?

install.packages("rpart")
install.packages("rpart.plot")

# Call the rpart and rpart.plot libraries
library(rpart)
library(rpart.plot)

# all other variables
tree1 = rpart(earn~., data = train)
rpart.plot(tree1)

prp(tree1,digits=5)

### sex



# 2. Based on the tree plot, which of the following is true 
# about people who earn the most?

# Shorter than 60 inches
# Sex is Female 
# Taller than 70 inches
# Older than 40.5 years
# Education less than 17.5 years

### Taller than 70 inches



# 3. Based on the tree plot for tree1, which of the following is true 
# about people who earn the least? 

# Education greater than 17.5 years
# Race is Asian
# Age less than 27.5 years
# Sex is Male

### Age less than 27.5



# 4. How many leaves does tree1 have? 

# end nodes are leaves

### 12



# 5. Now, compute the rmse for tree1. What is the rmse (on train sample)?

predictions = predict(tree1, data = train)
mse_tree1 <- mean((train$earn - predictions)^2)
rmse_tree1 <- sqrt(mean((train$earn - predictions)^2))
rmse_tree1
# 24367.89



# 6. Now, let us change the defaults for the tree model by, 
# first reducing complexity and then adding complexity. 
# We use the minbucket parameter, one of many ways to change complexity 
# of the default tree. You can learn more about controlling 
# tree structure by checking help for rpart.control.

# First, construct a simpler tree called treeSimp1 by adding 
#the following argument within the rpart function: 
# control=rpart.control(minbucket=20)

treeSimp1 = rpart(earn~.,data=train,control=rpart.control(minbucket=20))

# How many leaves does treeSimp1 have? 

rpart.plot(treeSimp1)

### 9



# 7. What is the rmse for treeSimp1 (on train sample)? 

predictions_Simp1 = predict(treeSimp1, data = train)
mse_treeSimp1 <- mean((train$earn - predictions_Simp1)^2)
rmse_treeSimp1 <- sqrt(mean((train$earn - predictions_Simp1)^2))
rmse_treeSimp1
# 25466.95

### 25466.95



# 8. Construct an even simpler tree with minbucket of 50. 
# Call this model, treeSimp2

# How many leaves does treeSimp2 have? 

treeSimp2 = rpart(earn~.,data=train,control=rpart.control(minbucket=50))
rpart.plot(treeSimp2)

### 7



# 9. What is the rmse for treeSimp2 (on train sample)?

predictions_Simp2 = predict(treeSimp2, data = train)
mse_treeSimp2 <- mean((train$earn - predictions_Simp2)^2)
rmse_treeSimp2 <- sqrt(mean((train$earn - predictions_Simp2)^2))
rmse_treeSimp2
# 26328.55

### 26328.55



# 10. Now, let us construct some large bushy trees. 
# Construct a tree with minbucket of 5. Call it treeComplex1.

# What is the rmse (on train sample)?

treeComplex1 = rpart(earn~.,data=train,control=rpart.control(minbucket=5))
rpart.plot(treeComplex1)

predictions_Complex1 = predict(treeComplex1, data = train)
mse_treeComplex1 <- mean((train$earn - predictions_Complex1)^2)
rmse_treeComplex1 <- sqrt(mean((train$earn - predictions_Complex1)^2))
rmse_treeComplex1
# 24348.58

### 24348.58



# 11. Next, construct a maximal tree called treeComplex2. 
# For this tree, set minbucket to 1.

# What is the rmse (on train sample)? 

treeComplex2 = rpart(earn~.,data=train,control=rpart.control(minbucket=1))
rpart.plot(treeComplex2)

predictions_Complex2 = predict(treeComplex2, data = train)
rmse_treeComplex2 <- mean((train$earn - predictions_Complex2)^2)
rmse_treeComplex2 <- sqrt(mean((train$earn - predictions_Complex2)^2))
rmse_treeComplex2
# 23180.9

### 23180.9



# 12. Which of the following tree models has the lowest rmse?

rmse_tree1 # 24367.89
rmse_treeSimp1 # 25466.95
rmse_treeSimp2 # 26328.55
rmse_treeComplex1 # 24348.58
rmse_treeComplex2 # 23180.9



### Section 4: Decision Trees ###

# 1. For an unbiased measure of model performance, we assess performance 
# on the test sample.

# What is the test rmse for the linear regression model with 
# the lowest train rmse?

my.rmse <- function(actual, expected){
  the.rmse <- sqrt(mean((actual-expected)^2))
  return(the.rmse)
}

predictions_model5_test = predict(model5, newdata = test)
my.rmse(test$earn,predictions_model5_test)
# 27949.29



# 2. What is the test set rmse for the tree1?

predictions_test = predict(tree1, newdata = test)
mse_tree1_test <- mean((test$earn - predictions_test)^2)
rmse_tree1_test <- sqrt(mean((test$earn - predictions_test)^2))
rmse_tree1_test
# 29545.45

predictions_test = predict(tree1, newdata = test)
my.rmse(test$earn,predictions_test)
# 29545.45

### 29545.45



# 3. What is the test set rmse for treeSimp2?

predictions_Simp2_test = predict(treeSimp2, newdata = test)
my.rmse(test$earn, predictions_Simp2_test)
# 28238.25

mse_treeSimp2_test <- mean((test$earn - predictions_Simp2_test)^2)
rmse_treeSimp2_test <- sqrt(mean((test$earn - predictions_Simp2_test)^2))
rmse_treeSimp2_test
# 28238.25

### 28238.25



# 4. What is the test set rmse for treeComplex2?

predictions_Complex2_test = predict(treeComplex2, newdata = test)
mse_treeComplex2_test <- mean((test$earn - predictions_Complex2_test)^2)
rmse_treeComplex2_test <- sqrt(mean((test$earn - predictions_Complex2_test)^2))
rmse_treeComplex2_test
# 28888.88

my.rmse(test$earn,predictions_Complex2_test)
# 28888.88


### 28888.88



# 5. Which of the above four models performed best on the test sample?

# model5_test = 27949.29
# tree1_test = 29545.45
# Simp2_test = 28238.25
# Complex2_test = 28888.88

