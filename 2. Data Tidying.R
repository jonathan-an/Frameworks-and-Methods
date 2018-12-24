install.packages('ggplot2')
install.packages('dplyr')
install.packages('tidyr')

library(ggplot2)
library(dplyr)
library(tidyr)

View(diamonds)

# 1. What is the average carat size of a diamond?
diamonds %>%
  summarize(mean(carat))

# alternative way
mean(diamonds$carat)


# 2. What is the average carat size of an Ideal cut diamond? use dplyr
diamonds %>%
  filter(cut == "Ideal") %>%
  summarize(mean(carat))

# alternative way
diamonds_ideal <- diamonds %>%
  filter(cut == "Ideal")

mean(diamonds_ideal$carat)


# 3. Which cut of diamond has the largest variance in carat size? 
diamonds %>%
  group_by(cut) %>%
  summarize(variance_carat = var(carat)) %>%
  arrange(desc(variance_carat))

# check Fair
diamonds %>%
  filter(cut == "Fair") %>%
  summarize(var(carat))

# check Good
diamonds %>%
  filter(cut == "Good") %>%
  summarize(var(carat))

# check Very Good
diamonds %>%
  filter(cut == "Very Good") %>%
  summarize(var(carat))

# check Premium
diamonds %>%
  filter(cut == "Premium") %>%
  summarize(var(carat))

# check Ideal
diamonds %>%
  filter(cut == "Ideal") %>%
  summarize(var(carat))


# alternative method using ggplot
ggplot(data = diamonds) +
  geom_boxplot(mapping = aes(x=cut, y = carat))


# 4. Compare number of diamonds by cut but only for color "D". 
# Which cut has the greatest selection (i.e., highest count) in color D?
diamonds %>%
  filter(color == "D") %>%
  group_by(cut) %>%
  count()

# alternative way
diamonds_D <- diamonds %>%
  filter(color == "D")

ggplot(data = diamonds_D) +
  geom_bar(mapping = aes(x = cut))


# 5. What is the average (i.e., mean) price in Euros of diamonds larger than 1 carat? 
# Assume the conversion is $1 = Euro 0.85. 
diamonds %>%
  filter(carat > 1) %>%
  mutate(euro_price = price * .85) %>%
  summarize(mean(euro_price))
  
# alternative way
diamonds_greaterthan1 <- diamonds %>%
  filter(carat >1) %>%
  mutate(euro_price = price * .85)

mean(diamonds_greaterthan1$euro_price)


# 6. Construct a density curve of price. Now, add faceting based on cut. 
# Next, construct a similar density curve but instead of using faceting, add cut as a color aesthetic.
# Based on these plots, indicate your agreement with the following statement:

# Ideal cut diamonds tend to be more expensive than Fair cut diamonds

ggplot(data = diamonds, aes(x = price)) +
  geom_density() +
  facet_grid(cut~.)

ggplot(data = diamonds, aes(x = price, color = cut)) +
  geom_density()
  
### yellow bar () is highest at lower price values, so this statement is false

# alternative method
ggplot(data = diamonds) +
  geom_density(mapping = aes(x=price)) +
  facet_grid(cut~.)

ggplot(data = diamonds) +
  geom_density(mapping = aes(x=price, color=cut))


# 7. Construct a density curve of carat. Now, add faceting based on cut. 
# Next, construct a similar density curve but instead of using faceting, add cut as a color aesthetic.
# Based on these plots, indicate your agreement with the following statement:

# Ideal cut diamonds tend to be larger (i.e., higher carat) than Fair cut diamonds. 

ggplot(data = diamonds, aes(x = carat)) +
  geom_density() +
  facet_grid(cut~.)

ggplot(data = diamonds, aes(x = carat, color = cut)) +
  geom_density()

### at 1 carat, purple line (Fair) has a higher density than Ideal
### yellow line is highest at <1 carat, so the statement is False

# alternative method
ggplot(data = diamonds) +
  geom_density(mapping = aes(x=carat)) +
  facet_grid(~cut)

ggplot(data = diamonds) +
  geom_density(mapping = aes(x=carat, color=cut))


# 8. Construct a histogram for carat. 
# Here is the code to run. Modify it, if it helps answer the question.
# ggplot(data=diamonds,aes(x=carat))+
  # geom_histogram(binwidth = 0.01)+
  # coord_cartesian(xlim=c(0,2.5))+
  # scale_x_continuous(breaks=seq(0,2.5,0.1))

# The spikes in the density plot represent the popularity of the diamond at a certain carat size. 
# Which of the following represent peaks (local maxima) in the density plot.  

# Note, the numbers in the choices for this question are approximations and may have an error of +/- 0.02.
# So, for example if you notice a peak at 0.32, check 0.3 below. 

ggplot(data=diamonds,aes(x=carat)) +
  geom_histogram(binwidth = 0.01) +
  coord_cartesian(xlim=c(0,2.5)) +
  scale_x_continuous(breaks=seq(0,2.5,0.1))

# zoom in
# local maximas at 0.3, 0.4, 0.5
ggplot(data=diamonds,aes(x=carat)) +
  geom_histogram(binwidth = 0.01) +
  coord_cartesian(xlim=c(0,.5)) +
  scale_x_continuous(breaks=seq(0,.5,0.1))

# zoom in
# local maximas at 0.7, 0.9, 1
ggplot(data=diamonds,aes(x=carat)) +
  geom_histogram(binwidth = 0.01) +
  coord_cartesian(xlim=c(.5,1)) +
  scale_x_continuous(breaks=seq(.5,1,0.1))

# zoom in
# local maximas at 1.5, 2
ggplot(data=diamonds,aes(x=carat)) +
  geom_histogram(binwidth = 0.01) +
  coord_cartesian(xlim=c(1,2.5)) +
  scale_x_continuous(breaks=seq(1,2.5,0.1))


# 9. Let us simulate some data that compares ten models on three commonly used indices, rmse, sse, and r2. 
# Run the following code to simulate this data.

# library(tidyr)
# model = paste('model',1:10,sep = '')
# sse = runif(10,min = 4000,max = 10000)
# rmse = sqrt(sse)
# r2 = ((rmse - min(rmse))/(max(rmse)-min(rmse)))*0.9
# results = data.frame(model, sse, rmse, r2)
# results

# This data is in a wide format which limits the types of analysis that can be run and functions that can be applied. 
# Please select the option that will transform results into a meaningful tall format with model in column 1, metric in column 2 and value in column 3.
# The final dataset should have 30 rows and 3 columns.

library(tidyr)
model = paste('model',1:10,sep = '')
sse = runif(10,min = 4000,max = 10000)
rmse = sqrt(sse)
r2 = ((rmse - min(rmse))/(max(rmse)-min(rmse)))*0.9
results = data.frame(model, sse, rmse, r2)
results

# alternative method
results %>%
  gather("sse","rmse","r2", key = metric, value = value)

# produces 40 x 3 df
results %>%
  gather(key=metric, value = value)

### Works!
results %>% 
  gather(key=metric, value=value, 2:4)

# doesn't compute
results %>% 
  gather('model', 'metric', 'value')

# ordering is off
results %>% 
  gather(key=model, value=metric, 1:3)


# 10. We want to compute the average of variable x. 
# However, some of the values for x are 0, which is not possible. 
# So, we would like to compute the average without these 0 values. 
# Which of the following options will achieve the goal? 

# what is the mean with the zeros?
mean(diamonds$x)

# what is the mean without the zeros?
diamonds_nozero <- diamonds %>%
  filter(x >0)

mean(diamonds_nozero$x)

### how do you expand decimal places?
diamonds %>%
  filter(x>0) %>%
  summarize(mean(x))

#### Works!
# Assigns anything with 0 to NA first
# na.rm - strips out the NA values before the computation proceeds
library(ggplot2)
data(diamonds)
diamonds$x[diamonds$x==0] = NA
mean(diamonds$x, na.rm=T)

# computes the mean with the zero
# there's no NA in the dataset, so nothing to omit
library(ggplot2)
data(diamonds)
mean(diamonds$x, na.rm=T)

# computes the mean with the zero
# there's no NA in the dataset, so nothing to omit
library(ggplot2)
data(diamonds)
diamonds$x = na.omit(diamonds$x)
mean(diamonds$x)
