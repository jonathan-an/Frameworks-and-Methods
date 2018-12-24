# 1. Compute 12345678 multiplied by 87654321 (Copy-paste answer from R) 
12345678 * 87654321


# 2.  What is the square root of 111222333444555666. Do not round answer from R.
sqrt(111222333444555666)


# 3.  What is the log to base 10 of 111222333444555666. Do not round answer from R. 
log10(111222333444555666)
log(x = 111222333444555666, base = 10)


# 4.  How many IDs does the following code generate? paste('ID',seq(from=10,to=100,by=10)) 
paste('ID',seq(from=10,to=100,by=10))
str(paste('ID',seq(from=10,to=100,by=10)))


# 5. How many observations does the following code generate? rnorm(n=10243,mean = 10,sd = 5)
#I hope you don’t plan to count! Hint: You could either look at the function argument or run length() on the entire expression)
rnorm(n=10243,mean = 10,sd = 5)
length(rnorm(n=10243,mean = 10,sd = 5))
str(rnorm(n=10243,mean = 10,sd = 5))


# 6. What is the class for 5?
class(5)


# 7. What is the class for 'FIVE'?
class('FIVE')


# 8. What is the class for FALSE? 
class(FALSE)


# 9. What class will the following statement return? Please read question carefully. 45 == 56
45 == 56
class(45 == 56)


# 10. What class will the following statement return? Please read question carefully. as.numeric(F)
as.numeric(F)
class(as.numeric(F))


# 11. What data structure is list('dog',5,'sloth')? 
list('dog',5,'sloth')
str(list('dog',5,'sloth'))


# 12. What will the following code yield? c(10,20,30,40) > 15
c(10,20,30,40) > 15


# 13. What will the following code yield? c(10,20,30,40) > c(15,25,35,45)
c(10,20,30,40) > c(15,25,35,45)


# 14. What will the following code yield? c(10,20,30,40) > c(15,25)
# Note: This question illustrates recycling behavior in R
c(10,20,30,40) > c(15,25)


# 15. Consider the following code which describes smartwatch sales at an electronics website for a week. 
# The watch was discounted on the weekend. Run the code below to create the objects.

# day_of_week = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
# number_of_smartwatches_sold = c(10,20,15,20,30,80,60)
# price_per_smartwatch = c(200,200,200,200,200,150,180)
# df = data.frame(day_of_week,number_of_smartwatches_sold,price_per_smartwatch)

# What is the total number of smartwatches sold during the week?
# Hint: Use sum())

day_of_week = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
number_of_smartwatches_sold = c(10,20,15,20,30,80,60)
price_per_smartwatch = c(200,200,200,200,200,150,180)
df = data.frame(day_of_week,number_of_smartwatches_sold,price_per_smartwatch)

sum(number_of_smartwatches_sold)


# 16. Consider the following code which describes smartwatch sales at an electronics website for a week. 
# The watch was discounted on the weekend. Run the code below to create the objects.

# day_of_week = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
# number_of_smartwatches_sold = c(10,20,15,20,30,80,60)
# price_per_smartwatch = c(200,200,200,200,200,150,180)
# df = data.frame(day_of_week,number_of_smartwatches_sold,price_per_smartwatch)

# Which of the following will extract price on Sunday?

day_of_week = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
number_of_smartwatches_sold = c(10,20,15,20,30,80,60)
price_per_smartwatch = c(200,200,200,200,200,150,180)
df = data.frame(day_of_week,number_of_smartwatches_sold,price_per_smartwatch)

df[df$day_of_week=='Sunday','price_per_smartwatch']
df[df$day_of_week=='Sunday',3]
df$price_per_smartwatch[df$day_of_week=='Sunday']
# All of the above


# 17. Consider the following code which describes smartwatch sales at an electronics website for a week. 
# The watch was discounted on the weekend. Run the code below to create the objects.

# day_of_week = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
# number_of_smartwatches_sold = c(10,20,15,20,30,80,60)
# price_per_smartwatch = c(200,200,200,200,200,150,180)
# df = data.frame(day_of_week,number_of_smartwatches_sold,price_per_smartwatch)

# What is the total sales revenue from smartphones sold in this week? 
# Total sales revenue is the product of unit sales and price for all days of the week 
# Hint: multiply vectors and do sum()

day_of_week = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
number_of_smartwatches_sold = c(10,20,15,20,30,80,60)
price_per_smartwatch = c(200,200,200,200,200,150,180)
df = data.frame(day_of_week,number_of_smartwatches_sold,price_per_smartwatch)

total_sales_per_day <- number_of_smartwatches_sold * price_per_smartwatch
total_sales_per_day
sum(total_sales_per_day)

sum(number_of_smartwatches_sold * price_per_smartwatch)

# 18. Consider the following code which describes smartwatch sales at an electronics website for a week. 
# The watch was discounted on the weekend. Run the code below to create the objects.

# day_of_week = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
# number_of_smartwatches_sold = c(10,20,15,20,30,80,60)
# price_per_smartwatch = c(200,200,200,200,200,150,180)
# df = data.frame(day_of_week,number_of_smartwatches_sold,price_per_smartwatch)

# On how many days were the number of smartwatches sold greater than 25?
# Hint: It might be tempting to eyeball the data and answer the question but resist the temptation and see if you can answer it by writing R code. 
# Hint: use a greater than sign to see which unit sales are greater than 25. 
# This will result in a vector of logicals (i.e, TRUE/FALSE). 
# Then apply sum() to these logicals. The sum() function will coerce logicals into numeric and then add them up to give you the answer.

#check all values of the vector to identify where number_of_smartwatches_sold > 25
check_over_25 <- number_of_smartwatches_sold > 25
sum(check_over_25)
sum(number_of_smartwatches_sold > 25)

# 19. Consider the following code which describes smartwatch sales at an electronics website for a week. 
# The watch was discounted on the weekend. Run the code below to create the objects.
# day_of_week = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
# number_of_smartwatches_sold = c(10,20,15,20,30,80,60)
# price_per_smartwatch = c(200,200,200,200,200,150,180)
# df = data.frame(day_of_week,number_of_smartwatches_sold,price_per_smartwatch)

# What will the following code return? price_per_smartwatch[c(6,7)]

day_of_week = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
number_of_smartwatches_sold = c(10,20,15,20,30,80,60)
price_per_smartwatch = c(200,200,200,200,200,150,180)
df = data.frame(day_of_week,number_of_smartwatches_sold,price_per_smartwatch)

# obtain the 6th and 7th value of this vector
price_per_smartwatch[c(6,7)]
price_per_smartwatch

# 20. Consider the following code which describes smartwatch sales at an electronics website for a week. 
# The watch was discounted on the weekend. Run the code below to create the objects.
# day_of_week = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
# number_of_smartwatches_sold = c(10,20,15,20,30,80,60)
# price_per_smartwatch = c(200,200,200,200,200,150,180)
# df = data.frame(day_of_week,number_of_smartwatches_sold,price_per_smartwatch)

# Which of the following will return the days when sales were greater than average?

day_of_week = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
number_of_smartwatches_sold = c(10,20,15,20,30,80,60)
price_per_smartwatch = c(200,200,200,200,200,150,180)
df = data.frame(day_of_week,number_of_smartwatches_sold,price_per_smartwatch)

df$day_of_week[df$number_of_smartwatches_sold>mean(df$number_of_smartwatches_sold)]
df[df$number_of_smartwatches_sold>mean(df$number_of_smartwatches_sold),"day_of_week"]
df[df$number_of_smartwatches_sold>mean(df$number_of_smartwatches_sold),1]

mean(number_of_smartwatches_sold)
#all of the above
