##########
# Quiz Week 3
##########

# Author: Avery Sandborn
# Date: January 12, 2016

##########

library(datasets)
data(iris)
?iris

# mean of Sepal.Length for Species virginica
my_subset <- subset(iris, Species == "virginica")
my_mean <- mean(my_subset$Sepal.Length)
round(my_mean)

# vector of means of the 4 variables
apply(iris[,1:4],2,mean)

library(datasets)
data(mtcars)
?mtcars

# calculate average miles per gallon by number of cylinders in car
sapply(split(mtcars$mpg, mtcars$cyl), mean)
tapply(mtcars$mpg, mtcars$cyl, mean)
with(mtcars, tapply(mpg, cyl, mean))

# absolute difference between average horsepower of 4 cylinder and average horsepower of 8 cylinder

hp4 <- subset(mtcars, cyl == 4)
hp8 <- subset(mtcars, cyl == 8)
mhp4 <- mean(hp4$hp)
mhp8 <- mean(hp8$hp)
answer <- abs(mhp4 - mhp8)
round(answer)

# Execution of 'ls' will suspend at the beginning of the function and you will be in the browser

