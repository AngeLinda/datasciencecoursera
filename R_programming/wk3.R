library(datasets)
data(iris)
?iris

# calculate the mean of "Sepal.Length" for the species virginica
sapply(split(iris$Sepal.Length,iris$Species), mean)

# return a vector of the means of variables
apply(iris[, 1:4], 2, mean)
colMeans(iris[, 1:4])

library(datasets)
data(mtcars)
?mtcars

# calculate the average miles per gallon (mpg) by number of cylinders in the car 
with(mtcars, tapply(mpg, cyl, mean))
tapply(mtcars$mpg, mtcars$cyl, mean)
sapply(split(mtcars$mpg, mtcars$cyl), mean)

# calculate the absolute difference between the average horsepower of 4-cylinder cars and 
# the average horsepower of 8-cylinder cars
mean_hp <- with(mtcars, tapply(hp, cyl, mean))
diff_mean_hp <- abs(mean_hp[1] - mean_hp[3])

