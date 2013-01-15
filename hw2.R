# Question 1
cube <- function(x, n) {
  x^3
}

# Question 2
pow <- function(x = 4, n = 3) {
  x^n
}

# Question 3
x <- 1:10
if(x > 5) {
  x <- 0
}

# Question 4
library(datasets)
data(iris)
s4 <- split(iris, iris$Species)
q4 <- lapply(s4$virginica[,1:4],mean)

# Question 5
q5 <- apply(iris[,1:4],2,mean)

# Question 6
data(mtcars)
q6 <- tapply(mtcars$mpg, mtcars$cyl, mean)

# Question 7
avgHP <- tapply(mtcars$hp, mtcars$cyl, mean)
q7 <- avgHP[3] - avgHP[1]

# Question 9
f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}





