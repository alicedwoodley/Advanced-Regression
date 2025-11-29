# Advanced regression practical 2

# Simulation
set.seed(123)
x1 <- rnorm(n = 100, mean = 2, sd = 3)
x2 <- rnorm(n = 100, mean = 8, sd = 1)
# expect x1 + x2 ~ N(10,10)
mean(x1 + x2) 
var(x1 + x2)
# estimates close to parameter values we expected but not exactly right
# increase sample size
set.seed(111)
x1 <- rnorm(n = 10000, mean = 2, sd = 3)
x2 <- rnorm(n = 10000, mean = 8, sd = 1)
mean(x1 + x2) 
var(x1 + x2)
# more convincing
# check x1 + x2 is normal
par(mfrow = c(1,2))
qqnorm(x1 + x2)
hist(x1 + x2, breaks = 'FD', freq = F)
curve(dnorm(x, mean = 10, sd = sqrt(10)), col = 'red', lwd = 2, add = TRUE)

# Exercise 1
# x1 + x2 is very clearly normal from both the histogram and the q-q plot

# Multivariate simulation
mu <- c(0,0)
Sigma <- c(5, 4, 4, 5)
dim(Sigma) <- c(2,2) # Exercise 2 : turns vector into 2x2 matrix
mu
Sigma

set.seed(456)
library(MASS)
Xsim <- mvrnorm(n = 1, mu = mu, Sigma = Sigma)
Xsim # sample of a single observation
Xsim <- mvrnorm(n = 1000, mu = mu, Sigma = Sigma)
View(Xsim)

# Exercise 3
# each row is an observation and each column is each variable

# Exercise 4
# checking each component in X
par(mfrow = c(2,2))
qqnorm(Xsim[,1])
hist(Xsim[,1], breaks = 'FD', freq = F)
curve(dnorm(x, mean = 0, sd = sqrt(5)), col = 'red', lwd = 2, add = TRUE)
qqnorm(Xsim[,2])
hist(Xsim[,2], breaks = 'FD', freq = F)
curve(dnorm(x, mean = 0, sd = sqrt(5)), col = 'red', lwd = 2, add = TRUE)
# both look univariate normal

# Mahalanobis distance
D <- function(X, mu, Sigma){ # writing a function to calculate mahalanobis distance
  sqrt( t(X - mu) %*% solve(Sigma) %*% (X - mu))
}

# Exercise 5
# t performs the transpose
# solve finds the inverse of Sigma
# %*% multiplies matrices

D(X = Xsim[1,], mu = mu, Sigma = Sigma)

n <- nrow(Xsim) # sample size
distances <- rep(NA, n) # empty vector to store distances in
for(i in 1:n){ # calculate distance for each sample
  distances[i] <- D(X = Xsim[i,], mu = mu, Sigma = Sigma)
}

p = ncol(Xsim)
q = qchisq(0.95, df = p) # 95% quantile of chi-squared dist
s <- ifelse(distances^2 < q, 1, 0) # check how many samples are inside 95% interval
sum(s)/n # sum up and divide by n - gives proportion of data inside the 95% interval

# Exercise 6
# implies mahalanobis distances follow chi-squared distribution

par(mar = c(4,4,1,1))
q = qchisq(0.5, df = p)
colour <- ifelse(distances^2 < q, 'blue', 'red')
plot(Xsim, col = colour, pch = 16, cex = 0.6)
sum(colour == 'blue')/n

# Exercise 7
# theorem 2.6

# Understanding chi squared Q-Q plots
par(mfrow = c(1,1))
hist(distances^2,
     freq = F,
     xlab = expression(d[i]^2),
     breaks = 'FD')
p = ncol(Xsim)
curve(dchisq(x, df = p), add = T, col = 'blue', lwd = 2)

# Exercise 8
# overlays chi squared density function
# if our sample comes from a normal distribution, we expect squared distances to have a shape similar to blue line

dsquared <- sort(distances^2)
n <- nrow(Xsim)
quantiles <- qchisq((1:n)/(n+1), df = p)
plot(quantiles, dsquared, main = 'Q-Q plot')

# Exercise 9
library(heplots)
cqplot(Xsim)
# looks slightly different but won't affect conclusions we make