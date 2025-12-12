### Practical 9

library(MASS)

# Kernels

K_epan <- function(u){ # Epanechnikov kernel
  0.75 * (1 - u^2) * (abs(u) < 1) # returns true / false which becomes 1 or 0
}

ugrid <- seq(-3, 3, length.out = 11) # grid of 11 equally spaced points from -3 to 3
plot(ugrid, K_epan(ugrid), type = "l", col = "blue")
for(u in ugrid){abline(v = ugrid, lty = 2, col = "gray")}

# can smooth the curve by using more points
ugrid <- seq(-3, 3, length.out = 1001)
plot(ugrid, K_epan(ugrid), type = "l", col = "blue")

# Exercise 1

K_uni <- function(u){ # uniform kernel
  0.5 * (abs(u) < 1)
}

plot(ugrid, K_uni(ugrid), type = "l", col = "green")

# Local constant regression

library(MASS)
data(mcycle) # load motorcycle data (built into R)

xdata <- mcycle$times # covariate observations xi
ydata <- mcycle$accel # response observations Yi

par(mar = c(5, 5, 1, 1))
plot(xdata, ydata, ylab = "acceleration (g)", xlab = "time (ms)")

xgrid <- seq(min(xdata), max(xdata), length.out = 1001)
# remember we chose xgrid they are not observed data

# using the uniform kernel

mhat <- function(x, xdata, ydata, h){ # evaluates mhat at any combo of x and h
  sum(K_uni((xdata - x)/h)*ydata)/sum(K_uni((xdata -x)/h))
}

m <- c() # empty vector to store estimated values of m(x)

for(x in xgrid){
  m <- c(m, mhat(x, xdata, ydata, h = 1)) # bandwidth 1
}

par(mar = c(5, 5, 1, 1))
plot(xdata, ydata, ylab = "acceleration (g)", xlab = "time (ms)")
lines(xgrid, m, col = "blue") # uniform kernel does not give a smooth curve

# Exercise 2

m <- c()

for(x in xgrid){
  m <- c(m, mhat(x, xdata, ydata, h = 2)) # experiment with bandwidth
}

par(mar = c(5, 5, 1, 1))
plot(xdata, ydata, ylab = "acceleration (g)", xlab = "time (ms)")
lines(xgrid, m, col = "blue")

# fit gets worse and worse
# very large h would lead to a straight line?
# h = 50 almost a straight line
# h = 2 seems pretty good

# Local linear regression

x <- 10

# need to create W and X 
W <- diag(K_uni((xdata - x)/h))
X <- cbind(rep(1, length(xdata)), xdata)

mhat <- function(x, xdata, ydata, h){
  W <- diag(K_uni((xdata - x)/h))
  X <- cbind(rep(1, length(xdata)), xdata)
  betahat <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% ydata
  return(betahat[1] + betahat[2]*x)
}

xgrid <- seq(min(xdata), max(xdata), length.out = 1001)
m <- c()
for(x in xgrid){
  m <- c(m, mhat(x, xdata, ydata, h = 50))
}

par(mar = c(5, 5, 1, 1))
plot(xdata, ydata, ylab = "acceleration (g)", xlab = "time (ms)")
lines(xgrid, m, col = "blue")

# Exercise 3

# I think the line will tend to the diagonal as h is large?
# h = 50 line is almost completely diagonal

# Bandwidths

K <- function(u){ dnorm(u) }

mhat <- function(x, xdata, ydata, h){
  W <- diag(K((xdata - x)/h))
  X <- cbind(rep(1, length(xdata)), xdata)
  betahat <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% ydata
  return(betahat[1] + betahat[2]*x)
}

xgrid <- seq(min(xdata), max(xdata), length.out = 1001)
m <- c()
for(x in xgrid){
  m <- c(m, mhat(x, xdata, ydata, h = 10))
}
par(mar = c(5, 5, 1, 1))
plot(xdata, ydata, ylab = "acceleration (g)", xlab = "time (ms)")
lines(xgrid, m, col = "blue")

# Exercise 4 and 5

hrandom <- runif(n = 1, 1, 30)

# run local linear regression with this bandwidth
xgrid <- seq(min(xdata), max(xdata), length.out = 1001)
m <- c()
for(x in xgrid){
  m <- c(m, mhat(x, xdata, ydata, h = 2))
}
par(mar = c(5, 5, 1, 1))
plot(xdata, ydata, ylab = "acceleration (g)", xlab = "time (ms)")
lines(xgrid, m, col = "blue")

# guess 1: h = 11
# answer h = 12.3

# guess 2: h = 25
# answer: h = 25.6

# h = 2 or 3 looks good to me

# Cross-validation

omega <- 1 # weights = 1 for simplicity

CV <- function(xdata, ydata, h){
  n <- length(ydata) # sample size
  estimates <- c()   # empty vector
  for(i in 1:n){
    estimates[i] <- mhat(xdata[i], xdata[-i], ydata[-i], h)
  }
  CV <- (1/n)*sum((ydata - estimates)^2)*omega
  return(CV)
}

# Check a few values of h
CV(xdata, ydata, h = 0.5)
CV(xdata, ydata, h = 2)
CV(xdata, ydata, h = 10)

hgrid <- seq(from = 0.5, to = 10, by = 0.1)
CV_values <- c()
for(j in 1:length(hgrid)){
  CV_values[j] <- CV(xdata, ydata, h = hgrid[j])
}
plot(CV_values ~ hgrid, type = "l")

# Exercise 6
# slightly less than 2, perhaps 1.5

# Local polynomial regression

data(mcycle)

xdata <- mcycle$times # explanatory variable
ydata <- mcycle$accel # response variable

par(mar = c(5, 5, 1, 1))

plot(xdata, ydata, ylab = "acceleration (g)", xlab = "time (ms)")

# apply local polynomial regression with degree 2
m <- loess(ydata ~ xdata, degree = 2, span = 0.5)
lines(xdata, m$fitted, col = "blue")

# Exercise 7
plot(xdata, ydata, ylab = "acceleration (g)", xlab = "time (ms)")
m <- loess(ydata ~ xdata, degree = 2, span = 0.1)
lines(xdata, m$fitted, col = "blue")

