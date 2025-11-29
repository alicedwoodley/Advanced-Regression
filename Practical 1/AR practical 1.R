### AR Practical 1

load("Module_Data.Rdata")
DataList

# Exploring a data set
galaxy[1:10,]
sort(galaxy$Clusters) # shows numbers of clusters in order from smallest to largest
galaxy$Clusters > 10000 # checks which observations meet this criteria
sum (galaxy$Clusters > 10000) # counts how many meet the criteria
which(galaxy$Clusters > 10000) # which observations meet the criteria
galaxy[galaxy$Clusters > 10000, ] # shows the whole rows of each observation meeting criteria
galaxy[galaxy$Clusters > 10000, c("Name", "Type")] # shows only name and type

# Loops
for(i in 2:6){
 print(sum(galaxy[ ,i])/nrow(galaxy)) # prints average of columns 2 to 6
}
for(i in 2:6){
  m <- sum(galaxy[,i])/nrow(galaxy) # calculates average
  if(m < 1000){
    print(m) # prints average only if less than 1000
  }
}

# Exercise 1
for(i in 1:nrow(galaxy)){ # for each observation
  if(galaxy$BlackHole[i] > 9){ # if mass of black hole is greater than 9
    print(galaxy[i, "Distance"]/1000) # print the distance of the galaxy to the earth divided by 1000
  }
}

# can this be achieved in a single line of code?
galaxy[galaxy$BlackHole > 9, "Distance"]/1000

# Bivariate data and correlation
# Example 1
x1 <- c(3, 3.5, 4, 5, 6, 7, 10, 12, 19.5, 30)
x2 <- c(11, 11, 17, 13, 14, 24, 16, 3, 3, 0)
X <- cbind(x1, x2) # combine into one matrix
par(mar = c(4,4,1,1)) # sets margin for future plots
plot(X) # makes scatter plot

# Exercise 2
# A: slightly negative correlation but not strong at all, -0.1
# B: very strong positive correlation, 0.9
# C: correlation near zero but very very clear relationship
# D: strongish negative correlation, -0.6
# E: strongish positive correlation, 0.6
# F: no correlation but potentially some other relationship
# G: no correlation but some other nonlinear relationship
# H: strong relationship but likely correlation of 0
# I: no correlation but nonlinear relationship, looks sineish

# Exercise 3
pairs(galaxy[,2:6])
# 1 0.6 0.4 0.6 0.5 
# 0.4 1 0.4 0.8 0.7
# 0.2 0.3 1 0.6 0.6
# 0.4 0.9 0.4 1 0.7
# 0.6 0.7 0.5 0.7 1
cor(galaxy[,2:6])

# Checking normality
# Q-Q plots
# Estimated parameters
mu <- mean(galaxy$Vdisp)
sigma <- sd(galaxy$Vdisp)
# Order statistics
sort(galaxy$Vdisp)
# Standardised order statistics
z <- (sort(galaxy$Vdisp) - mu)/sigma
# Vector of values of r
n <- length(galaxy$Vdisp)
r <- 1:n
# Quantiles of N(0.1)
q <- qnorm(r/(n+1))
# Normal Q-Q plot 
plot(q,z)
# Line with intercept 0 and slope 1
abline(a=0, b=1, col = "red")

# Quickest way
qqnorm(z)
abline(a=0, b=1, col = "red")
qqnorm(galaxy$Vdisp)

# Exercise 4
par(mfrow = c(3,2))
curve(dnorm, from = -3, to = 3, main = "true density function")
# Sampling from a standard normal N(0,1)
x <- rnorm(mean = 0, sd = 1, n = 200)
qqnorm(x, main = "Normal Q-Q plot when n = 200")

x <- rnorm(mean = 0, sd = 1, n = 30)
qqnorm(x, main = "Normal Q-Q plot when n = 30")

x <- rnorm(mean = 0, sd = 1, n = 30)
qqnorm(x, main = "Normal Q-Q plot when n = 30 again")

x <- rnorm(mean = 0, sd = 1, n = 10)
qqnorm(x, main = "Normal Q-Q plot when n = 10")

x <- rnorm(mean = 0, sd = 1, n = 10)
qqnorm(x, main = "Normal Q-Q plot when n = 10 again")

# Sampling from a definitely not normal distribution (exponential)
par(mfrow = c(2,2))

curve(dexp, from = 0, to = -10, main = "True density")

x <- rexp(n = 200)
qqnorm(x, main = "Normal Q-Q plot when n = 200")

x <- rexp(n = 30)
qqnorm(x, main = "Normal Q-Q plot when n = 30")

x <- rexp(n = 10)
qqnorm(x, main = "Normal Q-Q plot when n = 10")

# Sampling from a uniform distribution
par(mfrow = c(2,2))

curve(dunif, from = 0, to = 1, main = "True Density Function")

x <- runif(n = 200)
qqnorm(x)

x <- runif(n = 30)
qqnorm(x)

x <- runif(n = 10)
qqnorm(x)

# Q-Q plot less clear for small sample size

par(mfrow = c(1,1))

# Histograms
hist(galaxy$Vdisp, breaks = seq(from = 0, to = 480, by = 40))

# Exercise 5 
# I don't think velocity dispersion is normally distributed from this histogram
# One of the central bars is too low for it to be normal - gives it two peaks

# Exercise 6
hist(galaxy$Vdisp, breaks = seq(from = 0, to = 480, by = 80))
# This looks more normal and would probably change my opinion
# Choice of intervals impacts the histogram and therefore our decision on whether we see normality

# Exercise 7
# Continuous variables are BlackHole, Distance, Vdisp, Eradius
# Vdisp
hist(galaxy$Vdisp)
qqnorm(galaxy$Vdisp)
# Q-Q plot and histogram appear consistent with normality

# Distance
hist(galaxy$Distance) # Skewed to either end
qqnorm(galaxy$Distance) # Not a straight line
# Neither plot is consistent with normality

# BlackHole
hist(galaxy$BlackHole) # A little skewed
qqnorm(galaxy$BlackHole) # Very close to straight line
# Relatively normal

# Eradius
hist(galaxy$Eradius)
qqnorm(galaxy$Eradius)
# Histogram too skewed