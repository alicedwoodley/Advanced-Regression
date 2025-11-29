### AR practical 3

# T squared test
# school example

X <- matrix(c(6,9,10,6,8,3), nrow = 3, byrow = T)
Xbar <- colMeans(X) # sample mean vector
S <- var(X) # sample covariance matrix
Sinv <- solve(S) # inverse
n <- nrow(X) # sample size
mu0 <- c(9,5) # for null hypothesis

# make sure sample mean and mu0 are treated as column vectors
Xbar <- matrix(Xbar, ncol = 1) 
mu0 <- matrix(mu0, ncol = 1)

# calculate test statistic
T2 <- n*t(Xbar - mu0) %*% Sinv %*% (Xbar - mu0)

# test at 10% significance level
p <- ncol(X)
((n-1)*p/(n-p))*qf(0.9, p, n-p)
# T2 < 198 so fail to reject H0

# can calculate probability that T2 exceeds observed value - p value
p <- ncol(X)
n <- nrow(X)

F <- (n-p)*T2/((n-1)*p)
pvalue <- 1 - pf(F,2,1)

# pvalue is large (close to 1) suggesting evidence against H0 is weak
# can only reject H0 at significance levels of 84% or less

# MANOVA

od <- oreodonts # fossil measurements from 7 species of oreodonts
names(od) <- c("BrainCase", "CheekTooth", "BullaLength", "BullaDepth", "species")
od[1:5,] # look at top 5 rows

od$species <- factor(od$species) # species is stored numerically so tell R to treat it as a factor
od$species

# some plots
colours <- c("black", "red", "goldenrod4", "green", "magenta3", "blue", "orange")
pairs(od[,1:4], 
      col = colours[od$species],
      pch = 16,
      oma = c(3,3,3,11)) # sets margins so legend fits
legend("topright",
       legend = 1:7,
       col = colours,
       pch = 16,
       xpd = T, # legend goes outside plotting area
       title = "Species:")

# sample means for each species
X <- od[,1:4]
means <- aggregate(x = X, by = list(od$species), FUN = mean)

# standard deviation
SDs <- aggregate(x = X, by = list(od[,5]), FUN = sd)

# round and create tables
meantable <- round(as.matrix(means[,2:5]))
SDtable <- round(as.matrix(SDs[,2:5]))

meantable[] <- paste(meantable, "(", SDtable, ")", sep = "")                 
noquote(meantable)

# differences in sample means seem large relative to standard deviations so there are notable differences

# applying MANOVA
X <- as.matrix(od[,1:4]) # measurement data
s <- od[,5] # species data
m <- manova(X ~ s) # apply manova
summary(m, test = "Wilks") # see results

# exercise 1
# pvalue is super super close to 0 so evidence against H0 is strong
# would very likely reject H0

# exercise 2
# a - using moven data, test if bivariate normal
X <- moven # x1 is closed, x2 is open
X[1:3,]

plot(X) # looks roughly elliptical

qqnorm(X[,1])
qqnorm(X[,2])

library('heplots')
cqplot(X, ylim = c(0,6), xlim = c(0,9))

# all roughly straight so good evidence of bivariate normality

# b - t test at 5% significance level
Xbar <- colMeans(X)
S <- var(X)
Sinv <- solve(S)
n <- nrow(X)
mu0 <- c(0.562,0.589)

Xbar <- matrix(Xbar, ncol = 1) 
mu0 <- matrix(mu0, ncol = 1)

T2 <- n*t(Xbar - mu0) %*% Sinv %*% (Xbar - mu0)

dim(X) # 2 variables 42 observations

qf(0.95, 2, 40) # 5% significance level

# 2.05 x 3.23 = 6.6215 > T2 so fail to reject H0

# exercise 3
bl <- bulls
bl[1:5,]
# compare t = 3 breeds by applying MANOVA to p = 8 variables
bl$breed <- as.factor(bl$breed)

X <- as.matrix(bl[,2:9]) # measurement data
s <- bl[,1] # breed data
m <- manova(X ~ s) # apply manova
summary(m, test = "Wilks")
# very small pvalue so strong evidence for differences between mean vectors 

# exercise 4
paint <- painters
paint$School <- as.factor(paint$School)
pairs(paint[,2:5], col = paint$School, pch = 16)
paint[1:5,]

# no clear clustering of points by painter
X <- as.matrix(paint[,2:5]) # measurement data
s <- paint[,6] # school data
m <- manova(X ~ s) # apply manova
summary(m)
# pvalue super close to 0 so strong evidence for group differences