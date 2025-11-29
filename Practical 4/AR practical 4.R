# Practical 4

# PCA by covariance

ft <- fturtle
ft[1:6,]

s <- var(ft) # sample covariance matrix
eigs <- eigen(s) # find eigenvalues and vectors
eigs

# look at components of eigenvectors to find principal component loadings
# Y1 appears to reflect overall size
# Y2 appears to measure shape or roundness
# Y3 appears to measure how pointy the shell is
# so overall the turtles vary most in size, next most in shape and least in peakedness

# much easier way is to use:
pca <- princomp(ft)
summary(pca, loadings = T)

# comp 1 accounts for 98% of variance

# scree plot compares variances
plot(pca)

# PCA by correlations
# common to perform PCA using correlation matrix R
# equivalent to standardising all X variables to have unit variance
# because the diagonal is all 1 then the trace is p (no of variables)

pcacor <- princomp(ft, cor = T)
summary(pcacor, loadings = T)

# interpret the results similarly but may lead to different conclusions
# e.g. large Y2 means turtle is wide and short

# Exercise 1
an <- anthrop # body measurements of 30 secondary school kids
head(an)

pcacor_an <- princomp(an, cor = T)
summary(pcacor_an, loadings = T)
plot(pcacor_an, main = "Scree plot from correlations for body measurements", type = "line")

# I would choose either one or two components as the elbow appears to be at component 2
# comp 1: general/overall size
# comp 2: measure of how tall and skinny you are? body shape

# exercise 2

head(stock)
st <- stock
pcacor_st <- princomp(st, cor = T)
summary(pcacor_st, loadings = T)
plot(pcacor_st, main = "Scree plot from correlations for stock data", type = 'line')

# I would choose either one or two as comp 2 still accounts for a a decent amount of the variance compared to the others
# not looking at the scree plot i would say 3 or 4 as comps 3 or 4 still account for 10% of the variance

# comp 1: measure of overall return (overall market)
# comp 2: measure of mainly chemical return (varies depending on industry)

# Exercise 3

pp <- peper
head(pp)

pca_pp <- princomp(pp[,3:6])
summary(pca_pp, loadings = T)

# component i is dominated by the variable with the ith largest variance
# radio has a super small variance

pcacor_pp <- princomp(pp[,3:6], cor = T)
summary(pcacor_pp, loadings = T)

# comp 1: measure of 
# comp 2: measure of car and radio minus tv

# main thing to remember what your variables actually are (silly me did not read the question well enough)