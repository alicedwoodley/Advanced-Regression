# AR practical 6

galaxy[1:10,]

# example 1 - velocity disp and black hole mass

modelVB <- lm(Vdisp ~ BlackHole, data = galaxy)
summary(modelVB)

par(mfrow = c(1,2))
betahat <- modelVB$coefficients
epsilon <- modelVB$residuals
plot(Vdisp ~ BlackHole, data = galaxy) # scatter plot
abline(betahat, col = "blue") # add regression line

hist(epsilon, # histogram of residuals
     main = expression("Histogram of"~hat(epsilon)[i]), 
     xlab = expression(hat(epsilon)[i]))

# exercise 1
# scatter plot does support linearity assumption, less sure about constant variance
# histogram could support normality assumption but seems skewed - would check further

par(mfrow = c(1,2))
plot(epsilon ~ galaxy$BlackHole)
abline(h = 0, col = "blue")
qqnorm(epsilon)

# exercise 2
# q-q plot suggests normality
# the first plot suggests linearity and homoscedasticity

# also useful to plot residuals against fitted values (model's predictions for each Yi)
# hope to see no patterns!

par(mfrow = c(1,1))
muhat <- predict(modelVB)
plot(epsilon ~ muhat)
abline(h = 0, col = "blue")

# can create all those plots at the same time!
par(mfrow = c(2,2))
plot(modelVB)
# plot 1 is residuals v fitted values
# plot 2 is the Q-Q residuals plot
# the others you'll do in semester 2

# example 2
modelVT <- lm(Vdisp ~ Type, data = galaxy)
summary(modelVT) # lm has recognised that type is catagorical and made dummy variables normality

# exercise 3
par(mfrow = c(2,2))
plot(modelVT)
# residuals are normal but residuals v fitted looks odd (due to catagorical type)
# points spread out more as the fitted value increases so there could be a relationship between type and the variance
# this would violate our assumption of constant variance
# there are few elliptical and lenticular galaxies so we may continue with the analysis
# as long as we recognise that possible heteroscedacity casts some doubt on results

# angels of death (nurses and doctors who murdered their patients)
angel

# is there a relationship between length of cooling off period and sex of killer?

modelCS <- lm(Cooling ~ Killer_Sex, data = angel)
par(mfrow = c(2,2))
plot(modelCS)
