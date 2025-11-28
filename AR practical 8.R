library("lme4")

### Practical 8

### Three level models

water[1:4,]
# abstraction = daily observations of amount of water taken from hand pumps

water$station <- factor(water$station)
water$pump <- factor(water$pump)

table(water$station, water$pump)
# 3 rainfall stations and 12 pumps with 189 observations at each
# observations grouped by pump
# pumps grouped by location of rainfall station

# Exercise 1
# structure is hierarchical: rainfall station -> pumps -> observations
# 2 observations from the same pump are likely to be more similar than 
# observations at different pumps
# different pumps in the same location likely to be more similar than pumps 
# in different locations

colours <- rep(c("black", "blue", "red3", "magenta3"),3)
par(mfrow = c(1,3))
for(location in c("Makueni", "Kapiti", "Mtito")){
  local <- water[water$station == location,]
  plot(abstraction ~ rain, 
       data = local, 
       col = colours[pump], 
       main = location, 
       xlim = c(0,20), 
       ylim = c(0,8000))
}

# Exercise 2
model1 <- lmer(abstraction ~ rain + station + (1 | pump), data = water, REML = FALSE)
summary(model1)

# Three level random intercept model:

M3 <- lmer(abstraction ~ rain + (1 | station) + (1 | pump), data = water)
summary(M3)

# Exercise 3
totalvar <- 795556 + 936142 + 455883
ICC1 <- (795556 + 936142)/totalvar
ICC2 <- 795556/totalvar
ICC1; ICC2

# Three level random slope model:
M5 <- lmer(abstraction ~ rain + (1 | station) + (1 + rain | pump), data = water)
summary(M5)

# Exercise 4
# No this is not an ICC as it is negative

### Repeated measures models
# longitudinal data - we are taking repeated measures of same variable over time

Makueni <- water[water$station == "Makueni",]

# growth curve model
RM1 <- lmer(abstraction ~ days + (1 + days | pump), data = Makueni)
RM1
# returns a warning message
# try and simplify by assuming sigmau01 = 0
RM2 <- lmer(abstraction ~ days + (1 | pump) + (0 + days | pump), data = Makueni)
# still returns a warning
# huge difference between estimated variances for u0 and u1
# try rescaling time variable

Makueni$months <- Makueni$days/30
RM3 <- lmer(abstraction ~ months + (1 | pump) + (0 + months | pump), data = Makueni)
summary(RM3)

# error message relating to convergence means experiment with rescaling, 
# centering or standardising continuous variables

# rescaling time has solved all issues so this runs with no issues
RM4 <- lmer(abstraction ~ months + (1 + months | pump), data = Makueni)

randomeffects <- ranef(RM4)
u0 <- randomeffects$pump$"(Intercept)"
u1 <- randomeffects$pump$"months"

beta0 <- RM4@beta[1]
beta1 <- RM4@beta[2]

colours <- c("black", "blue", "red3", "magenta3")
par(mfrow = c(1,1))
plot(abstraction ~ months,  data = Makueni, 
     col = colours[pump], 
     main = "Makueni", 
     ylab = "abstraction", xlab = "months")

for(i in 1:4){
  abline(a = beta0 + u0[i], b = beta1 + u1[i], col = colours[i])
}

legend(title = "pump", x = "topright", col = colours, legend = 1:4, lwd = 2)
