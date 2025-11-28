# Practical 7

library(lme4)

angel$Killer_ID <- factor(angel$Killer_ID)
angel[1:7,]

# Exercise 1
# A killer's cooling off periods might be correlated which violates independence
# assumption of a normal linear regression model

# Exercise 2
# Yes the data is hierarchical as each victim was killed by one killer

# Exercise 3

model1 <- lmer(Cooling ~ Vic_Number + (1 | Killer), data = angel)

summary(model1)

randomeffects <- ranef(model1)
u0 <- randomeffects$Killer$"(Intercept)"
u0

beta0 <- model1@beta[1]
beta1 <- model1@beta[2]
beta0; beta1

# beta0 has no meaning here
# beta1 says after each additional kill the expected cooling off period 
# decreases by around 2.5 weeks

colours <- c("blue", "red3", "goldenrod4", "darkgreen","magenta3", "darkorchid4", "black")

plot(Cooling ~ Vic_Number, data = angel, col = colours[Killer_ID], pch = as.character(Killer_ID))

for(j in 1:7){ abline(a = beta0 + u0[j], b= beta1, col = colours[j])}

# Exercise 4

epsilon <- resid(model1)
par(mfrow = c(1,2))
qqnorm(u0)
qqnorm(epsilon)

# Drug trial data

trial$hospital <- factor(trial$hospital)
trial$drug <- factor(trial$drug)
trial[1:4,]

# Exercise 5

model2 <- lmer(QoL ~ age + drug + (1 + age | hospital), data = trial)

randomeffects <- ranef(model2)
u0 <- randomeffects$hospital$"(Intercept)"
u1 <- randomeffects$hospital$"age"

beta0 <- model2@beta[1]
beta1 <- model2@beta[2]
beta2 <- model2@beta[3]

colours <- c("blue", "red3", "goldenrod4", "darkgreen","magenta3", "darkorchid4")

par(mfrow = c(1,1))
plot(QoL ~ age, data = trial, col = colours[hospital], pch = as.character(hospital))
# Drug A:
for(j in 1:6){ abline(a = beta0 + u0[j], b = beta1 + u1[j], col = colours[j]) }
# Drug B
for(j in 1:6){ abline(a = beta0 + beta2 + u0[j], b = beta1 + u1[j], col = colours[j], lty = 2) }

# Exercise 6
model3 <- lmer(QoL ~ age + drug + (1 + drug | hospital), data = trial)

randomeffects <- ranef(model3)
u0 <- randomeffects$hospital$"(Intercept)"
u1 <- randomeffects$hospital$"drug"

beta0 <- model3@beta[1] # intercept
beta1 <- model3@beta[2] # age
beta2 <- model3@beta[3] # drug (random)

plot(QoL ~ age, data = trial, col = colours[hospital], pch = as.character(hospital))
# Drug A
for(j in 1:6) { abline(a = beta0 + u0[j], b = beta1, col = colours[j]) }
# Drug B
for(j in 1:6) { abline(a = beta0 + u0[j] + beta2 + u1[j], b = beta1, col = colours[j], lty = 2) }

# Many random effects

M3 <- lmer(QoL ~ age + drug + (1 + age + drug | hospital), data = trial)

randomeffects <- ranef(M3)

u0 <- randomeffects$hospital$"(Intercept)"
u1 <- randomeffects$hospital$"age"
u2 <- randomeffects$hospital$"drug"

beta0 <- M3@beta[1]
beta1 <- M3@beta[2]
beta2 <- M3@beta[3]

par(mfrow = c(1,1), oma = c(0,0,0,0), mar = c(5,5,1,1))
plot(QoL ~ age, data = trial, col = colours[hospital], pch = as.character(hospital))

for(i in 1:6) { abline(a = beta0 + u0[i], b = beta1 + u1[i], col = colours[i]) }

for(i in 1:6) { abline(a = beta0 + beta2 + u2[i] + u0[i], b = beta1 + u1[i], col = colours[i], lty = 2) }
