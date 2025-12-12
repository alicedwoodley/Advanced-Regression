# Practical 10

load(file = "Module_Data.Rdata")
head(galaxy)
galaxy$Type <- as.factor(galaxy$Type)
table(galaxy$Type)

pairs(galaxy[,2:6],
      col = c("black", "red3","blue")[galaxy$Type],
      pch = c(1, 16, 17)[galaxy$Type], cex = 1.5)

# MANOVA to compare all three types of galaxy
# null hypothesis is that all groups have same mean

X <- as.matrix(galaxy[,2:6]) # measured data
t <- galaxy[,7]              # group data
m <- manova(X ~ t)
summary(m, test = "Wilks")

# perhaps no evidence for group effects
# know multivariate normal from previous section

# factor analysis

fact1 <- factanal(galaxy[,2:6], factors = 1, rotation = "none" )
fact1

# very small p value so likely more than 1 factor

fact2 <- factanal(galaxy[,2:6], factor = 2, rotation = "none")
fact2

# 2 common factors seems right