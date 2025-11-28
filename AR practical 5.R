# AR practical 5

stock[1:3,]

# Exercise 1
# assuming there is one common factor

stockfact1 <- factanal(stock, factors = 1, rotation = 'none')
stockfact1
# small p value so would reject null hypothesis
# 1 factor is not enough

stockfact2 <- factanal(stock, factors = 2, rotation = 'none')
stockfact2
# large pvalue so would not reject
# 2 factors is plenty

# all variables have large positive loadings on the first factor
# could be interpreted as general market condition

# second factor interpretation is not clear
# as solutions are not unique we can try rotating!
# try to rotate in a way that each factor has a few large loadings and many small loadings
# process called varimax rotation

stockfact2 <- factanal(stock, factors = 2, rotation = 'varimax')
stockfact2

# rotated loadings indicate chemical stock load highly on first factor
# oil stocks load highly on the second factor
# factors differentiate industry

# Exercise 2
usc <- uscrime
head(usc)

# a

uscfact1 <- factanal(usc, factors = 1, rotation = 'none')
uscfact1

# pvalue is low so would reject hypothesis that 1 factor is sufficient

uscfact2 <- factanal(usc, factors = 2, rotation = 'none')
uscfact2

# pvalue is larger so 2 factors likely sufficient

# c 
# factor 1 all quite large and positive
# likely an overall measure of crime

# factor 2 a measure of stealing?

# d 

uscfact2 <- factanal(usc, factors = 2, rotation = 'varimax')
uscfact2

# factor 1 becomes a measure of stealing and factor 2 becomes a measure of violent crime

# Exercise 3

drugs <- as.matrix(drugs) # correlation matrix of survey

View(drugs)

drugsfact <- factanal(covmat = drugs, factors = 2, rotation = 'none')

drugsfact$uniqueness

# estimates of communalities is 1 - uniqueness as using correlation matrix is already standardised

drugsfact

# factor 1 overall measure
# factor 2 measure of drugs vs traditional stuff

drugsfact <- factanal(covmat = drugs, factors = 2, rotation = 'varimax')
drugsfact

# factor 1 measure of alcohol? - legal and soft
# factor 2 more illegal?? - illegal and hard

# exercise 4

score <- as.matrix(score) # correlation matrix

score

# a

scorefact <- factanal(covmat = score, factors = 2, rotation = 'varimax')
scorefact

# b 

scorefact$uniquenesses # specific variances

1 - scorefact$uniquenesses # communalities

# c

# factor 1 measure of mathematical ability
# factor 2 measure of written ability

