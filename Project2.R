# Group Project 2
# TryMoreThai
# Analysis of which Virginia math SOL tests best predicts student enrollment in college.

# package for descriptive statistics
library(pastecs)
library(ResourceSelection)
library(ISLR)
library(tree)
library(lmtest)
library(MASS)

# read in dataset
setwd("/Users/hilaryligon/Desktop/GW Data Science/DATS 6101")
sols <- read.csv('math.csv')
head(sols)

# set categorical variables (factors)
County = as.factor(sols$County)
School = as.factor(sols$School)
Race = as.factor(sols$Race)
Gender = as.factor(sols$Gender)
Disadvantaged = as.factor(sols$Disadvantaged)

# set numeric variables
CollegeEnrollmentCount <- sols$CollegeEnrollmentCount
Algebra1_avgSOLScore <- sols$Algebra1_avgSOLScore
Algebra1_PassRate <- sols$Algebra1_PassRate
Algebra2_avgSOLScore <- sols$Algebra2_avgSOLScore
Algebra2_PassRate <- sols$Algebra2_PassRate
Geometry_avgSOLScore <- sols$Geometry_avgSOLScore
Geometry_PassRate <- sols$Geometry_PassRate
CollegeEnrollmentRatio <- sols$CollegeEnrollmentRatio

# tell R to use white and male as reference levels
Race = relevel(Race, ref='White')
Gender = relevel(Gender, ref='Male')
levels(Race)
levels(Gender)

# perform some plots to help us create models
plot(Algebra1_PassRate, CollegeEnrollmentRatio)
plot(Algebra2_PassRate, CollegeEnrollmentRatio)
plot(Geometry_PassRate, CollegeEnrollmentRatio)
# We see heteroscedasticity, so we will need to try to correct it later.

# check for correlation between tests
cor(Algebra1_PassRate, Algebra2_PassRate, use="complete.obs")
cor(Algebra1_PassRate, Geometry_PassRate, use="complete.obs")
cor(Geometry_PassRate, Algebra2_PassRate, use="complete.obs")
# We see that the tests are correlated.  If we put them together in the model,
# we could end up with an autocorrelation problem.  So we will test each separately.

# plots of the tests also show that they are correlated with each other
plot(Algebra1_PassRate, Algebra2_PassRate)
plot(Algebra1_PassRate, Geometry_PassRate)
plot(Geometry_PassRate, Algebra2_PassRate)
# Plot show the same - that we have multicolinearity we want to avoid in our models.

# Linear regression models
# for each group of students, ratio of number enrolled in college to total number who graduated high school is Y
# race, gender, disadvantaged status, and SOL pass rate are Xs

# looking just at each test
fit1 <- lm(CollegeEnrollmentRatio ~ Algebra1_PassRate, data=sols)
summary(fit1)

fit2 <- lm(CollegeEnrollmentRatio ~ Algebra2_PassRate, data=sols)
summary(fit2)

fit3 <- lm(CollegeEnrollmentRatio ~ Geometry_PassRate, data=sols)
summary(fit3)
# Each test was statistically significant.  Algebra2 had the highest R-squared value.

# 3 regressions looking at each test passing rate with all demographic variables
fit4 <- lm(CollegeEnrollmentRatio ~ Algebra1_PassRate + Race + Gender + Disadvantaged, data=sols)
summary(fit4)
# Algebra1 is not statistically significant.  Some demographic variables are.

fit5 <- lm(CollegeEnrollmentRatio ~ Algebra2_PassRate + Race + Gender + Disadvantaged, data=sols)
summary(fit5)
# Algebra2 is statistically significant.  Some demographic variables are, too.
# R-squared is .51

fit6 <- lm(CollegeEnrollmentRatio ~ Geometry_PassRate + Race + Gender + Disadvantaged, data=sols)
summary(fit6)
# Geometry is statistically significant (but right on the line).  Some demographic variables are, too.
# R-squared is .49

# adding county
fit7 <- lm(CollegeEnrollmentRatio ~ Algebra1_PassRate + Race + Gender + Disadvantaged + County, data=sols)
summary(fit7)
# As before, Algebra1 is not statistically significant.  Some demographic variables are.

fit8 <- lm(CollegeEnrollmentRatio ~ Algebra2_PassRate + Race + Gender + Disadvantaged + County, data=sols)
summary(fit8)
# As before, Algebra2 is statistically significant.  Some demographic variables are, too. Surprisingly, county is also significant.
# R-squared is .52

fit9 <- lm(CollegeEnrollmentRatio ~ Geometry_PassRate + Race + Gender + Disadvantaged + County, data=sols)
summary(fit9)
# Geometry is statistically significant (and a little more significant now).  Some demographic variables are, too.  Surprisingly, county is also significant.
# R-squared is .51

# Test regression models for goodness of fit
#Testing for heteroscedasticity with Breusch-Pagan test
# P-value should be more than 0.05.  If lower, we have heteroscedasticity.
lmtest::bptest(fit8)
lmtest::bptest(fit9)
# We confirmed what we thought when we did initial plots.  We have heteroscedasticity.

# Box-Cox transformation
bc = boxcox(CollegeEnrollmentRatio ~ Algebra2_PassRate + Race + Gender + Disadvantaged + County, data=sols, plotit=T)
bc$x[which.max(bc$y)]
bc2 = boxcox(CollegeEnrollmentRatio ~ Algebra2_PassRate + Race + Gender + Disadvantaged + County, data=sols, plotit=T, lambda = seq(-3, 3, length = 20))
bc2$x[which.max(bc2$y)]
# boxcox is above 2, so not the best, but we will try it
fitBC <- lm(CollegeEnrollmentRatio^2.8 ~ Algebra2_PassRate + Race + Gender + Disadvantaged + County, data=sols)
summary(fitBC)
plot(fitBC)

# compare to residuals vs fitted for Fit8, the version without the BoxCox transformation
plot(fit8)
# For our model, the BoxCox transformation only makes a little improvement,
# but it is more difficult to interpret.  So, we will stick with the more simple model.
# OLS regression is robust against the homoscedasticity assumption so the un-formed model is still the most appropriate.

# Now we do the same thing for the model with Geometry.
bc = boxcox(CollegeEnrollmentRatio ~ Geometry_PassRate + Race + Gender + Disadvantaged + County, data=sols, plotit=T)
bc$x[which.max(bc$y)]
bc2 = boxcox(CollegeEnrollmentRatio ~ Geometry_PassRate + Race + Gender + Disadvantaged + County, data=sols, plotit=T, lambda = seq(-3, 3, length = 20))
bc2$x[which.max(bc2$y)]
# boxcox is above 2, so not the best, but we will try it
fitBC <- lm(CollegeEnrollmentRatio^2.7 ~ Geometry_PassRate + Race + Gender + Disadvantaged + County, data=sols)
summary(fitBC)
plot(fitBC)

# compare to residuals vs fitted for Fit8, the version without the BoxCox transformation
plot(fit9)
# For our model, the BoxCox transformation makes a little improvement,
# but it is more difficult to interpret.  For now, we will stick with the more simple model.
# OLS regression is robust against the homoscedasticity assumption so the un-formed model is still the most appropriate.


# Checking to see if higher orders of variables improve goodness of fit.
# The p-value should be less than 0.05
resettest(CollegeEnrollmentRatio ~ Algebra2_PassRate, data = sols)
# Cannot reject null hypothesis.  Some transformation might help.
resettest(CollegeEnrollmentRatio ~ Algebra2_PassRate, power=2, type='regressor', data=sols)
# This is worse.  Stick with simpler model.

# Reset test on Geometry
resettest(CollegeEnrollmentRatio ~ Geometry_PassRate, data = sols)
# Cannot reject null hypothesis.  Some transformation might help.


# make exponent variables
logAlgebra2 <- log(sols$Algebra2_PassRate)
logGeometry <- log(sols$Geometry_PassRate)
# plots
# plot(logAlgebra2, CollegeEnrollmentRatio)
# plot(logGeometry, CollegeEnrollmentRatio)

# Regression test log variable
fit10 <- lm(CollegeEnrollmentRatio ~ logAlgebra2 + Race + Gender + Disadvantaged + County, data=sols)
summary(fit10)
# As before, Algebra2 is statistically significant.  Some demographic variables are, too. Surprisingly, county is also significant.
# R-squared is .53
# Not sure that the transformation improves model fit.

fit11 <- lm(CollegeEnrollmentRatio ~ logGeometry + Race + Gender + Disadvantaged + County, data=sols)
summary(fit11)
# Geometry is statistically significant.  Some demographic variables are, too. Surprisingly, county is also significant.
# R-squared is .51
# The transformation does not improves model fit.


# Decision Trees
#list columns
colnames(sols)

# dropping school names and other variables we are not using
sols2 = subset(sols, select = -c(X, School, Algebra1_avgSOLScore, Algebra2_avgSOLScore, Geometry_avgSOLScore, HSGradCount, CollegeEnrollmentCount))
head(sols2)

# create training set
train <- sample(1:nrow(sols2), nrow(sols2)/2)

# create decision tree
tree.sols2 <- tree(CollegeEnrollmentRatio~., sols2, subset = train)
summary(tree.sols2)

# look at tree
plot(tree.sols2)
text(tree.sols2, pretty=1)

# cross validate
cv.sols2 <- cv.tree(tree.sols2)
plot(cv.sols2$size, cv.sols2$dev, type = 'b')

# pruned tree
prune.sols2 <- prune.tree(tree.sols2, best=8)
plot(prune.sols2)
text(prune.sols2, pretty=1)
