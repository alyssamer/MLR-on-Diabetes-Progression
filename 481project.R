
################################################################################
################################################################################
############################# Diabetes Model ###################################
################################################################################
################################################################################

diabetes <- read.table("dataset/Diabetes.txt", header = TRUE)

diabetes$SEX = as.factor(diabetes$SEX)
sum(is.na(diabetes))
summary(diabetes)
sapply(diabetes[ , -2], mean)
sapply(diabetes[ , -2], sd)
par(mfrow = c(2,5))
hist(diabetes$AGE, xlab = "Age", main = "")
boxplot(Target ~ SEX, data = diabetes, main = "")
hist(diabetes$BMI, xlab = "Body Mass Index", main = "")
hist(diabetes$BP, xlab = "Avg. Blood Pressure", main = "")
hist(diabetes$S1, xlab = "S1", main = "")
hist(diabetes$S2, xlab = "S2", main = "")
hist(diabetes$S3, xlab = "S3", main = "")
hist(diabetes$S4, xlab = "S4", main = "")
hist(diabetes$S5, xlab = "S5", main = "")
hist(diabetes$S6, xlab = "S6", main = "")

################################################################################
############################# Original Model ###################################
################################################################################
################################################################################

dLM <- lm(Target ~AGE + SEX + BMI + BP + S1 + S2 + S3 + S4 + S5 + S6, data = diabetes)
anova(dLM) 
summary(dLM)
library(car)
vif(dLM) 
cor(diabetes[, c("AGE", "BMI", "BP","S1", "S2", "S3", "S4", "S5", "S6")])
# s1 has severe multicollinearity

######################################################## L.I.N.E assumptions

# linearity 
par(mfrow = c(2,5))
plot(x = diabetes$AGE, y = dLM$residuals)
abline(h=0)
plot(x = diabetes$SEX, y = dLM$residuals, xlab = "diabetes$SEX")
abline(h=0)
plot(x = diabetes$BMI, y = dLM$residuals)
abline(h=0)
plot(x = diabetes$BP, y = dLM$residuals)
abline(h=0)
plot(x = diabetes$S1, y = dLM$residuals)
abline(h=0)
plot(x = diabetes$S2, y = dLM$residuals)
abline(h=0)
plot(x = diabetes$S3, y = dLM$residuals)
abline(h=0)
plot(x = diabetes$S4, y = dLM$residuals)
abline(h=0)
plot(x = diabetes$S5, y = dLM$residuals)
abline(h=0)
plot(x = diabetes$S6, y = dLM$residuals)
abline(h=0)

# normality, H0: normal, do not reject
par(mfrow = c(2,3))
qqnorm(dLM$residuals)
qqline(dLM$residuals)
shapiro.test(dLM$residuals)

# equal variances, H0: equal, reject
ncvTest(dLM)
plot(x = dLM$fitted.values, y = dLM$residuals, xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, lty = 2)

############################################### remove S1

dLM2 <- lm(Target ~AGE + SEX + BMI + BP + S2 + S3 + S4 + S5 + S6, data = diabetes)
vif(dLM2)
# This  elimates multico. in the rest of variables

################################################################################
############################# Box Cox Model ####################################
################################################################################
################################################################################

####################################################### transformation
library(MASS)
trans <- boxcox(Target ~AGE + SEX + BMI + BP + S2 + S3 + S4 + S5 + S6, 
                data = diabetes, plotit = F, lambda = seq(-3, 3, by = 0.125))
lambda <- trans$x[which.max(trans$y)] #  lambda value

## Add new column to data frame with transformed variable
diabetes$TargetY <- ((diabetes$Target^lambda) - 1) /lambda

##################################################### model after transformation

dLM3 <- lm(TargetY ~AGE + SEX + BMI + BP + S2 + S3 + S4 + S5 + S6, data = diabetes)
anova(dLM3)
summary(dLM3)

######################################################## L.I.N.E assumptions

# Linearity
par(mfrow = c(2,5))
plot(x = diabetes$AGE, y = dLM3$residuals)
abline(h=0)
plot(x = diabetes$SEX, y = dLM3$residuals, xlab = "diabetes$SEX")
abline(h=0)
plot(x = diabetes$BMI, y = dLM3$residuals)
abline(h=0)
plot(x = diabetes$BP, y = dLM3$residuals)
abline(h=0)
plot(x = diabetes$S2, y = dLM3$residuals)
abline(h=0)
plot(x = diabetes$S3, y = dLM3$residuals)
abline(h=0)
plot(x = diabetes$S4, y = dLM3$residuals)
abline(h=0)
plot(x = diabetes$S5, y = dLM3$residuals)
abline(h=0)
plot(x = diabetes$S6, y = dLM3$residuals)
abline(h=0)

# normality, H0: normal, do not reject
par(mfrow = c(2,3))
qqnorm(dLM3$residuals)
qqline(dLM3$residuals)
shapiro.test(dLM3$residuals)

# equal variances, do not reject
ncvTest(dLM3)
plot(x = dLM3$fitted.values, y = dLM3$residuals, xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, lty = 2)

################################################################################
############################# Full Model ####################################
################################################################################
################################################################################

diabetes2 <- data.frame(Target = diabetes$TargetY,
                        AGE = diabetes$AGE,
                        SEX = diabetes$SEX,
                        BMI = diabetes$BMI,
                        BP = diabetes$BP,
                        S2 = diabetes$S2,
                        S3 = diabetes$S3,
                        S4 = diabetes$S4,
                        S5 = diabetes$S5,
                        S6 = diabetes$S6)

full <- lm(Target ~., data = diabetes2)
anova(full)
summary(full)

########################################################### backward selection

# AGE, S2, S4, S6 have large p values

lmR1 <- lm(Target ~SEX + BMI + BP + S2 + S3 + S4 + S5 + S6, data = diabetes2)
anova(full, lmR1)
summary(lmR1)

lmR2 <- lm(Target ~SEX + BMI + BP + S2 + S3 + S5 + S6, data = diabetes2)
anova(lmR1, lmR2)
summary(lmR2)

lmR3 <- lm(Target ~SEX + BMI + BP + S2 + S3 + S5, data = diabetes2)
anova(lmR2, lmR3)
summary(lmR3)

lmR4 <- lm(Target ~SEX + BMI + BP + S3 + S5, data = diabetes2)
anova(lmR3, lmR4)
summary(lmR4)

################################################################################
################################################################################
############################## Final Model #####################################
################################################################################
################################################################################

final <- lm(Target ~SEX + BMI + BP + S3 + S5, data = diabetes2)
summary(final)
anova(final)

# Linearity
par(mfrow = c(2,3))
plot(x = diabetes2$SEX, y = final$residuals, xlab = "diabetes$SEX")
abline(h=0)
plot(x = diabetes2$BMI, y = final$residuals)
abline(h=0)
plot(x = diabetes2$BP, y = final$residuals)
abline(h=0)
plot(x = diabetes2$S3, y = final$residuals)
abline(h=0)
plot(x = diabetes2$S5, y = final$residuals)
abline(h=0)

# normality, H0: normal, do not reject
par(mfrow = c(2,2))
qqnorm(final$residuals)
qqline(final$residuals)
shapiro.test(final$residuals)


# equal variances, do not reject
ncvTest(final)
plot(x = final$fitted.values, y = final$residuals, xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, lty = 2)