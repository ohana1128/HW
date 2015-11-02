#1.load the data
library(MASS)
data(Boston)

#2.Add an another variable to the dataframe “Boston”, name this variable “logmedv” and set it equal to the log of the variable “medv”. Remove the “medv” column.
Boston$logmedv <- log(Boston$medv)
Boston <- subset(Boston,select=-c(14))

#3.Use forward and backward stepwise selection to find the subset with the lowest AIC.
#Forward Stepwise Selection
templm <- lm(logmedv~1,data=Boston)
tempScope <- formula(lm(logmedv~.,Boston))
step <- stepAIC(templm, 
                scope=tempScope,
                direction="forward")
step$anova
#The lowest AIC is -1669.9987, which is is achieved by the 12th model, which includes all of the variables except "age" and "indus".

#Backward Stepwise Selection
templm <- lm(logmedv~.,data=Boston)
tempScope <- formula(lm(logmedv~.,Boston))
step <- stepAIC(templm, 
                scope=tempScope,
                direction="backward")
step$anova
#The lowest AIC is -1670.0,which is achieved by the 3rd model, which includes all of the variables except "age" and "indus".

#4.For either the model chosen by forward or the model chosen by backward stepwise selection,print the linear regression summary (summary(lm)). Comment on the p-values for the t-testsand the F-test.
lm1 <- lm(logmedv~. -age -indus, data=Boston)
(summary(lm1))

#5. The p-values calculated from step (4) are only meaningful when the tests’ assumptions are satisfied. It is impossible to check some of these assumptions but you should always use all of the diagnostic tools at your disposal.
#(a)Normality of residuals: Check the normality of the residuals using a test for normality and by plotting the QQ plot.
shapiro.test(lm1$residuals)
library(ggplot2)
library(car)
qqPlot(lm1)
# there are very heavy tails
#(b)Homoskedasticity and linearity: For each variable in Boston (there are 13 variables besides medv), plot the variable versus the residuals of your final fit. Check for homoskedasticity. Furthermore, identify if any transformations to the variables might be necessary.
library(car)
ncvTest(lm1)
#Since the inference assumptions are violated by doing (a)and(b), the p-values we see in the summary above should be looked at sceptical 
library(reshape)
Boston.melt <- melt(Boston)
Boston.melt3 <- cbind(Boston.melt,resid=lm1$residuals)
ggplot(Boston.melt3,aes(x=value,y=resid))+geom_point()+geom_smooth(method="loess")+facet_wrap(~variable,scales="free")
#There are non-linearities with respect to crim,rm,lstat variables.So I will go in that order and try polynomial regression with different terms for each of the variables. I will choose the order of polynomials using cross validation
#Transforming lstat, minimum is obtained with a fourth degree polynomial for lstat.
library(boot)
lstatMSE <- rep(0,10)
for(i in 1:10){
  templm <- glm(logmedv~.-age-indus-lstat+poly(lstat,i),data=Boston)
  tempCV <- cv.glm(Boston,templm,K = 10)
  lstatMSE[i] <- tempCV$delta[1]
}
plot(lstatMSE)
#Transforming crim,minimum is obtained with a second degree polynomial for crim.
crimMSE <- rep(0,10)
for(i in 1:10){
  templm <- glm(logmedv~.-age-indus-lstat-crim
                +poly(lstat,4)+poly(crim,i),data=Boston)
  tempCV <- cv.glm(Boston,templm,K = 10)
  crimMSE[i] <- tempCV$delta[1]
}
plot(crimMSE)
ncvTest(lm1) #
##Transforming rm,minimum is obtained with a fourth degree polynomial for rm.
rmMSE <- rep(0,10)
for(i in 1:10){
  templm <- glm(logmedv~.-age-indus-lstat-crim-rm
                +poly(lstat,4)+poly(crim,2)+poly(rm,i),data=Boston)
  tempCV <- cv.glm(Boston,templm,K = 10)
  rmMSE[i] <- tempCV$delta[1]
}
plot(rmMSE)
#Therefore,the final model is summarized as below.
finalLm <- lm(logmedv~.-age-indus-lstat-crim-rm
              +poly(lstat,4)+poly(crim,2)+poly(rm,4),data=Boston)
summary(finalLm)

#final model MSE
finalGLM <- glm(logmedv~.-age-indus-lstat-crim-rm
                +poly(lstat,4)+poly(crim,2)+poly(rm,4),data=Boston)
cv.glm(Boston,finalGLM,K=10)$delta[1]

finalGLm <- glm(logmedv~poly(crim,2)+zn+chas+nox+poly(rm,4)+dis
              +rad+tax+ptratio+black+poly(lstat,4),data=Boston)
cv.glm(Boston,finalGLM,K=10)$delta[1]
#(c)Outliers: Plot Cook’s distances and the leverage scores of the residuals. Identify which points could be outliers.
plot(lm1,which=4)
plot(lm1,which=6)
#381,406,413 could be the outliers.

#6. Compare the full model (the linear regression fit that contains all of the variables) and your model in step (4) using cross validation (LOOCV or K-fold).
glm1 <- glm(logmedv~. -age -indus, data=Boston)
glm2 <- glm(logmedv~.,data = Boston)
cv.glm(Boston,glm1,K=10)$delta[1]
cv.glm(Boston,glm2,K=10)$delta[1]

#lm2 with MSE 0.03777384 has lower MSE.But we could find the transformed final model in step(5) has even lower MSE.

