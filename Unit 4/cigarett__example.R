#
# News flash! Smoking makes you live longer!

## Read the data in R
smoking = read.table("CIGARETT.dat",sep="",row.names=NULL)
names(smoking) = c("Age","Smoker","Survived","At.risk")
attach(smoking)

## Plot survival proportion versus age
plot(Age,Survived/At.risk, xlab="Age", ylab="Survival Proportion", col=c("red","blue"),lwd=3)
legend(30,0.2, legend=c("Non-smokers","Smokers"),pch=1, col=c("red","blue"))

prop.survival=Survived/At.risk
plot(Age,log(prop.survival/(1-prop.survival)),col=c("red","blue"),xlab="Age", ylab="Logit(Survival rate)", lwd=3)
legend(30,0, legend=c("Non-smokers","Smokers"),pch=1, col=c("red","blue"))

## Model 1: Smoking Factor Only
smoke1 = glm(Survived/At.risk ~ Smoker, weights=At.risk, family=binomial)
summary(smoke1)
exp(coef(smoke1)[-1])
## Test for overall regression
gstat = smoke1$null.deviance - deviance(smoke1)
cbind(gstat, 1-pchisq(gstat,length(coef(smoke1))-1))
1-pchisq(smoke1$dev,smoke1$df.residual)

## Model 2: Smoking & Age Factors
smoke2 = glm(Survived/At.risk ~ Smoker + Age, weights=At.risk,family=binomial)
summary(smoke2)

## Test for overall regression
gstat = smoke2$null.deviance - deviance(smoke2)
cbind(gstat, 1-pchisq(gstat,length(coef(smoke2))-1))

## Test for GOF: Using deviance residuals
deviances2 = residuals(smoke2,type="deviance")
dev.tvalue = sum(deviances2^2)
c(dev.tvalue, 1-pchisq(dev.tvalue,11))
#OR
c(deviance(smoke2), 1-pchisq(deviance(smoke2),11))

## Test for GOF: Using Person residuals
pearres2 = residuals(smoke2,type="pearson")
pearson.tvalue = sum(pearres2^2)
c(pearson.tvalue, 1-pchisq(pearson.tvalue,11))

plot(Age,log((Survived/At.risk)/(1-Survived/At.risk)), ylab="Logit of survival",
     main="Scatterplot of logit survival rate vs age", col=c("red","blue"),lwd=3)

## Model 3: Smoking & Age, Age squared Factors
Age.squared = Age*Age
smoke3 = glm(Survived/At.risk ~ Smoker + Age + Age.squared,weights=At.risk, family=binomial)
summary(smoke3)

## Test for GOF: Using Person residuals
pearres3 = residuals(smoke3,type="pearson")
pearson = sum(pearres3^2)
c(pearson, 1-pchisq(pearson,10))
## Test for GOF: Using deviance residuals
c(deviance(smoke3), 1-pchisq(deviance(smoke3),10))

## Residual Analysis

res = resid(smoke3,type="deviance")
par(mfrow=c(2,2))
plot(Age.squared,res,ylab="Std residuals",xlab="Age Squared")
abline(0,0,col="blue",lwd=2)
boxplot(res~Smoker,ylab = "Std residuals")
qqnorm(res, ylab="Std residuals")
qqline(res,col="blue",lwd=2)
hist(res,10,xlab="Std residuals", main="")

#
 
## Model 4: Smoking & Age as categorical/qualitative variables
smoke4 = glm(Survived/At.risk ~ Smoker + factor(Age),
              weights=At.risk, family=binomial)
summary(smoke4)

gstat = smoke4$null.deviance - deviance(smoke4)
cbind(gstat, 1-pchisq(gstat,length(coef(smoke4))-1))
1-pchisq(smoke1$dev,smoke1$df.residual)
#
# The LR test for the Age effect is the difference between
# the deviance statistic for the model that includes it
# and the deviance for the model that has no Age effect.
# It is compared to a chi-squared distribution on
# k-1 degrees of freedom, where k is the number of
# Age categories (here 7-1=6).
#
agetest = deviance(smoke1)-deviance(smoke4)
cbind(agetest, 1-pchisq(agetest,6))
#
# The car library gives LR tests (and others as options) for
# glm objects as well as lm and aov objects
#
pearres4 = residuals(smoke4,type="pearson")
pearson = sum(pearres4^2)
c(pearson, 1-pchisq(pearson,6))
round(c(deviance(smoke4), 1-pchisq(deviance(smoke4),6)),2)

par(mfrow=c(2,2))
smoke4.diag = ls.diag(lsfit(model.matrix(smoke4)[,2:4],
                             Survived/At.risk))
qqnorm(smoke4.diag$std.res, ylab="Std residuals")
plot(fitted(smoke4),smoke4.diag$std.res,
     xlab="Fitted values",ylab="Std residuals",
     main="Versus Fits")
hist(smoke4.diag$std.res,10,xlab="Std residuals",
     main="Histogram")
plot(smoke4.diag$std.res,type="l",ylab="Std residuals",
     xlab="Observation Order",main="Versus Order")
#
## Model 5: Use of different link function 
smoke5 = glm(Survived/At.risk ~ Smoker + Age + Age.squared, weights=At.risk, 
family=binomial(link = probit))
summary(smoke5)
pearres5 = residuals(smoke5,type="pearson")
pearson = sum(pearres5^2)
c(pearson, 1-pchisq(pearson,10))
c(deviance(smoke5), 1-pchisq(deviance(smoke5),10))

# Simpson Paradox
# ## Relationship between smoking and age
cbind(unique(Age),smoking[Smoker==1,]$At.risk/
(smoking[Smoker==1,]$At.risk +smoking[Smoker==0,]$At.risk))


         

