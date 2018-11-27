meddcor = read.table("meddcor.txt", sep="", header = FALSE)
n = dim(meddcor)[1] # number of observations
colnames(meddcor) = c("sales", "advertising", "bonuses", "marketshare", "largestcomp", "region")
meddcor$region = as.factor(meddcor$region)
model = lm(sales ~ ., data = meddcor)
summary(model)

## Predicting Values for the 1st office
newdata = meddcor[1,2:6]
## Estimate standard deviation
s2 = summary(model)$sigma^2
xstar = as.double(newdata)
X = data.matrix(meddcor[,2:6])
predvar = s2*(xstar%*%solve(t(X)%*%X)%*%xstar)
sqrt(predvar)
## Confidence Interval
predict(model, newdata, interval="confidence")

## Change the value of the competitor's sales
newdata[4] = 303
## Estimate standard deviation
s2 = summary(model)$sigma^2
xstar = as.double(newdata)
X = data.matrix(meddcor[,2:6])
predvar = s2*(1+xstar%*%solve(t(X)%*%X)%*%xstar)
sqrt(predvar)
## Confidence Interval
predict(model, newdata, interval="prediction")

### Assessing Assumptions
## Scatterplot matrix
plot(meddcor[,1:5])

resids = model$resid
par(mfrow =c(2,2))
plot(meddcor[,2],resids,xlab="Adv Expenditure",ylab="Residuals")
abline(0,0,col="red")
plot(meddcor[,3],resids,xlab="Bonuses",ylab="Residuals")
abline(0,0,col="red")
plot(meddcor[,4],resids,xlab="Market Share",ylab="Residuals")
abline(0,0,col="red")
plot(meddcor[,5],resids,xlab="Competitor's Sales",ylab="Residuals")
abline(0,0,col="red")

library(car)
fits = model$fitted
cook = cooks.distance(model)
par(mfrow =c(2,2))
plot(fits, resids, xlab="Fitted Values",ylab="Residuals")
abline(0,0,col="red")
qqPlot(resids, ylab="Residuals", main = "")
hist(resids, xlab="Residuals", main = "",nclass=10,col="orange")
plot(cook,type="h",lwd=3,col="red", ylab = "Cook’s Distance")

cor(meddcor[,2:5])

vif(model)

summary(model)$r.squared
