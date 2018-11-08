
###################################################################

obdata = read.table("obesitydata.txt",h=T)
attach(obdata)

##Data before aggregation
obesityind = factor(Obesity,labels=c("NotObese","Obese"))
agegr = factor(AgeGroup, labels=c("18to24", "25to34",
              "35to44", "45to64", "65+"))
gender = factor(Gender,labels=c("Male","Female"))
edu = factor(Education,labels=c("<9thGrade","9to11Grade",
             "HighSchool","SomeCollege","College+"))

## Exploratory data analysis: Response vs Predictors
tb_obage = xtabs(~obesityind+agegr)
tb_obgender = xtabs(~obesityind+gender)
tb_obedu = xtabs(~obesityind+edu)


barplot(prop.table(tb_obage),axes=T,space=0.3,
        xlab="Proportion of Not Obese (blue) vs Obese (Brown)",
        horiz=T, col=c("blue","brown"),main="Obesity by Age Group")
barplot(prop.table(tb_obgender),axes=T,space=0.3,
        xlab="Proportion of Not Obese (blue) vs Obese (Brown)",
        horiz=T, col=c("blue","brown"),main="Obesity by Gender")
barplot(prop.table(tb_obedu),axes=T,space=0.3, horiz=T,
        xlab="Proportion of Not Obese (blue) vs Obese (Brown)",
        col=c("blue","brown"),main="Obesity by Education Level")

# Exploratory data analysis:  Predictors
tb_ageedu = xtabs(~agegr+edu)
library(vcd)
mosaicplot(tb_ageedu,xlab="Age Group",ylab="Education",color=TRUE,main="")

## Fit a logistic regression model
model = glm(Obesity~agegr+gender+edu,family=binomial)
summary(model)

## Test for overall regression
gstat = model$null.deviance - deviance(model)
cbind(gstat, 1-pchisq(gstat,length(coef(model))-1))

round(coefficients(summary(model))[,4],4)

## Is it meaningful to do a goodness of fit test? No.
## Is it meaningful to do a residual analysis? No. Logistic regression without repetitions

## What is the predictive power? Cross-validation
library(boot)
##
cost0.5 = function(y, pi){
 ypred=rep(0,length(y))
 ypred[pi>0.5] = 1
 err = mean(abs(y-ypred))
 return(err)
}
n = length(Obesity)
obdata.fr = data.frame(cbind(Obesity,agegr,gender,edu))
## classification error for 10-fold cross-validation
cv.err = cv.glm(obdata.fr,model,cost=cost0.5, K=10)$delta[1]

## Consider different thresholds for the probability

cost0.3 = function(y, pi){
 ypred=rep(0,length(y))
 ypred[pi>0.3] = 1
 err = mean(abs(y-ypred))
 return(err)
}

cost0.35 = function(y, pi){
 ypred=rep(0,length(y))
 ypred[pi>0.35] = 1
 err = mean(abs(y-ypred))
 return(err)
}

cost0.4 = function(y, pi){
 ypred=rep(0,length(y))
 ypred[pi>0.4] = 1
 err = mean(abs(y-ypred))
 return(err)
}

cost0.45 = function(y, pi){
 ypred=rep(0,length(y))
 ypred[pi>0.45] = 1
 err = mean(abs(y-ypred))
 return(err)
}


cost0.55 = function(y, pi){
 ypred=rep(0,length(y))
 ypred[pi>0.55] = 1
 err = mean(abs(y-ypred))
 return(err)
}
cost0.6 = function(y, pi){
 ypred=rep(0,length(y))
 ypred[pi>0.6] = 1
 err = mean(abs(y-ypred))
 return(err)
}
cost0.65 = function(y, pi){
 ypred=rep(0,length(y))
 ypred[pi>0.65] = 1
 err = mean(abs(y-ypred))
 return(err)
}

cv.err0.3 = cv.glm(obdata.fr,model,cost=cost0.3,K=10)$delta[1]
cv.err0.35 = cv.glm(obdata.fr,model,cost=cost0.35,K=10)$delta[1]
cv.err0.4 = cv.glm(obdata.fr,model,cost=cost0.4,K=10)$delta[1]
cv.err0.45 = cv.glm(obdata.fr,model,cost=cost0.45,K=10)$delta[1]
cv.err0.5 = cv.glm(obdata.fr,model,cost=cost0.5,K=10)$delta[1]
cv.err0.55 = cv.glm(obdata.fr,model,cost=cost0.55,K=10)$delta[1]
cv.err0.6 = cv.glm(obdata.fr,model,cost=cost0.6,K=10)$delta[1]
cv.err0.65 = cv.glm(obdata.fr,model,cost=cost0.6,K=10)$delta[1]
cv.err = c(cv.err0.35,cv.err0.35,cv.err0.4,cv.err0.45,cv.err0.5,
           cv.err0.55,cv.err0.6,cv.err0.65)

## Smallest prediction error is 0.3824
plot(c(0.3, 0.35,0.4,0.45,0.5,0.55,0.6,0.65),cv.err,
     type="l",lwd=3,xlab="Threshold",ylab="CV Classification Error")

####################################################################
###################### PART II #################################
## Prediction given a set of new observations
testobdata = read.table("testobesitydata.txt",h=T)
agegr.t = factor(testobdata$AgeGroup, labels=c("18to24", "25to34",
                    "35to44", "45to64", "65+"))
gender.t = factor(testobdata$Gender,labels=c("Male","Female"))
edu.t = factor(testobdata$Education,labels=c("<9thGrade","9to11Grade",
                  "HighSchool","SomeCollege","College+"))
pred.data = data.frame(agegr=agegr.t,gender=gender.t,edu=edu.t)
pred.test = predict.glm(model,pred.data,type="response")

err0.3 = cost0.3(testobdata$Obesity,pred.test)
err0.35 = cost0.35(testobdata$Obesity,pred.test)
err0.4 = cost0.4(testobdata$Obesity,pred.test)
err0.45 = cost0.45(testobdata$Obesity,pred.test)
err0.5 = cost0.5(testobdata$Obesity,pred.test)
err0.55 = cost0.55(testobdata$Obesity,pred.test)
err0.6 = cost0.6(testobdata$Obesity,pred.test)
err0.65 = cost0.65(testobdata$Obesity,pred.test)
err = c(err0.35,err0.35,err0.4,err0.45,err0.5,
           err0.55,err0.6,err0.65)
plot(c(0.3, 0.35,0.4,0.45,0.5,0.55,0.6,0.65),err,
     type="l",lwd=3,xlab="Threshold",ylab="Classification Error")

##########################################################################
####################### PART III #########################################
### Aggregate data for Logistic Regression with repetitions
obdata.agg.n = aggregate(Obesity~agegr+gender+edu,FUN=length)
obdata.agg.y = aggregate(Obesity~agegr+gender+edu,FUN=sum)

agegr.agg = factor(obdata.agg.n$agegr, labels=c("18to24", "25to34",
              "35to44", "45to64", "65+"))
gender.agg = factor(obdata.agg.n$gender,labels=c("Male","Female"))
edu.agg = factor(obdata.agg.n$edu,labels=c("<9thGrade","9to11Grade",
             "HighSchool","SomeCollege","College+"))
             
obdata.agg = data.frame(Obesity = obdata.agg.y$Obesity,
                        Total = obdata.agg.n$Obesity,
                        agegr = agegr.agg,
                        gender=gender.agg,
                        edu=edu.agg)
model.agg = glm(cbind(Obesity,Total-Obesity)~agegr+gender+edu,
                data = obdata.agg,family=binomial)
## Test for overall regression
gstat = model.agg$null.deviance - deviance(model.agg)
cbind(gstat, 1-pchisq(gstat,length(coef(model.agg))-1))

## Test for GOF: Using deviance residuals
deviances2 = residuals(model.agg,type="deviance")
dev.tvalue = sum(deviances2^2)
c(dev.tvalue, 1-pchisq(dev.tvalue,40))
#OR
c(deviance(model.agg), 1-pchisq(deviance(model.agg),40))

## Residual Analysis
res = resid(model.agg,type="deviance")
par(mfrow=c(2,2))
boxplot(res~agegr,xlab="Age Group",ylab = "Std residuals",data = obdata.agg)
boxplot(res~gender,xlab="Gender",ylab = "Std residuals",data = obdata.agg)
qqnorm(res, ylab="Std residuals")
qqline(res,col="blue",lwd=2)
hist(res,10,xlab="Std residuals", main="")

model.agg.2 = glm(cbind(Obesity,Total-Obesity)~agegr+gender+edu,
                data = obdata.agg,family=binomial(link = cloglog))

## Residual Analysis
par(mfrow=c(2,2))
res = resid(model.agg.2,type="deviance")
par(mfrow=c(2,2))
boxplot(res~agegr,xlab="Age Group",ylab = "Std residuals",data = obdata.agg)
boxplot(res~gender,xlab="Gender",ylab = "Std residuals",data = obdata.agg)
qqnorm(res, ylab="Std residuals")
qqline(res,col="blue",lwd=2)
hist(res,10,xlab="Std residuals", main="")
