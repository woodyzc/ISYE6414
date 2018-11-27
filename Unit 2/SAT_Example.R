## Read the data using the 'read.table()' R command because it is an ASCII file
data = read.table("SATData.txt", header = TRUE)
## Check data to make sure correctly read in R
data[1:4,]
## Check dimensionality of the data file
dim(data)
attach(data)
###

## Explore the shape of the distributions of the response & predicting variables
par(mfrow = c(2, 4))
hist(sat, main = "Histogram of SAT Scores", xlab = "Mean SAT Score", col = 1)
hist(takers, main = "Histogram of Takers",
xlab = "Percentage of students tested", col = 2)
hist(income, main = "Histogram of Income", xlab = "Mean Household Income ($100s)", col = 3)
hist(years, main = "Histogram of Years", xlab = "Mean Years of Sciences and Humanities", col = 4)
hist(public, main = "Public Schools Percentage", xlab = "Percentage of Students in Public Schools", col = 5)
hist(expend, main = "Histogram of Expenditures", xlab = "Schooling Expenditures/Student ($100s)", col = 6)
hist(rank, main = "Histogram of Class Rank", xlab = "Median Class Ranking Percentile", col = 7)

## Explore the relationships between all variables using scatterplots
## Evaluate the scatter plot matrix of the data ’, ignoring the first column
par(mfrow = c(1, 1))
plot(data[,-1])

## Explore the correlation coefficients
round(cor(data[,-1]), 2)

#Fit a full regression line
regression.line = lm(sat ~ takers + + rank + income + years + public + expend)
summary(regression.line)
## compute partial-F statistic
fstat = ((2858+16080+252+4745)/4)/(29842/43)
pvalue = 1-pf(fstat,4,43) 
pvalue


#### Ranking states by SAT score

## Consider the model with the two controlling facttor to correct for bias
reduced.line = lm(sat~takers + rank)
## obtain the order of states by the residuals of the reduced model
order.vec = order(reduced.line$res, decreasing = TRUE)
## Re-order the states and create a table including state name, new and old order. 
states = factor(data[order.vec, 1])
newtable = data.frame(State = states, Residual = as.numeric(round(reduced.line$res[order.vec], 1)),
oldrank = (1:50)[order.vec])

#### Residual Analysis (for reduced model only)

res = reduced.line$res
cook = cooks.distance(reduced.line)
par(mfrow = c(2,3))
plot(sat, res, xlab = "SAT Score", ylab = "Residuals", pch = 19)
abline(h = 0)
plot(takers, res, xlab = "Percent of Students Tested", ylab = "Residuals", pch = 19)
abline(h = 0)
plot(rank, res, xlab = "Median Class Ranking Percentile", ylab = "Residuals", pch = 19)
abline(h = 0)
hist(res, xlab="Residuals", main= "Histogram of Residuals")
qqnorm(res)
qqline(res)
plot(cook,type="h",lwd=3, ylab = "Cook’s Distance")

## Transform the predicting variable 'takers'
regression.line = lm(sat ~log(takers)  + rank + income + years + public + expend)
summary(regression.line)

cook = cooks.distance(regression.line)
res = regression.line$res
par(mfrow = c(2,3))
plot(sat, res, xlab = "SAT Score", ylab = "Residuals", pch = 19)
abline(h = 0)
plot(log(takers), res, xlab = "Log of Percent of Students Tested", ylab = "Residuals", pch = 19)
abline(h = 0)
plot(rank, res, xlab = "Median Class Ranking Percentile", ylab = "Residuals", pch = 19)
abline(h = 0)
hist(res, xlab="Residuals", main= "Histogram of Residuals")
qqnorm(res)
qqline(res)
plot(cook,type="h",lwd=3,col="red", ylab = "Cook’s Distance")

