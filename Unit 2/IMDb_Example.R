## Read data using read.csv 
data = read.csv("training.csv",header=TRUE,sep=",")
## check data
data[1,]  
## how many observations?
dim(data)[1]

## Response Variable: scaled between 0 and 100
imdb=data$imdb*10

## Predictors: Quantitative or Qualitative?

## Quantitative Predicting Variables
# Number of imdb user votes for the movie
votes=data$votes
# The duration of the movie
duration=data$time
# Gross earnings in 1000's
earnings = data$boxoffice/1000
# Total budget in millions
production=data$productionbudget

## Exploratory Data Analysis for Quantitative Data
# Scatter plot matrix
pqmat = as.data.frame(cbind(imdb, votes,duration,earnings,production))
plot(pqmat)


## Qualitative Predicting Variables
# Year: If data are observed over multiple years, quantitative; otw qualitative
table(data$year)
# We have 102 obsevations for 5 years: consider it be a qualitative variable  
year=as.factor(data$year)
# Rating of a film's suitability for certain audiences, based on its content
rating=as.factor(data$rating)
# English (1) and Other languages (0)
language=data$language
language[language==1] = "English"
language[language==0] = "Other"
language = as.factor(language)
# Film's genre
genre=data$genre
genre[genre==1]= "Action"
genre[genre==2]= "Docuementary"
genre[genre==3]= "Comedy"
genre[genre==4]= "Horror, Sci-Fi"
genre = as.factor(genre)
# Director Rating
rtdirector=data$directorrating
rtdirector[rtdirector==1]="Awarded"
rtdirector[rtdirector==2]="Nominated"
rtdirector[rtdirector==3]="None"
rtdirector = as.factor(rtdirector)
## Actor Rating
rtactor=as.factor(data$actorrating)
# Movie Awards: Avarded (1), Nominated (2), None (3)
awards=data$movieaward
awards[awards==1]="Awarded"
awards[awards==2]="Nominated"
awards[awards==3]="None"

## ## Exploratory Data Analysis for Qualitative Data 
par(mfrow=c(2,3))
boxplot(imdb~year,col="blue",main="Year")
boxplot(imdb~rating,col="red",main="Rating for Audience")
boxplot(imdb~language,col="green",main="Language")
boxplot(imdb~genre,col="purple",main="Genre")
boxplot(imdb~rtdirector,col="purple",main="Director Awards")
boxplot(imdb~rtactor,col="grey",main="Actor Performance Rating")
#boxplot(imdb~awards,col="black",main="Awards")


## Correlation and Multicolinearity
## Quantitative data
round(cor(pqmat),2)
## Qualitative data & the response
summary(aov(imdb~year))
## Qualitative predicting variables
table(rtdirector,awards)
chisq.test(rtdirector,awards)

## Fit a Linear Regression Model	
fit=lm(imdb ~ votes+duration+earnings+production+year+rating+language+
genre+rtdirector+rtactor+awards)
summary(fit)

#Residuals
par(mfrow = c(2,3))
plot(votes,residuals(fit),xlab="No of Votes", ylab="Residuals")
abline(h=0,col="red")
plot(duration,residuals(fit),xlab="Movie Run Time", ylab="Residuals")
abline(h=0,col="red")
plot(production,residuals(fit),xlab="Production Budget", ylab="Residuals")
abline(h=0,col="red")
plot(earnings,residuals(fit),xlab="Box Office Earnings", ylab="Residuals")
abline(h=0,col="red")
hist(residuals(fit),xlab="Residuals",main="Histogram of residuals",col="blue")
qqnorm(residuals(fit))
qqline(residuals(fit),col="blue")



## Coding Dummy Variables
genre  = data$genre
genre.1 = rep(0,length(genre))
genre.1[genre==1] = 1
genre.2 = rep(0,length(genre))
genre.2[genre==2] = 1
genre.3 = rep(0,length(genre))
genre.3[genre==3] = 1
genre.4 = rep(0,length(genre))
genre.4[genre==4] = 1
## Include all dummy variables without intercept
fit.1 = lm(imdb~genre.1+ genre.2+ genre.3+ genre.4-1)
## Include 3 dummy variables with intercept
fit.2 = lm(imdb~genre.1+ genre.2+ genre.3)
## 
genre  = as.factor(data$genre)
fit.3=lm(imdb~genre)
 
 
 
## Marginal versus Conditional Modeling 
summary(aov(imdb~rtdirector))
summary(lm(imdb ~ duration))
summary(lm(imdb ~ earnings))


############## Prediction #########################################
## How well can we predict ranking given the movie attributes considered in this study?
testdat = read.csv("Testing.csv",header=TRUE,sep=",")
dim(testdat)[1]

## Response Variable: scaled between 0 and 100
nimdb=testdat$imdb*10

## Predictors: Quantitative or Qualitative?

## Quantitative Predicting Variables
# Number of imdb user votes for the movie
nvotes=testdat$votes
# The duration of the movie
nduration=testdat$time
# Gross earnings in 1000's
nearnings = testdat$boxoffice/1000
# Total budget in millions
nproduction=testdat$productionbudget
## Qualitative Predicting Variables  
nyear=as.factor(testdat$year)
# Rating of a film's suitability for certain audiences, based on its content
nrating=as.factor(testdat$rating)
# English (1) and Other languages (0)
nlanguage=testdat$language
nlanguage[nlanguage==1] = "English"
nlanguage[nlanguage==0] = "Other"
nlanguage = as.factor(nlanguage)
# Film's genre
ngenre=testdat$genre
ngenre[ngenre==1]= "Action"
ngenre[ngenre==2]= "Docuementary"
ngenre[ngenre==3]= "Comedy"
ngenre[ngenre==4]= "Horror, Sci-Fi"
ngenre = as.factor(ngenre)
# Director Rating
nrtdirector=testdat$directorrating
nrtdirector[nrtdirector==1]="Awarded"
nrtdirector[nrtdirector==2]="Nominated"
nrtdirector[nrtdirector==3]="None"
nrtdirector = as.factor(nrtdirector)
## Actor Rating
nrtactor=as.factor(testdat$actorrating)
# Movie Awards: Avarded (1), Nominated (2), None (3)
nawards=testdat$movieaward
nawards[nawards==1]="Awarded"
nawards[nawards==2]="Nominated"
nawards[nawards==3]="None"

fit=lm(imdb ~ votes+duration+earnings+production+rating+language+
genre+rtdirector+rtactor+awards)
newdat = data.frame(votes=nvotes, duration=nduration, earnings=nearnings, production=nproduction, year=nyear, rating=nrating, language=nlanguage, genre=ngenre,rtdirector=nrtdirector, rtactor=nrtactor,awards=nawards)
# Specify whther a confidence or prediction interval
predict(fit,newdat,interval=c("prediction"))
 
predict.testdata = predict(fit,newdat,interval=c("prediction"))
imdb.pred = predict.testdata[,1]
imdb.lwr = predict.testdata[,2]
imdb.upr = predict.testdata[,3]
### Mean Squared Prediction Error
mean((imdb.pred-nimdb)^2)
### Mean Absolute Prediction Error
mean(abs(imdb.pred-nimdb))
### Mean Absolute Percentage Error
mean(abs(imdb.pred-nimdb)/nimdb)
### Precision Measure
sum((imdb.pred-nimdb)^2)/sum((nimdb-mean(nimdb))^2)

### Does the observed data fall in the prediction intervals?
sum(nimdb<imdb.lwr)+sum(nimdb>imdb.upr)


