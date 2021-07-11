---
title: "Boyun-Jang"
Anumber: 20437298
output: html_document
toc: yes
toc_float: yes
author: Boyun Jang
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Exercise 1

##1.1 chapter: 1
1. No, it is select DB query
2. No, it's arithmatical calculation
3. No, it's aggregation calculation
4. No, it's orderby query
5. No, it's combined probability calculation
6. Yes, it's data mining task using regression model or other techniques and time series prediction is possible.
7. Yes, classification or anomaly detection are can be used for data mining task.
8. Yes, the range of waves can be observed by classification model and it can be built by a data mining task.
9. No it's signal processing, not a data mining task

##chapter: 3
a. No, census is pubilc data
b. Yes, IP addresses and visit times is private so it has to be certain user's information.
c. No, this images are public.
d. No
e. No

## 1.2 Tan, Chapter 2 

   chapter: 2
1.Binary, Qualitative, Ordinal
2.Continuous, Quantitative, Ratio
3.Discrete, Qualitatice, Ordinal
4.Continuous, Quantitative, Ratio
5.Discrete, Qualitative, Ordinal
6.Continuous.Quantitative, Ratio
7.Discrete, Quantitative, Ratio
8.Discrete, Qualitative, Nominal
9.Discrete, Qualitative, Ordinal
10.Discrete, Qualitative, Ordinal
11.Continuous, Quantitative, Ratio
12.Discrete, Quantitative, Ratio
13.Discrete, Qualitative, Nominal

##chapter: 3
1. The Boss is right. I would look the costomer satisfacion and total sales count of the product.
2. The count of satisfaction attributes can be different. also, original product satisfaction might not that related to the 

##chapter: 7
 Daily temperature. Because temperature is observed time periodically on time lag then rainfall.

##chapter: 12
1.No, by definition
2.Yes, it can be. 
3.No, not alway.
4.No
5.Yes

#1.3 ISLR
##chapter: 1
The budgets of TV, radio, or newpaper don't affect on sales value.
For the p-value, the p-value tv and radio are significant but newspaper is not. 
so, the null hypothesis of radio can be accepted, and the newpaper ad budget don't affect on sales.

##chapter: 3
a. iii is right answer. 
  Formula; y=50+20*gpa+0.07*IQ+35*gender+0.01*(gpa*IQ)-10(gpa*gender)
  males<-50 + 20*gpa + 0.07*IQ + 0.01(gpa*IQ)
  females<-85 + 10*gpa + 0.07*IQ + 0.01(gpa*IQ)
  since if the gpa is high enough, male earn more on average than females
b.
  85 + 10*4.0 + 0.07*110 + 0.01(110*4.0) = 137.1 which means $137,100
c.
  faulse. Need verification of the hypothesis test on H0:B4=0
  Also, comparing p-vale to verity the interaction term has significant effect or not.
  
##chapter: 4-a
Since X and Y are on linear relation, we might expect linear regression RSS may be lower than cubic regression RSS

# Programming Problems  
## Part 2.1-a
```{r}
setwd('D:/2020 Spring/Data Mining/Ass1')
getwd()
df<-read.csv("College.csv")
library(dplyr)
cols<-c(1,5,8,10)
df[1:5,cols]
```
## Part 2.1-b
```{r}
private<- sum(df$Private=="Yes")
public<-sum(df$Private=="No")
cat("There are",private," private colleges, and", public, "public colleages in the dataset")
```
## Part 2.1-c
```{r}
newdf<-select(df,"Private","Apps","Accept","Enroll","PhD","perc.alumni","S.F.Ratio","Grad.Rate")
head(newdf)
```
## Part 2.1-d
```{r}
privatePhD<-select(filter(newdf,newdf$Private =="Yes"),c("PhD","Private"))
hist(privatePhD$PhD,main = "PhD holders in private colleages",xlab="PhD Holders")
publicPhD<-select(filter(newdf,newdf$Private =="No"),c("PhD","Private"))
hist(publicPhD$PhD,main = "PhD holders in public colleages",xlab="PhD Holders")
hist(privatePhD$PhD,main = "PhD holders in private colleages",xlab="PhD Holders",col=rainbow(7))
hist(publicPhD$PhD,main = "PhD holders in pucblic colleages",xlab="PhD Holders",col=rainbow(7))
```
## Part 2.1-e 
```{r}
df$Name[order(newdf$Grad.Rate)[1:5]]
attach(newdf)
sort_hgr<-newdf[order(Grad.Rate),]
sort_lgr<-newdf[order(-Grad.Rate),]
head(sort_hgr,5)
head(sort_lgr,5)
```
## Part 2.1-f
###i
```{r}
library(psych)
pairs.panels(select(newdf,"PhD","S.F.Ratio","Grad.Rate"))
###ii
  #Which two attributes have the highest correlation? 
    #PhD and Grad.Rate have highest correlationship, 0.31.At the plot, the most-right-upper is the correaltion between PhD and Grad.Rate.
    #It has highest value then others. IN my perspective, we can predict PhD Degree students has high posibility of graduation.
###iii
  #Which two attributes have the lowest correlation? 
    #correlation between S.F.Ratio and Graduation Rate is the lowest among those 3 attrebutes, -0.31. The right-middle polt tells us its negative correlation.
    #Student/faculty ratio does not related to the graduation rate. 
```
## Part 2.1-g
```{r}
boxplot(df$perc.alumni ~ df$Private,data = df, 
        main="Which alumni donate more to their colleges",
        xlab="Private or not", ylab="Donate percentage")
```
## Part 2.1-h
```{r}
###iii
grid(plot(ecdf(df$Expend),main="Cumulative distributed Graph of Expend"))

###i->expend median
median(df$Expend)

###ii-> student percentage=80%
se<-ecdf(df$Expend)
se(df$Expend)
max(select(filter(df,se(df$Expend)<="0.8"),c("Expend")))

```
# 2.2 Regression 1

## Part 2.2 - 8.a
```{r}
library(ISLR)
data(Auto)
model_mh<-lm(mpg~horsepower,data= Auto)
summary(model_mh)
####i Yes, p-value is small so it has a relationship 
####ii we an use RSE, R-square, or RMSE to see how strong this relationship is. It is approximately more than 60%.
####iii The relationship is negative. we can see the coefficients between horsepower and mpg, -0.157845.
####iv 
predict(model_mh,data.frame(horsepower=98))
predict(model_mh,data.frame(horsepower=98),interval="confidence", level = 0.95)
predict(model_mh,data.frame(horsepower=98),interval="prediction", level = 0.95)
```
# Part 2.2 -8.b
```{r}
plot(Auto$horsepower,Auto$mpg,main="mpg & horsepower",
     xlab = "horsepower",
     ylab = "mpg",
     abline(model_mh,col="red"))
```
# Part 2.2 -8.c
```{r}
par(mfrow=c(2,2))
plot(model_mh)
###Comments: On the plots, we can see the outliers under-2, and over 2.
###Also, at the Residuals vs Fitted plot, we can predict this data set is not a linear set.
```
# 2.3 Regression 2
```{r}
set.seed(1122)
index<- sample(1:nrow(Auto),0.95*dim(Auto)[1])
train.df<-Auto[index,]
test.df<-Auto[-index,]
```
## Part 2.3-a
```{r}
data("Auto")
fullmodel<-lm(mpg ~. -name,data=train.df)
summary(fullmodel)
###i)
###name is not numeric, for prediction, name is not related to the other attributes
```
```{r}
###ii print the summary of the regression model, and how well the model fits the data R-square,rse,rmse.(print out the R2,RSE,RMSE)
n <- dim(train.df)[1]
p <- dim(train.df)[2] - 1
RSS_fm  <- sum((train.df$mpg - fullmodel$fit)^2)
RSE_fm  <- sqrt(1/(n-p-1)*RSS_fm)
R.sq_fm <- cor(train.df$mpg, fullmodel$fit)^2
RSS_fm
RSE_fm
R.sq_fm
```
```{r}
###iii
plot(fullmodel$fitted.values,fullmodel$residuals,xlab = "Fitted Model",ylab="Residuals",main = "Residual of the model",abline(0,0))
plot(fullmodel)
```
```{r}
###iv compare to the Gaussian distribution, bell curve
hist(fullmodel$residuals,main="Histogram of Residuals of the Model1",xlab="Residuals")  
#### Comments: it follows the normal distribution. This histogram is looks bell curve which means well distributed, the middle value is on the 0. 
```
# Part 2.3-b
```{r}
###i find significant 3 prdictors. I chose 3 lowest p-value attributes

model1<-lm(mpg ~ year+weight+origin,data=train.df)
summary(model1)
```
```{r}
###ii print the summary, and how well the model fits the data R-square,rse,rmse.
  ####(print out the R2,RSE,RMSE)
RSS_m1  <- sum((train.df$mpg - model1$fit)^2)
RSE_m1  <- sqrt(1/(n-p-1)*RSS_m1)
R.sq_m1 <- cor(train.df$mpg, model1$fit)^2
RSS_m1
RSE_m1
R.sq_m1
```
```{r}
###iii
plot(model1$fitted.values,model1$residuals,xlab = "Fitted Model",ylab="Residuals",main = "Residual of Mymodel")
```
```{r}
###iv 
hist(model1$residuals,main="Histogram of Residuals of the Model1",xlab="Residuals")
###Comments: compare to the Gaussian distribution, this histogram also follows bell curve. the residuals are well distributed
```
```{r}
###v compare model a and model b
summary(fullmodel)
summary(model1)
###when I compare to the R-squared value, the R-squred vlaue of first model, all attributes are included except name, is higher then 3 attributes model.
###It means the fullmodel(except name attribute), full model is better.
```
# Part 2.3 -d
```{r}
prediction <- predict.lm(model1, test.df , interval="prediction", level=0.95, datatype="numeric")
prediction

####new data frame by test dataset
verify_prediction <- data.frame(Prediction=as.integer(prediction[,1]), Respons=test.df$mpg)

####put test response vars as 2nd col vector in new data frame
match_check <- verify_prediction$Prediction == verify_prediction$Respons
match_check
sprintf("Number of exact match with predicted to actual %i", sum(match_check))
```
# Part 2.3 -e
```{r}
prediction <- data.frame(predict.lm(model1, test.df , interval="prediction", level=0.95, datatype="numeric"))
prediction
verify_prediction$Lower<-prediction$lwr
verify_prediction$Upper<-prediction$upr
verify_prediction$Matches<-as.integer(match_check)
verify_prediction
cat("Total observations correctly predicted:",sum(verify_prediction$Matches))
```
