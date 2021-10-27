---
title: "Data Mining Assignment 2"
author: "Boyun Jang"
date: "March 6, 2020"
output: html_document
---
#2. Problems
##2.1  Exploratory data analysis on the Hotels Bookings dataset. (2 points)
## (a)
How many observations exist in the dataset for hotel type H1 and hotel type H2? Use R commands to print the frequency of the hotel types.
```{r}
library(plyr)
library(psych)
library(dplyr)
setwd('D:/2020 Spring/Data Mining/Ass2')
df<-read.csv("hotel_bookings.csv")
count(df,'hotel')
cat("City Hotel frequency:",nrow(filter(df,df$hotel=='City Hotel')))
cat("Resort Hotel frequency:",nrow(filter(df,df$hotel=='Resort Hotel')))
```

## (b)
What is the distribution of the class label in the dataset? 

```{r}
cat("Number of guests who canceled the reservation:",nrow(filter(df,df$is_canceled=="1")))
cat("Number of guests who did not cancel the reservation:",nrow(filter(df,df$is_canceled=="0")))

```

## (c)
Which customer has the most reservations?

```{r}
customer_type_count<-count(df,df$customer_type)
max_customer_type<-customer_type_count[customer_type_count$n==max(customer_type_count$n),]
cat("Customer type with the most reservations is",as.character( max_customer_type$`df$customer_type`[1]),", with",max(customer_type_count$n)," reservations")
```

## (d)
what was the most number of parking spaces required by customers? 
And how many customers requested that many parking spaces? 

```{r}
c_parking<-count(df,c(df$required_car_parking_spaces))
c_parking
cat(max(count(df,df$required_car_parking_spaces)),"customers required the most number of parking spaces 0.")
```

## (e)
What was the least number of parking spaces required by customers? And how many customers requested that many parking spaces?

```{r}
a=0 
p_spaces=0
for(i in 2:5){
  a=(as.integer(c_parking[i,1])*as.integer(c_parking[i,2]))
  p_spaces=p_spaces+a }
b=0
park_request=0
for(i in 2:5){
  b=as.integer(c_parking[i,2])
  park_request=park_request+b }

cat(park_request,"customers required the least number of parking spaces",p_spaces)
```

## (f)
How many people who expressed a preference for a particular room type during reservation were actually assigned that specific room type? 

```{r}
a=0
for(i in 1:119390){
  if(as.factor(df$reserved_room_type[i])==as.factor(df$assigned_room_type[i])){
    a <- a+1
  }
}
cat(round(a/119390*100,digits = 2),"% of the people who expressed a room preference during reservation got the
 room during check-in.")
```

## (g)
Order the dataset between city hotels and resort hotels.

```{r}
order_by_hotel <- df[order(df$hotel),]

city_hotel <- filter(order_by_hotel,hotel=='City Hotel',is_canceled==0)
city_hotel
ch_country <- count(city_hotel,country)
ch_country
top10_city <- ch_country[order(-ch_country$n), ][1:10,]
top10_city
barplot(height=top10_city[order(top10_city$n),]$n,names=top10_city[order(top10_city$n),]$country,main = "top 10 countries in City Hotel",col = rainbow(10))

resort_hotel<-filter(df,df$hotel=='Resort Hotel')
r_h_country<- count(resort_hotel,country)
r_h_country<-r_h_country[!r_h_country$country=='NULL',]
r_h_country
top10_resort<-r_h_country[order(-r_h_country$n), ][1:10,]
top10_resort
barplot(height=top10_resort[order(top10_resort$n),]$n,names=top10_resort[order(top10_resort$n),]$country,main = "top 10 countries in Resort Hotel",col = rainbow(10))
```

## (h)
 You will note that the most visitors to either type of the hotels arrive from a specific country.
 (i) Print the name of this country.
(ii) What can you say about the origin of the dataset based on (i)?
```{r}
sprintf("i) PRT has the most visitosrs to either type of hotels. we can see most frequent booking customers country is PRT ")
```

##2.2 Decision Tree
##(a)
 Create the best decision tree model that you can to predict whether a booking will be canceled or not. You
may use as many (or as least) number of predictor variables, it is up to you.
Use a seed of 1122 to randomly assign 90% of the dataset to training, and 10% to testing.
```{r}
library(rpart)
library(rpart.plot)
library(caret)
library(ROCR)
set.seed(1122)
index<- sample(1:nrow(df),0.90*dim(df)[1])
sapply(df,class)
df_trans<-transform(df,
              hotel=as.factor(hotel),
              is_canceled=as.integer(is_canceled),
              lead_time=as.integer(lead_time),
              arrival_date_year=as.integer(arrival_date_year),
              arrival_date_month=as.factor(arrival_date_month),
              arrival_date_week_number=as.integer(arrival_date_week_number),
              arrival_date_day_of_month=as.integer(arrival_date_day_of_month),
              stays_in_weekend_nights=as.integer(stays_in_weekend_nights),
              stays_in_week_nights=as.integer(stays_in_week_nights),
              adults=as.integer(adults),
              children= as.integer(children),
              babies=as.integer(babies),
              meal=as.factor(meal),
              country=as.factor(country),
              market_segment=as.factor(market_segment),
              distribution_channel=as.integer(distribution_channel),
              is_repeated_guest=as.integer(is_repeated_guest),
              previous_cancellations=as.integer(previous_cancellations),
              previous_bookings_not_canceled=as.integer(previous_bookings_not_canceled),
              reserved_room_type =as.factor(reserved_room_type),
              assigned_room_type=as.factor(assigned_room_type),
              booking_changes=as.integer(booking_changes),
              deposit_type=as.factor(deposit_type),
              agent=as.integer(agent),
              company=as.integer(company),
              days_in_waiting_list=as.integer(days_in_waiting_list),
              customer_type=as.factor(customer_type),
              adr=as.integer(adr),
              required_car_parking_spaces=as.integer(required_car_parking_spaces),
              total_of_special_requests=as.integer(total_of_special_requests),
              reservation_status=as.integer(reservation_status),
              reservation_status_date=as.factor(reservation_status_date)
              )
na.omit(df_trans)
train.df<-df_trans[index,]
test.df<-df_trans[-index,]
```
(i) Plot the decision tree.
```{r}
names(train.df)
md1 <- rpart(is_canceled ~ lead_time+deposit_type+previous_cancellations+previous_bookings_not_canceled, method="class", data=train.df)
summary(md1)
rpart.plot(md1, extra="auto",main="Cancellation rate")
```

(ii) List which variables are important.
```{r}
vars_md1<-c("lead_time","deposit_type","previous_cancellations","previous_bookings_not_canceled")
vars_md1
```

(iii) Fit the model on the held out test dataset, and from the resulting confusion matrix, print the following attributes: 
  Accuracy, Error, Balanced Accuracy, Specificity, Sensitivity.
```{r}
test.df[,vars_md1]
md1.pred <- predict(md1, test.df, type="class")
confusionMatrix(md1.pred, as.factor(test.df[,2]))
table_mat <- table(md1.pred,test.df$is_canceled)
cat("Accuracy:",sum(diag(table_mat)) / sum(table_mat),"Specificity:",specificity(table_mat),"Sensitivity:",sensitivity(table_mat))
```  
(iv) Plot a ROC curve on your held out test dataset. (Make sure you use library(ROCR) to use the ROC-specific APIs.)
```{r}
pred.rocr <- predict(md1, newdata=test.df, type="prob")[,2]
pred <- prediction(pred.rocr, test.df$is_canceled)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=T, lwd=3)
```
(v) What is the AUC of the ROC curve.
```{r}
auc <- performance(pred, measure = "auc")
cat(paste("AUC of this model is ", round(auc@y.values[[1]], 3)))
```

##(b)
Examine the complexity parameters for the tree you created in (a). If the tree complexity requires pruning,
then prune the tree.
  (i) Print out the complexity parameters and determine if the tree needs pruning. Please indicate in your answer to (b)(i)
  whether or not the tree needs to be pruned after you examine the complexity parameters associated with your tree.
```{r}
printcp(md1)
sprintf("lowest xerror: 0.63061 and cp is 0.01. We choose cp level which has lowest xerror to prune. so we choose the complexity parameter as 0.01 to prune our model.")
```
  (ii) If you determine that the tree is to be pruned, prune the tree, and using the pruned tree, fit your new (pruned) tree to the
  held-out test dataset. From the resulting confusion matrix, print the following attributes: Accuracy, Error, Balanced
  Accuracy, Specificity, Sensitivity.
```{r}
pruned_md<-prune(md1,cp = 0.01)
prune_md.pred <- predict(pruned_md, test.df, type="class")

confusionMatrix(prune_md.pred, as.factor(test.df[,2]))
table_mat <- table(md1.pred,test.df$is_canceled)
table_mat
cat("Accuracy:",sum(diag(table_mat)) / sum(table_mat),"Specificity:",specificity(table_mat),"Sensitivity:",sensitivity(table_mat))
```
  (iii) For the pruned tree, plot a ROC curve on your held out test dataset and note the AUC of the new ROC curve. (If
  possible, overlay the new ROC curve over the ROC curve in (a)(iv); see the ¡°add¡± parameter to the ROC plot() command.)
```{r}
pred.rocr <- predict(pruned_md, newdata=test.df, type="prob")[,2]
pred <- prediction(pred.rocr, test.df$is_canceled)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=T, lwd=3)
auc <- performance(pred, measure = "auc")
cat(paste("AUC of this model is ", round(auc@y.values[[1]], 3)))
```
  (iv) Comment on the model in (a) and (b) in terms of which model performs better on the held-out dataset.
```{r}
sprintf("If I prune this model, we can choose lowest cp level to prune. So I chose the cp level as 0.01 to prune this model. But the pruned model's sensitivity, specificity, and accuracy were same")
```

