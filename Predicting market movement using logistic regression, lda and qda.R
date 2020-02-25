#use lda to predict smarket movement
#smarket data, part of the islr library
#this data set consists of percentage returns for the S&P500 stock incdex over 1250 days (from 2001 to 2005). lag1 to lag5 show the percentage returns for each of the five trading days. Volume is the number of shares traded on previous day, in billions. 
#today is the percentage return on the date in question
#Direction is whether the market was up or down on this date.

library(ISLR)
names(Smarket)
#basic stats about the data:
dim(Smarket)
summary(Smarket)
#to see if there is any correlation between today's returns and previous days' returns:
cor(Smarket[,-9])
#essentially 0 corr between the returns 
#using logistic regression to predict the direction of the returns
attach(Smarket)
glm.fit<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,family=binomial)
summary(glm.fit)
#checking the p values associated with the lag terms
#smallest p value is with lag1. Essentially we can reject the null at level=15%, too high. Essentially useless 
#using predict to well, predict the direction
glm.probs<-predict(glm.fit,type="response")
glm.probs[1:10] #predicting the prob for the first 10 outcomes in this case
contrasts(Direction)
#need to convert the predicted probs into up or down
glm.pred<-rep("Down",1250)
glm.pred[glm.probs>.5]="up"
table(glm.pred,Direction)
mean(glm.pred==Direction)
#our error rate is too high, hence logistic regression not that useful

#now to check for the days for which the info is unknown i.e. using the model to check its actual accuracy in terms of its accuracy
train<-(Year<2005)
Smarket.2005<-Smarket[!train,]
dim(Smarket.2005)
Direction.2005<-Direction[!train]
glm.fit<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,family=binomial,data=Smarket,subset=train)
glm.probs<-predict(glm.fit,Smarket.2005,type="response")
glm.pred<-rep("Down",252)
glm.pred[glm.probs>0.5]="up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
#still not accurate enough, can try and remove variables that are not useful via checking the p values but better to switch to another model
#using linear discriminant analysis to do the same analysis 
library(MASS)
lda.fit<-lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
#using only lag1 and lag2 as they seem to be relevant ones based on the p values
lda.fit 
#the coeffs of linear discriminants formulate the lda decision rule
#the prior prob of groups tells us the chances the days are either up or down
#the group means tell us that if the masket was up, there is a tendency for the previous 2 days' returns to be negative. vice versa also true
lda.pred<-predict(lda.fit,Smarket.2005)
names(lda.pred)
#class is the lda's predictions about the movement of the market. 
#posterior is the matrix that contains posterior prob 
#x is the linear discriminants
lda.class<-lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)
#this model is 55% accurate. slightly better than a random guess!

#Using Quadratic Discriminant analysis to do the same analysis
qda.fit<-qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit
qda.class<-predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)
#this model is almost 60% accurate. maybe worth investing in!
