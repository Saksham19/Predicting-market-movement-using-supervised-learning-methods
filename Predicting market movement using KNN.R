#using K-nearest neighbors to predict market movement
#knn uses distance formula to calculate the distance between different data points
#do standardize the data to get better outcomes
library(ISLR)
attach(Smarket)
train<-(Year<2005)
Smarket.2005<-Smarket[!train,]
Direction.2005<-Direction[!train]
#to use knn - class library
library(class)
train.X<-cbind(Lag1,Lag2)[train,]
test.X<-cbind(Lag1,Lag2)[!train,]
train.Direction<-Direction[train]
#set a random seed to ensure replication of results
set.seed(1)
knn.pred<-knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)
#k=1 is not good enough, trying for k=3
knn.pred<-knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)
#roughly 54% accurate. not bad.
