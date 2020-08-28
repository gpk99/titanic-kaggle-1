train=read.csv(file.choose())
train<-train[,c(-4,-9,-11)]
mis<-missForest(train)
train<-mis$ximp
library(missForest)
missForest(train)
train$Sex<-factor(train$Sex,levels = c("male","female"),labels = c(0,1))
train$Embarked<-factor(train$Embarked,levels = c("S","Q","C"),labels = c(1,2,3))
regressor<-lm(formula = Survived~Pclass+Sex+Age+SibSp,data = train)
summary(regressor)

test=read.csv(file.choose())
test<-test[,c(-3,-8,-10)]
str(test)
m<-missForest(test)
test<-m$ximp
test$Sex<-factor(test$Sex,levels =c("male","female"),labels=c(0,1))
test$Embarked<-factor(test$Embarked,levels = c("S","Q","C"),labels = c(1,2,3))
prid<-predict(regressor,newdata = test)
prid

prediction<-data.frame(Passengerid=test$PassengerId,Survived=prid)
prediction$Survived<-sapply(prid,FUN=(function(x) ifelse(x>=0.5,1,0)))
str(prediction)

