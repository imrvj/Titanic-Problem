data.train<-read.csv('titanic_train.csv')
head(data.train)
colnames(data.train)
str(data.train)


#Data Analysis Using Ameliya
library(Amelia)
missmap(data.train,main="Missing Map 1",col = c('yellow','black'),legend = FALSE)

library(ggplot2)
#Survivors
pl<-ggplot(data.train,aes(Survived))+geom_bar()
print(pl)

#PClass
pl2<-ggplot(data.train,aes(Pclass))+geom_bar(aes(fill=factor(Pclass)))
print(pl2)

#Gender
pl3<-ggplot(data.train,aes(Sex))+geom_bar(aes(fill=factor(Sex)))
print(pl3)

#Age
pl4<-ggplot(data.train,aes(Age))+geom_histogram(alpha=.5,fill='blue',bins = 20)
print(pl4)

#SiblingsCOunt
pl5<-ggplot(data.train,aes(SibSp))+geom_bar(fill='red')
print(pl5)

#Fare
pl6<-ggplot(data.train,aes(Fare))+geom_histogram(bins = 20,alpha=.5,fill='Green',color='Black')
print(pl6)

#Class VS Age
pl7<-ggplot(data.train,aes(Pclass,Age))+geom_boxplot(aes(group=Pclass,fill=factor(Pclass),alpha=.4))+scale_y_continuous(breaks = seq(min(0),max(80),by=2))+theme_bw()
print(pl7)


#Imputation of age Based on class
impute_age <- function(age,class){
  out <- age
  for (i in 1:length(age)){
    
    if (is.na(age[i])){
      
      if (class[i] == 1){
        out[i] <- 37
        
      }else if (class[i] == 2){
        out[i] <- 29
        
      }else{
        out[i] <- 24
      }
    }else{
      out[i]<-age[i]
    }
  }
  return(out)
}
########
fixed.ages <- impute_age(data.train$Age,data.train$Pclass)
print(fixed.ages)

data.train$Age <- fixed.ages

missmap(data.train,main='Imputation Check' ,col=c('yellow','black'),legend = FALSE)


##building Logistic Model
str(data.train)
head(data.train,3)

##select relevant coloum only
library(dplyr)
str(data.train)

data.train<-select(data.train,-PassengerId,-Name,-Ticket,-Cabin)
print(head(data.train))

data.train$Pclass<-factor(data.train$Pclass)
data.train$Parch<-factor(data.train$Parch)
data.train$Survived<-factor(data.train$Survived)
data.train$SibSp<-factor(data.train$SibSp)

str(data.train)

#Logistic Model
model<-glm(Survived~.,family = binomial(link = 'logit'),data = data.train)
summary(model)


##Test Set
install.packages('caTools')
library(caTools)
set.seed(101)
split<-sample.split(data.train$Survived,SplitRatio = 0.7)
final.train<-subset(data.train,split==TRUE)
final.test<-subset(data.train,split==FALSE)
final.log.model<-glm(Survived~.,family=binomial(link = 'logit'),data=final.train)
summary(final.log.model)                     


fitted.probabilities<-predict(final.log.model,final.test,type = 'response')
fitted.results<-ifelse(fitted.probabilities>0.5,1,0)
misClassError <-mean(fitted.results != final.test$Survived)
print(1- misClassError)

#Confusion Matrix
table(final.test$Survived,fitted.probabilities>0.5)
