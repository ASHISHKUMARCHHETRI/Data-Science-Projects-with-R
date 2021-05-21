setwd("C:/Mba notes/novels/project_predictions")
titanic.train<-read.csv("C:/Mba notes/novels/project data/titanic_train.csv",header=T,stringsAsFactors = F)
titanic.train
tail(titanic.train)
head(titanic.train)
titanic.test<-read.csv("C:/Mba notes/novels/project data/titanic_test.csv",header=T,stringsAsFactors = F)
head(titanic.test)
str(titanic.test)
median(titanic.train$Age,na.rm=T)
median(titanic.test$Age,na.rm=T)
titanic.train$IsTrainSet<-TRUE
tail(titanic.train)
titanic.test$IsTrainSet<-F
ncol(titanic.train)
ncol(titanic.test)
names(titanic.train)
names(titanic.test)
titanic.test$Survived<-NA
ncol(titanic.test)
names(titanic.test)
titanic.full<-rbind(titanic.train,titanic.test)
ncol(titanic.full)
nrow(titanic.full)
head(titanic.full)
tail(titanic.full)
table(titanic.full$IsTrainSet)
table(titanic.full$Embarked)
#categorical casting
titanic.full$Pclass<-as.factor(titanic.full$Pclass)
titanic.full$Sex<-as.factor(titanic.full$Sex)
titanic.full$Embarked<-as.factor(titanic.full$Embarked)
titanic.full[is.na(titanic.full$Fare),"Fare"]
#clean misssing values of fare
#removing outliers
boxplot(titanic.full$Fare)
boxplot.stats(titanic.full$Fare)
boxplot.stats(titanic.full$Fare)$stats
upper.whisker<-boxplot.stats(titanic.full$Fare)$stats[5]
outlier.filter<-titanic.full$Fare<upper.whisker
titanic.full[outlier.filter,]
fare.equation="Fare~Pclass + Sex + Age + SibSp + Parch + Embarked"
fare.model<-lm(
  formula=fare.equation,
  data=titanic.full[outlier.filter,]
)



titanic.full$Embarked==""
titanic.full[titanic.full$Embarked=="","Embarked"]<-"S"
table(titanic.full$Embarked)
table(is.na(titanic.full$Age))
age.median<-median(titanic.full$Age,na.rm=T)
titanic.full[is.na(titanic.full$Age),"Age"]<-age.median
table(is.na(titanic.full$Age))
table(is.na(titanic.full$Fare))
fare.median<-median(titanic.full$Fare,na.rm=T)
titanic.full[is.na(titanic.full$Fare),"Fare"]<-fare.median
#

table(is.na(titanic.full$Fare))
titanic.full$Pclass<-as.factor(titanic.full$Pclass)
titanic.full$Sex<-as.factor(titanic.full$Sex)
titanic.full$Embarked<-as.factor(titanic.full$Embarked)

titanic.train<-titanic.full[titanic.full$IsTrainSet==T,]

str(titanic.train)
titanic.test<-titanic.full[titanic.full$IsTrainSet==F,]
str(titanic.test)
titanic.train$Survived<-as.factor(titanic.train$Survived)
survived.equation<-"Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula<-as.formula(survived.equation)
install.packages("randomForest")
library(randomForest)
titanic.model<-randomForest(formula=survived.formula,data=titanic.train,ntree=500,mtry=3,nodesize=0.01*nrow(titanic.test))
features.equation<- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived<-predict(titanic.model,newdata=titanic.test)
PassengerId<-titanic.test$PassengerId
output=data.frame(PassengerId,Survived)
output
write.csv(output,file="kaggle_titanic_submission.csv",row.names = F)

