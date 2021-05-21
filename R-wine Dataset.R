wine<-read.csv("C:/Mba notes/novels/winequalityN.csv",stringsAsFactors = F)
View(wine)
sum(is.na(wine))
colSums(is.na(wine))
barplot(table(wine$quality))
boxplot(wine$quality)
wine$taste<-ifelse(wine$quality<6,"bad","good")
wine$taste[wine$quality==6]<-"normal"
wine$taste<-as.factor(wine$taste)
table(wine$taste)
wine1<-na.omit(wine)
wine1

library(caret)
set.seed(123)
samp<-sample(nrow(wine1),0.8*nrow(wine1))
train<-wine1[samp,]
test<-wine1[-samp,]
library(randomForest)
model<-randomForest(taste~.-quality,data=train)
model
pred<-predict(model,test)
conf<-confusionMatrix(pred,test$taste)
print(conf)
imp<-varImp(model)
table(pred,test$taste)
plot(importance)
