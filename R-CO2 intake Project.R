library(help=datasets)
data(CO2)
df1<-CO2
head(df1)
View(df1)
summary(df1)
summary(df1$Type)
sum(is.na(df1))
library(skimr)
df1 %>%
  dplyr::group_by(Type) %>%
  skim()
plot(df1,col="orange")
plot(df1$conc,df1$uptake)
plot(df1$Type,df1$uptake)  
hist(df1$uptake)  
hist(df1$conc) 
library(caret)  
featurePlot(x=df1,
            y=df1$uptake,
            plot="box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales=list(x=list(relation="free"),y=list(relation="free")))
set.seed(50)
training<-createDataPartition(df1$Type,p=0.8,list=F)
sum(is.na(df1))  
trainingset=df1[training,]
testingset=df1[-training,]
View(trainingset)  
View(testingset)  
plot(trainingset,col="red")  
model=train(Type~ .,data=trainingset,
            method="svmPoly")
pred<-predict(model,trainingset)
pred1<-predict(model,testingset)
conf<-confusionMatrix(pred,trainingset$Type)
conf1<-confusionMatrix(pred1,testingset$Type)
conf1
model.cv<-train(Type~ .,data=trainingset,method="svmPoly",trcontrol=trainControl(method="cv"))
pred2<-predict(model.cv,trainingset)
conf2<-confusionMatrix(pred2,trainingset$Type)
conf2
summary(conf2)
importance<-varImp(model)
plot(importance,top=2)
iuplot(importance)
table(df1$Type)
library(randomForest)
head(CO2)
formula<-"Type~Treatment+conc+Plant+uptake"
equation<-as.formula(formula)
model.r<-randomForest(equation,data=trainingset)
pred3<-predict(model.r,trainingset)
conf3<-confusionMatrix(pred3,trainingset$Type)
conf3











