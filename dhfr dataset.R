data(Titanic)
df2<-Titanic
View(Titanic)
sum(is.na(df2))
data(dhfr)
df3<-dhfr
View(dhfr)
factor(dhfr$Y)
table(dhfr$Y)
skim(dhfr)
dhfr %>%
  dplyr::group_by(Y) %>%
  skim()
featurePlot(x=dhfr[,2:16],
            y=dhfr$Y,
            plot="box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales=list(x=list(relation="free"),
                        y=list(relation="free")))
table(is.na(dhfr))
sample(1:nrow(dhfr),4)
na.gen<-function(data,n){
  i<- 1
  while(i<n+1){
    idx1<-sample(1:nrow(data),1)
    idx2<-sample(1:ncol(data),1)
    data[idx1,idx2]<-NA
    i=i+1
  }
  return(data)
}
dhfr<-na.gen(dhfr,100)
sum(is.na(dhfr))
colSums(is.na(dhfr))
str(dhfr)
missingdata<-dhfr[!complete.cases(dhfr),]
sum(is.na(missingdata))
View(missingdata)
clean.data<-na.omit(dhfr)
clean.data
sum(is.na(dhfr))
sum(is.na(clean.data))
dhfr.impute<-dhfr
for (i in which(sapply(dhfr.impute,is.numeric))){
  dhfr.impute[is.na(dhfr.impute[,i]),i]<-median(dhfr.impute[,i],na.rm=T)
}
sum(is.na(dhfr.impute))
trainingIndex<-createDataPartition(dhfr$Y,p=0.8,list=F)
Training<-dhfr[trainingIndex,]
Testing<-dhfr[-trainingIndex,]
library(randomForest)
head(dhfr)
formula<-"Y~dhfr[-Y]"
equation<-as.formula(formula)
sum(is.na(dhfr))
sum(is.na(Training))
for (i in which(sapply(Training,is.numeric))){
  Training[is.na(Training[,i]),i]<-mean(Training[,i],na.rm = T)
}
sum(is.na(Testing))
for (i in which(sapply(Testing,is.numeric))){
  Testing[is.na(Testing[,i]),i]<-mean(Testing[,i],na.rm = T)
}
sum(is.na(Testing))
sum(is.na(Training))
model<-train(Y~.,data = Training,method="svmPoly")
pred<-predict(model,Testing)
conf<-confusionMatrix(pred,Testing$Y)
length((pred))
length(Testing$Y)
######## 
install.packages("mlbench")
library(mlbench)
library(caret)
data(BostonHousing)
head(BostonHousing)
sum(is.na(BostonHousing))
set.seed(100)
TrainingIndex<-createDataPartition(BostonHousing$medv,p=0.8,list=F)
TrainingSet<-BostonHousing[TrainingIndex,]
TestingSet<-BostonHousing[-TrainingIndex,]
Model<-train(medv~.,data = TrainingSet,
             method="lm",
             na.action = na.omit,
             preProcess=c("scale","center"),
             trcontrol=trainControl(method="none"))
pred1<-predict(Model,TrainingSet)
pred2<-predict(Model,TestingSet)
conf1<-confusionMatrix(pred1,TrainingSet$medv)
length(pred1)
length(TrainingSet$medv)
plot(TrainingSet$medv,pred1,col="blue")
plot(TestingSet$medv,pred2,col="red")
summary(Model)
R.training<-cor(TrainingSet$medv,pred1)
R.testing<-cor(TestingSet$medv,pred2)
R2.training<-R.training^2
R2.testing<-R.testing^2
