library(datasets)
data(iris)
iris<-datasets::iris
head(iris,4)
View(iris)
help("datasets")
library(help="datasets")
summary(iris)
summary(iris$Sepal.Length)
sum(is.na(iris))
sum(is.na(iris$Sepal.Length))
install.packages("skimr") 
library(skimr)
skim(iris)
iris %>%
  dplyr::group_by(Species) %>%
  skim()
plot(iris)
plot(iris,col="red")
plot(iris$Sepal.Width,iris$Sepal.Length,col="blue",xlab="Sepal Width",ylab="sepal length",title("Relation"))
hist(iris$Sepal.Width,col = "Yellow")
library(caret)
featurePlot(x=iris[,1:4],
            y=iris$Species,
            plot="box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales=list(x=list(relation="free"),y=list(relation="free")))
set.seed(100)
TrainingIndex<-createDataPartition(iris$Species,p=0.8,list=F)
TrainingSet<-iris[TrainingIndex,]
TestingSet<-iris[-TrainingIndex,]
plot(TrainingSet)
featurePlot(x=TrainingSet[,1:4],
            y=TrainingSet$Species,
            plot="box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales=list(x=list(relation="free"),y=list(relation="free")))
Model<-train(Species ~ .,data=TrainingSet,
             method="svmPoly",
             na.action = na.omit,
             preProcess=c("scale","center"),
             trControl=trainControl(method="none"),
             tuneGrid=data.frame(degree=1,scale=1,C=1))
Model.cv<-train(Species ~ .,data=TrainingSet,
             method="svmPoly",
             na.action = na.omit,
             preProcess=c("scale","center"),
             trControl=trainControl(method="cv",number = 10),
             tuneGrid=data.frame(degree=1,scale=1,C=1))
Model.training<-predict(Model,TrainingSet)
Model.testing<-predict(Model,TestingSet)
Model.cv<-predict(Model.cv,TrainingSet)
Model.training.confusion<-confusionMatrix(Model.training,TrainingSet$Species)
Model.testing.confusion<-confusionMatrix(Model.testing,TestingSet$Species)
Model.cv.confusion<-confusionMatrix(Model.cv,TrainingSet$Species)
print(Model.training.confusion)
print(Model.testing.confusion)
print(Model.cv.confusion)
importance<-varImp(Model)
plot(importance,col="red")

