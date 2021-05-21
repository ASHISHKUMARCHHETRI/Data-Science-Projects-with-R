data=read.csv("C:/Mba notes/novels/heart.csv",header=F,stringsAsFactors = T)
head(data)
colnames(data)<-c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","hd")
head(data)
data=data[-1,]
head(data)
str(data)
data=read.csv("C:/Mba notes/novels/heart.csv",header=T)
head(data)
str(data)
data[data=="?"]<-NA
length(data$sex==0)
table(data$sex)
data[data$sex==0,]$sex<-"F"
data[data$sex==1,]$sex<-"M"
table(data$sex)
data$sex=as.factor(data$sex)
str(data)
data$cp<-as.factor(data$cp)
data$fbs<-as.factor(data$restecg)
data$restecg<-as.factor(data$restecg)
data$exang<-as.factor(data$exang)
data$slope<-as.factor(data$slope)

data$ca<-as.integer(data$ca)
data$ca<-as.factor(data$ca)

data$thal<-as.integer(data$thal)
data$thal<-as.factor(data$thal)

unique(data$target)

data[data$target==0,]$target<-"Unhealthy"
data[data$target==1,]$target<-"Healthy"
data$target<-as.factor(data$target)
str(data)

colnames(data)[1]<-"age"
str(data)
set.seed(42)
h=list()
u=list()
for( i in data$target){
  if(i=="Healthy"){
    h<-append(h,1)
  }
  else{
    h<-append(h,0)
  }
}
library(caret)
index<-createDataPartition(data$target,p=0.8,list)
library(randomForest)

training<-data[index,]
testing<-data[-index,]

View(testing)

data.imputed<-rfImpute(target~.,data = testing,iter=6)

model<-randomForest(target~.,data=training,proximity=T)

pred<-predict(model,testing)
cm<-confusionMatrix(pred,testing$target)
cm
imp<-varImp(model)
plot(imp)
obb.error.data<-data.frame(
  Trees=rep(1:nrow(model$err.rate),times=3),
  Type=rep(c("OOB","Healthy","Unhealthy"),each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"],
  model$err.rate[,"Healthy"],
  model$err.rate[,"Unhealthy"])
)
ggplot(data = obb.error.data,aes(x=Trees,y=Error))+geom_line(aes(color=Type))

oob.values<-vector(length = 10)
for(i in 1:10){
  temp.model<-randomForest(target~.,data=training,mtry=i,ntree=1000)
  oob.values[i]<-temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values
#so mtry=1 is most optimal here
distance.matrix<-dist(1-model$proximity)
mds.stuff<-cmdscale(distance.matrix,eig = T,x.ret=T)
mds.var.per<-round(mds.stuff$eig/sum(mds.stuff$eig)*100,1)
mds.values<-mds.stuff$points
mds.data<-data.frame(Sample=rownames(mds.values),
                     X=mds.values[,1],
                     Y=mds.values[,2],
                     status=training$target)













