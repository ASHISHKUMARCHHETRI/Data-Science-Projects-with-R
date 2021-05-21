library(pROC)
library(randomForest)
set.seed(300)
num.samples<-200
weight<-sort(rnorm(n=num.samples,mean = 172,sd=29))
obese<-ifelse(test = (runif(n=num.samples)<(rank(weight)/200)),yes = 1,no=0)
obese
plot(x=weight,y=obese,col="green")
glm.fit=glm(obese~weight,family=binomial)
lines(weight,glm.fit$fitted.values)
roc(obese,glm.fit$fitted.values,plot=T)
par(pty="s")
roc(obese,glm.fit$fitted.values,plot=T,legacy.axes=T,percent=T,xlab="Faslse Positive Percentage",
    ylab="True Positive Percentage",col="red",lwd=4)
roc.info<-roc(obese,glm.fit$fitted.values,legacy.axes=T)
roc.df<-data.frame(
  tpp=roc.info$sensitivities*100,
  fpp=(1-roc.info$specificities)*100,
  thresholds=roc.info$thresholds
)
head(roc.df)
tail(roc.df)

roc.df[roc.df$tpp>60 & roc.df$tpp<80,]
roc(obese,glm.fit$fitted.values,plot=T,legacy.axes=T,percent=T,xlab="Faslse Positive Percentage",
    ylab="True Positive Percentage",col="red",lwd=4,print.auc=T,print.auc.x=45,partial.auc=c(100,90),
    auc.polygon=T,auc.polygon.col="green")

rf.model<-randomForest(factor(obese)~weight)
roc(obese,glm.fit$fitted.values,plot=T,legacy.axes=T,percent=T,xlab="Faslse Positive Percentage",
    ylab="True Positive Percentage",col="red",lwd=4,print.auc=T)

plot.roc(obese,rf.model$votes[,1],percent=T,col="green",lwd=4,print.auc=T,add=T,print.auc.y=40)
legend("bottomright",legend=c("Logistic Regression","Random Forest"),col=c("red","green"),lwd=4)
par(pty="m")












