data=read.csv(("C:/Mba notes/novels/500_Person_Gender_Height_Weight_Index.csv"),header=T,stringsAsFactors=F)
head(data)
equation="Index ~ Gender + Height + Weight"
formula=as.formula(equation)
library(randomForest)
model=randomForest(formula=formula,data=data)
str(data)
data$c(Gender,Index)=as.factor(data$c(Gender,Index))
str(data)
data$Gender=as.factor(data$Gender)
data$Index=as.factor(data$Index)
str(data)
table(data$Gender)
prop.table(table(data$Gender))
prop.table(table(data$Index))
library(carey)
indexes=createDataPartition(data,times=1,p=0.8,list=F)
install.packages("gapminder")
install.packages("caret")
help(install.packages)
library(gapminder)
data("gapminder")
sum(is.na(gapminder))
head(gapminder)
colnames(gapminder)
attach(gapminder)
median(gdpPercap)
hist(lifeExp,col="green")
hist(pop)
hist(log(pop))
boxplot(lifeExp~continent)
plot(lifeExp~log(gdpPercap))
library(dplyr)
gapminder %>%
  select(country,lifeExp) %>%
  filter(country=="South Africa" | country=="Ireland") %>%
  group_by(country) %>%
  summarise(Average_life=mean(lifeExp))
df1<-gapminder %>%
  select(country,lifeExp) %>%
  filter(country=="South Africa" | country=="Ireland")
t.test(data=df1,lifeExp~country)
library(ggplot2) 
gapminder %>%
  filter(gdpPercap<50000) %>%
  ggplot(aes(x=log(gdpPercap),y=lifeExp,col=continent,size=pop))+
  geom_point(alpha=0.3)+
  geom_smooth(method=lm)+
  facet_wrap(~continent)
gapminder %>%
  filter(gdpPercap<50000) %>%
  ggplot(aes(x=log(gdpPercap),y=lifeExp,col=year,size=pop))+
  geom_point(alpha=0.3)+
  geom_smooth(method=lm)+
  facet_wrap(~continent)
lm(lifeExp~log(gdpPercap)+log(pop))
install.packages("tidyverse")
library()
View(starwars)
starwars %>%
  select(name,gender,hair_color,height) %>%
  filter(!complete.cases(.)) %>%
  View()
install.packages("mice")
library(mice)
md.pattern(starwars)
table(colnames(is.na(starwars)))
starwars%>%
  select(name,gender,hair_color,height)%>%
  drop_na(height)%>%
  mutate(gender=replace_na(gender,"none"))
library(caret)
TrainingIndex<-createDataPartition(data$Bmi,p=0.8,list=FALSE)
Train<-data[TrainingIndex,]
Test<-data[-TrainingIndex,]
start.time<-proc.time()
model<-train(Bmi~.,
             data=Train,
             method="rf"
             )
stop.time<-proc.time()
run.time<-stop.time-start.time
print(run.time)
install.packages("doParallel")
install.packages("caret")
library(doParallel)
library(skimr)
library(caret)
cl<-makePSOCKcluster(5)
registerDoParallel(cl)
start.time<-proc.time()
model<-train(Bmi~.,
             data=data,
             method="rf")
end.time<-proc.time()
run.time<-end.time-start.time
print(run.time)
stopCluster(cl)
pred<-predict(model,data)
pred<-round(pred,0)
conf<-confusionMatrix(pred,data$Bmi)
View(pred)
important<-varImp(model)
plot(important)
method<-train(Bmi~.,
              data=data,
              method="rf",
              tuneGrid=data.frame(mtry=seq(5,15,by=5)))
