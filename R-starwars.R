dataset(starwars)
library(tidyverse)
View(starwars)
attach(starwrs)

# Data Cleaning

sum(is.na(starwars))
table(is.na(starwars$hair_color))
colnames(starwars)
duplicated(starwars$name)
df1<-starwars
attach(df1)
sum(is.na(df1))
df2<-subset(df1,select =-c(name,films,vehicles,starships))
df3<-na.omit(df2)
unique(df1$hair_color)
View(df3)
library(dplyr)
a<-starwars %>%
  select(name,height,mass,sex,gender,hair_color) %>%
  filter(hair_color=="blond" | hair_color=="brown") %>%
  group_by(hair_color) 
View(a)
  
library(caret)
Model<-train(species ~ .,data=df3,
             method="svmPoly",
             na.action = na.omit,
             preProcess=c("scale","center"),
             trControl=trainControl(method="none"),
             tuneGrid=data.frame(degree=1,scale=1,C=1))
importance<-varImp(Model)
vtr<-c(10,11,1,12)

#Data Cleaning

# 1. column height

sum(is.na(df1))
sum(is.na(df1$height))
barplot(df1$height)
boxplot(df1$height)
attach(df1)
mean(height,na.rm=T)
median(height,na.rm=TRUE)
(180+174)/2
df1$height[is.na(df1$height)]<-177
sum(is.na(df1$height))

# 2.column mass

sum(is.na(df1$mass))
df1 %>%
  select(gender,mass) %>%
  group_by(gender) %>%
  summarise(Mean=mean(mass,na.rm = T))
df1$mass[is.na(df1$gender<-"masculine")]
df1$mass[is.na(df1$gender<-"feminine")]
   
    
    
    
    















