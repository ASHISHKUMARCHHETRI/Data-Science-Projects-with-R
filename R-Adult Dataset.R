adult<-read.csv("C:/Mba notes/novels/adult.csv",stringsAsFactors = T)
str(adult)
sum(is.na(adult))
table(adult$workclass)
adult$workclass<-as.character(adult$workclass)
View(adult)
adult$workclass[adult$workclass=="Without-pay" | adult$workclass =="Never-worked"]<-"unemployed"
adult$workclass[adult$workclass=="State-gov" | adult$workclass=="Local-gov"]<-"SL-gov"
adult$workclass[adult$workclass=="Self-emp-inc" | adult$workclass=="Self-emp-not-inc"]<-"self-employed"
table(adult$workclass)
table(adult$marital.status)
adult$marital.status[adult$marital.status=="Married-AF-spouse" |
                       adult$martial.status=="Married-civ-spouse" |
                       adult$marital.status=="Married-spouse-absent"]<- "Married"
adult$marital.status[adult$marital.status=="Never-married" |
                       adult$martial.status=="Separated" |
                       adult$marital.status=="Widowed"]<- "Unmarried"
table(adult$marital.status)
adult[adult=="?"]<-NA
table(adult$workclass)
sum(is.na(adult))
adult1<-na.omit(adult)
library("ggplot2")
ggplot(adult1,aes(age)) + geom_histogram(aes(fill=income),color="green")
ggplot(adult,aes(hours.per.week)) + geom_histogram()
