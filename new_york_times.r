setwd("A:\\Subjects\\dic\\r\\Programs\\data_files_nyt")
data1 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt1.csv"))


head(data1)
data1$age_group <-cut(data1$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
summary(data1)

install.packages("doBy")
library("doBy")
siterange <- function(x){c(length(x), min(x), mean(x), max(x))}
summaryBy(Age~age_group, data =data1, FUN=siterange)
summaryBy(Gender+Signed_In+Impressions+Clicks~age_group,data =data1)


install.packages("ggplot2")
library(ggplot2)
ggplot(data1, aes(x=Impressions, fill=age_group))+geom_histogram(binwidth=1)
ggplot(data1, aes(x=age_group, y=Impressions, fill=age_group))+geom_boxplot()

#2a
data1$hasimps <-cut(data1$Impressions,c(-Inf,0,Inf))
summaryBy(Clicks~hasimps, data =data1, FUN=siterange)
ggplot(subset(data1, Impressions>0), aes(x=Clicks/Impressions,colour=age_group)) + geom_density()
ggplot(subset(data1, Clicks>0), aes(x=Clicks/Impressions,colour=age_group)) + geom_density()
ggplot(subset(data1, Clicks>0), aes(x=age_group, y=Clicks,fill=age_group)) + geom_boxplot()
ggplot(subset(data1, Clicks>0), aes(x=Clicks, colour=age_group))+ geom_density()


data1$scode[data1$Impressions==0] <- "NoImps"
data1$scode[data1$Impressions >0] <- "Imps"
data1$scode[data1$Clicks >0] <- "Clicks"

data1$scode <- factor(data1$scode)
head(data1)

clen <- function(x){c(length(x))}
etable<-summaryBy(Impressions~scode+Gender+age_group,data = data1, FUN=clen)
head(etable)

ggplot(subset(data1,data1$Age<18),aes(x=Gender))+geom_density()
ggplot(data1,aes(x= Signed_In))+geom_density()


#data which calculates gender and age less than 18
genderDataPercent <- table(subset(data1,data1$Age<18)$Gender)/length(subset(data1,data1$Age<18)$Gender)
barplot(genderDataPercent, main="Gender with age less than 18", xlab="Gender", ylab="%")
pie(genderDataPercent, main="Chart for gender with age less than 18")

#data which calculates gender and age less than 18 and signed in users
genderDataPercent <- table(subset(data1,data1$Age<18 & data1$Signed_In == 1)$Gender)/length(subset(data1,data1$Age<18 & data1$Signed_In == 1)$Gender)
barplot(genderDataPercent, main="Gender with age less than 18 and signed_in users", xlab="Gender", ylab="%")
pie(genderDataPercent, main="Chart for gender with age less than 18 and signed_in users")

#data which calculates gender with all age group
totalGenderDataPercent <- table(data1$Gender)/length(data1$Gender)
barplot(totalGenderDataPercent, main="Gender with all age group", xlab="Gender", ylab="%")
pie(totalGenderDataPercent, main="Chart for gender with all age group")

#data which calculates gender with all age group and signed in users
totalGenderDataPercent <- table(subset(data1,data1$Signed_In == 1)$Gender)/length(subset(data1,data1$Signed_In == 1)$Gender)
barplot(totalGenderDataPercent, main="Gender with all age group and signed_in users", xlab="Gender", ylab="%")
pie(totalGenderDataPercent, main="Chart for gender with all age group and signed_in users")



data2 <- summaryBy(Impressions+Clicks~Gender+age_group,data=data1,FUN=c(var,sum, length,sd,mean))
data2$CTR[data2$Impressions.sum==0] <- 0
data2$CTR[data2$Impressions.sum>0] <- data2$Clicks.sum/data2$Impressions.sum
data2$CTR <- factor(data2$CTR)
data2

files <- list.files()
dataList <- list()
for (i in 1:length(files)){
  dataList[[i]] <- read.csv(files[[i]])
}
CTR_DAY<-seq(length(dataList))
for(i in 1:length(dataList)){
  CTR_DAY[[i]] <- (sum(dataList[[i]]["Clicks"])/sum(dataList[[i]]["Impressions"]))*100
}
barplot(CTR_DAY, main="CTR values per DAY", xlab="DAYS", ylab="CTR")
days<-seq(length(CTR_DAY))
CTR<-cut(CTR_DAY,c(0,1,2,Inf))
ggplot()+geom_line(aes(x=days,y=CTR_DAY))+geom_point(aes(x=days,y=CTR_DAY, fill=CTR))

