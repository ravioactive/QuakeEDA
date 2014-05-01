setwd("A:\\Subjects\\dic\\r\\Programs\\data_file_main_set")
data <- read.csv(file="jan2013.csv",fill=T)

install.packages("rworldmap")
install.packages("rworldxtra")
install.packages("ggmap")
install.packages("sqldf")
install.packages("doBy")
install.packages("ggplot2")

mag = table(data$mag)
barplot(data$mag)
barplot(mag)

data$magnitude_group <-cut(data$mag,c(-Inf,2,4,6,8,Inf))
summary(data)

library("doBy")
siterange <- function(x){c(length(x), min(x), mean(x), max(x), sd(x), var(x))}
summaryBy(mag~magnitude_group, data =data, FUN=siterange)

library(ggplot2)
ggplot(data, aes(x=mag, fill=magnitude_group))+geom_histogram(binwidth=1)
ggplot(data, aes(x=mag, y=depth, fill=magnitude_group))+geom_boxplot()

data.features = table(data$magnitude_group, data$depth)
result = kmeans(data.features,3)
result
result$size
result$cluster

map <- get_map(zoom = 2)
ggmap(map) + geom_plot(data = data, aes(x =data$longitude, y= data$latitude))

library(rworldmap)
library(rworldxtra)
library(ggmap)
library(sqldf)
library(doBy)
library(ggplot2)

files <- list.files()
dataList <- list()
dataList[[1]] <- read.csv(file="jan2013.csv")
dataList[[2]] <- read.csv(file="feb2013.csv")
dataList[[3]] <- read.csv(file="mar2013.csv")
dataList[[4]] <- read.csv(file="apr2013.csv")
dataList[[5]] <- read.csv(file="may2013.csv")
dataList[[6]] <- read.csv(file="jun2013.csv")
dataList[[7]] <- read.csv(file="jul2013.csv")
dataList[[8]] <- read.csv(file="aug2013.csv")
dataList[[9]] <- read.csv(file="sep2013.csv")
dataList[[10]] <- read.csv(file="oct2013.csv")
dataList[[11]] <- read.csv(file="nov2013.csv")
dataList[[12]] <- read.csv(file="dec2013.csv")

no_earthquakes<-seq(length(dataList))
max_magnitude<-seq(length(dataList))
for(i in 1:length(dataList)){
  no_earthquakes[[i]] <- length(dataList[[i]]$mag)
  max_magnitude[[i]] <- max(dataList[[i]]$mag)
}
barplot(no_earthquakes, main="Earthquakes for Each month in 2013", xlab="Month", ylab="No of Earthquakes")
months<-seq(length(no_earthquakes))
ggplot()+geom_line(aes(x=months,y=no_earthquakes))+geom_point(aes(x=months,y=no_earthquakes))
ggplot()+geom_line(aes(x=months,y=max_magnitude))+geom_point(aes(x=months,y=max_magnitude))

map<- getMap(resolution = "low")
plot(map, asp = 1)
for (i in 1:length(dataList)){
  points(dataList[[i]]$longitude, dataList[[i]]$latitude, col = "red", cex = .6)
}

