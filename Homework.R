install.packages("zoo")
library(zoo)
library(dplyr)
setwd("D:/R/Robo/Course5")
data<-read.csv("activity.csv", header=TRUE, sep=",")
#data for histogram
hist<-aggregate(data$steps, by=list(data$date), FUN=sum)
#histogram
hist(hist$x, main="Total number of steps taken each day", xlab="Steps")
#mean and median number of steps taken per day
#meanS<-aggregate(data$steps, by=list(data$date), FUN=mean)
meanS<-mean(hist$x, na.rm=TRUE)
abline(v=meanS, col="red")
medianS<-median(hist$x, na.rm=TRUE)
abline(v=medianS, col="blue")
View(meanS)
View(medianS)
#daily activity time series
dataNA<-na.omit(data)
daily<-aggregate(dataNA$steps, by=list(dataNA$interval), FUN=mean)
plot(x=daily[,1], y=daily[,2], type="l", col="blue", main="", xlab="5-minute interval", ylab="Average number of steps taken")
#daily acitivity max steps 5-minute interval
max<-aggregate(dataNA$steps, by=list(dataNA$interval), FUN=max)
max[which.max(max$x),]
#total number of rows with NA
sum(is.na(data))
sum(is.na(data))/nrow(data)*100
#strategy for filling missing values
#  I choose mean value for that interval because of NAs
NAreplace<-na.aggregate(data$steps, by=list(data$interval))
dataNew<-data
dataNew[,1]<-NAreplace
#new histogram
histNew<-aggregate(dataNew$steps, by=list(dataNew$date), FUN=sum)
hist(histNew$x, main="Total number of steps taken each day without NA", xlab="Steps")
#new mean and median number of steps taken per day
#meanNAS<-aggregate(dataNA$steps, by=list(dataNA$date), FUN=mean)
meanNAS<-mean(histNew$x)
#medianNAS<-aggregate(dataNA$steps, by=list(dataNA$date), FUN=median)
medianNAS<-median(histNew$x)

#weekday or weekend
tmplocale<-Sys.getlocale() #to store your language code. Unfortunately you have to manually switch back to your language.
Sys.setlocale("LC_TIME", "English") #necessary to work in different languages
dataNew$date<-as.Date(dataNew$date)
dataNew<-mutate(dataNew, week=weekdays(date, abbreviate=TRUE))
weekend<-filter(dataNew, week=="Sat"| week=="Sun")
weekday<-filter(dataNew, week!="Sat"& week!="Sun")
weekend$week<-"weekend"
weekday$week<-"weekday"
dataWeek<-rbind(weekday,weekend)
dataWeek$week<-as.factor(dataWeek$week)
Sys.setlocale("LC_TIME", "Polish_Poland.1250")

#panel plot
meanEnd<-aggregate(weekend$steps, by=list(weekend$interval), FUN=mean)
meanDay<-aggregate(weekday$steps, by=list(weekday$interval), FUN=mean)
par(mfrow=c(2,1))
plot(x=meanEnd[,1], y=meanEnd[,2], type="l", col="blue", main="weekend", xlab="Interval", ylab="Number of steps")
plot(x=meanDay[,1], y=meanDay[,2], type="l", col="blue", main="weekday", xlab="Interval", ylab="Number of steps")
par(mfrow=c(1,1))