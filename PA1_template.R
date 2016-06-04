## R preparations & Loading in the necessary packages
library(knitr)
opts_chunk$set(echo = TRUE)

library(dplyr)
install.packages('ggplot2')
library(ggplot2)

## 1. Lodding and preprocessing the data
activity <- read.csv("activity.csv", header = TRUE, sep = ',', colClasses = c("numeric", "character", "integer"))
activity$day <- weekdays(as.Date(activity$date))
activity$DateTime<- as.POSIXct(activity$date, format="%Y-%m-%d")
##pulling data without nas
clean <- activity[!is.na(activity$steps),]

## 2. What is mean total number of steps taken per day?
## 2-1. Calculate the total number of steps taken per day.
sumTable <- aggregate(activity$steps ~ activity$date, FUN=sum, )
colnames(sumTable)<- c("Date", "Steps")
## 2-2. Make a histogram of the total number of steps taken each day
ggplot(sumTable, aes(Steps))+
        geom_histogram(fill = "firebrick", binwidth = 1000) +
        labs(title = "Histogram of Steps per day", x = "Steps per day", y = "Frequency")
## 2-3. Calculate and report the mean and median of the total number of steps taken per day
as.integer(mean(sumTable$Steps))
as.integer(median(sumTable$Steps))

## 3. What is the average daily activity pattern?

