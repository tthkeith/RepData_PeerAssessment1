---
title: "Reproducible Research: Peer Assessment 1"
author: "Author : Keith TTH"
---

## Loading necessary libraries and set options
```{r Loading Library, echo = TRUE}
library(ggplot2)
library(knitr)
```

## Loading and preprocessing the data 
```{r Loading and processing data, echo = TRUE}
setwd('D:\\Data Science Course\\Module5_Assignment1\\Module5_Assignment1')
data <- read.csv("activity.csv",na.strings="NA", colClasses=c("numeric", "character", "numeric"))
data$inteval <- factor(data$interval)
data$date <- as.Date(data$date, format="%Y-%m-%d")
```

## What is the mean total number of steps taken per day?
```{r Calculating the mean and median, echo = TRUE}
steps <- aggregate(steps ~ date, data=data, FUN=sum)
barplot(steps$steps, names.arg=steps$date, xlab="date", ylab="steps")
mean(steps$steps)
median(steps$steps)
```

## What is the average daily data pattern?
Plotting time series plot
``` {r Plot Time series chart, echo = TRUE}
average_interval <- aggregate(steps ~ interval, data=data, FUN=mean)
plot(average_interval, type="l")
```

Calculating which 5-minute interval contain the max number of steps 
``` {r Calcuating max number of steps, echo = TRUE} 
average_interval$interval[which.max(average_interval$steps)]
```


## Imputing missing values
Calculate and Report Total number of missing values 
``` {r Total Missing Values, echo = TRUE} 
sum(is.na(data))
```


How to fill the missing values?
``` {r Filling Missing Values, echo = TRUE} 
data <- merge(data, average_interval, by="interval", suffixes=c("",".y"))
temp <- is.na(data$steps)
data$steps[temp] <- data$steps.y[temp]
data <- data[,c(1:3)]
```


Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
``` {r Plotting Histogram, echo = TRUE} 
steps <- aggregate(steps ~ date, data=data, FUN=sum)
barplot(steps$steps, names.arg=steps$date, xlab="date", ylab="steps")
mean(steps$steps)
median(steps$steps)
```

###Any Difference?
No, there is little impact between original data and modified data with missing values filled in. This can be observed in the 2 plots above.



## Are there differences in data patterns between weekdays and weekends?
Creating Function to calculate if date falls on weekend or weekday
```{r Create New Factor Variable, echo = TRUE}
Days_Func <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
data$Days_Func <- as.factor(sapply(data$date, Days_Func))
```

Panel Plot across Weekends and Weekdays
```{r}
par(mfrow=c(2,1))
for (type in c("weekend", "weekday")) 
  {
    steps.type <- aggregate(steps ~ interval,data=data, 
                            subset=data$Days_Func==type,FUN=mean)
    plot(steps.type, type="l", main=type)
}
```
