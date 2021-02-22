---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
## Loading and preprocessing the data
1.Load the data.

```r
rm(list=ls())
setwd("C:/Users/Timo/Desktop/Prog/Coursera/Reproducible Research/Woche 2/Datasets")
activitydata <- read.csv("activity.csv")
head(activitydata)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
## What is mean total number of steps taken per day?
1.Calculate the total number of steps taken per day.

```r
dailysteps <- aggregate(steps~date,data=activitydata,FUN=sum)
head(dailysteps)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```
2.Create a histogram of the total number of steps taken each day.

```r
png(filename="plot1.png", width=480, height=480)
hist(dailysteps$steps, xlab="Total daily steps", main="Histogram of total number of steps taken each day")
dev.off()
```

```
## png 
##   2
```
3.Calculate and report the mean and median of the total number of steps taken per day.

```r
meandailysteps <- mean(dailysteps$steps)
meandailysteps
```

```
## [1] 10766.19
```

```r
mediandailysteps <- median(dailysteps$steps)
mediandailysteps
```

```
## [1] 10765
```
## What is the average daily activity pattern?
1.Create a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days.

```r
intervalsteps <- aggregate(steps~interval,data=activitydata,FUN=mean)
png(filename="plot2.png",width=480,height=480)
plot(steps~interval,data=intervalsteps,type="l",xlab="5-minute interval",main="Time series plot of average daily activity pattern")
dev.off()
```

```
## png 
##   2
```
2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
intervalwithmaxsteps<-intervalsteps[which.max(intervalsteps$steps),]
intervalwithmaxsteps
```

```
##     interval    steps
## 104      835 206.1698
```
## Imputing missing values
1.Calculate and report the total number of missing values in the dataset.

```r
missingvalues<-sum(is.na(activitydata$steps))
missingvalues
```

```
## [1] 2304
```
2.Devise a strategy for filling in all of the missing values in the dataset.

```r
meanintervalsteps<-function(interval){
    intervalsteps[intervalsteps$interval==interval,]$steps
}
```
3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
cleanedactivitydata<-activitydata
for(i in 1:nrow(cleanedactivitydata)){
    if(is.na(cleanedactivitydata[i,]$steps)){
        cleanedactivitydata[i,]$steps <- meanintervalsteps(cleanedactivitydata[i,]$interval)
    }
}
head(cleanedactivitydata)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```
4.Create a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```r
dailysteps <- aggregate(steps~date,data=cleanedactivitydata,FUN=sum)
head(dailysteps)
```

```
##         date    steps
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

```r
png(filename="plot3.png",width=480,height=480)
hist(dailysteps$steps,xlab="Total daily steps",main="Histogram of total number of steps taken each day")
dev.off()
```

```
## png 
##   2
```

```r
meandailysteps <- mean(dailysteps$steps)
meandailysteps
```

```
## [1] 10766.19
```

```r
mediandailysteps <- median(dailysteps$steps)
mediandailysteps
```

```
## [1] 10766.19
```
## Are there differences in activity patterns between weekdays and weekends?
1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
cleanedactivitydata$date <- as.Date(strptime(cleanedactivitydata$date,format="%Y-%m-%d"))
cleanedactivitydata$day <- weekdays(cleanedactivitydata$date)
for (i in 1:nrow(cleanedactivitydata)) {
    if (cleanedactivitydata[i,]$day %in% c("Samstag","Sonntag")) {
        cleanedactivitydata[i,]$day<-"weekend"
    }
    else{
        cleanedactivitydata[i,]$day<-"weekday"
    }
}
dailysteps <- aggregate(steps~interval+day,data=cleanedactivitydata,FUN=mean)
head(dailysteps)
```

```
##   interval     day      steps
## 1        0 weekday 2.25115304
## 2        5 weekday 0.44528302
## 3       10 weekday 0.17316562
## 4       15 weekday 0.19790356
## 5       20 weekday 0.09895178
## 6       25 weekday 1.59035639
```
2.Create a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.

```r
library(lattice)
png(filename="plot4.png",width=480,height=480)
xyplot(steps~interval|day,data=dailysteps,type ="l",layout=c(1, 2),xlab="Interval",ylab="Number of steps")
xyplot
```

```
## function (x, data, ...) 
## UseMethod("xyplot")
## <bytecode: 0x00000000200fc138>
## <environment: namespace:lattice>
```

```r
dev.off()
```

```
## png 
##   2
```
