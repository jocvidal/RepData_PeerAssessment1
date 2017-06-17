# Reproducible Research: Peer Assessment 1
josevidal  
17 de junio de 2017  



## Loading and preprocessing the data

```r
library(data.table)
```

```
## Warning: package 'data.table' was built under R version 3.3.2
```

```r
setwd("C:/Users/Dalvin/Documents/experian/ejemplos de R/Reproducible Research/week2/course_project")
```
Load data

```r
activity      <- read.csv( "activity.csv" )

activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
activity      <- as.data.table(activity)
```
Transform variable date as a Date format

## What is mean total number of steps taken per day?
Calculate the total number of steps taken per day

```r
step <- activity[ , .(step_per_day = sum(steps, na.rm = T)), by=date]
```

Histogram of the total number of steps taken each day

```r
hist(step$step_per_day, main ="Histogram of the total number of steps taken each day",
     xlab = "Total number of step per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

mean of the total number of steps taken per day

```r
mean(step$step_per_day)
```

```
## [1] 9354.23
```

median of the total number of steps taken per day

```r
median(step$step_per_day)
```

```
## [1] 10395
```

## What is the average daily activity pattern?
average number of steps taken across all days 

```r
step<- activity[ , .(steps_int = mean(steps, na.rm = T)), by =interval]
plot(step, type = "l", ylab="Number of steps", main="Daily activity pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Interval with maximum number of steps

```r
step[ which.max(step$steps_int), interval ]
```

```
## [1] 835
```

## Imputing missing values


```r
#total number of missing values in the dataset 
table(!complete.cases(activity))
```

```
## 
## FALSE  TRUE 
## 15264  2304
```

```r
#the mean steps by 5-minute interval and round to a integer
step <- activity[, .(mean_step = round(mean(steps, na.rm = T))), by=interval]
#Create and fill new data set with the mean steps by interval
activity_new<- activity
for (i in  step$interval){
    activity_new[interval==i & !complete.cases(activity_new), steps:=step[interval==i, mean_step]]
}
#Total number of step by date
step_total <- activity_new[ , .(step_per_day = sum(steps, na.rm = T)), by=date]
#histogram of the total number of steps taken each day
hist(step_total$step_per_day, main ="Histogram of the total number of steps taken each day",
     xlab = "Total number of step per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
#mean of the total number of steps taken per day
mean(step_total$step_per_day)
```

```
## [1] 10765.64
```

```r
#median of the total number of steps taken per day
median(step_total$step_per_day)
```

```
## [1] 10762
```

The mean of total number of steps difer in 1411.41 steps and the median difer in 367 steps. Imputing missing data impact in the mean, median and the distribution of total step, this will report more accurate estimation.

## Are there differences in activity patterns between weekdays and weekends?
Create factor variable weekday

```r
activity_new$weekday <- "weekday"
activity_new$weekday[ weekdays(activity[,date]) %in% c("sábado", "domingo") ] <- "weekend"
activity_new[, weekday:=as.factor(weekday)]
```

Activity patterns between weekdays and weekends

```r
step_int_2<- activity_new[ weekday=="weekday", .(steps_int = mean(steps, na.rm = T)), by = interval]
plot(step_int_2, type = "l", ylab = "Number of steps", main = "Weekday", ylim = c(0,230))
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
step_int_2<- activity_new[ weekday=="weekend", .(steps_int = mean(steps, na.rm = T)), by = interval]
plot(step_int_2, type = "l", ylab = "Number of steps", main = "Weekend", ylim = c(0,230))
```

![](PA1_template_files/figure-html/unnamed-chunk-11-2.png)<!-- -->
