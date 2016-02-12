---
title: "REPRODUCIBLE RESEARCH - COURSE PROJECT 1"
author: "Pablo Morán Collantes"
date: "February 2016"
output: html_document
---

**LOADING AND PREPROCESSING THE DATA**

1. Load the data


```r
fileurl<- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileurl, destfile = "./Project.zip")
unzip("Project.zip", overwrite = TRUE)
data <- read.csv("activity.csv", header = TRUE, sep = ",", dec = ".")
```

2. Transform the data into a more suitable format


```r
data$date <- as.Date(data$date)
comp_data <- data[complete.cases(data),]
```


**WHAT IS MEAN TOTAL NUMBER OF STEPS TAKEN PER DAY?**

1. Calculate the total number of steps taken per day


```r
daily_sum <- tapply(comp_data$steps, comp_data$date, sum, simplify=T)
```

2. Make a histogram of the total number of steps taken each day


```r
hist(daily_sum,
     main = "DISTRIBUTION OF TOTAL DAILY STEPS",
     xlab = "Total daily steps",
     ylab = "Frequency",
     col = "red",
     xlim=c(0, 22000),
     ylim=c(0, 10),
     breaks=seq(0,22000,by=1000))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(daily_sum)
```

```
## [1] 10766.19
```

```r
median(daily_sum)
```

```
## [1] 10765
```


**WHAT IS THE AVERAGE DAILY ACTIVITY PATTERN?**

1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
int_avrg <- tapply(comp_data$steps, comp_data$interval, mean, simplify=T)
df_int_avrg <- data.frame(interval=as.integer(names(int_avrg)), average=int_avrg)
plot(df_int_avrg$interval, df_int_avrg$average,
     type = "l",
     main = "AVERAGE DAILY ACTIVITY PATTERN",
     xlab = "Interval",
     ylab = "Average number of steps",
     xlim=c(0, 2355),
     ylim=c(0, 220))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
int_max <- max(df_int_avrg$average)
subset(df_int_avrg, average == int_max)
```

```
##     interval  average
## 835      835 206.1698
```


**IMPUTING MISSING VALUES**

1. Calculate the total number of missing values in the dataset


```r
sum(is.na(data))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset: median for every interval


```r
daily_avrg <- tapply(comp_data$steps, comp_data$interval, median, simplify=T)
```

3. Create a new dataset with the missing data filled in


```r
data2 <- data
na_data <- is.na(data2$steps)
data2$steps[na_data] <- daily_avrg[as.character(data2$interval[na_data])]
```

4. Make a histogram of the total number of steps taken each day


```r
new_daily_sum <- tapply(data2$steps, data2$date, sum, simplify=T)
hist(new_daily_sum,
     main = "DISTRIBUTION OF TOTAL DAILY STEPS (Adjusted)",
     xlab = "Total daily steps",
     ylab = "Frequency",
     col = "red",
     xlim=c(0, 22000),
     ylim=c(0, 10),
     breaks=seq(0,22000,by=1000))
```

![plot of chunk graph](figure/graph-1.png)

5. Calculate the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment?


```r
mean(new_daily_sum)
```

```
## [1] 9503.869
```

```r
mean(daily_sum) == mean(new_daily_sum)
```

```
## [1] FALSE
```

```r
median(new_daily_sum)
```

```
## [1] 10395
```

```r
median(daily_sum) == median(new_daily_sum)
```

```
## [1] FALSE
```

```r
## The result is the same because we have chosen to analyse the median of each interval.
## If we had analysed the mean, for example, results could have been different.
```


**ARE THERE DIFFERENCES IN ACTIVITY PATTERNS BETWEEN WEEKDAYS AND WEEKENDS?**

1. Create a new factor variable in the dataset indicating whether a given date is a weekday or weekend day.


```r
Sys.setlocale("LC_TIME", "eng")
```

```
## [1] "English_United Kingdom.1252"
```

```r
is.weekend <- function(day) {
  type_day <- weekdays(day)
  if (type_day == "Saturday" | type_day == "Sunday") {
    "weekend"
  } else {
    "weekday"
  }
}
week <- sapply(data2$date, is.weekend)
data2$week <- as.factor(week)
```

2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
plot_data <- aggregate(steps ~ week + interval, data = data2, FUN = mean)
library(lattice)
xyplot(steps ~ interval | factor(week),
       data = plot_data,
       xlab="Interval",
       ylab="Number of steps",
       type="l",
       aspect=1/3)
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)
