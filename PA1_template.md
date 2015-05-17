Loading the required data 

```r
setwd('H:/coursera stuff/data science specialization/05 Reproducible Research/Course Projects/Project 1/Input Data')
activity <- read.csv("activity.csv")
```

Aggregating the data at a date level to get the total steps taken per day

```r
date_steps_agg <- aggregate(steps ~ date, data = activity, sum)
```

Plotting the histogram of the total number of steps taken each day

```r
hist(date_steps_agg$steps)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Reporting the mean and median of the total number of steps taken per day

```r
mean(date_steps_agg$steps)
```

```
## [1] 10766.19
```

```r
median(date_steps_agg$steps)
```

```
## [1] 10765
```

Aggregating the data at a interval level

```r
interval_steps_agg <- aggregate(steps ~ interval, data = activity, mean)
```

Reporting 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps

```r
plot(interval_steps_agg,type="l")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

Reporting the 5-minute interval that contains the maximum number of steps

```r
interval_steps_agg[interval_steps_agg[,2]==max(interval_steps_agg$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

Reporting the total number of missing values in the dataset

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

Filling in all of the missing values in the dataset with the mean of the
5-minute interval

```r
activity <- transform(activity,
                      intervalavesteps = ave(steps,
                                             interval,
                                             FUN=function(x) mean(x,na.rm=TRUE)))
activity_impute <- transform(activity, 
                             steps =ifelse(is.na(steps),intervalavesteps,steps))
```

Making a histogram of the total number of steps taken each day

```r
date_steps_impute_agg <- aggregate(steps ~ date, data = activity_impute, sum)
hist(date_steps_impute_agg$steps)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

Computing the mean and median total number of steps taken per day for the normal and the iputed datasets

```r
mean_norm=mean(date_steps_agg$steps)
median_norm=median(date_steps_agg$steps)

mean_impute=mean(date_steps_impute_agg$steps)
median_impute=median(date_steps_impute_agg$steps)
```

There is absolutely no difference between the two means (1.0766189 &times; 10<sup>4</sup> and 1.0766189 &times; 10<sup>4</sup>) while the difference in the medians (1.0766189 &times; 10<sup>4</sup> and 10765) is in the decimal places

Creating a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day

```r
library(chron)
activity_impute$WeekendFlag <- is.weekend(as.Date(activity_impute$date))
activity_impute[activity_impute$WeekendFlag==TRUE,]$WeekendFlag = "Weekend"
activity_impute[activity_impute$WeekendFlag==FALSE,]$WeekendFlag = "Weekday"
```

Making a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days

```r
library(lattice)
WF_interval_steps_agg <- aggregate(steps ~ interval + WeekendFlag, 
                                   data = activity_impute, mean)
xyplot(steps~interval | WeekendFlag, data=WF_interval_steps_agg,
       type="l",layout=c(1,2))
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

