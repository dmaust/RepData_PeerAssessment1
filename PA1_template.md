# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
```



## What is mean total number of steps taken per day?


```r
daily.agg <- aggregate(steps ~ date, activity, FUN = sum)
hist(daily.agg$steps, xlab = "Steps per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


Mean steps per day:

```r
mean(daily.agg$steps)
```

```
## [1] 10766
```


Median steps per day:

```r
median(daily.agg$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?


```r
interval.agg <- aggregate(steps ~ interval, activity, FUN = mean)
plot(steps ~ interval, interval.agg, type = "l", main = "Steps taken over intervals throughout the day")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 



```r
interval.max <- interval.agg[interval.agg$steps == max(interval.agg$steps), 
    ]
interval.max
```

```
##     interval steps
## 104      835 206.2
```

Maximum average steps 206.1698 occured at interval 835.

## Imputing missing values

Number of missing values:

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

Build a model to estimate missing values:

```r
model <- lm(steps ~ as.factor(interval), activity)
imputed_values <- predict(model, activity[is.na(activity$steps), ], type = "response")
activity[is.na(activity$steps), ]$steps <- imputed_values
```


New metrics after imputing missing values:

```r
daily.agg <- aggregate(steps ~ date, activity, FUN = sum)
hist(daily.agg$steps, xlab = "Steps per day")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 


Mean steps per day:

```r
mean(daily.agg$steps)
```

```
## [1] 10766
```


Median steps per day:

```r
median(daily.agg$steps)
```

```
## [1] 10766
```


Number of missing values:

```r
sum(is.na(activity$steps))
```

```
## [1] 0
```


## Are there differences in activity patterns between weekdays and weekends?


```r
library(lattice)
library(plyr)
activity$Weekend <- as.factor(weekdays(as.Date(activity$date)) %in% c("Saturday", 
    "Sunday"))
activity$Weekend <- revalue(activity$Weekend, c(`FALSE` = "weekday", `TRUE` = "weekend"))
interval.agg <- aggregate(steps ~ interval + Weekend, activity, FUN = mean)

xyplot(steps ~ interval | Weekend, interval.agg, layout = c(1, 2), type = "l")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 

