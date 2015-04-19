# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data
    ###In this preprocessing step, working directory was set, data was unzipped and lattice package 
    is being utilized

```r
getwd()
```

```
## [1] "C:/Users/Suresh/RepData_PeerAssessment1"
```

```r
setwd("C:/Users/Suresh/RepData_PeerAssessment1")
activity <- read.csv("activity.csv", colClasses = c("numeric", "character", "numeric"))
head(activity)
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

```r
names(activity)
```

```
## [1] "steps"    "date"     "interval"
```

```r
library(lattice)
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```



## What is mean total number of steps taken per day?

```r
StepsTotal <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
hist(StepsTotal$steps, main = "Total steps by day", xlab = "day", col = "green")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean(StepsTotal$steps)
```

```
## [1] 10766.19
```

```r
median(StepsTotal$steps)
```

```
## [1] 10765
```



## What is the average daily activity pattern?

```r
time_series <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
plot(row.names(time_series), time_series, type = "l", xlab = "5-min interval", 
     ylab = "Avg. across all Days", main = "Avg. number of steps taken", 
     col = "magenta")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
max_interval <- which.max(time_series)
names(max_interval)
```

```
## [1] "835"
```



## Imputing missing values

```r
activity_NA <- sum(is.na(activity))
activity_NA
```

```
## [1] 2304
```

```r
StepsAverage <- aggregate(steps ~ interval, data = activity, FUN = mean)
fillNA <- numeric()
    for (i in 1:nrow(activity))
        {
    obs <- activity[i, ]
    if (is.na(obs$steps)) {
        steps <- subset(StepsAverage, interval == obs$interval)$steps
        } 
    else 
    {
        steps <- obs$steps
    }
    fillNA <- c(fillNA, steps)
        }

new_activity <- activity
new_activity$steps <- fillNA

StepsTotal2 <- aggregate(steps ~ date, data = new_activity, sum, na.rm = TRUE)
hist(StepsTotal2$steps, main = "Total steps by day", xlab = "day", col = "purple")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
    mean(StepsTotal2$steps)
```

```
## [1] 10766.19
```

```r
    median(StepsTotal2$steps)
```

```
## [1] 10766.19
```



## Are there differences in activity patterns between weekdays and weekends?

```r
day <- weekdays(activity$date)
daylevel <- vector()
for (i in 1:nrow(activity)) {
    if (day[i] == "Saturday") {
        daylevel[i] <- "Weekend"
    } else if (day[i] == "Sunday") {
        daylevel[i] <- "Weekend"
    } else {
        daylevel[i] <- "Weekday"
    }
}
activity$daylevel <- daylevel
activity$daylevel <- factor(activity$daylevel)

stepsByDay <- aggregate(steps ~ interval + daylevel, data = activity, mean)
names(stepsByDay) <- c("interval", "daylevel", "steps")

xyplot(steps ~ interval | daylevel, stepsByDay, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 
