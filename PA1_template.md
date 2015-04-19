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
     ylab = "Average across all Days", main = "Average number of steps taken", 
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



## Are there differences in activity patterns between weekdays and weekends?
