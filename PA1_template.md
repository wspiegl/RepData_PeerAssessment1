# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
steps <- read.csv("./data/activity.csv")
stepsClean <- steps[complete.cases(steps), ]
```



## What is mean total number of steps taken per day?


```r
stepsPerDay <- tapply(stepsClean$steps, as.factor(as.character(stepsClean$date)), 
    FUN = sum, rm.na = T)

(mn <- mean(stepsPerDay, na.rm = T))
```

```
## [1] 10767
```

```r
(md <- median(stepsPerDay, na.rm = T))
```

```
## [1] 10766
```

```r

hist(stepsPerDay)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 



## What is the average daily activity pattern?  
### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
dayPatterns <- tapply(stepsClean$steps, stepsClean$interval, FUN = mean, rm.a = T)

# simple, with default tick marks
# plot(levels(as.factor(stepsClean$interval)), dayPatterns, type='l',
# ylab='Step count [per 5-min interval]', xlab='5-minute time interval
# during the day', main = 'Average daily activity pattern')


# more costly with custom labels on tick marks
plot(dayPatterns, type = "l", xaxt = "n", xlab = "Time")
axis(1, at <- seq(1, to = 288, length.out = 10), labels <- names(dayPatterns)[seq(1, 
    to = 288, length.out = 10)], las = 2)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r

```


### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
which.max(dayPatterns)
```

```
## 835 
## 104
```


## Imputing missing values


### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
indices.of.rows.with.missing.values <- which(!complete.cases(steps))
(number.of.missing.value.rows <- length(indices.of.rows.with.missing.values))
```

```
## [1] 2304
```


### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

solution: take mean from the dayPatterns-data.frame (s.o)
### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

for each row, do  
 a) get the row with the missing value: rowIndex.with.missing.value  
 b) imput by stepsImpute[rowIndex.with.missing.value, stepsImpute$steps]  
                 <- dayPatterns[stepsImpute[rowIndex.with.missing.value,  
                                            stepsImpute$interval], dayPatterns$steps]  


```r
steps4impute <- read.csv("./data/activity.csv")
for (i in indices.of.rows.with.missing.values) {
    steps4impute[i, "steps"] <- dayPatterns[as.character(steps4impute[i, "interval"])]
}
```


### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
stepsPerDayFilled <- tapply(steps4impute$steps, as.factor(as.character(steps4impute$date)), 
    FUN = sum, rm.na = T)

mn <- mean(stepsPerDayFilled, na.rm = T)
md <- median(stepsPerDayFilled, na.rm = T)

hist(stepsPerDayFilled)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

```r


nrow(stepsPerDay)
```

```
## [1] 53
```

```r
nrow(stepsPerDayFilled)
```

```
## [1] 61
```


## Are there differences in activity patterns between weekdays and weekends?
### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
print(nrow(steps4impute))
```

```
## [1] 17568
```

```r
# calc for weekday or not as.POSIXlt('2012-10-06')$wday %in% c(0,6)
for (i in 1:nrow(steps4impute)) {
    steps4impute$wday[i] <- ifelse(as.POSIXlt(steps4impute$date[i])$wday %in% 
        1:5, "weekday", "weekend")
}
```


### 2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:

```r
library(lattice)
library(reshape2)

s1 <- melt(steps4impute[, c(1, 3, 4)], id.vars = c("interval", "wday"))
s2 <- dcast(s1, interval + wday ~ variable, mean)
xyplot(s2$steps ~ s2$interval | s2$wday, layout = c(1, 2), type = "l", xlab = "Interval", 
    ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

