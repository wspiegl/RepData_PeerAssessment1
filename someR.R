library(reshape2)

steps <- read.csv("./data/activity.csv")
stepsClean <- steps[complete.cases(steps),]

# Sum up all 5-min values per day (tapply) and get the mean/median over all days
stepsPerDay <- tapply(stepsClean$steps, 
                      as.factor(as.character(stepsClean$date)), 
                      FUN=sum, rm.na=T)

mn <- mean(stepsPerDay,na.rm = T)
# > 10767.19
md <- median(stepsPerDay,na.rm = T)
# > 10766

hist(stepsPerDay)


dayPatterns <- 
  tapply(stepsClean$steps, stepsClean$interval, FUN=mean, rm.a = T)

# simple, with default tick marks
# plot(levels(as.factor(stepsClean$interval)), dayPatterns, type="l",
#    ylab="Step count [per 5-min interval]", xlab="5-minute time interval during the day",
#    main = "Average daily activity pattern")


# more costly with custom labels on tick marks
plot(dayPatterns, type="l", xaxt="n", xlab="Time")
axis(1, 
     at <- seq(1, to=288, length.out=10),
     labels <- names(dayPatterns)[seq(1, to=288, length.out=10)],
     las=2)


# gimme the index-position (with index-name aka 5-min-interval) of the max-value
which.max(dayPatterns)




## Imputing missing values
# 1. total number of missing values in the dataset
indices.of.rows.with.missing.values <- which( ! complete.cases( steps ) )
number.of.missing.value.rows <- length( indices.of.rows.with.missing.values )

# 2. devise a strategy for filling in all of the missing values in the dataset
# example: mean/median for that day or mean for that 5-minute interval
# solution: take mean from the dayPatterns-data.frame (s.o)


# 3. Create a new dataset that is equal to the original dataset
# but with the missing data filled in.

# for each row, do
# a) get the row with the missing value: rowIndex.with.missing.value
# b) imput by stepsImpute[rowIndex.with.missing.value, stepsImpute$steps]
#                 <- dayPatterns[stepsImpute[rowIndex.with.missing.value,
#                                            stepsImpute$interval], dayPatterns$steps]
steps4impute <- read.csv("./data/activity.csv")
for (i in indices.of.rows.with.missing.values) {
  steps4impute[i, "steps"] <-
    dayPatterns[as.character(steps4impute[i, "interval"])]
}



# 4. Make a histogram of the total number of steps taken each day
# and Calculate and report the mean and median total number of steps
# taken per day. Do these values differ from the estimates from
# the first part of the assignment? What is the impact of imputing
# missing data on the estimates of the total daily number of steps?
# Sum up all 5-min values per day (tapply) and get the mean/median over all days

stepsPerDayFilled <- tapply(steps4impute$steps, 
                      as.factor(as.character(steps4impute$date)), 
                      FUN=sum, rm.na=T)

mn <- mean(stepsPerDayFilled,na.rm = T)
md <- median(stepsPerDayFilled,na.rm = T)

hist(stepsPerDayFilled)


nrow(stepsPerDay)
nrow(stepsPerDayFilled)


## Are there differences in activity patterns between weekdays and weekends?
# 1. Create a new factor variable in the dataset with two levels – “weekday”
# and “weekend” indicating whether a given date is a weekday or weekend day.
print(nrow(steps4impute))
# calc for weekday or not
# as.POSIXlt("2012-10-06")$wday %in% c(0,6)
for ( i in 1:nrow(steps4impute)) {
  steps4impute$wday[i] <- 
    ifelse(as.POSIXlt(steps4impute$date[i])$wday %in% 1:5, "weekday", "weekend")
}

# 2. Make a panel plot containing a time series plot (i.e. type = "l") of
# the 5-minute interval (x-axis) and the average number of steps taken,
# averaged across all weekday days or weekend days (y-axis).

#lattice-package
library(lattice)
# > set.seed(10)
# > x <- rnorm(100)
# > f <- rep(0:1, each = 50)
# > y <- x + f - f * x + rnorm(100, sd = 0.5)
# > f <- factor(f, labels = c("Group 1", "Group 2"))
# > xyplot(y ~ x | f, layout = c(1, 2))  ## Plot with 2 panels

#plot(levels(as.factor(stepsClean$interval)), dayPatterns, type="l",
     #    ylab="Step count [per 5-min interval]", xlab="5-minute time interval during the day",
     #    main = "Average daily activity pattern")
     

# names(steps4impute)[names(steps4impute)=="weekday"] <- wd
# 
# weekday.steps <- steps4impute["wday" == "weekend",]
# weekday.pattern <- 
#   tapply(stepsClean$steps, stepsClean$interval, FUN=mean, rm.a = T)
# 
# > weekend.steps <- steps4impute[steps4impute$wday == "weekend",]
# > weekday.steps <- steps4impute[steps4impute$wday == "weekday",]
# > nrow(weekday.steps)
# [1] 12960
# > nrow(weekend.steps)
# [1] 4608
# > 


s1 <- melt(steps4impute[,c(1,3,4)], id.vars=c("interval", "wday"))
s2 <- dcast(s1, interval + wday ~ variable, mean)
xyplot(s2$steps ~ s2$interval | s2$wday, layout = c(1, 2),
       type="l", xlab = "Interval", ylab="Number of steps")




