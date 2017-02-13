# Reproducible Research: Peer Assessment 1

## Load required libraries

```r
# Required for weekday and weekend plots
library(lattice)
```

## Loading and preprocessing the data

```r
dataFile <- "activity.csv"
# Unzip the file
if(!file.exists(dataFile)){
  unzip(zipfile="activity.zip")
}

# Read the csv into memory
data <- read.csv(dataFile)

# Tidy data
data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day?

```r
# Calculate the total of steps by date and exclude NA
steps.date.total <- tapply(data$steps, data$date, sum, na.rm=TRUE)

# Make a histogram of the of the total steps
hist(steps.date.total, main = "Total number of steps per day", xlab = "Total steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# Calculate the mean and the median
mean(steps.date.total)
```

```
## [1] 9354.23
```

```r
median(steps.date.total)
```

```
## [1] 10395
```


## What is the average daily activity pattern?

```r
# Average steps per interval
steps.interval.mean <- tapply(data$steps, data$interval, mean, na.rm=TRUE)

# Make a time series plot of the of the average number of steps
plot(
  names(steps.interval.mean), 
  steps.interval.mean, 
  type = "l", 
  main = "Average Steps by Interval", 
  xlab = "Interval",
  ylab = "Average steps"
)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
# Highest 5-minute interval
steps.interval.mean[which.max(steps.interval.mean)]
```

```
##      835 
## 206.1698
```


## Imputing missing values

```r
# Total number of missing values
sum(is.na(data))
```

```
## [1] 2304
```

```r
# Fill in the missing values with mean in a new dataset
data.imputed <- transform(data, steps=ifelse(is.na(steps), steps.interval.mean, steps))
# Tapply attempt
# steps.date.mean <- tapply(data$steps, data$date, mean, na.rm = TRUE)
# data.na <- is.na(data$steps)
# data.imputed$steps[data.na] <- steps.date.mean[data$date[data.na]]
# This produces NaNs in dates without any data.
# data.imputed$steps[is.nan(data.imputed$steps)] = 0

# Calculate the total of steps by date
imputed.steps.date.total <- tapply(data.imputed$steps, data.imputed$date, sum)

# Make a histogram of the of the total steps
hist(
  imputed.steps.date.total, 
  main = "Total number of steps per day with imputed data", 
  xlab = "Total steps"
)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
# Calculate the mean and the median
mean(imputed.steps.date.total)
```

```
## [1] 10766.19
```

```r
median(imputed.steps.date.total)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

```r
# Create a new two level factor column with weekend and weekdays
data.imputed$day <- sapply(data.imputed$date, function(date){
  day <- weekdays(date)
  if(day == "Saturday" | day == "Sunday") "weekend"
  else "weekday"
})
# factor column
data.imputed$day <- as.factor(data.imputed$day)

# Average steps per interval and day
imputed.steps.day <- aggregate(steps ~ day + interval, data.imputed, mean)

# Lattice timeplot for weekdays and weekends
xyplot(
  steps ~ interval | day,
  imputed.steps.day, 
  type = "l",
  layout = c(1, 2),
  xlab = "Interval",
  ylab = "Steps"
)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
