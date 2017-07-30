# Reproducible Research: Peer Assessment 1


## Loading libraries

```r
library(dplyr)
library(ggplot2)
```

## Loading and preprocessing the data

```r
zipfile <- "activity.zip"
filename <- "activity.csv"
if(!file.exists(filename)) {
    unzip(zipfile)
}
activity <- read.csv(filename, header = TRUE)
```

## What is mean total number of steps taken per day?

1. Plotting total no. of steps taken per day

```r
stepsPerDay <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
qplot(stepsPerDay, main = "Total no. of steps taken per day", xlab = "No. of steps", ylab= "Frequency", binwidth = 500)
```

![](PA1_template_files/figure-html/no. of step-1.png)<!-- -->

2. Mean and median total number of steps per day

```r
meanStep <- mean(stepsPerDay)
medianStep <- median(stepsPerDay)
```
* Median total number of steps taken per day = __10395__
* Mean toal number of steps taken per day = __9354.2295082__

## What is the average daily activity pattern?

1. Finding out Average Steps per Day

```r
by_interval <- activity %>% filter(!is.na(steps)) %>% group_by(interval)
avgStepsPerDay <- summarise(by_interval, meanSteps = mean(steps))
```

2. Plotting Average Steps per day vs interval

```r
g <- ggplot(avgStepsPerDay, aes(y = meanSteps, x = interval))
g + geom_line() + labs(title = "Average daily activity pattern", xlab= "Interval", ylab = "Average no. of steps taken")
```

![](PA1_template_files/figure-html/plot time series-1.png)<!-- -->

3. Finding out which interval has maximum number of steps

```r
maxStep <- avgStepsPerDay %>% filter(meanSteps == max(meanSteps))
maxStepInterval <- maxStep[,1]
```
* The interval which contains maximum number of steps is __835__

## Imputing missing values

1. No of rows with NA values

```r
missingData <- sum(!complete.cases(activity))
```
* Number of rows with NA is __2304__

## Are there differences in activity patterns between weekdays and weekends?
