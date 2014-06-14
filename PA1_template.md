# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Load the activity monitoring data. The script assumes that [activity.zip](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) is present in the current working directory.



```r
if (!file.exists("activity.csv")) {
  unzip("activity.zip")
}
```

Open the data and print out a few lines.


```r
dataset <- read.csv("activity.csv")
head(dataset)
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

## What is mean total number of steps taken per day?

The script removes all NAs then aggregates the number of steps over the date.
After that a histogram is draw and the mean and median are printed.


```r
dailysteps <- dataset[!is.na(dataset$steps),]
dailysteps <- aggregate(dailysteps$steps, list(dailysteps$date), "sum")

names(dailysteps)[1] <- "date"
names(dailysteps)[2] <- "total"

hist(dailysteps$total, breaks=10, xlab="Number of Steps", 
     main="Histogram of steps taken per day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
summary(dailysteps$total)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8840   10800   10800   13300   21200
```

## What is the average daily activity pattern?

Now the script averages the number of steps taken in an interval over all days.
It draws the result in a time series diagram and outputs the interval with the largest average.


```r
averageininterval <- dataset[!is.na(dataset$steps),]
averageininterval <- aggregate(averageininterval$steps, 
                               list(averageininterval$interval), "mean")
names(averageininterval)[1] <- "interval"
names(averageininterval)[2] <- "average"

plot.ts(averageininterval$interval, averageininterval$average, 
        xlab="Inteval", ylab="Average", 
        main="Average number of steps taken in an interval over all days")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
averageininterval <- averageininterval[order(averageininterval$average, 
                                             decreasing=TRUE),]
averageininterval[1,]
```

```
##     interval average
## 104      835   206.2
```

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
