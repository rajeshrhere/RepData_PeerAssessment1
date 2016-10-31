# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
setwd("C:/Git/datasharing/ReproducibleResearch")
activities <- read.csv("activity.csv");
activities[,2] <- as.Date(activities[,2]);
activities <- cbind(activities, weekday=weekdays(activities[,2]));
activities <- cbind(activities, weekend=ifelse(activities$weekday=="Saturday" | activities$weekday=="Sunday", "weekend", "weekday"));
```


## What is mean total number of steps taken per day?

```r
totalsteps <- aggregate(activities$steps, by=list(activities$date), FUN=sum, na.rm=TRUE);
names(totalsteps) <- c("Date", "Total Steps");
head(totalsteps);
```

```
##         Date Total Steps
## 1 2012-10-01           0
## 2 2012-10-02         126
## 3 2012-10-03       11352
## 4 2012-10-04       12116
## 5 2012-10-05       13294
## 6 2012-10-06       15420
```

```r
hist(totalsteps$`Total Steps`, col="Green", xlab="Total steps", ylab=c(0, 30), main="Total number of steps taken each day");
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean(totalsteps$`Total Steps`);
```

```
## [1] 9354.23
```

```r
median(totalsteps$`Total Steps`);
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
meansteps <- aggregate(activities$steps, by=list(activities$interval), FUN=mean, na.rm=TRUE);
names(meansteps) <- c("Interval", "Mean Steps");
head(meansteps);
```

```
##   Interval Mean Steps
## 1        0  1.7169811
## 2        5  0.3396226
## 3       10  0.1320755
## 4       15  0.1509434
## 5       20  0.0754717
## 6       25  2.0943396
```

```r
meansteps[which(meansteps$`Mean Steps`==max(meansteps$`Mean Steps`)),]$Interval
```

```
## [1] 835
```

```r
plot(meansteps$Interval, meansteps$`Mean Steps`, type="l", lwd=1, col="brown", xlab="Interval", ylab="Mean Steps", main="Mean Steps in a day");
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## Imputing missing values

```r
activities_updated <- activities; 
napos <- which(is.na(activities_updated$steps));
VectorMean <- rep(mean(activities_updated$steps, na.rm=TRUE), times=length(napos));
activities_updated[napos, 1] <- VectorMean;
#rm(VectorMean);
#rm(napos);
head(activities_updated);
```

```
##     steps       date interval weekday weekend
## 1 37.3826 2012-10-01        0  Monday weekday
## 2 37.3826 2012-10-01        5  Monday weekday
## 3 37.3826 2012-10-01       10  Monday weekday
## 4 37.3826 2012-10-01       15  Monday weekday
## 5 37.3826 2012-10-01       20  Monday weekday
## 6 37.3826 2012-10-01       25  Monday weekday
```

```r
totalsteps_u <- aggregate(activities_updated$steps, by=list(activities_updated$date), FUN=sum, na.rm=TRUE);
names(totalsteps_u) <- c("Date", "Total Steps");
head(totalsteps_u);
```

```
##         Date Total Steps
## 1 2012-10-01    10766.19
## 2 2012-10-02      126.00
## 3 2012-10-03    11352.00
## 4 2012-10-04    12116.00
## 5 2012-10-05    13294.00
## 6 2012-10-06    15420.00
```

```r
hist(totalsteps_u$`Total Steps`, col="Green", xlab="Total steps", ylim=c(0, 40), main="Total number of steps taken each day");
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean(totalsteps_u$`Total Steps`);
```

```
## [1] 10766.19
```

```r
median(totalsteps_u$`Total Steps`);
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

```r
weekend_activities <- subset(activities, weekend=='weekend');
weekend_meansteps <- aggregate(weekend_activities$steps, by=list(weekend_activities$interval), FUN=mean, na.rm=TRUE);
names(weekend_meansteps) <- c("Interval", "Mean Steps");
plot(weekend_meansteps$Interval, weekend_meansteps$`Mean Steps`, type="l", lwd=1, col="brown", xlab="Interval", ylab="Mean Steps", main="Mean Steps in a Weekend");
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
weekday_activities <- subset(activities, weekend=='weekday');
weekday_meansteps <- aggregate(weekday_activities$steps, by=list(weekday_activities$interval), FUN=mean, na.rm=TRUE);
names(weekday_meansteps) <- c("Interval", "Mean Steps");
plot(weekday_meansteps$Interval, weekday_meansteps$`Mean Steps`, type="l", lwd=1, col="brown", xlab="Interval", ylab="Mean Steps", main="Mean Steps in a Weekday");
```

![](PA1_template_files/figure-html/unnamed-chunk-5-2.png)<!-- -->


