---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



```r
        knitr::opts_chunk$set(echo=TRUE, warning=FALSE, message=FALSE)
        #setoptions, echo=TRUE
```
## Loading and preprocessing the data

Loading and preprocessing the data
Show any code that is needed to

 * Load the data (i.e. \color{red}{\verb|read.csv()|}read.csv())
 * Process/transform the data (if necessary) into a format suitable for your analysis
 
###1. Code for reading in the dataset and/or processing the data


```r
file_path <- "/Users/cn03881/Documents/R/W2_Peer-graded Assigment 1/activity.csv"
amd <- read.csv(file_path, header = TRUE)

library(lubridate)
library(dplyr)

# convert from factor to Date // class = Date
amd$date <- ymd(amd$date) 
# remove NA data from Steps
amd <- filter(amd, !is.na (steps))
steps <- amd %>% group_by(date) %>% summarize(steps = sum(steps))
head(steps,5)
```

```
## # A tibble: 5 x 2
##   date       steps
##   <date>     <int>
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
```


## What is mean total number of steps taken per day?
What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

 * Calculate the total number of steps taken per day
 * If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
 * Calculate and report the mean and median of the total number of steps taken per day

###2. Histogram of the total number of steps taken each day


```r
hist(steps$steps, main = "Histogram of the total number of steps taken each day", col = "green", xlab = "Date")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

###3. Mean and median number of steps taken each day


```r
x<-mean(steps$steps)
x = as.integer(x)
y <- median(steps$steps)
y = as.integer(y)
x
```

```
## [1] 10766
```

```r
y
```

```
## [1] 10765
```
####The mean is **10766** and the median is **10765**

## What is the average daily activity pattern?
 * Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
 * Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

###4. Time series plot of the average number of steps taken


```r
library(ggplot2)
steps <- amd %>% group_by(interval) %>% summarize(steps = mean(steps))
head(steps,5)
```

```
## # A tibble: 5 x 2
##   interval  steps
##      <int>  <dbl>
## 1        0 1.72  
## 2        5 0.340 
## 3       10 0.132 
## 4       15 0.151 
## 5       20 0.0755
```

What is the average daily activity pattern?


```r
ggplot(steps, aes(x=interval, y=steps)) + geom_line(color = "red")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

###5. The 5-minute interval that, on average, contains the maximum number of steps


```r
max <- steps[which.max(steps$steps),]
max
```

```
## # A tibble: 1 x 2
##   interval steps
##      <int> <dbl>
## 1      835  206.
```
####The maximum number of steps on average is **206.1698113** and it is located at the interval **835**

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as \color{red}{\verb|NA|}NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

 * Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
 * Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
 * Create a new dataset that is equal to the original dataset but with the missing data filled in.
 * Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

###6. Code to describe and show a strategy for imputing missing data

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
amd2 <- read.csv(file_path, header = TRUE)

# convert from factor to Date // class = Date
amd2$date <- ymd(amd2$date)

miss <- sum(is.na(amd2$steps))
miss
```

```
## [1] 2304
```

####Total missing values = **2304**

###7. Histogram of the total number of steps taken each day after missing values are imputed

Introducing nean values to missing data


```r
nas <- is.na(amd2$steps)
mean_interval <- tapply(amd2$steps, amd2$interval, mean, na.rm=TRUE, simplify=TRUE)
amd2$steps[nas] <- mean_interval[as.character(amd2$interval[nas])]

amd2_sumry <- amd2 %>% group_by(date) %>% summarize(steps = sum(steps))
head(amd2_sumry,5)
```

```
## # A tibble: 5 x 2
##   date        steps
##   <date>      <dbl>
## 1 2012-10-01 10766.
## 2 2012-10-02   126 
## 3 2012-10-03 11352 
## 4 2012-10-04 12116 
## 5 2012-10-05 13294
```



```r
hist(amd2_sumry$steps, main = "Histogram of the total number of steps taken each day", col = "red", xlab = "Date")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
Mean and median number of steps taken each day with missing values



```r
x<-mean(amd2_sumry$steps)
x = as.integer(x)
y <- median(amd2_sumry$steps)
y = as.integer(y)
x
```

```
## [1] 10766
```

```r
y
```

```
## [1] 10766
```
####The mean is **10766** and the median is **10766**

## Are there differences in activity patterns between weekdays and weekends?
For this part the \color{red}{\verb|weekdays()|}weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

 * Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
 * Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

###8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


```r
amd2 <- mutate(amd2, weektype = ifelse(weekdays(amd2$date) == "Saturday" | weekdays(amd2$date) == "Sunday", "weekend", "weekday"))

amd2$weektype <- as.factor(amd2$weektype)
head(amd2,5)
```

```
##       steps       date interval weektype
## 1 1.7169811 2012-10-01        0  weekday
## 2 0.3396226 2012-10-01        5  weekday
## 3 0.1320755 2012-10-01       10  weekday
## 4 0.1509434 2012-10-01       15  weekday
## 5 0.0754717 2012-10-01       20  weekday
```


```r
data_sumry <- amd2 %>% group_by(interval, weektype) %>% summarise(steps = mean(steps))

g <- ggplot(data_sumry, aes(x=interval, y=steps, color = weektype)) 

g + geom_line() + facet_wrap(~weektype, ncol = 1, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

###9.All of the R code needed to reproduce the results (numbers, plots, etc.) in the report
