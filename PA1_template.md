# Reproducible Research: Peer Assessment 1

After fork/clone of the repository is done, the repository contains the following:  
1. *doc* folder containing *instructions.pdf* describing the assignment  
2. *instructions_fig* folder containing *sample_panelplot* illustrating an example of plot from the data  
3. *activity* zipped folder containing the data in *activity.csv*  
4. *PA1_template.Rmd* containing the code and documentation  
5. *README.Md* containing instructions on the instructions  




## Loading and preprocessing the data

Data from *activity.csv* found in *activity.zip* zipped folder has to be unzipped and loaded to variable *data*

We read in the data from *activity.csv* in *activity.zip*

```r
data=read.csv(unz("activity.zip", "activity.csv"))
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

We then process the date and interval column into POSIXct format and store in date column

```r
data$date=as.POSIXct(data$date, format="%Y-%m-%d")
data$interval = sprintf("%04d", data$interval)
data$combined=paste(data$date,data$interval, sep = " ")
data$combined=as.POSIXct(data$combined, format="%Y-%m-%d %H%M")
```


## What is mean total number of steps taken per day?

Calculate the total number of steps taken for each day

```r
totaldailysteps=aggregate(steps~date, data, sum)
```

Make a histogram of the total number of steps taken each day

```r
library(ggplot2)
library(scales)
g <- ggplot(totaldailysteps, aes(date, steps))
g+geom_histogram(stat="identity")+labs(title="Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

Calculate and report the mean and median total number of steps taken
per day

```r
meansteps=mean(totaldailysteps$steps)
sprintf("The mean total number of steps taken per day is %.0f", meansteps)
```

```
## [1] "The mean total number of steps taken per day is 10766"
```

```r
mediansteps=median(totaldailysteps$steps)
sprintf("The median total number of steps taken per day is %.0f", mediansteps)
```

```
## [1] "The median total number of steps taken per day is 10765"
```


## What is the average daily activity pattern?

Calculate the average number of steps taken per 5-minute interval, averaged across all days

```r
averageStepsPerInterval=aggregate(steps~interval, data, mean)
```

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
and the average number of steps taken, averaged across all days (y-axis)

```r
#we need to format interval column to POSIXct so that x-axis ticks can be displayed properly
averageStepsPerInterval$interval=as.POSIXct(averageStepsPerInterval$interval, format="%H%M")
g <- ggplot(averageStepsPerInterval, aes(interval, steps, group=1))
g+geom_line()+labs(title="Average number of steps per 5-minute interval")+scale_x_datetime(breaks = pretty_breaks(10), labels = date_format("%H%M"))
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

Find the 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps

```r
maxinterval=averageStepsPerInterval[which.max(averageStepsPerInterval$steps),1]
sprintf("The maximum number of steps taken on average occurred at %s", maxinterval)
```

```
## [1] "The maximum number of steps taken on average occurred at 2015-05-17 08:35:00"
```


## Imputing missing values

Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs)

```r
numberofna=sum(is.na(data$steps))
sprintf("The total number of missing values in the dataset is %.0f", numberofna)
```

```
## [1] "The total number of missing values in the dataset is 2304"
```

Fill in all of the missing values in the dataset. The method is to fill in the mean value of the 5 minute interval for the 5 minute interval where the steps value is NA. The new dataset where the missing values are filled in is called filleddata.

```r
library(dplyr)
filleddata = data %>%
    group_by(interval) %>%
    mutate(steps=ifelse(is.na(steps), mean(steps,na.rm=TRUE),steps))
```

Calculate the total number of steps taken for each day for filleddata

```r
filledtotaldailysteps=aggregate(steps~date, filleddata, sum)
```

Make a histogram of the total number of steps taken each day for filleddata

```r
g <- ggplot(filledtotaldailysteps, aes(date, steps))
g+geom_histogram(stat="identity")+labs(title="Total number of steps (filled) taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

Calculate and report the mean and median total number of steps taken
per day

```r
filledmeansteps=mean(filledtotaldailysteps$steps)
sprintf("The mean total number of steps taken per day before filling is %.2f and after filling is %.2f", meansteps, filledmeansteps)
```

```
## [1] "The mean total number of steps taken per day before filling is 10766.19 and after filling is 10766.19"
```

```r
filledmediansteps=median(filledtotaldailysteps$steps)
sprintf("The median total number of steps taken per day is %.2f and after filling is %.2f", mediansteps, filledmediansteps)
```

```
## [1] "The median total number of steps taken per day is 10765.00 and after filling is 10766.19"
```

Since the filling strategy was to use the mean values for each 5 minute interval, the mean for total number of steps taken per day did not change. However, the median values changed. The filling in of NAs resulted in slight alteration of the data.


## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday"
and "weekend" indicating whether a given date is a weekday or weekend
day.

```r
filleddata$day=weekdays(data$date, abbreviate=FALSE)
filleddata= filleddata %>% 
    mutate(day= ifelse(day %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))
```

Calculate the average number of steps taken per 5-minute interval, averaged across weekday days or weekend days

```r
sepAverageStepsPerInterval=aggregate(steps~interval+day, filleddata, mean)
```

Make a panel plot containing a time series plot (i.e. type = "l") of the
5-minute interval (x-axis) and the average number of steps taken, averaged
across all weekday days or weekend days (y-axis).

```r
#we need to format interval column to POSIXct so that x-axis ticks can be displayed properly
sepAverageStepsPerInterval$interval=as.POSIXct(sepAverageStepsPerInterval$interval, format="%H%M")
g <- ggplot(sepAverageStepsPerInterval, aes(interval, steps, group=1))
g+geom_line()+facet_grid(day~.)+labs(title="Average number of steps per 5-minute interval")+scale_x_datetime(breaks = pretty_breaks(10), labels = date_format("%H%M"))
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png) 

As made apparent in the plot, there is significant difference in the average number of steps between weekdays and weekends.
