Project 1 - Coursera Reproducible Research
==================================================

This R markdown file has been created for the first assignment of the Reproducible Research course on Coursera.  Data for the below analysis is from a personal monitoring device from October and November 2012.

##Loading and Preprocessing the data

The first part of this assignment is to load the necesary libraries and load the .csv file with the data provided on the course website.


```r
library(dplyr)
library(ggplot2)
setwd("/Users/lindsayferguson/Documents/Coursera/Data Science/Course 5_Reproducible Research")
act <- read.csv("activity.csv")
```

To see if any of the data needs to be preprocessed or transformed, the first step is to see a summary of the data.


```r
str(act)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

The only preprocessing needed right now is to convert the date column from a factor type to date type so it can be used later for plotting.  


```r
act[,2] <- as.Date(act[,2])
```

##What is the mean total steps taken per day?

Ignoring the missing values, the next part of the assignment is to find the total number of steps taken each day in the data set.  This will be done with the dplyr package.


```r
total_steps <- act %>%
  group_by(date) %>%
  summarise(TotalDailySteps = sum(steps))
```

With the calculated total steps per day, create a histogram of the new data frame.


```r
hist(total_steps$TotalDailySteps, xlab = "Total Daily Steps", 
     main = "Histogram of Total Daily Steps Taken")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

The last part of this section is to calculate the mean and median of the total number of steps taken per day.


```r
mean(total_steps$TotalDailySteps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(total_steps$TotalDailySteps, na.rm=TRUE)
```

```
## [1] 10765
```

##What is the average daily activity pattern?

The next section of this assignment is to make a time series plot of the 5-minute intervals and the average number of steps taken in each interval across all days.  


```r
avg_steps <- act %>%
  group_by(interval) %>%
  summarise(AvgSteps = mean(steps, na.rm=TRUE))

avg_steps <- as.data.frame(avg_steps)

with(avg_steps, plot(interval, AvgSteps, type="l", xlab="Time", 
                     ylab="Average Number of Steps", main = "Average Number of Steps Taken over the 
                     Course of a Day"))
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

The interval with the most steps, averaged across all days.


```r
avg_steps[which.max(as.matrix(avg_steps[,2])),1]
```

```
## [1] 835
```

##Imputing missing values

First calculate and report the total number of missing values (NA) in the data set


```r
sum(rowSums(is.na(act)))
```

```
## [1] 2304
```

To fill in these missing values, we will take the mean value for the interval (calculated above in the avg_steps data frame) and insert it into the steps column.  All this will be done in a new data frame.


```r
imp_act <- act

for(i in 1:nrow(imp_act)){
  if(is.na(imp_act[i,1])){
    imp_act[i,1] <- avg_steps[match(imp_act[i,3], avg_steps$interval),2]
  }
}
```

With this new data frame, the next step is to find the total number of steps each day and create a histogram of this information.


```r
tot_imp_act <- imp_act %>%
  group_by(date) %>%
  summarise(TotalDailySteps = sum(steps))

hist(tot_imp_act$TotalDailySteps, xlab = "Total Daily Steps", 
     main = "Histogram of Total Daily Steps Taken with Imputed Values")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

Next, the mean and median for this new data frame are calculated.


```r
mean(tot_imp_act$TotalDailySteps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(tot_imp_act$TotalDailySteps, na.rm=TRUE)
```

```
## [1] 10766.19
```

These values both match the mean calculated when no data was imputed.  This is because we used the mean for that interval.  By imputing the missing steps using the mean from that interval, there isn't a significant impact on the mean or distribution of the data but there is a significant impact on the total steps per day. 

##Are there differences in activity patterns between weekdays and weekends?

First, we need to determine which days are weekdays and which are weekends.  This is done with the weekdays function and a column is added to the data frame.


```r
steps_w_day <- imp_act %>% mutate(day=ifelse(weekdays(date) %in% c("Saturday", "Sunday"), 
                                                   "weekend", "weekday"))
```

The see how the average steps per interval varies between weekdays and weekends, the two sets of information are plotted side-by-side.


```r
tot_steps_day <- steps_w_day %>%
  group_by(day, interval) %>%
  summarise(AvgSteps = mean(steps, na.rm=TRUE))

#create plot of avg interval steps by weekday versus weekend daily steps
ggplot(tot_steps_day, aes(interval, AvgSteps))+geom_line()+facet_grid(day~.)+
  labs(x="Date", y="Average Steps", title="Average Interval Steps")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)
