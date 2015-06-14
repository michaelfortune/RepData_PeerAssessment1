Reproducible Research: Peer Assessment 1
========================================

1. Loading and preprocessing the data
-------------------------------------

a) Include libraries ggplot2 and dplyr 
```r
library(ggplot2)
library(dplyr)
```
b) Unzip activity.csv from activity.zip 
```r
unzip(zipfile="activity.zip")
```
c) Use read.csv to read the activity data from activity.csv 
```r
actdata <- read.csv("activity.csv")
```

2. What is mean total number of steps taken per day?
----------------------------------------------------
a) Use tapply to calculate the total number of steps taken each day
```r
tot.steps <- tapply(actdata$steps, actdata$date, FUN=sum, na.rm=TRUE)
```
b) Plot a histogram showing the total number of steps taken each day
```r
x <- qplot(tot.steps, binwidth = 1000, xlab = "total number of steps taken each day") 
x + theme(panel.background = element_rect(fill = 'light green', colour = 'red')) 
```

![plot1] (https://github.com/michaelfortune/RepData_PeerAssessment1/blob/master/Plot1.png)

c) Calculate and display the mean of the total number of steps taken per day
```r
print(mean(tot.steps, na.rm=TRUE))
```
```r
## [1] 9354.23
```
d) Calculate and display the median of the total number of steps taken per day
```r
print(median(tot.steps, na.rm=TRUE))
```
```r
## [1] 10395
```
3. What is the average daily activity pattern?
----------------------------------------------
a) Calculate averages
```r
averages <- aggregate(x=list(steps=actdata$steps), by=list(interval=actdata$interval),
                      FUN=mean, na.rm=TRUE)
```
b) Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
and the average number of steps taken, averaged across all days (y-axis)
```r
avgplot <- ggplot(data=averages, aes(x=interval, y=steps)) +
    geom_line(aes(group=1), colour="#000099") +
    xlab("Interval") +
    ylab("Average number of steps taken")
```
c) Display the plot
```r
avgplot + theme(panel.background = element_rect(fill = 'light green', colour = 'red'))    
```

![plot2] (https://github.com/michaelfortune/RepData_PeerAssessment1/blob/master/Plot2.png)


d) Determine which 5-minute interval, on average across all the days in the dataset, 
contains the maximum number of steps and display the value.
```r
print(averages[which.max(averages$steps),])
```
```r
##     interval    steps
## 104      835 206.1698
```

4. Imputing missing values
--------------------------
There are a number of days/intervals where there are missing values (coded as NA). 
These missing values may introduce bias into some calculations or summaries of the data.

a) Determine where the data is missing i.e. NAs 
```r
NAs <- is.na(actdata$steps)
```
b) Second, display the number of missing values i.e. NAs
```r
print(sum(NAs))
```
```r
## [1] 2304
```
c) Strategy: create a new dataset that is equal to the original dataset but with the missing data filled in
Where value is not NA, use the value, otherwise use the average value for the missing value thereby eliminating
bias.
```r
miss.value <- function(steps, interval) {
    complete <- NA
    if (!is.na(steps))
        complete <- c(steps)
    else
        complete <- (averages[averages$interval==interval, "steps"])
    return(complete)
}
complete.data <- actdata
complete.data$steps <- mapply(miss.value, complete.data$steps, complete.data$interval)
```
d) Output the data and histogram (as we did without the imputed values).
Use tapply to calculate the total number of steps taken each day
```r
tot.steps <- tapply(complete.data$steps, complete.data$date, FUN=sum)
```
e) Plot a histogram showing the total number of steps taken each day
```r
x <- qplot(tot.steps, binwidth = 1000, xlab = "Daily steps with imputed values") 
x + theme(panel.background = element_rect(fill = 'light green', colour = 'red')) 
```

![plot3] (https://github.com/michaelfortune/RepData_PeerAssessment1/blob/master/Plot3.png)


f) Calculate and display the mean of the total number of steps taken per day
```r
print(mean(tot.steps))
```
```r
## [1] 10766
```
g) Calculate and display the median of the total number of steps taken per day
```r
print(median(tot.steps))
```
```r
## [1] 10766
```

5. Are there differences in activity patterns between weekdays and weekends?
----------------------------------------------------------------------------
a) Identify each day as a weekday, or weekend
```r
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("Data contains an invalid date")
}
complete.data$date <- as.Date(complete.data$date)
complete.data$day <- sapply(complete.data$date, FUN=weekday.or.weekend)
```
b) Calculate aggregate values
```r
averages <- aggregate(steps ~ interval + day, data=complete.data, mean)
```
c) Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
```r
daycomp <- ggplot(averages, aes(interval, steps)) + geom_line(aes(group=1), colour="#000099") + 
  facet_grid(day ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")
```
d) Display the plot 
```r
daycomp +  theme(panel.background = element_rect(fill = 'light green', colour = 'red')) 
```
![plot4] (https://github.com/michaelfortune/RepData_PeerAssessment1/blob/master/Plot4.png)

