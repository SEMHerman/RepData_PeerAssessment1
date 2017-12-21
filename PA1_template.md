---
title: 'Course 5 Assignment #1'
author: "SEMHerman"
date: "December 4, 2017"
output: 
  html_document: 
    fig_caption: yes
    keep_md: yes
---

###Loading and preprocessing the data

Below is code that shows how the data is loaded and processed

The data was loaded into the variable "Activity"" and then transformed into a data table and saved as 
the variable "Data".  The date column was then transformed from a factor type into a date type.
Necessary packages were loaded for the assignment.


```r
zip<-"repdata_data_activity.zip" 
unzip(zip,exdir=".")
Activity<-read.csv("activity.csv")
Data<-as.data.frame(Activity)
Data$date<-as.Date(Data$date, format="%Y-%m-%d")
library(dplyr)
library(ggplot2)
```

###What is mean total number of steps taken per day?

We first determined What the mean total number of steps taken per day was. For this part of the 
assignment, we are ignoring the missing values in the dataset.

First we calculated the total number of steps taken per day and saved this as the variable "StepsPerDay".
Next we displayed this information as a histogram of the total number of steps taken each day.  


```r
StepsPerDay<-Data %>%
   group_by(date) %>%
   summarise(steps=sum(steps))
hist(StepsPerDay$steps, xlab="Number of steps per day",main="Histogram of steps per day")
```

![](PA1_template_files/figure-html/steps-1.png)<!-- -->

Lastly, we calculated and reported the mean (saved as the variable Mean) and median (saved as the variable
Median) of the total number of steps taken per day.


```r
Mean<-mean(StepsPerDay$steps, na.rm=TRUE)
Median<-median(StepsPerDay$steps, na.rm=TRUE)
cat("The mean =", Mean, "and the median =", Median,".")
```

```
## The mean = 10766.19 and the median = 10765 .
```

###What is the average daily activity pattern?

We next determined what the average daily activity pattern was. Again we ignored the missing values
in the dataset.

First we calculated the average number of steps taken across all days and  saved it as variable "AvgSteps".
We then made a time series plot of the 5-minute interval and the average number of steps taken,
averaged across all the days in the dataset.  


```r
AvgSteps<-Data %>%
   group_by(interval) %>%
   summarise(steps = mean(steps, na.rm=TRUE))
plot(AvgSteps$interval, AvgSteps$steps,type="l", xlab = "Time intervals", 
    ylab = "Average steps", main = "Average steps across time intervals")
```

![](PA1_template_files/figure-html/interval-1.png)<!-- -->

Lastly, we determined Which 5-minute interval, on average across all the days in the dataset, 
contained the maximum number of steps and saved this as the value "Interval" along with the 
corresponding minute saved as the value "Minute".


```r
Max<-max(AvgSteps$steps,na.rm=TRUE)
Interval<-which(grepl(Max,AvgSteps$steps))
Minute<-AvgSteps$interval[Interval]
cat("The", Interval,"interval or the", Minute, "minute contains the maximum number of steps.")
```

```
## The 104 interval or the 835 minute contains the maximum number of steps.
```

###Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence 
of missing days may introduce bias into some calculations or summaries of the data.  To determine if this 
is true for this dataset we imputed the missing values using the previously caclulated mean number of steps 
across all days. 

We first calculated and reported the total number of missing values in the dataset (i.e. the total number 
of rows with NAs) and saved this as variable "Na".  Next, we used our devised strategy (mean number of
steps across all days) to fill in all of the missing values in the dataset.  To do this, we made a new
dataset that will be equal to the original but with the missing data filled in and saved it as 
"ImputedData".  We created a dataset of only the missing values of our steps column in "Data" and 
saved it as "MissingData".  Using a for loop we filled in each row of MissingData with the average 
steps calculated previously.  We then combined our "ImputedData" and our missing data and saved over 
"ImputedData" to create a dataset equal to the original dataset  but without missing values.  

Similar to before we calculated the total number of steps taken per day and saved this as the variable
"AvgStepsImp". Next we displayed this information as a histogram of the total number of steps taken each 
day.  


```r
Na<-sum(is.na(Data))
cat("the number of Na values is", Na)
```

```
## the number of Na values is 2304
```

```r
ImputedData<-NULL
MissingData<-Data[is.na(Data$steps), ]
for(i in 1:nrow(MissingData)) {
    row<-MissingData[i,]
    row$steps<- AvgSteps[AvgSteps$interval == row$interval, ]$steps[1]
    ImputedData<<- rbind(row, ImputedData)
}
ImputedData<-rbind(ImputedData,Data[!is.na(Data$steps), ])
AvgStepsImp<-ImputedData %>%
   group_by(date) %>%
   summarise(steps=sum(steps))
hist(AvgStepsImp$steps, xlab="Number of steps per day",main="Histogram of steps per day")
```

![](PA1_template_files/figure-html/impute-1.png)<!-- -->

We then  calculated and reported the mean (saved as the variable Mean1) and median 
(saved as the variable Median1) of the total number of steps taken per day.


```r
Mean1<-mean(AvgStepsImp$steps)
Median1<-median(AvgStepsImp$steps)
cat("Mean =", Mean1, "and Median =", Median1)
```

```
## Mean = 10766.19 and Median = 10766.19
```

Comparing these to the original we found that the means were the same. The median values differed, 
but by only a small amount.  Of note, the new data set resulted in a median and a mean that are equal.

###Are there differences in activity patterns between weekdays and weekends?

To determine if there are differences in activity patterns between weekdays and weekends we used 
the weekdays() function to assign each day to a weekday by creating a new variable "days".This 
was then joined with the "ImputedData" dataset to create then new dataset "ImputedDays" 
We then transformed the "days" column into a factor variable with two levles, so that Saturday and Sundays 
were subsitued for weekend and the remainder of the days were subsituted for weekday. Next we calculated
the average number of steps taken across all weekend days or weekday days and saved this as the dataset
"mean".  Lastly, we created a panel plot containing a time series plot of the 5-minute interval and the 
average of steps taken, averaged across all weekday days or weekend days. 


```r
days<- weekdays(ImputedData$date)
ImputedDays<-cbind(ImputedData,days)
Weekend<-gsub("Saturday|Sunday", "weekend", ImputedDays$days)
ImputedDays<-cbind(ImputedDays,Weekend)
ImputedDays$days<-NULL
Weekday<-gsub("Monday|Tuesday|Wednesday|Thursday|Friday", "weekday", ImputedDays$Weekend)
ImputedDays<-cbind(ImputedDays,Weekday)
ImputedDays$Weekend<-NULL
mean<-aggregate(ImputedDays$steps, by = list(ImputedDays$interval, ImputedDays$Weekday), 
    mean)
names(mean) <- c("interval", "day_type", "steps")
Graph<-ggplot(mean, aes(interval))+
  facet_grid(~day_type)+
  geom_line(aes(y=steps))+
  ylab("Average steps")+
  xlab("Time intervals")+
  ggtitle("Average steps across time intervals")+
  theme(plot.title = element_text(hjust = 0.5))
print(Graph)
```

![](PA1_template_files/figure-html/days-1.png)<!-- -->

We found that on weekdays more steps were taken in the earlier time intervals whereas on the
weekend many steps were taken across all the time intervals.

