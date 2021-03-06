---
title: "Reproducible Research: Activity Data Report (Project 1)"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_float: true
    toc_depth: 2
---



## Loading and preprocessing the data


```r
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
              temp, mode = "wb")
file <- unz(temp, "activity.csv")
data <- read.csv(file)
unlink(temp)
```

Now we'll take a look at the data briefly to determine if any preprocessing is needed  

```r
summary(data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

It seems every date has 288 readings, let's make sure that's true across the whole data set.  

```r
all(summary(data$date) == 288)
```

```
## [1] TRUE
```

We're going to change the name of the interval variable to "time" 

```r
library(tidyverse)
data <- data %>% mutate(time = as.factor(format(
        strptime(sprintf("%04d", data$interval), format = "%H%M"),
        format = "%H:%M"))) %>%
        select(steps, date, time)
```

We're also going to create a variable determining if the given day is a weekday or a weekend

```r
data <- data %>% 
        mutate(daytype = 
                       as.factor(ifelse(is.element(
                               as.numeric(format(strptime(
                                date, format = "%Y-%m-%d"), "%u")), 1:5), 
                               "Weekday", "Weekend")))
```

It's also seen in the summary that `data$steps` has some NAs, so let's see what dates those occurred and how many.  

```r
datesOfNA <- summary(data$date[is.na(data$steps)])
datesOfNA[datesOfNA != 0]
```

```
## 2012-10-01 2012-10-08 2012-11-01 2012-11-04 2012-11-09 2012-11-10 2012-11-14 
##        288        288        288        288        288        288        288 
## 2012-11-30 
##        288
```
Looks like the steps weren't recorded on certain days. We'll store these dates in a variable for later use.

```r
datesNAIndex <- which(datesOfNA != 0)
datesOfNA <- names(datesOfNA[datesNAIndex])
```




## What is mean total number of steps taken per day?

```r
#Calculate the total number of steps taken per day
summary <- data %>% group_by(date) %>%
        summarize(total = sum(steps, na.rm = TRUE))
meanPerDay <- mean(summary$total)
medPerDay <- median(summary$total)

## Create histogram of total steps per day  
plot <- ggplot(summary, aes(total))
plot + 
        geom_histogram(binwidth = 1000, 
                       color = "black", fill = "darkgrey") +
        xlab("Total Steps per Day") +
        scale_x_continuous(breaks = seq(0,
                ceiling(max(summary$total)/1000)*1000, by = 2000)) +
        ylab("Count") +
        scale_y_continuous(breaks = seq(0, 10, by = 2)) +
        ggtitle("Histogram of Total Steps per Day") +
        ## Display mean and median total number of steps per day  
        geom_vline(xintercept = meanPerDay, 
                   color = "red", lwd = 2) +
        geom_vline(xintercept = medPerDay,
                   color = "#5BC2E7", lwd = 2)
```

![](PA1_template_files/figure-html/Steps_Per_Day-1.png)<!-- -->

```r
## This code is just to disable scientific notation for the following line  
OGscipen <- getOption("scipen")
options(scipen = 999)
```
  
The histogram above also shows the mean (9354) in red and the median (10395) in blue.




## What is the average daily activity pattern?

```r
summary <- data %>% 
        group_by(time) %>%
        summarize(avg = mean(steps, na.rm = TRUE))

xlabs <- summary$time[seq(1, length(summary$time), by = 12)]
plot <- ggplot(summary, aes(time, avg, group = 1))
plot + geom_line() +
        scale_x_discrete(breaks = xlabs,
                labels = format(strptime(xlabs, format = "%H:%M"),
                                format = "%H")) +
        xlab("Hour of Day") +
        ylab("Average Steps (5 min. Intervals)") +
        ggtitle("Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/daily_pattern-1.png)<!-- -->

The time interval with the max average steps was 08:35 with an average of 206.17 steps.  

## Imputing missing values
To impute the missing values we're first going to see if the missing days are weekdays or weekends as these factors may change the activity levels  

```r
as.character(data$daytype[datesNAIndex])
```

```
## [1] "Weekday" "Weekday" "Weekday" "Weekday" "Weekday" "Weekday" "Weekday"
## [8] "Weekday"
```
Seeing now that these are all weekdays we can impute the missing value by using the values of the other weekdays. We're going to choose the impute method to use based on **[this paper](https://lgreski.github.io/datasciencedepot/references/a-comparison-of-six-methods-for-missing-data-imputation-2155-6180-1000224.pdf)** and the percent of missing values, 13.11% of all data and 17.78% of the data from weekdays.
After reviewing this and considering our percent of missing values we've decided to impute the values based on the mean of the same time interval  

```r
#steps is a discrete variable so the imputed values should be too
means <- data %>% filter(daytype == "Weekday") %>%
            group_by(time) %>% 
            summarize(avg = round(mean(steps, na.rm = TRUE)))
imputedData <- data
imputedData[which(is.na(imputedData$steps)),]$steps <- means$avg
```

We'll now look at how the distribution of steps per day has changed with these imputed values.

```r
imputedSums <- imputedData  %>%
        group_by(date) %>%
        summarize(total = sum(steps))

## Create histogram of total steps per day  
plot <- ggplot(imputedSums, aes(total))
plot + 
        geom_histogram(binwidth = 1000, 
                       color = "black", fill = "darkgrey") +
        xlab("Total Steps per Day") +
        scale_x_continuous(breaks = seq(0,
                ceiling(max(imputedSums$total)/1000)*1000, by = 2000)) +
        ylab("Count") +
        scale_y_continuous(breaks = seq(0, 10, by = 2)) +
        ggtitle("Histogram of Total Steps per Day with Imputed Values")
```

![](PA1_template_files/figure-html/Imputed_Steps_Histogram-1.png)<!-- -->
Significantly less counts near 0 are seen in the imputed data. This is because when `sum` evaluates a subset that is al `NA` it sets the sum to 0.
  

Now we'll look at how the means and medians have changed with the new imputed data set. We're interested in the change of the median so we'll exclude readings of 0 from this measurement to get a better idea of how it has changed.

```r
comparision <- data.frame(
  Mean = c(mean(data$steps, na.rm = TRUE), mean(imputedData$steps)),
  Median = c(median(data$steps[data$steps > 0], na.rm = TRUE),
             median(imputedData$steps[imputedData$steps > 0])),
  row.names = c("Original", "Imputed"))
comparision <- rbind(comparision, Differences = 
                       comparision[1,] - comparision[2,])

comparision
```

```
##                   Mean Median
## Original    37.3825996     56
## Imputed     37.1138434     46
## Differences  0.2687562     10
```




### Extra: Looking at Standard Deviation
Now we'll find the differences between the original and imputed standard deviations to see how the values have changed.

```r
imputedSummary <- imputedData  %>%
        group_by(time) %>%
        summarize(std = sd(steps))

oldSummary <- data %>% 
        group_by(time) %>%
        summarize(std = sd(steps, na.rm = TRUE))

xlabs <- imputedSummary$time[seq(1, length(imputedSummary$time), by = 12)]
difPlot <- ggplot(imputedSummary, aes(time, (oldSummary$std - std), 
                                      group = 1))
difPlot + geom_line() +
        scale_x_discrete(breaks = xlabs,
                labels = format(strptime(xlabs, format = "%H:%M"),
                                format = "%H")) +
        xlab("Hour of Day") +
        ylab("Difference (Original - Imputed)") +
        ggtitle("Difference in Standard Deviation with Imputed Steps")
```

![](PA1_template_files/figure-html/extra_look_sd-1.png)<!-- -->
  
Aside from showing the difference in standard deviations, this also shows time ranges where the original data set had a lot of variation. Adding points that were equal to the mean caused the deviation to decrease. Since more points are now near the mean the deviation has changed the most in time intervals that had more variability in the number of steps, such as the 8 o'clock hour. 
We predict that the large variability that starts around 6 o'clock may be from  a morning commute, when the individual has to consistently walk some set distance. In addition the peak from 8 to 9 could be from the beginning of a job's routine where they have to travel around the workplace a good bit. These times may vary so much because a number of factors can offset one's morning by at least five minutes, if not more.


## Are there differences in activity patterns between weekdays and weekends?

```r
daytypeSummary <- imputedData  %>%
        group_by(daytype, time) %>%
        summarize(avg = mean(steps))
plot <- ggplot(daytypeSummary, aes(time, avg, group = 1))
plot + geom_line() +
  facet_grid(daytype ~ .) +
        scale_x_discrete(breaks = xlabs,
                labels = format(strptime(xlabs, format = "%H:%M"),
                                format = "%H")) +
        xlab("Hour of Day") +
        ylab("Average Steps (5 min. Intervals)")+
        ggtitle("Weekdays vs. Weekends Daily Activity Pattern")
```

![](PA1_template_files/figure-html/Daytype_Pattern-1.png)<!-- -->
  
The large drop in average steps around 8-9 supports our previous theory that this large spike might be from a work-related activity. The large drop off of average steps in between 6-8 also supports the previous theory that this could be caused from a morning commute. 
There is also a clear drop in the average between 1800 and 1900, which might suggest this is their regular time-frame for the commute home, or it could be something like an afternoon jog. There is also large peak a little before 1600 on the weekdays, so this may also be the individual's normal returning commute. On the weekends this time frame also has higher averages, so a clear drop can't be easily seen.  
