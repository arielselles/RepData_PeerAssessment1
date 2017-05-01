General configurations
----------------------

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(ggplot2)
    library(lubridate)

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

    # Plotting template
    common_theme <- theme_bw() +
        theme(plot.title = element_text(color="steelblue4", face="bold", size=16, hjust=0.5),
              axis.title = element_text(color="steelblue", face="bold", size=12),
              axis.text.x = element_text(colour="grey20", size=8),
              axis.text.y = element_text(colour="grey20", size=8),
              strip.background =element_rect(fill="skyblue1"))

Loading and preprocessing the data
----------------------------------

The first step is loading and preprocessing the data.

    activity <- read.csv("activity.csv")

    # A new field interval2 is created, to distribute uniformly the data between 0 
    #   and 1440 (1440 minutes in one day).
    # Therefore, for example, the interval 645 (06:45) is converted to 405
    activity$interval2 <- 
        floor(activity$interval/100) * 60 + 
        (activity$interval - (floor(activity$interval/100) * 100))

What is mean total number of steps taken per day?
-------------------------------------------------

The next thing to do is to calculate the sum of the number of steps
taken by day. Then, mean and median from the resulting set of values.

    dailySummary <- activity %>%
        group_by(date) %>%
        summarise(sum_steps = sum(steps, na.rm=T))

    meanStepsByDay <- as.integer(round(mean(dailySummary$sum_steps)))

    medianStepsByDay <- median(dailySummary$sum_steps)

The set of sums is shown in the following histogram.

    print(
        ggplot(dailySummary, aes(sum_steps)) +
            geom_histogram(binwidth = 2000, fill="aquamarine4", col="brown2") +
            scale_x_continuous(breaks = seq(0, 23000, by = 2000)) +
            scale_y_continuous(breaks = seq(0, 22, by = 1)) +
            ggtitle("Histogram: sum of steps taken each day") +
            ylab("Amount of days") +
            xlab("Sum of steps") +
            common_theme
    )

![](PA1_template_files/figure-markdown_strict/histogram%20total%20steps%20by%20day-1.png)

From the previous calculations, the mean of steps taken by day is
**9354**, and the median **10395**.

What is the average daily activity pattern?
-------------------------------------------

Make a time series plot (i.e. type = "l") of the 5-minute interval
(x-axis) and the average number of steps taken, averaged across all days
(y-axis). Although, before that it is needed to calculate the mean

    # Calculating the mean. Pay attention at the field used to group the data.
    intervalSummary <- activity %>%
        group_by(interval2) %>%
        summarise(avg_steps = mean(steps, na.rm=T))

    # Plotting the time series
    print(
        ggplot(intervalSummary, aes(interval2/60, avg_steps)) +
            geom_line(size=0.1) +
            scale_x_continuous(breaks = seq(0, 24, by = 2)) +
            common_theme +
            ggtitle("Mean number of steps per 5-minutes interval") +
            ylab("Mean number of steps") +
            xlab("5-minutes interval (from 0:00 to 24:00)")
    )

![](PA1_template_files/figure-markdown_strict/time-series%20plot%20with%20NA-1.png)

Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?

    maxNumStepsInterval <- na.omit(activity[activity$steps==max(activity$steps, na.rm = T),])

The previous calculation shows that the answer is **806**. This maximum
of steps was taken the date **2012-11-27** at time **615**.

Imputing missing values
-----------------------

Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs).

The strategy for filling in all of the missing values in the dataset
will be calculating the mean of all other days in the same interval.

While Creating a new dataset that is equal to the original dataset but
with the missing data filled in, it will be calculated the number of
NAs.

    # Store the number of NA
    numNA <- 0

    # function fillNA: records-loop to find NAs and overwrite with the mean of the Not Nas in the same interval
    fillNA <- function(data) {
        
        for(i in 1:nrow(data)) {
            if (is.na(data[i,]$steps)) {
                numNA <<- numNA + 1
                data[i,]$steps <- as.integer(round(mean(data[data$interval==data[i,]$interval,]$steps, na.rm=T)))
            }
        }

        data
    }

    activity0NA <- fillNA(activity)

There are **2304** 5-minutes intervals with invalid data as a number of
steps.

A new histogram is shown for the dataset with filled NAs.

    # Calculations: sumary dataset
    dailySummary0NA <- activity0NA %>%
        group_by(date) %>%
        summarise(sum_steps = sum(steps, na.rm=T))

    # Mean and median
    meanStepsByDay0NA <- as.integer(round(mean(dailySummary0NA$sum_steps)))

    medianStepsByDay0NA <- median(dailySummary0NA$sum_steps)

    # Plot the histogram
    print(
        ggplot(dailySummary0NA, aes(sum_steps)) +
            geom_histogram(binwidth = 2000, fill="aquamarine4", col="brown2") +
            scale_x_continuous(breaks = seq(0, 23000, by = 2000)) +
            scale_y_continuous(breaks = seq(0, 22, by = 1)) +
            common_theme +
            ggtitle("Histogram: sum of steps by day") +
            ylab("Amount of days") +
            xlab("Sum of steps")
    )

![](PA1_template_files/figure-markdown_strict/histogram%20total%20steps%20by%20day%20without%20NA-1.png)

After filling the NAs, the new mean of steps taken by day is **10766**,
and the new median **10762**. These new values are greater than the
previously calculated with the dataset with some NAs.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

Create a new factor variable in the dataset with two levels - "weekday"
and "weekend" indicating whether a given date is a weekday or weekend
day.

    activity0NA$typeOfDay <- apply(activity, 1, function(x) {
        dateTime <- ymd(x['date'], tz="UTC")
        if(wday(dateTime) %in% c(1,7)) {
            "weekend"
        } else {
            "weekday"
        }})

    activityWW <- activity0NA %>%
        group_by(typeOfDay, interval) %>%
        summarise(avg_steps = mean(steps))

    activityWW$interval2 <- floor(activityWW$interval/100) * 60
    activityWW$interval2 <- activityWW$interval2 + (activityWW$interval - (floor(activityWW$interval/100) * 100))

Make a panel plot containing a time series plot (i.e. type = "l") of the
5-minute interval (x-axis) and the average number of steps taken,
averaged across all weekday days or weekend days (y-axis).

    print(
        ggplot(activityWW, aes(interval2/60, avg_steps)) +
            geom_line(size=0.1) +
            facet_grid(typeOfDay ~ .)+
            scale_x_continuous(breaks = seq(0, 24, by = 2)) +
            common_theme +
            ggtitle("Mean number of steps per 5-minutes interval all along the period") +
            ylab("Mean number of steps") +
            xlab("5-minutes interval (from 0:00 to 24:00)")
    )

![](PA1_template_files/figure-markdown_strict/weekday-weekend%20time%20series%20plot-1.png)
