# Reproducible Research: Peer Assessment 1
library(dplyr)
library(lubridate)
library(ggplot2)

## Loading and preprocessing the data
####1. Load the data (i.e. read.csv()) and
data <- read.csv("activity.csv", 
                  header = TRUE, 
                  sep = ',', 
                  colClasses = c("numeric", "character", "integer"))
                  
####2. Process/transform the data (if necessary) into a format suitable for your analysis
data$date <- ymd(data$date)

## What is mean total number of steps taken per day?

steps <- data %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print

####1. Make a histogram of the total number of steps taken each day
ggplot(steps, aes(x = steps)) +
  geom_histogram(fill = "red", binwidth = 1000) +
  labs(title = "Histogram of Steps per day", x = "Steps per day", y = "Frequency")
  
####2. Calculate and report the mean and median of the total number of steps taken per day  
mean_steps <- mean(steps$steps, na.rm = TRUE)
median_steps <- median(steps$steps, na.rm = TRUE)

#####Mean 
mean_steps

#####Median
median_steps
  
## What is the average daily activity pattern?


interval <- data %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps = mean(steps))
  
####1. Make a time series plot

  ggplot(interval, aes(x=interval, y=steps)) +
  geom_line(color = "red")
  
####2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

interval[which.max(interval$steps),]

## Imputing missing values
####1. Calculate and report the total number of missing values in the dataset
sum(is.na(data$steps))

####2. Devise a strategy for filling in all of the missing values in the dataset
####3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

data_full <- data
nas <- is.na(data_full$steps)
avg_interval <- tapply(data_full$steps, data_full$interval, mean, na.rm=TRUE, simplify=TRUE)
data_full$steps[nas] <- avg_interval[as.character(data_full$interval[nas])]
sum(is.na(data_full$steps))

steps_full <- data_full %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print
  
####4. Make a histogram of the total number of steps taken each day

ggplot(steps_full, aes(x = steps)) +
  geom_histogram(fill = "red", binwidth = 1000) +
  labs(title = "Histogram of Steps per day, including missing values", x = "Steps per day", y = "Frequency")  
  
####... and Calculate and report the mean and median total number of steps taken per day.
mean_steps_full <- mean(steps_full$steps, na.rm = TRUE)
median_steps_full <- median(steps_full$steps, na.rm = TRUE)

mean_steps_full

median_steps_full
  
## Are there differences in activity patterns between weekdays and weekends?

####1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

data_full <- mutate(data_full, weektype = ifelse(weekdays(data_full$date) == "Saturday" | weekdays(data_full$date) == "Sunday", "weekend", "weekday"))
data_full$weektype <- as.factor(data_full$weektype)
head(data_full)

####2. Make a panel plot containing a time series plot

interval_full <- data_full %>%
  group_by(interval, weektype) %>%
  summarise(steps = mean(steps))
s <- ggplot(interval_full, aes(x=interval, y=steps, color = weektype)) +
  geom_line() +
  facet_wrap(~weektype, ncol = 1, nrow=2)
print(s)

