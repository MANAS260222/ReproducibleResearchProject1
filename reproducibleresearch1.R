library(knitr)
library(ggplot2)
library(dplyr)
library(plyr)
temperature <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", temperature, mode="wb")
unzip(temperature, "activity.csv")
activity <- read.csv("activity.csv",header=T)
unlink(temperature)
#1
totalstepsdaily <- aggregate(steps ~ date, data = activity, FUN = sum, na.rm = TRUE)
head(totalstepsdaily)
#2
activity$date <- as.Date(activity$date, "%Y-%m-%d")
hist(totalstepsdaily$steps, 
     main="Total Steps every day", 
     xlab="Number of Steps every Day", 
     ylab = "Interval",
     col="red",
     breaks=50)
#3
meansteps <- mean(totalstepsdaily$steps)
meansteps
mediansteps <- median(totalstepsdaily$steps)
mediansteps
summary(totalstepsdaily)
#4
fiveminitue <- aggregate(steps ~ interval, data = activity, FUN = mean, na.rm = TRUE)
plot(x = fiveminitue$interval, 
     y = fiveminitue$steps, 
     type = "l", 
     col = "orange",
     xlab = "5-minute Intervals",
     ylab = "Average Steps Taken ~ Days",
     main = "Average Daily Activity Pattern")
#5
maxsteps <- fiveminitue$interval[which.max(fiveminitue$steps)]
maxsteps
#6 
activity2 <- activity
nas <- is.na(activity2$steps)
avg_interval <- tapply(activity2$steps, activity2$interval, mean, na.rm=TRUE, simplify = TRUE)
activity2$steps[nas] <- avg_interval[as.character(activity2$interval[nas])]
names(activity2)
#7 Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
#Plotting
#Setting up the pannel for one row and two columns
par(mfrow=c(1,2))

##  analysis without NAs 
totalstepsperday2 <- aggregate(steps ~ date, data = activity2, FUN = sum, na.rm = TRUE)
head(totalstepsperday2)
#histogram with zero nas
hist(totalstepsperday2$steps, 
     main = "Total Steps per Day (no-NA)", 
     xlab = "Number of Steps per Day", 
     ylab = "Interval",
     col="green",
     breaks=50)
##Histogram with the orginal dataset
hist(totalstepsperday$steps, 
     main="Total Steps per Day (Original)", 
     xlab="Number of Steps per Day", 
     ylab = "Interval",
     col="orange",
     breaks=50)
#Resetting the panel
par(mfrow=c(1,1))
## What is the impact of imputing data?
summary(totalstepsperday)

summary(totalstepsperday2)

head(activity2)

## Add the new weekend/weekday field
activity2<- activity2%>%
  mutate(typeofday= ifelse(weekdays(activity2$date)=="Saturday" | weekdays(activity2$date)=="Sunday", "Weekend", "Weekday"))
head(activity2)

#8 Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

## Plot - Line chart
fivemin2<- aggregate(steps ~ interval, data = activity2, FUN = mean, na.rm = TRUE)
head(fivemin2)

ggplot(activity2, aes(x =interval , y=steps, color=typeofday)) +
  geom_line() +
  labs(title = "Ave Daily Steps (type of day)", x = "Interval", y = "Total Number of Steps") +
  facet_wrap(~ typeofday, ncol = 1, nrow=2)


