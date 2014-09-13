# Reproducible research - Peer Assessment 1
#
library (ggplot2)
library(knitr)

# Set working directory
setwd("~/Documenti/Coursera/DS5_Reproducible_Research/Prj1/RepData_PeerAssessment1/")
#
# Load the dataset
fileN <- "../dataset/activity.csv"
ds_activity <- read.csv(fileN, header=TRUE, sep=",", na.strings="NA")
#
# Convert dates
ds_activity$date <- as.Date(ds_activity$date, "%Y-%m-%d")

#
# What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the dataset.
# 1) Make a histogram of the total number of steps taken each day
# 2) Calculate and report the mean and median total number of steps taken per day

dates <- data.frame(unique(ds_activity$date))
for (i in 1:dim(dates)[1]) {
   dates[i, 2] <- sum(ds_activity$steps[ds_activity$date == dates[i, 1]], na.rm=FALSE)
}
names(dates) <- c("Day", "Total_steps")
total_mean_steps <- mean(dates[, 2], na.rm=TRUE)
total_median_steps <- median(dates[, 2], na.rm=TRUE)

pl <- ggplot(dates, aes(x = Day, y = Total_steps, group=1)) +
   theme(axis.text.x = element_text(size = rel(1.0), angle = 90, hjust = 1)) +
   labs(x = "Day",
        y = "Total steps") +
   ggtitle("Total steps per day") +
   scale_x_date(breaks="2 day")

pl + geom_bar(stat = "identity", position = "stack", width=0.8)


# What is the average daily activity pattern?
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

daily_pattern <- data.frame(unique(ds_activity$interval))
for (i in 1:dim(daily_pattern)[1]) {
   daily_pattern[i, 2] <- 
      mean(ds_activity$steps[ds_activity$interval == daily_pattern[i, 1]], na.rm=TRUE)
}
names(daily_pattern) <- c("Interval", "Average_steps")

daily_pattern[which(daily_pattern$Average_steps==max(daily_pattern$Average_steps)),1]

pl2 <- ggplot(daily_pattern, aes(x = Interval, y = Average_steps, group=1)) +
   theme(axis.text.x = element_text(size = rel(1.0), angle = 90, hjust = 1)) +
   labs(x = "Interval",
        y = "Average steps") +
   ggtitle("Average steps per interval")

pl2 + geom_line(width=0.8)




# Imputing missing values
# Note that there are a number of days/intervals where there are missing
# values (coded as NA). The presence of missing days may introduce bias into
# some calculations or summaries of the data.

#1) Calculate and report the total number of missing values in the dataset
# (i.e. the total number of rows with NAs)

#nas <- ds_activity[is.na(ds_activity[,1]),]
nas <- as.vector(which(is.na(ds_activity[,1])))
length(nas)

#2) Devise a strategy for filling in all of the missing values in the dataset.
# The strategy does not need to be sophisticated. For example, you could use
# the mean/median for that day, or the mean for that 5-minute interval, etc.

#3) Create a new dataset that is equal to the original dataset but with
# the missing data filled in.

# New database
ds_activity_2 <- ds_activity

# NA substitution with average steps for the same interval
for (i in 1:length(nas)) {
   nainterv <- ds_activity$interval[nas[i]]
   avg <- daily_pattern$Average_steps[daily_pattern$Interval == nainterv]
   ds_activity_2[nas[i], 1] <- avg
}



#4) Make a histogram of the total number of steps taken each day and
# Calculate and report the mean and median total number of steps taken
# per day. Do these values differ from the estimates from the first part
# of the assignment? What is the impact of imputing missing data on the
# estimates of the total daily number of steps?

dates2 <- data.frame(unique(ds_activity_2$date))
for (i in 1:dim(dates2)[1]) {
   dates2[i, 2] <- 
      sum(ds_activity_2$steps[ds_activity_2$date == dates2[i, 1]], na.rm=FALSE)
}
names(dates2) <- c("Day", "Total_steps")
total_mean_steps2 <- mean(dates2[, 2], na.rm=TRUE)
total_median_steps2 <- median(dates2[, 2], na.rm=TRUE)

pl3 <- ggplot(dates2, aes(x = Day, y = Total_steps, group=1)) +
   theme(axis.text.x = element_text(size = rel(1.0), angle = 90, hjust = 1)) +
   labs(x = "Day",
        y = "Total steps") +
   ggtitle("Total steps per day") +
   scale_x_date(breaks="2 day")

pl3 + geom_bar(stat = "identity", position = "stack", width=0.8)


# Are there differences in activity patterns between weekdays and weekends?

wdwe <- vector(mode="character", length=dim(ds_activity_2)[1])
for (i in 1:dim(ds_activity_2)[1]) {
   wd <- weekdays(ds_activity_2[i, 2])
   if (wd == "Saturday" | wd == "Sunday"){
      wdwe[i] <- "Weekend"
   }
   else {
      wdwe[i] <- "Weekday"
   }
}

wdwe <- as.factor(wdwe)

daily_pattern2 <- data.frame(unique(ds_activity_2$interval))
daily_pattern2 <- rbind(daily_pattern2, daily_pattern2)

n4 <- as.numeric(dim(daily_pattern2)[1])
n2 <- n4/2
n3 <- n2+1

for (i in 1:n2) {
   daily_pattern2[i, 2] <-
      mean(ds_activity_2$steps[
         ds_activity_2$interval == daily_pattern2[i, 1] &
         wdwe == "Weekend"
            ], na.rm=TRUE)
   daily_pattern2[i, 3] <- "Weekend"
}

for (i in n3:n4) {
   daily_pattern2[i, 2] <-
   mean(ds_activity_2$steps[
      ds_activity_2$interval == daily_pattern2[i, 1] &
         wdwe == "Weekday"
      ], na.rm=TRUE)
   daily_pattern2[i, 3] <- "Weekday"
}

names(daily_pattern2) <-
   c("Interval", "Average_steps", "wdwe")

pl4 <- ggplot(daily_pattern2, aes(x = Interval, y = Average_steps, group=1)) +
   theme(axis.text.x = element_text(size = rel(1.0), angle = 90, hjust = 1)) +
   labs(x = "Interval",
        y = "Average steps") +
   ggtitle("Average steps per interval") + 
   geom_line(width=0.8) + 
   facet_grid(wdwe ~ .)

pl4