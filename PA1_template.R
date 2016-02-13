library(knitr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(lattice)


# extract the csv file from the zip file and load the data
if(!file.exists("activity.csv")) {
        data <- read.csv(unz("activity.zip", "activity.csv"), header = TRUE,
                         sep = ',', colClasses = c("numeric", "character", "integer"))
        data$date <- ymd(data$date)
}

# tage a look at the sturcture and daa
str(data)
head(data)

# aggregate steps by date
steps.by.day <- aggregate (steps~date, data, sum)

# make a histogram of the total number of steps taken each day
hist(steps.by.day$steps, 
     col="blue", 
     main = "Histogram of Steps Taken Each Day",
     xlab = "Steps per Day")

# Calculate and report the mean and meadian of total steps per day
summary(steps.by.day$steps)

# make the time series plot
steps.by.interval <- aggregate(steps ~ interval, data, mean)
plot(steps.by.interval$interval,
     steps.by.interval$steps,
     type = "l",
     main = "Average Number of Steps Taken By 5-Minute Interval",
     xlab = "Interval",
     ylab = "Number of Steps"
)

# Calculate the number of rows with NAs
incomplete <- sum(!complete.cases(data))

# fill in the missing values for the data by inserting the average for that interval
data.imputed <- transform(data, steps = ifelse(is.na(data$steps), steps.by.interval$steps[match(data$interval, steps.by.interval$interval)], data$steps))
data.imputed[as.character(data.imputed$date) == "2012-10-01", 1] <- 0

# aggregate steps by date for the imputed data
steps.by.day.imputed <- aggregate (steps~date, data.imputed, sum)

# make a histogram of the total number of steps taken each day
hist(steps.by.day.imputed$steps, 
     col="red", 
     main = "Steps Taken Each Day",
     xlab = "Steps per Day")

# make a histogram for the non imputed data and place it on top
hist(steps.by.day$steps, 
     col="blue", 
     main = "Steps Taken Each Day",
     xlab = "Steps per Day",
     add = TRUE)

# add a legend
legend("topright", c("Imputed", "Non-imputed"), 
       lty=c(1,1),
       lwd=c(2.5,2.5),
       cex=0.8,
       col=c("blue", "red"))

# Calculate and report the mean and meadian of total steps per day
summary(steps.by.day.imputed$steps)
summary(steps.by.day$steps)

# define DoW for weekdays
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

# create a factor variable called dow with 2-levels for weekdays and weekends
data.imputed$dow = as.factor(ifelse(is.element(weekdays(as.Date(data.imputed$date)), 
                                               weekdays), "Weekdays", "Weekend"))

# compute average number of steps taken for each interval
steps.by.interval.int <- aggregate(steps ~ interval + dow, data.imputed, mean)

# time series plot steps by interval for weekend and weekdays
xyplot(steps.by.interval.int$steps ~ steps.by.interval.int$interval|steps.by.interval.int$dow, 
       main="Average Steps per Day by Interval",
       xlab="Interval", 
       ylab="Steps",
       layout=c(1,2), 
       type="l"
)
