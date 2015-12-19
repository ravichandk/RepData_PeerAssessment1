#************Loading and preprocessing the data************#
#Unzip the csv
unzip("repdata-data-activity.zip")

#read csv
activity <- read.csv("activity.csv")
#**********************************************#

#***************What is mean total number of steps taken per day?********#
totalStepPerDay <- tapply(activity$steps, activity$date, sum, na.rm = TRUE)
qplot(totalStepPerDay, xlab = "Total steps per day")
mean(totalStepPerDay)
median(totalStepPerDay)
#***************What is mean total number of steps taken per day?********#

#************What is the average daily activity pattern?*************#
averageStepsPerInterval <- aggregate(x = list(steps = activity$steps), by = list(interval = activity$interval), FUN = mean, na.rm = TRUE)
ggplot(data = averageStepsPerInterval, aes(x = averageStepsPerInterval$interval, y = averageStepsPerInterval$steps)) 
	+ geom_line() 
	+ xlab("5-minute interval") 
	+ ylab("Average steps taken")
	
 averageStepsPerInterval[which.max(averageStepsPerInterval$steps),]
#************What is the average daily activity pattern?*************#

#*********************Imputing missing values******************************#
missingValues <- is.na(activity$steps)
table(missingValues)

filledData <- activity

for(index in 1:length(filledData[[1]])) {
    if(is.na(filledData[index,]$steps)) {
        filledData[index,]$steps <- averageStepsPerInterval[averageStepsPerInterval$interval == filledData[index,]$interval, "steps"]
    }
}

totalStepPerDayAfterFillingData <- tapply(filledData$steps, filledData$date, sum, na.rm = TRUE)
qplot(totalStepPerDayAfterFillingData, xlab = "Total steps per day")
mean(totalStepPerDayAfterFillingData)
median(totalStepPerDayAfterFillingData)
#*********************Imputing missing values******************************#

#******************Are there differences in activity patterns between weekdays and weekends?***********#
weekday <- function(date) {
    day <- weekdays(date)
    if(day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    {
        return("Weekday")
    } else if (day %in% c("Saturday", "Sunday")) {
        return("Weekend")
    }
    return("Invalid date")
}
filledData$day <- sapply(as.Date(filledData$date), FUN = weekday)

averageStepsPerWeekday <- aggregate(steps ~ interval + day, data = filledData, mean)

ggplot(averageStepsPerWeekday, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + xlab("5 minute interval") + ylab("Number of steps")
#******************Are there differences in activity patterns between weekdays and weekends?***********#

