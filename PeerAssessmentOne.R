#Load the data and change date column to date format
df<-read.csv("activity.csv")
df$date <- as.Date(df$date)

#What is mean total number of steps taken per day? 
#For this part of the assignment, you can ignore the missing values in the dataset.
#Calculate the total number of steps taken per day
stepsPerDay<-aggregate(steps~date, data=df, FUN=sum, na.rm="TRUE")

#Make a histogram of the total number of steps taken each day
library(ggplot2)
hist <- ggplot(stepsPerDay,aes(x = steps)) +
            ggtitle("Steps Per Day") +
            xlab("Steps (binwidth 2000)") +
            geom_histogram(binwidth = 2000)
hist

#Calculate and report the mean and median of the total number of steps taken per day
avg<-mean(stepsPerDay$steps, na.rm="TRUE")
med<-median(stepsPerDay$steps, na.rm="TRUE")


#What is the average daily activity pattern?
#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
#the average number of steps taken, averaged across all days (y-axis)
intervalSteps <- aggregate(steps~interval, data=df, FUN=mean, na.rm=TRUE)

ggplot(intervalSteps, aes(x=interval, y=steps)) +   
  geom_line(color="blue", size=1) +  
  labs(title="Average Daily Activity Pattern", x="Interval", y="Number of steps")

#Which 5-minute interval, on average across all the days in the dataset, 
#contains the maximum number of steps?
maxInterval <- intervalSteps[which.max(intervalSteps$steps),]


#Impute missing values
#Note that there are a number of days/intervals where there are missing values 
#(coded as NA). The presence of missing days may introduce bias into some calculations
#or summaries of the data.
#Calculate and report the total number of missing values in the dataset (i.e. the 
#total number of rows with NAs)
numNA <- nrow(df[which(is.na(df$steps)),])

#Devise a strategy for filling in all of the missing values in the dataset. 
#Strategy: use mean value.

#Create a new dataset that is equal to the original dataset but with the missing data
#filled in.
dfNew <- transform(df,
                   steps=ifelse(is.na(df$steps),
                                intervalSteps[match(intervalSteps$interval, df$interval), ]$steps,
                                df$steps))

#Make a histogram of the total number of steps taken each day and Calculate and 
#report the mean and median total number of steps taken per day. Do these values 
#differ from the estimates from the first part of the assignment? 
#What is the impact of imputing missing data on the estimates of the total daily 
#number of steps?
stepsPerDayNew<-aggregate(steps~date, data=dfNew, FUN=sum, na.rm="TRUE")
library(ggplot2)
hist2 <- ggplot(stepsPerDayNew,aes(x = steps)) +
  ggtitle("Steps Per Day") +
  xlab("Steps (binwidth 2000)") +
  geom_histogram(binwidth = 2000)
hist2

avgNew<-mean(stepsPerDayNew$steps)
medNew<-median(stepsPerDayNew$steps)
#There is no impact on the mean (it remains the same)
#There is a slight impact on the median; the new median now matches the mean.

#Are there differences in activity patterns between weekdays and weekends?
#For this part the weekdays() function may be of some help here. 
#Use the dataset with the filled-in missing values for this part.
#Create a new factor variable in the dataset with two levels – “weekday” and 
#“weekend” indicating whether a given date is a weekday or weekend day.
dfNew$weekdays <- as.factor(ifelse(weekdays(dfNew$date) %in% c("Saturday", "Sunday"),
                                   "weekend", "weekday"))

#Make a panel plot containing a time series plot (i.e. type = "l") of the
#5-minute interval (x-axis) and the average number of steps taken, averaged 
#across all weekday days or weekend days (y-axis). See the README file in the
#GitHub repository to see an example of what this plot should look like using 
#simulated data.
weekAgg <- aggregate(steps ~ weekdays + interval, dfNew, FUN=mean)
ggplot(weekAgg, aes(x=interval, y=steps)) + 
  geom_line(color="green") + 
  facet_wrap(~ weekdays, nrow=2, ncol=1) +
  labs(x="Interval", y="Number of steps")