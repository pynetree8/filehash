#Coursera Project 1 R Markdown File

This is my first attempt at creating an R Markdown File. This was created to satisfy the course requirements for Project 1 of the Reproducible Research Course on Coursera.

Here I will load my data:


```r
library(ggplot2)
library(plyr)
library(lattice)

#read in data
activity <- read.csv("activity.csv")
#transform to date
activity$day <-weekdays(as.Date(activity$date))
```

##What is mean total number of steps taken per day?

With this code I will calculate the average number of steps per day.


```r
Sumperday<-aggregate(activity$steps~ activity$date+activity$day, FUN=sum,)
colnames(Sumperday)<- c("Date", "Day", "Steps")
```

Now I will create a histogram from that data

```r
hist1<-hist(Sumperday$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

Finally, I will determine some basic statistics

```r
mean(Sumperday$Steps)
```

```
## [1] 10766.19
```

```r
median(Sumperday$Steps)
```

```
## [1] 10765
```

##What is the average daily activity pattern?

First I will make a time series plot for every 5 minute interval across a day.

```r
cleanedup <- activity[!is.na(activity$steps),]

intervalavg <- ddply(cleanedup, .(interval), summarize, Avg = mean(steps))

p <- ggplot(intervalavg, aes(x=interval, y=Avg), xlab = "Interval", ylab="Average Number of Steps")
p + geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

Then, I will identify the interval with the highest average activity level.

```r
max <- max(intervalavg$Avg)
intervalwithmax<-intervalavg[intervalavg$Avg==max,1]
intervalwithmax
```

```
## [1] 835
```

##Imputing missing values

First I will calculate how many NAs there are in my dataset.

```r
nummissing<-nrow(activity[is.na(activity$steps),])
nummissing
```

```
## [1] 2304
```
Second, I will develop a strategy to fill in the missing values.

```r
avgTable <- ddply(cleanedup, .(interval, day), summarize, Avg = mean(steps))
nas <- activity[is.na(activity$steps),]
merged<-merge(nas, avgTable, by=c("interval", "day") )
merged<-merged[,c(5,4,1,2)]
colnames(merged)[1]<-"steps"
```
Third, I will create a new dataset.

```r
newdata<-rbind(cleanedup, merged)
```
Finally, I will make a histogram like the first and new summary stats . 

```r
Sumperday2<-aggregate(newdata$steps~ newdata$date+newdata$day, FUN=sum,)
colnames(Sumperday2)<- c("Date", "Day", "Steps")

hist2<-hist(Sumperday2$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

```r
mean(Sumperday2$Steps)
```

```
## [1] 10821.21
```

```r
median(Sumperday2$Steps)
```

```
## [1] 11015
```

##Are there differences in activity patterns between weekdays and weekends?

First, I will create a new factor labeling by the class of the day of the week.

```r
newdata$weekclass <-ifelse(newdata$day %in% c("Saturday", "Sunday"), "weekend", "weekday")
intervalTable2 <- ddply(newdata, .(interval, weekclass), summarize, Avg = mean(steps))
```

Second, I will make a lattice plot comparing the two histograms


```r
xyplot(Avg~interval|weekclass, data=intervalTable2, type="l",  layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       ylab="Average Number of Steps", xlab="Interval")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)
