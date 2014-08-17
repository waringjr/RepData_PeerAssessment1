
Peer Assessment 1
======================================================================

Loading and preprocessing the data
=====================================
The raw data is read from the working directory into a data frame.  
The date column in the data frame is converted to "date"" data type.


```r
data<-read.csv("activity.csv",header=TRUE)
data[,2]<-as.Date(data[,'date']) #convert 2nd column to date type
```

What is mean total number of steps taken per day?
=========================================

Next, the number of steps are summed over each date.  
NAs are ignored  
The results are plotted in a histogram.  


```r
c<-seq(min(data[,2]),max(data[,2]),by="days")
d<-vector(mode="integer",length=length(c))
df<-data.frame(c,d)
names(df)<-c("date","steps")

for(i in 1:length(c)){
  df[i,2]<-sum(data[data$date==df[i,1],1],na.rm=TRUE)
}
hist(df[,2],xlab="Number of steps per day",main="Part 1, Question 1")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

Part 1, Question 2: calculate the mean and median of the total number of 
steps per day


```r
mean(df[,2])
```

```
## [1] 9354
```

```r
median(df[,2])
```

```
## [1] 10395
```

What is the average daily activity pattern?
===========================================
Now create new data frame (df2) that sums steps across each interval in a day.
The results are shown in a time series plot.


```r
e<-seq(0,max(data[,3]),by=5)
f<-vector(mode="integer",length=length(e))
df2<-data.frame(e,f)
names(df2)<-c("interval","steps")
for(i in 1:length(e)){
  df2[i,2]<-mean(data[data$interval==df2[i,1],1],na.rm=TRUE)
}
plot(df2[,"interval"],df2[,"steps"],type = "l",xlab="Time Interval",ylab="Number of Steps",main="Part 3 Question 1")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
df2[max(df2$steps,na.rm=TRUE),1]
```

```
## [1] 1025
```

Inputing missing values
========================================
Calculate and report the total number of missing values in the dataset

```r
sum(is.na(data[,"steps"]))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset.
Each NA is replaced with the average number of steps per day.

```r
newdata<-data
newdata[is.na(newdata[,"steps"])==TRUE,"steps"]<-mean(data$steps,na.rm=TRUE)
```
This modified data frame is called `newdata`.  
Next, the steps in newdata are summed across each day, as before.  
The results of the summations are stored in data frame df3.  
It is then plotted as a histogram  

```r
df3<-data.frame(c,d)
names(df3)<-c("date","steps")
for(i in 1:length(c)){
  df3[i,2]<-sum(newdata[newdata$date==df3[i,1],1],na.rm=TRUE)
}
hist(df3[,2],xlab="Number of steps per day",main="Part 4, Question 4:NAs replaced")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

Finally, calculate the mean and median of the steps per day in `newdata`

```r
mean(df3[,2])
```

```
## [1] 10766
```

```r
median(df3[,2])
```

```
## [1] 10766
```

Are there differences in activity patterns between weekdays and weekends?
===================================================
A new data frame is created.  The dates are converted to days of the week.  
The days of the week are then modified to either 'weekday' or 'weekend'

```r
d4<-newdata
d4$date<-weekdays(d4$date)
d4[(d4$date=='Sunday'),'date']<-'weekend'
d4[(d4$date=='Saturday'),'date']<-'weekend'
d4[(d4$date=='Monday'),'date']<-'weekday'
d4[(d4$date=='Tuesday'),'date']<-'weekday'
d4[(d4$date=='Wednesday'),'date']<-'weekday'
d4[(d4$date=='Thursday'),'date']<-'weekday'
d4[(d4$date=='Friday'),'date']<-'weekday'
d4$date<-factor(d4$date,levels=c("weekday","weekend"))
weekend<-aggregate(d4[d4$date=='weekend','steps'],list(date=d4[d4$date=='weekend','interval']),mean)
weekday<-aggregate(d4[d4$date=='weekday','steps'],list(date=d4[d4$date=='weekday','interval']),mean)
```
Finally, the steps taken at each interval for weedays and weekends are plotted.

```r
old.par <- par(mfrow=c(2, 1))
plot(weekday[,1],weekday[,2],type='l',main="Weekday steps by time interval",xlab='interval',ylab='steps')
plot(weekend[,1],weekend[,2],type='l',main="Weekend steps by time interval",xlab='interval',ylab='steps')
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
