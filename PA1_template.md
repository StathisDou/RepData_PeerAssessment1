---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data


```r
unzip("activity.zip")                     #unzip the zip file with our data
bufferdata<-read.csv("./activity.csv")    #read our data
```

## What is mean total number of steps taken per day?


```r
library(ggplot2)                          #load ggplot2 package
#Create a data frame with step summaries
histdata<-data.frame(with(bufferdata,tapply(bufferdata$steps,bufferdata$date,FUN=sum, na.rm=TRUE)))
#Change col name
colnames(histdata)<-c("steps")
#Create histogram plot
qplot(histdata$steps, binwidth = 1200, xlab = "(steps number) / (day)",fill = I("lightsalmon2"),ylim=c(0,11),colour=I("red"))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
#Get mean
paste("mean steps : ", mean(histdata$steps))
```

```
## [1] "mean steps :  9354.22950819672"
```

```r
#Get median
paste("median steps : ", median(histdata$steps))
```

```
## [1] "median steps :  10395"
```

## What is the average daily activity pattern?


```r
#Aggregate mean steps by interval
stepsinv<-aggregate(steps ~ interval, bufferdata, mean)
#Plot for interval and steps on aggregated table
ggplot(data = stepsinv, aes(x = interval, y = steps, color="red")) + geom_line() + xlab("5 minute interval") + ylab("average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#Get interval with most average steps and the average for it
stepsinv[which.max(stepsinv$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values


```r
#In order to fill the missing values we'll first check how many of them we have
paste("Rows with NA are", sum(is.na(bufferdata$steps)))
```

```
## [1] "Rows with NA are 2304"
```

```r
#We replace NA values with mean value
tempvec<-(replace(bufferdata$steps, is.na(bufferdata$steps), mean(bufferdata$steps, na.rm = TRUE)))
#clone the bufferdata
imputeddata<-bufferdata
#replace steps with imputed steps (NA-mean)
imputeddata$steps<-tempvec
#Re-create our histogram with imputed date
histdataQ<-data.frame(with(imputeddata,tapply(imputeddata$steps,imputeddata$date,FUN=sum, na.rm=TRUE)))
colnames(histdataQ)<-c("steps")
qplot(histdataQ$steps, binwidth = 1200, xlab = "(steps number) / (day)",fill = I("lightsalmon2"),ylim=c(0,11),colour=I("red"))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
#Re-calculate mean with imputed values
paste("mean steps : ", mean(histdataQ$steps))
```

```
## [1] "mean steps :  10766.1886792453"
```

```r
#Re-calculate median with imputed values
paste("median steps : ", median(histdataQ$steps))
```

```
## [1] "median steps :  10766.1886792453"
```

## Are there differences in activity patterns between weekdays and weekends?


