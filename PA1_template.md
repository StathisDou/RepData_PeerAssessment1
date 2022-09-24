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

## Imputing missing values

## Are there differences in activity patterns between weekdays and weekends?

