#PA1-RScript

unzip("activity.zip")                     #unzip the zip file with our data
bufferdata<-read.csv("./activity.csv")    #read our data

library(ggplot2)                          #load ggplot2 package
#Create a data frame with step summaries
histdata<-data.frame(with(bufferdata,tapply(bufferdata$steps,bufferdata$date,FUN=sum, na.rm=TRUE)))
#Change col name
colnames(histdata)<-c("steps")
#Create histogram plot
png(filename = "plot1_stepsum.png", width = 480, height = 480, units = "px",bg = "white")
qplot(histdata$steps, binwidth = 1200, xlab = "(steps number) / (day)",fill = I("lightsalmon2"),ylim=c(0,11),colour=I("red"))
dev.off()
#Get mean
paste("mean steps : ", mean(histdata$steps))
#Get median
paste("median steps : ", median(histdata$steps))

#Aggregate mean steps by interval
stepsinv<-aggregate(steps ~ interval, bufferdata, mean)
#Plot for interval and steps on aggregated table
png(filename = "plot2_intervals.png", width = 480, height = 480, units = "px",bg = "white")
ggplot(data = stepsinv, aes(x = interval, y = steps, color="red")) + geom_line() + xlab("5 minute interval") + ylab("average number of steps taken")
dev.off()
#Get interval with most average steps and the average for it
stepsinv[which.max(stepsinv$steps), ]

#In order to fill the missing values we'll first check how many of them we have
paste("Rows with NA are", sum(is.na(bufferdata$steps)))

#We replace NA values with mean value
tempvec<-(replace(bufferdata$steps, is.na(bufferdata$steps), mean(bufferdata$steps, na.rm = TRUE)))
#clone the bufferdata
imputeddata<-bufferdata
#replace steps with imputed steps (NA-mean)
imputeddata$steps<-tempvec
#Re-create our histogram with imputed date

histdataQ<-data.frame(with(imputeddata,tapply(imputeddata$steps,imputeddata$date,FUN=sum, na.rm=TRUE)))
colnames(histdataQ)<-c("steps")
png(filename = "plot3_imputed.png", width = 480, height = 480, units = "px",bg = "white")
qplot(histdataQ$steps, binwidth = 1200, xlab = "(steps number) / (day)",fill = I("lightsalmon2"),ylim=c(0,11),colour=I("red"))
dev.off()

#Re-calculate mean with imputed values
paste("mean steps : ", mean(histdataQ$steps))
#Re-calculate median with imputed values
paste("median steps : ", median(histdataQ$steps))

#Save imputed data on a new table
hd1<-imputeddata
#Converting date col to date and then to weekday
hd1$date<-weekdays(as.Date(hd1$date))
#Converting actual weekday to factor (Weekend or Weekday day)
hd1$date <- ifelse(hd1$date == "Saturday"|hd1$date=="Sunday", "Weekend", "Weekday")   
#Create an aggregated table by (week(day/end)-factor and interval
meanfactorday<-aggregate(steps ~ interval+date, hd1, mean)
#Comparison side by side
png(filename = "plot4_sidebyside.png", width = 480, height = 480, units = "px",bg = "white")
ggplot(data = meanfactorday, aes(x = interval, y = steps,color=date)) + geom_line()+facet_grid(.~date) + xlab("5 minute interval") + ylab("average number of steps taken")
dev.off()
#Comparison on same plot
png(filename = "plot5_combined.png", width = 480, height = 480, units = "px",bg = "white")
ggplot(data = meanfactorday, aes(x = interval, y = steps,color=date)) + geom_line() + xlab("5 minute interval") + ylab("average number of steps taken")
dev.off()