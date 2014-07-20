## Reproducible Research - Assignment #1
#### Igor Protsenko, Jul 19th, 2014
========================================



### Loading and preprocessing the data

* Loading data from `activity.zip` file


```r
unzip("activity.zip")
data <- read.csv("activity.csv")
summary(data)
```

```
##      steps               date          interval   
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589  
##  Median :  0.0   2012-10-03:  288   Median :1178  
##  Mean   : 37.4   2012-10-04:  288   Mean   :1178  
##  3rd Qu.: 12.0   2012-10-05:  288   3rd Qu.:1766  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355  
##  NA's   :2304    (Other)   :15840
```

* Processing data into format suitable for analysis


### What is mean total number of steps taken per day?

* Making a histogram of the total number of steps taken each day


```r
# aggregating data by date
temp <- aggregate(steps~date,data=data,sum,na.rm=T)
# plotting a histogram
hist(temp$steps, main = "Distribution of Daily Steps", xlab = "Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

* Calculating **mean** and **median** total number of steps taken per day


```r
cat("Mean:",mean(temp$steps),"\n","Median:",median(temp$steps))
```

```
## Mean: 10766 
##  Median: 10765
```

### What is the average daily activity pattern?

* Making a time series plot of average steps taken per 5-minute interval across days


```r
# aggregating data by date
temp <- aggregate(steps~interval,data=data,mean,na.rm=T)
#plotting time series
plot(temp$interval,temp$steps,type="l",
     main = "Average Steps per Interval across Days",
     xlab = "Intervals",
     ylab = "Steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

* Calculating, which 5-minute interval, on average across days, contains maximum number of steps


```r
# returning an observation with maximum number of steps
temp[which(temp$steps==max(temp$steps)),]
```

```
##     interval steps
## 104      835 206.2
```

### Imputting missing values

* Calculating total number of rows wih NA's in the dataset


```r
# calculating and printing number of rows with NA's
cat("Number of rows with NA's:",length(which(rowSums(is.na(data)) > 0)))
```

```
## Number of rows with NA's: 2304
```

```r
# printing summary of a subset consisiting of NA rows
summary(data[rowSums(is.na(data)) > 0, ])
```

```
##      steps              date        interval   
##  Min.   : NA    2012-10-01:288   Min.   :   0  
##  1st Qu.: NA    2012-10-08:288   1st Qu.: 589  
##  Median : NA    2012-11-01:288   Median :1178  
##  Mean   :NaN    2012-11-04:288   Mean   :1178  
##  3rd Qu.: NA    2012-11-09:288   3rd Qu.:1766  
##  Max.   : NA    2012-11-10:288   Max.   :2355  
##  NA's   :2304   (Other)   :576
```

* NA Removal Strategy: substitute NA's with Means for that interval


```r
# getting row numbers with NA's
rowsNA <- which(is.na(data$steps))
head(rowsNA)
```

```
## [1] 1 2 3 4 5 6
```

```r
# calculating Means for each interval across days to use as NA substitute
meansNA <- aggregate(steps~interval,data=data,mean,na.rm=T)
head(meansNA)
```

```
##   interval   steps
## 1        0 1.71698
## 2        5 0.33962
## 3       10 0.13208
## 4       15 0.15094
## 5       20 0.07547
## 6       25 2.09434
```

* Creating a new dataset with NA's filled in with Means for the interval


```r
newdata <- data
# substituting NA's for Means for the interval
temp <- data.frame(rowsNA,data[which(is.na(data$steps)),"interval"])
names(temp) <- c("rowsNA","interval")
temp <- merge(temp,meansNA,by="interval")
newdata[rowsNA,"steps"] <- temp[which(temp$rowsNA==rowsNA),"steps"]
#head(data)
#head(newdata)
summary(data)
```

```
##      steps               date          interval   
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589  
##  Median :  0.0   2012-10-03:  288   Median :1178  
##  Mean   : 37.4   2012-10-04:  288   Mean   :1178  
##  3rd Qu.: 12.0   2012-10-05:  288   3rd Qu.:1766  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355  
##  NA's   :2304    (Other)   :15840
```

```r
summary(newdata)
```

```
##      steps               date          interval   
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589  
##  Median :  0.0   2012-10-03:  288   Median :1178  
##  Mean   : 35.6   2012-10-04:  288   Mean   :1178  
##  3rd Qu.: 24.0   2012-10-05:  288   3rd Qu.:1766  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355  
##                  (Other)   :15840
```

* Make a histogram


```r
# aggregating data by date
tempOld <- aggregate(steps~date,data=data,sum,na.rm=T)
tempNew <- aggregate(steps~date,data=newdata,sum,na.rm=T)
# plotting two histograms side-by-side
par(mfrow=c(1,2))
hist(tempOld$steps, main = "Daily Steps (data with NA's)", xlab = "Steps")
hist(tempNew$steps, main = "Daily Steps (data without NA's)", xlab = "Steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

```r
# calculating Mean and Median for old and new datasets
cat("Mean (NA=Yes):",mean(tempOld$steps),"\n","Median (NA=Yes):",median(tempOld$steps),"\n\n",
    "Mean (NA=No):",mean(tempNew$steps),"\n","Median (NA=No):",median(tempNew$steps))
```

```
## Mean (NA=Yes): 10766 
##  Median (NA=Yes): 10765 
## 
##  Mean (NA=No): 10255 
##  Median (NA=No): 10395
```

### Are there differences in activity patterns between weekdays and weekends?

* Creating a new Weekday/Weekend factor variable


```r
# creating new variable
newdata$ww <- weekdays(as.Date(newdata$date))
# changing factor levels to weekday/weekend
newdata[which(
        newdata$ww %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")
              ),"ww"] <- "weekday"
newdata[which(newdata$ww!="weekday"),"ww"] <- "weekend"
newdata$ww <- as.factor(newdata$ww)
summary(newdata)
```

```
##      steps               date          interval          ww       
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0   weekday:12960  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589   weekend: 4608  
##  Median :  0.0   2012-10-03:  288   Median :1178                  
##  Mean   : 35.6   2012-10-04:  288   Mean   :1178                  
##  3rd Qu.: 24.0   2012-10-05:  288   3rd Qu.:1766                  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355                  
##                  (Other)   :15840
```

* Creating a panel plot with average steps per interval averaged across weekday/weekend


