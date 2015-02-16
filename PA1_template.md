# Reproducible Research: Peer Assessment 1
Piyush Madan  
## 1. Loading and preprocessing the data 
###1.1 Set working directory to directory having cloned repo of [Assignment](https://github.com/rdpeng/RepData_PeerAssessment1), extract zip  & read the file

```r
setwd("~/Documents/projects/RepData_PeerAssessment1")
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

```r
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
###1.2 Use variables in correct format

```r
data$date <- as.Date(data$date)
#data$steps <- as.integer(data$steps)
#data[is.na(data$steps),]$steps <- 0
```

## 2. What is mean total number of steps taken per day?
###2.1 Calculate sum of steps per day

```r
library(data.table)
DT <- data.table(data)
#get sum of steps per day
DT_daywise <- DT[, sum(steps), by = date]
```
Count of Total steps per day (printing top 10 result)

```r
head(DT_daywise,10)
```

```
##           date    V1
##  1: 2012-10-01    NA
##  2: 2012-10-02   126
##  3: 2012-10-03 11352
##  4: 2012-10-04 12116
##  5: 2012-10-05 13294
##  6: 2012-10-06 15420
##  7: 2012-10-07 11015
##  8: 2012-10-08    NA
##  9: 2012-10-09 12811
## 10: 2012-10-10  9900
```
##2.2 Histogram of steps taken per day.

```r
hist( DT_daywise$V1 , main="Histogram: Count of steps per day", xlab="Steps per day", breaks=50)
```

![plot of chunk unnamed-chunk-5](PA1_template_files/figure-html/unnamed-chunk-5.png) 

## 2.3 Finding mean steps per day
Based on days when steps were available (not NA)

*Mean*

```r
mean(DT_daywise[DT_daywise$V1!="NA"]$V1)
```

```
## [1] 10766
```
*Median*

```r
median(DT_daywise[DT_daywise$V1!="NA"]$V1)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
Summary of steps taken (per day)

```r
summary(DT_daywise$V1)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8840   10800   10800   13300   21200       8
```

A time series plot with average steps at given time

```r
DT_Average_day <- DT[DT$steps != 0, list(AvgSteps = mean(steps, na.rm = TRUE)), by = interval]

plot.ts(DT_Average_day$AvgSteps, main="Average time series with Average steps in 5 min interval", xlab="5-min interval",ylab="count of steps",type = "l")
```

![plot of chunk unnamed-chunk-9](PA1_template_files/figure-html/unnamed-chunk-9.png) 

5-minute interval that, on average, contains the maximum number of steps


```r
DT_max_at_interval <- DT[DT$steps != 0, list(MaxSteps = max(steps, na.rm = TRUE)), by = interval]

plot.ts(DT_max_at_interval$MaxSteps, main="Time series plot with Max steps in 5 min interval", xlab="5-min interval",ylab="count of steps",type = "l")
```

![plot of chunk unnamed-chunk-10](PA1_template_files/figure-html/unnamed-chunk-10.png) 


## Imputing missing values

###Total no. of missing NAs 

```r
length(DT[is.na(DT$steps)]$steps)
```

```
## [1] 2304
```

To imput the missing values (`NA`), we replace it with median value of that interval, if that median value is also `NA`, we check median value for that day, else 0
#DT_imputted <- DT[is.na(DT$steps), list(step = mean(steps, na.rm = FALSE))] # , by = interval]



```r
# imputing 
imputeValues <- function(record) {
  #check if steps are NA for that interval, if yes, replace with median of that particular interval for all available days
  if(is.na(record[1])){
     DT_same_interval_withoutNA_steps <- DT[DT$interval==record[3] & !is.na(DT$steps)]$steps    
    imputted_value_median_by_interval <- median(DT_same_interval_withoutNA_steps)
  #if calculated median is NA, then find median of steps available throughout the day  
    if(is.na(imputted_value_median_by_interval)){    
        DT_same_day_withoutNA_steps <- DT[DT$date==record[2] & !is.na(DT$steps)]$steps     
        imputted_value_median_by_day <- median(DT_same_interval_withoutNA_steps)
        
        if(is.na(imputted_value_median_by_day)) {
          return(0)
       } else {
         return(as.integer(imputted_value_median_by_day))           
       }      
    } else {    
        return(as.integer(imputted_value_median_by_interval))
    }    
  } else {  
    # use original steps if available
   return(as.integer(record[1]))
  }   
}


DT$imputtedSteps <- as.integer(apply(DT, 1, imputeValues))
head(DT)
```

```
##    steps       date interval imputtedSteps
## 1:    NA 2012-10-01        0             0
## 2:    NA 2012-10-01        5             0
## 3:    NA 2012-10-01       10             0
## 4:    NA 2012-10-01       15             0
## 5:    NA 2012-10-01       20             0
## 6:    NA 2012-10-01       25             0
```

After imputting values,  time series plot with average steps at given time

```r
DT_Average_day <- DT[DT$steps != 0, list(AvgSteps = mean(imputtedSteps, na.rm = TRUE)), by = interval]

plot.ts(DT_Average_day$AvgSteps, main="Average time series with Average steps in 5 min interval (After imputting)", xlab="5-min interval",ylab="count of steps",type = "l")
```

![plot of chunk unnamed-chunk-13](PA1_template_files/figure-html/unnamed-chunk-13.png) 

After imputting values, 5-minute interval that, on average, contains the maximum number of steps


```r
DT_max_at_interval <- DT[DT$steps != 0, list(MaxSteps = max(imputtedSteps, na.rm = TRUE)), by = interval]

plot.ts(DT_max_at_interval$MaxSteps, main="Time series plot with Max steps in 5 min interval (After imputting)", xlab="5-min interval",ylab="count of steps",type = "l")
```

![plot of chunk unnamed-chunk-14](PA1_template_files/figure-html/unnamed-chunk-14.png) 

## After imputting, histogram of the total number of steps taken each day after missing values were imputed


```r
#DT_daywise$V1[is.na(DT_daywise$V1)] <- 0
summary(DT_daywise)
```

```
##       date                  V1       
##  Min.   :2012-10-01   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 8841  
##  Median :2012-10-31   Median :10765  
##  Mean   :2012-10-31   Mean   :10766  
##  3rd Qu.:2012-11-15   3rd Qu.:13294  
##  Max.   :2012-11-30   Max.   :21194  
##                       NA's   :8
```

Get sum of steps per day on imputted values

```r
DT_daywise_imputted <- DT[, sum(imputtedSteps), by = date]
```
Count of Total steps per day (printing top 10 result)

```r
head(DT_daywise_imputted,10)
```

```
##           date    V1
##  1: 2012-10-01   488
##  2: 2012-10-02   126
##  3: 2012-10-03 11352
##  4: 2012-10-04 12116
##  5: 2012-10-05 13294
##  6: 2012-10-06 15420
##  7: 2012-10-07 11015
##  8: 2012-10-08   488
##  9: 2012-10-09 12811
## 10: 2012-10-10  9900
```
Histogram

```r
hist( DT_daywise_imputted$V1 , main="Histogram: Count of steps per day(After imputting)", xlab="Steps per day", breaks=50)
```

![plot of chunk unnamed-chunk-18](PA1_template_files/figure-html/unnamed-chunk-18.png) 

## Are there differences in activity patterns between weekdays and weekends?

http://stackoverflow.com/questions/1169248/r-function-for-testing-if-a-vector-contains-a-given-element

Setting weekends column equal to TRUE / FALSE based on date

```r
#First setting all dates as FALSE and then Saturday & Sunday as TRUE
DT_daywise$weekend <- FALSE
DT_daywise$weekend[weekdays(as.Date(DT_daywise$date)) %in% c("Saturday","Sunday")] <- TRUE
head(DT_daywise)
```

```
##          date    V1 weekend
## 1: 2012-10-01    NA   FALSE
## 2: 2012-10-02   126   FALSE
## 3: 2012-10-03 11352   FALSE
## 4: 2012-10-04 12116   FALSE
## 5: 2012-10-05 13294   FALSE
## 6: 2012-10-06 15420    TRUE
```

Summary of steps on weekdays

```r
summary( DT_daywise$V1[DT_daywise$weekend])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    8820   10700   12100   12400   14400   15400       2
```

```r
summary( DT_daywise$V1[!DT_daywise$weekend])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    7840   10300   10200   12800   21200       6
```
