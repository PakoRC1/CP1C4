Course Project 1 Reproducible Research
======================================

Author: Francisco Ramirez
-------------------------

First, we read the data.

``` r
library(data.table)
data<-read.csv("activity.csv",stringsAsFactors = TRUE)
#we convert string to date
data<-data.table(data)
```

We converted the data frame with the information to a data table to ease the analysis. So, we need to do numerous things:

1.  What is mean total number of steps taken per day?

-   Calculate the total number of steps taken per day.

``` r
steps_per_day<-data[,sum(steps,na.rm=TRUE),by=as.factor(data$date)]
names(steps_per_day)<-c("date","spd")
head(steps_per_day)
```

    ##          date   spd
    ## 1: 2012-10-01     0
    ## 2: 2012-10-02   126
    ## 3: 2012-10-03 11352
    ## 4: 2012-10-04 12116
    ## 5: 2012-10-05 13294
    ## 6: 2012-10-06 15420

-   Make a histogram of the total number of steps taken each day

``` r
library(ggplot2)
bin<-(range(steps_per_day$spd)[2]-range(steps_per_day$spd)[1])/30
qplot(spd,data=steps_per_day,binwidth=bin)
```

![](PA1_template_files/figure-markdown_github/1b-1.png)

-   Calculate and report the mean and median of the total number of steps taken per day

``` r
report<-c(mean(steps_per_day$spd),median(steps_per_day$spd))
names(report)<-c("mean","median")
report
```

    ##     mean   median 
    ##  9354.23 10395.00

1.  What is the average daily activity pattern?

-   Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

``` r
steps_per_int<-data[,mean(steps,na.rm=TRUE),by=as.factor(data$interval)]
names(steps_per_int)<-c("interval","mean_steps")
steps_per_int$interval<-unique(data$interval)
ggplot(data=steps_per_int,aes(x=interval,y=mean_steps))+geom_line()+labs(x="interval",y="mean steps per interval")
```

![](PA1_template_files/figure-markdown_github/2a-1.png)

-   Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
steps_per_int[mean_steps==max(mean_steps),]
```

    ##    interval mean_steps
    ## 1:      835   206.1698

We see that the interval corresponding to 835 has the maximum number of steps on average.

1.  Imputing missing values

-   Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

``` r
nrow(data)-sum(complete.cases(data))
```

    ## [1] 2304

-   Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

We assign the mean steps per interval (mpi)

``` r
replace<-data[,mean(steps,na.rm=TRUE),by=as.factor(data$interval)]
names(replace)<-c("interval","mpi")
replace$interval<-unique(data$interval)
data2<-merge(data,replace,by="interval")
data2$steps<-as.double(data2$steps)
data2[is.na(data2$steps),]$steps<-data2[is.na(data2$steps),]$mpi
data2$mpi<-NULL
```

-   Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
setorder(data2,date,interval)
```

-   Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

We just adapt the previous code to the new database

``` r
steps_per_day2<-data2[,sum(steps,na.rm=TRUE),by=as.factor(data$date)]
names(steps_per_day2)<-c("date","spd")
head(steps_per_day2)
```

    ##          date      spd
    ## 1: 2012-10-01 10766.19
    ## 2: 2012-10-02   126.00
    ## 3: 2012-10-03 11352.00
    ## 4: 2012-10-04 12116.00
    ## 5: 2012-10-05 13294.00
    ## 6: 2012-10-06 15420.00

``` r
bin<-(range(steps_per_day2$spd)[2]-range(steps_per_day2$spd)[1])/30
qplot(spd,data=steps_per_day2,binwidth=bin)
```

![](PA1_template_files/figure-markdown_github/3d-1.png)

Now we compare this histogram to the first one, and we see that the bar corresponding to 0 steps per day is shorter in this histogram than the one from the first one. This is because, when we removed the NA in the first histogram, the days where there weren't any steps registered were assigned a 0, and now as we have imputed the missing values, most of these days have at least a value of steps per day greater than 0.

We also noticed that a bar near 10,000 steps per day grew considerably.

1.  Are there differences in activity patterns between weekdays and weekends?

-   Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

``` r
#we initialize the variable
data2$weekday<-" "
data2[weekdays(as.Date(data2$date))%in% c("lunes","martes","miércoles","jueves","viernes")]$weekday<-"weekday"
data2[weekdays(as.Date(data2$date))%in% c("sábado","domingo")]$weekday<-"weekend"
data2$weekday<-as.factor(data2$weekday)
```

-   Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

``` r
#we use the data frame with no NA

aux_g<-aggregate(steps ~ interval + weekday, data =data2,FUN="mean")
library(lattice)
xyplot(steps ~ interval|weekday,data=aux_g,layout=c(1,2),type="l")
```

![](PA1_template_files/figure-markdown_github/4b-1.png)
