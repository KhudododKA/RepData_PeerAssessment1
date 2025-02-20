# Reproducible Research: Assignment 1
***



## Preparing the data

This stage will take two steps; reading the data and processing

### Reading the data


```r
url<-download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",destfile = "repdata1")
unzip('repdata1')

df_activity<-read.csv("activity.csv",header = TRUE)
```
### Processing data

After reading the data, I convert the date column into a proper date format. Second, I will extract the weekdays and the month from the date variable to use later in the analysis. And finally, I will create the analytic data by calculating the total number of steps for each day, which I will use to answer the first question.


```r
dim(df_activity)
```

```
## [1] 17568     3
```

```r
str(df_activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
## convert date to date format
df_activity<-df_activity%>%
  mutate(date=ymd(date))

df_activity<-df_activity%>%
  mutate(interval_week=weekdays(date),
         interval_month=month(date))
## create the analytic data
## aggregate the steps taken over the date variable. This is basically the sum of the total 
## number of steps taken each day, i.e, the sum over all 5 minute intervals
df_analytic<-df_activity%>%
  group_by(date)%>%summarise(steps=sum(steps))
```

### Analysis

Here I will show the response to each of the questions using the data above.  

#### Mean total number of steps taken per day

1. Histogram of the total number steps taken each day


```r
plot1<-df_analytic%>%
  ggplot(.,aes(x=steps))+
  geom_histogram(fill="slateblue",alpha=0.7,color="darkblue")+
  theme_classic()+
  labs(title="Number of steps per day",
       caption = "Histogram with normal plot")+
  theme(plot.background = element_rect(fill = "pink2"),
        panel.background = element_rect(fill="mintcream"),
        panel.grid.minor = element_line(linetype = 2,color = "black"))

plot1
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

<img src="figure/unnamed-chunk-3-1.png" title="plot of chunk unnamed-chunk-3" alt="plot of chunk unnamed-chunk-3" style="display: block; margin: auto;" />

The histogram above shows the total number of steps taken each day. 

2. **Mean** and **Median** total steps taken

<table>
 <thead>
  <tr>
   <th style="text-align:right;"> mean_step </th>
   <th style="text-align:right;"> median_step </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 10766.19 </td>
   <td style="text-align:right;"> 10765 </td>
  </tr>
</tbody>
</table>

Interestingly, the *mean* and *median* steps taken does not differ from one another. 

#### What is the average daily activity pattern


1. Average steps taken for each interval: time series plot

To create  the times series plot, I will first calculate the average steps in each interval storing it as a separate variable in the data. I will then construct the time series  plot using *ggplot2* package.  


```r
df_activity<-df_activity%>%
  group_by(interval)%>%mutate(avg_steps_interval=mean(steps,na.rm = TRUE))
```


```r
plot2<-df_activity%>%
  ggplot(.,aes(x=interval,y=avg_steps_interval))+
  geom_line(lty=1,lwd=1,colour="darkred")+scale_x_continuous(breaks = seq(0,2400,by=60))+
  labs(title = "Average steps taken in 5-minute interval",y="average steps taken")+
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill="lightyellow"),
        panel.grid.major = element_line(linetype=2,size = 0.001,color = "orange"),
        axis.text.x = element_text(size = 7.5,angle = 25,colour = "white"),
        axis.text.y = element_text(size = 7.5,color = "white"),
        plot.title = element_text(colour = "white"))

plot2
```

<img src="figure/plot2-1.png" title="plot of chunk plot2" alt="plot of chunk plot2" style="display: block; margin: auto;" />

2. Maximum steps for specific interval

To find the maximum steps in each interval, use *which.max* command to locate the row position at the interval has the maximum average steps. Then I will select those rows and the specific columns.


```r
which.max(df_activity$avg_steps_interval)
```

```
## [1] 104
```

```r
d<-df_activity[103:105,]
d<-d%>%select(interval,avg_steps_interval)
kable(d)
```

<table>
 <thead>
  <tr>
   <th style="text-align:right;"> interval </th>
   <th style="text-align:right;"> avg_steps_interval </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 830 </td>
   <td style="text-align:right;"> 177.3019 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 835 </td>
   <td style="text-align:right;"> 206.1698 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 840 </td>
   <td style="text-align:right;"> 195.9245 </td>
  </tr>
</tbody>
</table>
In the above table the row 104 corresponds to *206.1698* average steps taken, which is the highest or the maximum. You can also see that before that row the average steps are lower as well as after that row. 

#### Imputing missing values

1. Total number of missing values in the data


This number is **2304**

2. Imputation strategy

As a simple method, I will use the average steps in each interval and replace the missing values. One can also use a regression based approach or more sophisticated machine learning models. I will provide the code on how regression can be used to impute. There are also imputation packages that can be used such as the *Mice* package or *Amelia*. 

3. Create new dataset with newly imputed values


```r
fit<-lm(steps~interval+interval_week,data = df_activity)

df_activity<-df_activity%>%
  group_by(interval)%>%mutate(median_total_steps=median(steps,na.rm = TRUE))

df_activity<-df_activity%>%
  mutate(steps_imputed=ifelse(is.na(steps),predict(fit,df_activity,type = "response"),steps),
         steps_imputed_mean=ifelse(is.na(steps),avg_steps_interval,steps))
```

4. Histogram of the steps

In general imputing based on the mean does provide a different estimate. The mean usually tends to either over estimate or under estimate depending on the extreme values in the data. Since, this is a time series data the number of steps changes based on the interval and days. Below, I provide the two plots comparing the mean estimation with the original data on the number of steps per interval. The second plot after histogram compares the original number of steps with regression based approach and the mean based approach to imputation. Both estimation vary from the original, however the regression based approach seems to work better than the simple mean based imputation. 


```r
plot3<-df_activity%>%
  group_by(date)%>%
  summarise(steps_imputed=sum(steps_imputed_mean))%>%
  ggplot(.,aes(x=steps_imputed))+
  geom_histogram(fill="slateblue",alpha=0.7,color="darkblue")+
  theme_classic()+
  labs(title="Number of steps per day",
       caption = "Histogram with normal plot")+
  theme(plot.background = element_rect(fill = "pink2"),
        panel.background = element_rect(fill="mintcream"),
        panel.grid.minor = element_line(linetype = 2,color = "black"))
plot3
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="figure/unnamed-chunk-8-1.png" title="plot of chunk unnamed-chunk-8" alt="plot of chunk unnamed-chunk-8" style="display: block; margin: auto;" />

```r
plot4<-df_activity%>%group_by(date)%>%summarise(steps_mean=sum(steps_imputed_mean),step_lm=sum(steps_imputed),step_orig=sum(steps,na.rm = TRUE))%>%gather(key = "type",value = "value",-date)%>%ggplot(.,aes(x=date,y=value,color=type))+geom_line(lty=1,lwd=1)+
  labs(title = "Comparison of imputation approaches",subtitle = "Mean vs linear regression estimates")+
  scale_color_manual("",labels=c("Linear","Original","Mean"),
                     values = c("gold3","red","blue"))+
  theme(plot.background = element_rect(fill = "pink2"),
        panel.background = element_rect(fill = "mintcream"),
        panel.grid.minor = element_line(linetype = 2,color = "black"),
        legend.position = "bottom")
plot4
```

<img src="figure/unnamed-chunk-9-1.png" title="plot of chunk unnamed-chunk-9" alt="plot of chunk unnamed-chunk-9" style="display: block; margin: auto;" />


5. Mean and median

<table>
 <thead>
  <tr>
   <th style="text-align:right;"> mean_step </th>
   <th style="text-align:right;"> median_step </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 10766.19 </td>
   <td style="text-align:right;"> 10766.19 </td>
  </tr>
</tbody>
</table>
#### Differences in activity patterns for Weekdays and Weekend

In this final part, I will look at the difference in activity pattern depending on the day of the week. In other words, I will compare the weekdays steps taken to the weekend steps taken to see if there discernible differences. First, I will create the factor variable *day_type* to differentiate between *weekend* and *weekdays* and then I will create a graph using *ggplot2* package. While, the plot looks different from the *example* plot provided, it provides the answer to the question in a similar fashion as the lattice graph would. 

1. Create factor variable for Weekend and Weekdays


```r
df_activity<-df_activity%>%
  mutate(day_type=ifelse(interval_week %in% c("Saturday","Sunday"),"Weekend","Weekdays"))

df_activity<-df_activity%>%
  group_by(day_type,interval)%>%
  mutate(avg_steps_interval1=mean(steps,na.rm = TRUE))
```

2. Average steps for each interval by type of day: Weekend or Weekdays


```r
plot5<-df_activity%>%
  ggplot(.,aes(x=interval,y=avg_steps_interval1,color=day_type))+
  geom_line(show.legend = FALSE,lty=1,lwd=1)+scale_x_continuous(breaks = seq(0,2400,by=120))+
  facet_wrap(day_type~.,nrow = 2)+
  labs(title = "Average steps per interval",subtitle = "Weekend vs Weekdays",
       y="average steps taken",x="")+
  theme(plot.title = element_text(color = "white"),
        plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="lightyellow"),
        panel.grid.major = element_line(color = "orange",linetype = 2,size = 0.001),
        axis.text.x = element_text(color = "white",face = "bold",size = 7.5,angle = 25),
        axis.title.y = element_text(color = 'White'),
        axis.text.y = element_text(color = "white",size = 7.5,face = "bold"),
        strip.background = element_rect(fill = "pink"),
        strip.placement = "outside",
        strip.text = element_text(face = "bold"))

plot5
```

<img src="figure/unnamed-chunk-11-1.png" title="plot of chunk unnamed-chunk-11" alt="plot of chunk unnamed-chunk-11" style="display: block; margin: auto;" />

Looking at the plot, it shows a higher number of steps taken in each interval for *weekend* compared to *weekdays*. 
