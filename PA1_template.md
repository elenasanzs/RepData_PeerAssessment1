## "Reproducible Research: Peer Assessment 1"

## Asignment

This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.

## Loading and preprocessing the data

```{r}
if (!file.exists('activity.csv')) {
  unzip(zipfile = "activity.zip")
}

activityInfo <- read.csv(file="activity.csv", header=TRUE)
```

## What is mean total number of steps taken per day?

```{r}
numSteps <- aggregate(steps ~ date, activityInfo, FUN=sum)
hist(numSteps$steps,
     main = "Total Steps per Day")
meanSteps <- mean(numSteps$steps, na.rm = TRUE)
medSteps <- median(numSteps$steps, na.rm = TRUE)
```

Mean number of steps taken per day= `r meanSteps`
Median number of steps taken per day= `r medSteps`

## What is the average daily activity pattern?

```{r}
library(ggplot2)
avgStepsPerInterval <- aggregate(x=list(meanSteps=activityInfo$steps), by=list(interval=activityInfo$interval), FUN=mean, na.rm=TRUE)
ggplot(data=avgStepsPerInterval, aes(x=interval, y=meanSteps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken") 
```

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
mostSteps <- which.max(avgStepsPerInterval$meanSteps)
maxInterval <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", avgStepsPerInterval[mostSteps,'interval'])
```

Maximum number of steps : `r maxInterval`

## Imputing missing values

### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
missingValues <- length(which(is.na(activityInfo$steps)))
```

There are `r missingValues` missing values

### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

To replace the missing values with the 5-day average of that respective interval

### Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
library(plyr)
library(Hmisc)
actInfoImputed <- activityInfo
actInfoImputed$steps <- impute(activityInfo$steps, fun=mean)
```


### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
impStepsByInt <- aggregate(steps ~ date, actInfoImputed, FUN=sum)
hist(impStepsByInt$steps,
     main = "Imputed Number of Steps Per Day",
     xlab = "Number of Steps")
```
```{r}
impMeanSteps <- mean(impStepsByInt$steps, na.rm = TRUE)
impMedSteps <- median(impStepsByInt$steps, na.rm = TRUE)
differenMean = impMeanSteps - meanSteps
differenMed = impMedSteps - medSteps
```

Mean (imputed data): `r impMeanSteps`  COMPARE WITH THE PREVIOUS `r meanSteps`, DIFFERENCE = `r differenMean`

Median (imputed data): `r impMedSteps`  COMPARE WITH THE PREVIOUS `r medSteps`, DIFFERENCE = `r differenMed`

Mean values doesn't chang. However ther is a difference in meadian value.

## Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r}
actInfoImputed$dateType <-  ifelse(as.POSIXlt(actInfoImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
```

### Make a panel plot containing a time series plot

```{r}
avgActInfoImputed <- aggregate(steps ~ interval + dateType, data=actInfoImputed, mean)
ggplot(avgActInfoImputed, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dateType ~ .) +
    xlab("Interval") + 
    ylab("avarage number of steps")
```
