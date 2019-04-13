library(dplyr)
library(lubridate)

activity<-read.csv("activity.csv")
#Improving tidy format
activity<- activity %>% 
  mutate( date = ymd(date) )

# Part one  : Summary per day

totalPerDay <- activity %>% group_by(date) %>%
  summarise( steps = sum(steps) )

##Histogram of steps per day
hist(totalPerDay$steps, breaks=20, main = "Histogram of steps per day", xlab = "Steps per day")

## Mean and median per day
meanAndMedian <- totalPerDay[!is.na(totalPerDay$steps), ] %>% 
  summarise( mean = mean(steps), median = median(steps) ) 

sprintf("Mean: %s, Median : %s", meanAndMedian$mean, meanAndMedian$median)