
library(data.table)
library(lubridate)
library(dplyr)
library(lfe)
library(plm)
library(xtable)
library(outreg)
library(plyr)
library(stargazer)
library(rms)
library(quantreg)
library(car)
library(AER)
library(foreign)
library(readstata13)
library(splines)
library(graphics)
library(plyr)
library(stats)
library(ggplot2)
library(lubridate)
library(statar)
rm(list = ls())

setwd("C:/Users/liama/Documents/summer_data")


################### BOSTON Work
df <- readRDS("bos_bike.rds")
#df <-  df[1:10000]


##### This seperates the time and date into 2 seperate variables
df$sdate <- as.Date(sub(" \\d+:\\d+:?\\d*","",df$starttime))
df$edate <- as.Date(sub(" \\d+:\\d+:?\\d*","",df$stoptime))
df$stime <- sub("\\d+-\\d+-\\d* ","",df$starttime)
df$etime <- sub("\\d+-\\d+-\\d* ","",df$endtime)
df$hod <-  as.numeric(sub("(\\d+):\\d+:?\\d*","\\1",df$stime))

## ####Starting on some plots
#ggplot(data=df, aes(x=hod,fill=usertype)) +
# geom_bar(stat="count") + xlim(c(5,22))
#ggplot(data=df, aes(x=hod,group=usertype)) +
#  geom_line(stat="count") + xlim(c(5,22))

### Create weekly summary data
df$week <- isoweek(df$sdate)
df$week[df$week == 53] <- 0
df$month <- month(df$sdate)
df$year <- year(df$sdate)



boston_summary <- data.frame(table(df$weekyear))
###If I want user types later then this will do that, did this at the beginning so might have some problems with the other variables
#boston_summary <- data.frame(table(df$weekyear,df$usertype))

names(boston_summary) <- c("weekyear","trips")
boston_summary$city <- "Boston"
## Average trip length

for(i in boston_summary$weekyear){
  boston_summary$duration[boston_summary$weekyear == i ] <- round(mean(df$tripduration[df$weekyear == i])/60,digits =0)
}
saveRDS(boston_summary,file = "boston_summary.rds")

ggplot(data=df, aes(sdate)) +
  geom_bar(stat = "count") + facet_wrap(~ year,ncol = 3)

