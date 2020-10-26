
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
library(statar)
library(bit64)
rm(list = ls())

setwd("C:/Users/liama/Documents/summer_data")

#load necessary datasets
bike <-readRDS("summary.rds")
weather <- readRDS("weather.rds")
air_quality <- readRDS("air_quality.rds")

#date in the bike dataset is a factor here changing it to merge properly
bike$date <- as.Date(bike$date)

### merge bicycle daily trip data with daily weather data
df <- merge(x = bike, y = weather, by = c("date","city"), all = TRUE)
df <- merge(x= df , y= air_quality, by = c("date","city"),all = TRUE)

## Drop na values for air quality and trips
df <- df[!is.na(df$trips) & !is.na(df$pm25),]


saveRDS(df , file = "regressions.rds")

