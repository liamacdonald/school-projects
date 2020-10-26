
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

##############################read the weather files and save as weather ################################################
temp <- lapply(list.files(path = "weather", pattern = ".csv", full.names = TRUE), fread, sep=",")
df <- rbindlist(temp,fill = TRUE)
rm(temp)
saveRDS(df , file = "weather.rds")



# Read save weather file and remove duplicated column
df <- readRDS("weather.rds")
#There is two report type columns this just removes 1 they are identical
df <- df[, -c(4)]


#### Create city variable from station ID's  
df$city <- "Washington"
df$city[df$STATION == 72287493134] <- "Los Angeles"
df$city[df$STATION == 72494023234]<- "San Francisco"    ##### Maybe want to change this to dt san fran from the airport
df$city[df$STATION == 72505394728] <- "New York"
df$city[df$STATION == 72509014739] <- "Boston"
df$city[df$STATION == 72534014819] <- "Chicago"
df$city[df$STATION == 72408013739] <- "Philadelphia"


## Keep only daily averages and format date properly
df$REPORT_TYPE <-   sub(" +","",df$REPORT_TYPE)
df <- df[df$REPORT_TYPE == "SOD"]
df$DATE <- as.Date(sub("T\\d+:\\d+:?\\d*.?\\d*","",df$DATE))


# Remove all columns that have only NA values and others 
df <- Filter(function(x)!all(is.na(x)), df)
df <- select(df, -contains("Backup") , -contains("SOURCE"), -contains("Monthly"),-contains("Hourly"),-contains("Short"),
             -contains("Pressure"),-contains("WetBulb"),-contains("Dew"),-contains("Equipment"), -contains("DailyWeather")
             ,-contains("REM"),-contains("DailyPeak"))


saveRDS(df,file = "weather.rds")

df <- readRDS("weather.rds")
### Remove the s from observations

df$DailyAverageDryBulbTemperature <- sub("s","",df$DailyAverageDryBulbTemperature)
df$DailyPrecipitation <- sub("s","",df$DailyPrecipitation)
df$DailyCoolingDegreeDays <- sub("s","",df$DailyCoolingDegreeDays)
df$DailyHeatingDegreeDays <- sub("s","",df$DailyHeatingDegreeDays)
df$DailyMaximumDryBulbTemperature <- sub("s","",df$DailyMaximumDryBulbTemperature)
df$DailyMinimumDryBulbTemperature <- sub("s","",df$DailyMinimumDryBulbTemperature)
df$DailySnowDepth <- sub("s","",df$DailySnowDepth)
df$DailySnowfall <- sub("s","",df$DailySnowfall)

#Make temperature values numeric
df$DailyAverageDryBulbTemperature <- as.numeric(df$DailyAverageDryBulbTemperature)
df$DailyCoolingDegreeDays <- as.numeric(df$DailyCoolingDegreeDays)
df$DailyHeatingDegreeDays <- as.numeric(df$DailyHeatingDegreeDays)
df$DailyMaximumDryBulbTemperature <- as.numeric(df$DailyMaximumDryBulbTemperature)
df$DailyMinimumDryBulbTemperature <- as.numeric(df$DailyMinimumDryBulbTemperature)

#Creating precipitation variables
# Creating a dummy for trace observations don't know how I'm going to deal with this yet 
df$trace_rain <- 0
df$trace_rain[df$DailyPrecipitation == "T"] <- 1

df$trace_snowfall <- 0
df$trace_snowfall[df$DailySnowfall == "T"] <- 1

df$trace_snowdepth <- 0
df$trace_snowdepth[df$DailySnowDepth == "T"] <- 1


## Make trace rain == 0 then we can make numeric column for the amount of rain, might make rain dummy later
df$rain[df$DailyPrecipitation != "T"] <- as.numeric(df$DailyPrecipitation[df$DailyPrecipitation != "T"] )
df$rain[df$DailyPrecipitation == "T"] <- 0
df$rain <- as.numeric(df$rain)

# Creating a dummy for trace snowfall don't know how I'm going to deal with this yet 
df$trace_snowfall <- 0
df$trace_snowfall[df$DailySnowfall == "T"] <- 1

## Make trace == 0 then we can make numeric column for the amount of rain, might make rain dummy later
df$rain[df$DailyPrecipitation != "T"] <- as.numeric(df$DailyPrecipitation[df$DailyPrecipitation != "T"] )
df$rain[df$DailyPrecipitation == "T"] <- 0
df$rain <- as.numeric(df$rain)

df$snow[df$DailySnowfall != "T"] <- as.numeric(df$DailySnowfall[df$DailySnowfall != "T"] )
df$snow[df$DailySnowfall == "T" | df$DailySnowfall == ""] <- 0
df$snow <- as.numeric(df$snow)

df$snowdepth[df$DailySnowDepth != "T"] <- as.numeric(df$DailySnowDepth[df$DailySnowDepth != "T"] )
df$snowdepth[df$DailySnowDepth == "T" | df$DailySnowDepth == ""] <- 0
df$snowdepth <- as.numeric(df$snowdepth)


#Remove the old precipitation variables from dataset
df <- select(df,-contains("DailyPrecipitation"),-contains("DailySnow"),-contains("Station"),-contains("Report"))

names(df) <- c("date","temp","humid","ws","cooling_days","temp_deviation","heating_days","max_temp","min_temp","sus_wd","sus_ws",
               "sunrise","sunset","city","trace_rain","trace_snow","trace_snow_depth","rain","snow","snow_depth")


saveRDS(df,file = "weather.rds")