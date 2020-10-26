
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
temp <- lapply(list.files(path = "air_quality", pattern = ".csv", full.names = TRUE), fread, sep=",")
df <- rbindlist(temp,fill = TRUE)
rm(temp)
saveRDS(df , file = "air_quality.rds")



##### Delete some irrelevant columns
df <- select(df, -contains("Source") , -contains("AQS"),-contains("code"),-contains("county"),-contains("CBSA"),
             -contains("percent"))


### Make the date variable
df$Date <-  as.Date(df$Date, format = "%m/%d/%Y")

df$city[df$`Site ID` == 360610079  ] <- "New York"


#### Only keep the monitors I'm going to use 

df <- df[df$`Site ID` == 	060371103  | df$`Site ID` == 360610079 | df$`Site ID` == 	060750005 | 
           df$`Site ID` == 250250002 | df$`Site ID` == 	170313301 |
           df$`Site ID`  ==  	510130020   | 	df$`Site ID` == 421010055,]

#### New york, chicago, boston has data only ever 3rd day
#### LA, san fran, philly have data everyday
##### WASHINGTON
####11-001-0043 is at north of washington, weather data is south but daily observations back to start of data
####	11-001-0053 is at south and daily but begins in 2018
#### 	51-013-0020 is south and back to 2016 but every 3rd day


### This is based off documentation saved in dropbox POC 1 is what you want to use but some cities don't have it
df <- df[df$POC == 1 | df$POC == 3 ,]



df$city <- "Washington"
df$city[df$`Site ID` == 60371103  ] <- "Los Angeles"
df$city[df$`Site ID` == 360610079  ] <- "New York"
df$city[df$`Site ID` == 060750005 ] <- "San Francisco"
df$city[df$`Site ID` == 250250002 ] <- "Boston"
df$city[df$`Site ID` == 170313301  ] <- "Chicago"
df$city[df$`Site ID` == 421010055  ] <- "Philadelphia"



### Keep only necessary variables
df <- select(df,-contains("site"),-contains("OBS"),-contains("POC"),-contains("STATE"))

names(df) <- c("date","pm25","units","aqi","city")

saveRDS(df , file = "air_quality.rds")
