
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
rm(list = ls())

setwd("C:/Users/liama/Documents/summer_data")



##### All this is done from beginnning of 2016 or most recent, san fran is only one without data that far back starts in 2017


##############################load all the files for LA and save ################################################
temp <- lapply(list.files(path = "la", pattern = ".csv", full.names = TRUE), fread, sep=",")
df <- rbindlist(temp,fill = TRUE)
rm(temp)

keep <- c("duration","start_time","end_time","start_lat","start_lon",
          "end_lat","end_lon","bike_id","passholder_type")
la <- subset(df, select = keep)


names(la) <- c("duration","s_time","e_time","s_lat","s_long",
               "e_lat","e_long","bike_id","user_type")
la$city <- "Los Angeles"
saveRDS(la,file="la_bike.rds")


##############################load all the files for NY and save ################################################


temp <- lapply(list.files(path = "ny", pattern = ".csv", full.names = TRUE), fread, sep=",")
df <- rbindlist(temp)
rm(temp)

keep <- c("tripduration","starttime","stoptime","start station latitude","start station longitude",
          "end station latitude","end station longitude","bikeid","usertype")
ny <- subset(df, select = keep)

names(ny) <- c("duration","s_time","e_time","s_lat","s_long",
               "e_lat","e_long","bike_id","user_type")
ny$city <- "New York"
saveRDS(ny,file="ny_bike.rds")


##############################load all the files for San Francisco and save ################################################
temp <- lapply(list.files(path = "sanfran", pattern = ".csv", full.names = TRUE), fread, sep=",")
df <- rbindlist(temp,fill = TRUE)
rm(temp)
keep <- c("duration_sec","start_time","end_time","start_station_latitude","start_station_longitude",
          "end_station_latitude","end_station_longitude","bike_id","user_type")
sf <- subset(df, select = keep)
names(sf) <- c("duration","s_time","e_time","s_lat","s_long",
               "e_lat","e_long","bike_id","user_type")
sf$city <- "San Francisco"
saveRDS(sf,file="sanfran_bike.rds")


##############################load all the files for Boston, keep the necessary variables and save ################################################
temp <- lapply(list.files(path = "bos", pattern = ".csv", full.names = TRUE), fread, sep=",")
df <- rbindlist(temp,fill = TRUE)
rm(temp)
keep <- c("tripduration","starttime","stoptime","start station latitude","start station longitude",
          "end station latitude","end station longitude","bikeid","usertype")

bos <- subset(df, select = keep)

names(bos) <- c("duration","s_time","e_time","s_lat","s_long",
                "e_lat","e_long","bike_id","user_type")

bos$city <-  "Boston"

saveRDS(bos,file="bos_bike.rds")

##############################load all the files for Chicago and save ################################################
temp <- lapply(list.files(path = "chicago", pattern = ".csv", full.names = TRUE), fread, sep=",")
df <- rbindlist(temp,fill = TRUE)
rm(temp)
#This solves the first problem with start time end time
df$start_time[is.na(df$start_time)] <- df$starttime[is.na(df$start_time)]
df$end_time[is.na(df$end_time)] <- df$stoptime[is.na(df$end_time)]
# Once the first one solved this solves second problem where all variables are wrong
df$start_time[is.na(df$trip_id)] <-  df$`01 - Rental Details Local Start Time`[is.na(df$trip_id)]
df$end_time[is.na(df$trip_id)] <- df$`01 - Rental Details Local End Time`[is.na(df$trip_id)]
df$tripduration[is.na(df$trip_id)] <- df$`01 - Rental Details Duration In Seconds Uncapped`[is.na(df$trip_id)]
df$bikeid[is.na(df$trip_id)] <- df$`01 - Rental Details Bike ID`[is.na(df$trip_id)]
df$usertype[is.na(df$trip_id)] <- df$`User Type`[is.na(df$trip_id)]

#keep and name variables property, no location data for CHI
keep <- c("tripduration","start_time","end_time","bikeid","usertype")
chi <- subset(df, select = keep)

names(chi) <- c("duration","s_time","e_time","bike_id","user_type")

chi$city <-  "Chicago"
saveRDS(chi,file="chicago_bike.rds")


##############################load all the files for Philly and save ################################################
temp <- lapply(list.files(path = "philly", pattern = ".csv", full.names = TRUE), fread, sep=",")
df <- rbindlist(temp,fill = TRUE)
rm(temp)

keep <- c("duration","start_time","end_time","start_lat","start_lon",
          "end_lat","end_lon","bike_id","passholder_type")
philly <- subset(df, select = keep)

names(philly) <- c("duration","s_time","e_time","s_lat","s_long",
                   "e_lat","e_long","bike_id","user_type")
philly$city <-  "Philadelphia"
saveRDS(philly,file="philly_bike.rds")



##############################load all the files for washington and save ################################################
temp <- lapply(list.files(path = "dc", pattern = ".csv", full.names = TRUE), fread, sep=",")
df <- rbindlist(temp,fill = TRUE)
rm(temp)

keep <- c("Duration","Start date","End date","Bike number","Member type")
dc <- subset(df, select = keep)

names(dc) <- c("duration","s_time","e_time","bike_id","user_type")



dc$city <-  "Washington"
saveRDS(dc,file="dc_bike.rds")



## Read all the cities as individual datasets
dc <- readRDS("dc_bike.rds")
la <- readRDS("la_bike.rds")
ny <- readRDS("ny_bike.rds")
philly <- readRDS("philly_bike.rds")
sf <- readRDS("sanfran_bike.rds")
bos <- readRDS("bos_bike.rds")
chi <- readRDS("chicago_bike.rds")



########### Join 3 cities with smallest amount of observations first
#df1 <- rbind(la,sf,philly, fill = TRUE)
#saveRDS(df1, file = "bicycle_work.rds")



###### Drop new york
#df <- rbind(la,dc,sf,bos,chi,philly, fill = TRUE)
## Putting all the date formats in the proper columns 



## This includes all the data in joining cities
df <- rbind(la,dc,ny,sf,bos,chi,philly, fill = TRUE)


#Extract date from time values
df$date <- sub(" \\d+:\\d+:?\\d*.?\\d*","",df$s_time)
#Create date format and create variables for year,month and week variables
a <- as.Date(df$date, "%m/%d/%Y" )
b <- as.Date(df$date, "%Y-%m-%d")
a[is.na(a)] <- b[!is.na(b)]
df$date <- a
rm(a,b)
df$year <- year(df$date)
df$month <- month(df$date)
df$day <- weekdays(df$date)

#Remove date from time values get hod measure
df$s_time <- sub("\\d+(-|/)\\d+(-|/)\\d* ","",df$s_time)
df$e_time <- sub("\\d+(-|/)\\d+(-|/)\\d* ","",df$e_time)
df$hod <- sub("(\\d+):\\d+:?\\d*.?\\d*","\\1",df$s_time)
saveRDS(df,file = "bicycle.rds")

df$user_type[ df$user_type == "Monthly Pass" | df$user_type == "Flex Pass" | df$user_type == 
                "Annual Pass"  | df$user_type == "Indego30" | 
                df$user_type == "Indego365" | df$user_type == "IndegoFlex" | df$user_type == "Member"] <- "Subscriber"
df$user_type[df$user_type == "One Day Pass" | df$user_type == "Testing" | df$user_type == 
               "Walk-up" |df$user_type == "Day Pass" | df$user_type == "Two Day Pass" | 
               df$user_type == "Casual"] <- "Customer"

saveRDS(df, file = "bicycle.rds")

#### Get daily counts of trips for each city by membership type
summary <- data.frame(table(df$city,df$user_type,df$date))
names(summary) <- c("city","membership","date","trips")
summary <- summary[summary$membership  == "Subscriber" | summary$membership == "Customer",]
saveRDS(summary,file = "bicycle_summary.rds") 

rm(list = ls())

#############  WEATHER DATA

#load data
temp <- lapply(list.files(path = "weather", pattern = ".csv", full.names = TRUE), fread, sep=",")
df <- rbindlist(temp,fill = TRUE)
rm(temp)
saveRDS(df , file = "weather.rds")

#There is two report type columns this just removes 1 they are identical
df <- df[, -c(4)]


#### Create city variable from station ID's  
df$city <- "Washington"
df$city[df$STATION == 72287493134] <- "Los Angeles"
df$city[df$STATION == 72494023234]<- "San Francisco"    
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
rm(list = ls())


########################   AIR QUALITY DATA ########################################

##############################read the air quality files and save as weather ################################################
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

rm(list = ls())

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

df <- readRDS("regressions.rds")
df <- df[df$trips != 0,]
df <- df[!is.na(df$temp),]

saveRDS(df , file = "regressions.rds")


###Making summary statistics

#Total trips 

sum_stats <- ddply(df[df$membership == "Customer",], .(city), summarise, Temperature = round(mean(temp),digits = 2) , Rain = round(mean(rain),digits = 2) ,
                   Snow = round(mean(snow),digits = 2) )

#Make the total amount of trips by types of membership
sum_member <- ddply(df[df$membership == "Subscriber",], .(City), summarise , "Member Trips" = mean(Trips),"Standard Deviation" = sd(Trips))
sum_customer <- ddply(df[df$Membership == "Customer",], .(City), summarise , "Customer Trips" = mean(Trips),"Standard Deviation" = sd(Trips))

sum_mem <- merge(sum_customer,sum_member, by = "City")


stargazer(sum_stats, 
          type = "latex", summary = FALSE, rownames = FALSE)
stargazer(sum_mem, 
          type = "latex", summary = FALSE, rownames = FALSE)




