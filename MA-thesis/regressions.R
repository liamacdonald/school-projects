
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
library(haven)
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
library(dummies)
library(texreg)
rm(list = ls())

setwd("C:/Users/liama/Documents/summer_data")

df <- readRDS("regressions.rds")
df <- df[df$trips != 0,]
df <- df[!is.na(df$temp),]



#### This is the section of the code that is not in final code



#### Make some relevant variables
## Make trip variable log
df$l_trips <- log(df$trips)

## Create month year variable for fixed effects and dow variables
df$ym <- paste0(year(df$date),month(df$date))
df$dow <- weekdays(df$date)
df$weekday <- 1 
df$weekday[df$dow == "Saturday" | df$dow == "Sunday"] <- 0
df$weekend <- 0
df$weekend[df$dow == "Saturday" | df$dow == "Sunday"] <- 1


### Change variables from imperial to metric
#df$temp <- (df$temp - 32)*(5/9)
#df$rain <- df$rain*2.54
#df$snow <- df$snow*2.54
#df$snow_depth <- df$snow_depth*2.54
#df$ws <- df$ws*1.6


##### Make high and temperature and pm25 dummies
df$temp_high <- 0
df$temp_high[df$temp > 28] <- 1
df$pm_high <- 0
df$pm_high[df$pm25 > 75] <- 1
#Create a wind direction variable
df$wd <- "ne"
df$wd[df$sus_wd >= 90 & df$sus_wd < 180] <- "se"
df$wd[df$sus_wd >= 180 & df$sus_wd < 270] <- "sw"
df$wd[df$sus_wd >= 270 & df$sus_wd < 360] <- "nw"

### Creating dummy variable for each wind direction to fix my fixed effects multicolinearity problem
#temp <- data.frame(dummy(df$wd))
#df$ne <- temp$wd.ne
#df$nw <- temp$wd.nw
#df$sw <- temp$wd.sw
#df$se <- temp$wd.se

### Doing this a different way to make sure it's right this way is probably better
temp <- data.frame(df$city,df$date,df$membership,dummy(df$wd))
temp <- rename(temp,replace = c("df.city" = "city", "df.date" = "date","df.membership" = "membership"))
df <- merge(x = df, y = temp,by = c("date","city","membership"))
rm(temp)

## Rename wind direction variables
df <- rename(df,replace = c("cityne" = "ne","citynw" = "nw","citysw" = "sw","cityse" = "se"))




### Create dataframe where customer and subscriber trips are aggregated
dt <- data.table(select(df,-contains("member")))
df_sum <- dt[, total:=sum(trips), by=c("date","city")]
df_sum <- select(df_sum,-contains("trips"))
df_sum <- df_sum[!duplicated(df_sum),]



## Make trip variable log
df$l_trips <- log(df$trips)
df_sum$l_trips <- log(df_sum$total)






##############################   AGGREGATED REGRESSIONS #####################################

## First stage regressions
#Clustered and unclustered with no fe
fs_no_fe <- felm(pm25 ~   (ne*city) + (nw*city) + (sw*city) - city - ne - nw - sw + ws  + rain   + snow + snow_depth + weekend + temp_high + temp + 
                   temp*temp_high   , data = df_sum)


fs_no_fec <- felm(pm25 ~   (ne*city) + (nw*city) + (sw*city) - city - ne - nw - sw + ws  + rain   + snow + snow_depth + weekend + temp_high + temp + 
                    temp*temp_high   , data = df_sum)


## Cluster and unclustered with fe
#### This solves all my problems :)

fs_fe <- felm(pm25 ~   (ne*city) + (nw*city) + (sw*city) - city - ne - nw - sw + ws  + rain   + snow + snow_depth + weekend + temp_high + temp + 
                 temp*temp_high  | as.factor(city)*as.factor(ym)  , data = df_sum)

fs_fec <- felm(pm25 ~   (ne*city) + (nw*city) + (sw*city) - city - ne - nw - sw + ws  + rain   + snow + snow_depth + weekend + temp_high + temp + 
                 temp*temp_high  | as.factor(city)*as.factor(ym) |0| city , data = df_sum)


var_names <- c("Wind Speed","Rain","Snow","Snow Depth","Weekend","Above 28","Temperature","PM2.5","Above 28*Temperature","Constant","PM2.5")
texreg(list(fs_no_fe,fs_no_fec,fs_fe,fs_fec), custom.model.names= c("OLS","OLS Clustered","FE","FE Clustered"),longtable = TRUE)



## AGGREGATED REGRESSIONS that will be included in final model
#no FE
no_fe_ols <- felm(l_trips ~ ws  + rain   + snow + snow_depth + weekend + temp_high + temp + temp*temp_high + pm25  , data = df_sum)


 no_fe_ols_clr <- felm(l_trips ~ ws  + rain   + snow + snow_depth + weekend + temp_high + temp + temp*temp_high + pm25 , data = df_sum)
 
 

no_fe_iv <- felm(l_trips ~ ws  + rain   + snow + snow_depth + weekend + temp_high + temp + temp*temp_high | 0|
               (pm25 ~  (ne*city) + (nw*city) + (sw*city) - city  )  , data = df_sum)




no_fe_iv_clr <- felm(l_trips ~ ws  + rain   + snow + snow_depth + weekend + temp_high + temp + temp*temp_high  | 0|
                       (pm25 ~  (ne*city) + (nw*city) + (sw*city) - city  ) | city , data = df_sum)


#### Wtih FE 
fe_ols <- felm(l_trips ~ ws  + rain   + snow + snow_depth + weekend + temp_high + temp + temp*temp_high + pm25 | as.factor(city)*as.factor(ym)
               , data = df_sum)




fe_ols_clr <- felm(l_trips ~ ws  + rain   + snow + snow_depth + weekend + temp_high + temp + temp*temp_high + pm25 | as.factor(city)*as.factor(ym) | 0
                   |  city , data = df_sum)


fe_iv <- felm(l_trips ~ ws  + rain   + snow + snow_depth + weekend + temp_high + temp + temp*temp_high  | as.factor(city)*as.factor(ym)|
               (pm25 ~ (ne*city) + (nw*city) + (sw*city) - city )  , data = df_sum )


fe_iv_clr <- felm(l_trips ~    ws  + rain   + snow + snow_depth + weekend + temp_high + temp + temp*temp_high   | as.factor(city)*as.factor(ym)|
                (pm25 ~ (ne*city) + (nw*city) + (sw*city) - city - ne - nw - sw) | city  , data = df_sum )




#### Make table for paper 


var_names <- c("Wind Speed","Rain","Snow","Snow Depth","Weekend","Above 28","Temperature","PM2.5","Above 28*Temperature","Constant","PM2.5")
texreg(list(fe_ols,fe_ols_clr,no_fe_iv, no_fe_iv_clr,fe_iv,fe_iv_clr), custom.coef.names = var_names)





######################### SEPERATED INTO MEMBERS AND CUSTOMERS     ##################################################

### Generate seperate values for pm25 subscribers and customers this is necessary to get the instrumental variable approach to work

df$pm25s <- 0
df$pm25s[df$membership == "Subscriber"] <- df$pm25[df$membership == "Subscriber"]
df$pm25c <- 0
df$pm25c[df$membership == "Customer"] <- df$pm25[df$membership == "Customer"]



### NO FE

no_fe_ols <- felm(l_trips ~ (ws   + rain   + snow + snow_depth + weekend  + temp_high + temp + temp*temp_high  )*membership -membership + pm25c
                 + pm25s , data =  df)


no_fe_ols_clr <- felm(l_trips ~ (ws  + rain   + snow + snow_depth + weekend + temp_high + temp + temp*temp_high )*membership -membership + pm25c
                      + pm25s  , data = df)



no_fe_iv <- felm(l_trips ~ (ws  + rain   + snow + snow_depth + weekend + temp_high + temp + temp*temp_high)*membership - membership | 0|
                   (pm25s | pm25c ~  ((ne*city) + (nw*city) + (sw*city) - city  )*membership ), data = df)


no_fe_iv_clr <- felm(l_trips ~ (ws  + rain   + snow + snow_depth + weekend + temp_high + temp + temp*temp_high)*membership - membership  | 0|
                       (pm25s | pm25c ~  ((ne*city) + (nw*city) + (sw*city) - city  )*membership )| city , data = df)


#### Wtih FE 
fe_ols <- felm(l_trips ~ (ws  + rain   + snow + snow_depth + weekend + temp_high + temp + temp*temp_high )*membership - membership + pm25c
               + pm25s  | as.factor(city)*as.factor(ym)*as.factor(membership)  , data = df)




fe_ols_clr <- felm(l_trips ~ (ws  + rain   + snow + snow_depth + weekend + temp_high + temp + temp*temp_high )*membership - membership + pm25c
                   + pm25s | as.factor(city)*as.factor(ym)*as.factor(membership) | 0 |  city , data = df)


fe_iv <- felm(l_trips ~    (ws  + rain   + snow + snow_depth + weekend + temp_high + temp + temp*temp_high )*membership - membership|
                as.factor(city)*as.factor(ym)*as.factor(membership)| (pm25s | pm25c ~ ((ne*city) + (nw*city) + (sw*city) - city )*membership)
              , data = df )




fe_iv_clr <- felm(l_trips ~    (ws  + rain   + snow + snow_depth + weekend + temp_high + temp + temp*temp_high )*membership - membership|
               as.factor(city)*as.factor(ym)*as.factor(membership)| (pm25s | pm25c ~ ((ne*city) + (nw*city) + (sw*city) - city )*membership) |
                 city  , data = df)


#### Make table for paper 
screenreg(list(fe_ols,fe_ols_clr,no_fe_iv, no_fe_iv_clr,fe_iv,fe_iv_clr))

texreg(list(fe_ols,fe_ols_clr,no_fe_iv, no_fe_iv_clr,fe_iv,fe_iv_clr),reorder.coef = c(1:8,21,10:17,9,20,18,19)
          ,groups = list("Customers" =1:10,"Subscribers" = 11:20))




## Observation counts for pm25 over threshold

df$year <- year(df$date)
df$month <- month(df$date)

table(df[df$pm25s > 25 & df$membership == "Subscriber",c("city","ym")])

