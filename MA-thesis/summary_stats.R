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
library(gmodels)
rm(list = ls())

memory.limit(size = 50000000)
setwd("C:/Users/liama/Documents/summer_data")

df <- readRDS("bicycle.rds")
df$user_type[ df$user_type == "Monthly Pass" | df$user_type == "Flex Pass" | df$user_type == 
                                           "Annual Pass"  | df$user_type == "Indego30" | 
                df$user_type == "Indego365" | df$user_type == "IndegoFlex" | df$user_type == "Member"] <- "Subscriber"
df$user_type[df$user_type == "One Day Pass" | df$user_type == "Testing" | df$user_type == 
                                           "Walk-up" |df$user_type == "Day Pass" | df$user_type == "Two Day Pass" | 
               df$user_type == "Casual"] <- "Customer"

#################################################### ALL Of this code looking at dates is done in the compiling file now
## Putting all the date formats in the proper columns 
#Extract date from time values
#df$date <- sub(" \\d+:\\d+:?\\d*.?\\d*","",df$s_time)



#Create date format and create variables for year,month and week variables
#a <- as.Date(df$date, "%m/%d/%Y" )
#b <- as.Date(df$date, "%Y-%m-%d")
#a[is.na(a)] <- b[!is.na(b)]
#df$date <- a
#rm(a,b)
#df$year <- year(df$date)
#df$month <- month(df$date)
#df$week <- isoweek(df$date)

#Remove date from time values get hod measure
#df$s_time <- sub("\\d+(-|/)\\d+(-|/)\\d* ","",df$s_time)
#df$e_time <- sub("\\d+(-|/)\\d+(-|/)\\d* ","",df$e_time)
#df$hod <- sub("(\\d+):\\d+:?\\d*.?\\d*","\\1",df$s_time)



#Create a summary dataset by monthly rides per city
summary <- data.frame(table(df$city,df$user_type,df$date))
names(summary) <- c("city","membership","date","trips")
summary <- summary[summary$membership  == "Subscriber" | summary$membership == "Customer",]
saveRDS(summary,file = "summary.rds") 


#Bar graph of total trips by city 
ylab <- c(3, 6, 9, 12,20)
ggplot(data=df, aes(x=year,fill=city)) +
 geom_bar(stat="count") + xlab("Year") +   ylab("Number of Trips") +  ggtitle("Annual Trips By City") + theme(legend.position="bottom") + 
  scale_y_continuous(labels = paste0(ylab, "M"),
           breaks = 10^6 * ylab) 

####  Bar graphs of average weekly trips by membership type for select cities
philly <- df[df$city == "Philadelphia"]
summary_philly <- data.frame(table(philly$city,philly$user_type,philly$year,philly$month,philly$date))
names(summary_philly) <- c("City","Membership","Year","Month","date","Trips")
ggplot(data=summary_philly[summary_philly$Year == 2018,], aes(x=Month,y = Trips ,fill=Membership)) +
  geom_bar(stat = "summary") + xlab("Month") +   ylab("Average Daily Trips") +  ggtitle("Average Daily Trips By Type of Pass (Philadelphia)") +
  coord_flip() + theme(legend.position="bottom")

sf <- df[df$city == "San Francisco"]
summary_sf <- data.frame(table(sf$city,sf$user_type,sf$year,sf$month,sf$date))
names(summary_sf) <- c("City","Membership","Year","Month","date","Trips")
ggplot(data=summary_sf[summary_sf$Year == 2018,], aes(x=Month,y = Trips ,fill=Membership)) +
  geom_bar(stat = "summary") + xlab("Month") +   ylab("Average Daily Trips") +  ggtitle("Average Daily Trips By Type of Pass (San Fran)")+
  coord_flip() + theme(legend.position="bottom")

la <- df[df$city == "Los Angeles"]
summary_la <- data.frame(table(la$city,la$user_type,la$year,la$month,la$date))
names(summary_la) <- c("City","Membership","Year","Month","date","Trips")
ggplot(data=summary_la[summary_la$Year == 2018,], aes(x=Month,y = Trips ,fill=Membership)) +
  geom_bar(stat = "summary") + xlab("Month") +   ylab("Average Daily Trips") +  ggtitle("Average Daily Trips By Type of Pass (Los Angeles)")+
  coord_flip() + theme(legend.position="bottom")

bos <- df[df$city == "Boston"]
summary_boston<- data.frame(table(bos$city,bos$user_type,bos$year,bos$month,bos$date))
names(summary_boston) <- c("City","Membership","Year","Month","date","Trips")
ggplot(data=summary_boston[summary_boston$Year == 2018,], aes(x=Month,y = Trips ,fill=Membership)) +
  geom_bar(stat = "summary") + xlab("Month") +   ylab("Average Daily Trips") +  ggtitle("Average Daily Trips By Type of Pass (Boston)")+
  coord_flip() + theme(legend.position="bottom")




###Making summary statistics

#Total trips 

sum_stats <- ddply(summary, .(City), summarise, Total=sum(Trips) , Average=mean(Trips),"Standard Deviation" = sd(Trips))

#Make the total amount of trips by types of membership
sum_member <- ddply(summary[summary$Membership == "Subscriber",], .(City), summarise , "Member Trips" = mean(Trips),"Standard Deviation" = sd(Trips))
sum_customer <- ddply(summary[summary$Membership == "Customer",], .(City), summarise , "Customer Trips" = mean(Trips),"Standard Deviation" = sd(Trips))

sum_mem <- merge(sum_customer,sum_member, by = "City")


#table(summary$Trips,summary$Membership)
stargazer(sum_stats, 
          type = "latex", summary = FALSE, rownames = FALSE)
stargazer(sum_mem, 
          type = "latex", summary = FALSE, rownames = FALSE)
