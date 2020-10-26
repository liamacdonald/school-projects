



library(data.table)
library(lubridate)
library(dplyr)
library(lfe)
library(plm)
library(xtable)
library(outreg)
library(plyr)
# Clear the working directory
rm(list = ls())

# Set the working directory to the assignment folder
setwd("C:/Users/Liam MacDonald/Dropbox/school/assignments/Environmental/replication")

#load the data 
rep_data <- read.csv("econ573_replication_data.csv.gz", header = TRUE)

####keep only years before 2004 because of NOx regulations on fuel
rep_data <- rep_data[rep_data$year < 2004 , ]




#Delete the NA results for the variables listed in the assignment
rep_data <- rep_data[complete.cases(rep_data[ , 8:9, ]), ] 
rep_data <- rep_data[complete.cases(rep_data[ , 13:19, ]), ] 
rep_data <- rep_data[rep_data$ozone8Hr != 0 , ]
rep_data <- rep_data[rep_data$ozoneMax != 0 , ]

saveRDS(rep_data, "rep_data.rds")

## Create a variable to indentify unique county monitor and year
rep_data$yfips <- as.numeric(paste0(rep_data$year,rep_data$fips,rep_data$monitorID))

#lagged variables for tmin and tmax
rep_data <- setDT(rep_data)[, tMinL1:=c(NA , tMin[-.N]), by=yfips]
rep_data <- setDT(rep_data)[, tMaxL1:=c(NA , tMin[-.N]), by=yfips]

#### Drop all the pollution readings with less than 9 observations
rep_data <- rep_data[rep_data$pollutionReadings >= 9, ]


###keep only summer months
rep_data$mon <- month(as.POSIXlt(rep_data$date, format="%Y-%m-%d"))
rep_data <- rep_data[rep_data$mon >= 6 & rep_data$mon <= 8, ]
rep_data$mon <- NULL


####### This part removes the years that have less than 25% summer days

for(i in unique(rep_data$yfips)){
  if(nrow(rep_data[rep_data$yfips == i, ]) <
     69){rep_data <- rep_data[-which(rep_data$yfips == i), ] }
}


#### Remove observations that their neighbouring county is the treatment and they are not
rep_data <- rep_data[-which(rep_data$neighborCountyTreated == 1), ]






### Data saved after cleaning but prior to performing any analysis
saveRDS(rep_data, "rep_data_1.rds")








#=========================================================================
#QUESTION 4
#=========================================================================

### Read the cleaned data file
rep_data <- readRDS("rep_data_1.rds")

#Generate some necessary variables for creating the table
rep_data$urban_ind <- ifelse(rep_data$urban == 1 , rep_data$yfips, NA)
rep_data$rural_ind <- ifelse(rep_data$urban == 3 , rep_data$yfips, NA)
rep_data$RVP1_ind <- ifelse(rep_data$standard_RVP1 == 
                              9.5 |rep_data$standard_RVP1 == 
                              10.5 , rep_data$fips, NA)
rep_data$RVP2_ind <- ifelse(!is.na(rep_data$standard_RVP2) , rep_data$fips, NA)
rep_data$RFG_ind <- ifelse(rep_data$standard_RFG != 
                             "" & rep_data$standard_RFG != 
                             "CARB Phase 2", rep_data$fips, NA)
rep_data$CARB_ind <- ifelse(rep_data$standard_RFG == 
                              "CARB Phase 2", rep_data$fips, NA)


#Generate table 1 as a dataset
table_1 <- setDT(rep_data)[, .(Observations = 
                                 length(date), Counties =
                                 length(unique(fips)), "Total Monitors" = 
                                 length(unique(yfips)), Urban = 
                                 as.integer(length(unique(urban_ind))-1), Rural = 
                                 as.integer(length(unique(rural_ind))-1),RVP1 =
                                 as.integer(length(unique(RVP1_ind))-1),RVP2 =
                                 as.integer(length(unique(RVP2_ind))-1),RFG =
                                 as.integer( length(unique(RFG_ind))-1),CARB =
                                 as.integer( length(unique(CARB_ind))-1)), by = year]
table_1 <- table_1[order(year)]

#Export Table 1 to latex
addtorow <- list()
addtorow$pos <- list(-1,-1)
addtorow$command <- c( 
  "& & &\\multicolumn{3}{r}{Count of active monitors} & & & & \\\\\n", 
  "\\cline{4-6}")
print(xtable(table_1),
      add.to.row = addtorow, hline.after = c(-1 , -1), 
      include.rownames = FALSE)
rm(addtorow)


#Generate indicator variables for each type of regulation
rep_data$RVP1_ind <- ifelse(!is.na(rep_data$RVP1_ind), 1, 0 )
rep_data$RVP2_ind <- ifelse(!is.na(rep_data$RVP2_ind) , 1, 0 )
rep_data$RFG_ind <- ifelse(!is.na(rep_data$RFG_ind), 1, 0)                          
rep_data$CARB_ind <- ifelse(!is.na(rep_data$CARB_ind), 1, 0)

#Remove some of the irrelevant variables

rep_data$standard_RVP1<- NULL
rep_data$standard_RVP2 <- NULL
rep_data$standard_RFG <- NULL
rep_data$neighborCountyTreated <- NULL
rep_data$urban_ind <- NULL
rep_data$rural_ind <- NULL
rep_data$yfips <- NULL


###Data from the end of question 4 with some variables removed
saveRDS(rep_data, "rep_data_2.rds")



#===================================================================
#QUESTION 5
#===================================================================
rm(list = ls())
save.image()
##Read data from this point
rep_data <- readRDS("rep_data_2.rds")


############## First I generate all the controls I need for the regressions#################


# Generate the cluster variable
rep_data$state_year <- as.integer(paste0(rep_data$state_code, rep_data$year))

#Change all FE variables to factor so they work within the function felm
rep_data$fips <- as.factor(rep_data$fips)
rep_data$monitorID <- as.factor(rep_data$monitorID)
rep_data$year <- as.factor(rep_data$year)
rep_data$censusRegion <- as.factor(rep_data$censusRegion)


#day of week and day of year
rep_data$dow <- as.factor(weekdays(as.Date(rep_data$date, "%Y-%m-%d")))
rep_data$doy <- as.numeric(day(as.POSIXlt(rep_data$date, format="%Y-%m-%d")))

# Make all the weather controls from the existing weather controls
rep_data$rain1 <-  rep_data$rain
rep_data$rain2 <-  rep_data$rain^2 
rep_data$snow1<-  rep_data$snow
rep_data$snow2 <-  rep_data$snow^2 
rep_data$tMin1 <-  rep_data$tMin
rep_data$tMin2 <-  rep_data$tMin^2
rep_data$tMin3 <-  rep_data$tMin^3 
rep_data$tMax1 <- rep_data$tMax
rep_data$tMax2 <- rep_data$tMax^2 
rep_data$tMax3 <- rep_data$tMax^3 
rep_data$tMinMax <- rep_data$tMin*rep_data$tMax 
rep_data$rainMax <- rep_data$rain*rep_data$tMax 
rep_data$tMaxMinL1 <-  rep_data$tMax*rep_data$tMinL1
rep_data$tMaxMaxL1 <- rep_data$tMax*rep_data$tMaxL1


#Create interactions between day of year and all temperature variables

x <- rep_data[,which(colnames(rep_data) == "rain1"):which(colnames(rep_data) == "tMaxMaxL1")]*rep_data$doy
for(i in 1:14){
  colnames(x)[i] <- paste0("doy_int",i)
} 
rep_data <- data.frame(rep_data, x)

# Create interactions between day of week and weather variables
x <- model.matrix(~(dow)^2,rep_data) #dow into dummy variables
x <- as.data.frame(x[,2:7]) #remove the intercept column
x$dowFriday <- rowSums(x[,1:6])  ####THESE TWO LINES WERE ADDED IN CASE OF BREAK
x$dowFriday <- ifelse(x$dowFriday == 1 , 0 , 1)
for(i in 1:7){
  y <- x[,i]*rep_data$tMax
  rep_data <- within(rep_data,assign(paste0("tMaxdow",i), y))
  y <- x[,i]*rep_data$tMin
  rep_data <- within(rep_data,assign(paste0("tMindow",i), y))
  y <- x[,i]*rep_data$rain
  rep_data <- within(rep_data,assign(paste0("raindow",i), y))
  y <- x[,i]*rep_data$snow
  rep_data <- within(rep_data,assign(paste0("snowdow",i), y))
}
rm(y)
rm(x)


# Create the interaction terms between policy region and time
#First make a time variable for the number of days since specified date
#I picked this date since 
startdate <- as.Date("1960-01-01", "%Y-%m-%d")
rep_data$time  <- as.numeric(difftime(as.Date(rep_data$date),startdate ,units="days"))/365
rm(startdate)

#Now create variables for each trend and set them all equal to 0
for(i in 1:4){
  rep_data <- within(rep_data,assign(paste0("trendRVP",i), 0)) }
for(i in 1:4){
  rep_data <- within(rep_data,assign(paste0("trendRFG",i), 0))}
for(i in 1:4){
  rep_data <- within(rep_data,assign(paste0("trendCARB",i), 0))}
for(i in 1:4){
  rep_data <- within(rep_data,assign(paste0("trendRVPRFG",i), 0))}
for(i in 1:4){
  rep_data <- within(rep_data,assign(paste0("trendCARBRFG",i), 0))}

#This large loop sets the trend variable that applies to the county given
#as the time variable so it is interaction between time, region and policy

for(i in unique(rep_data$fips)){
  x <- rep_data[rep_data$fips == i, ]
  z <- c(which(rep_data$fips == i))
  for(k in which(colnames(rep_data) == "trendRVP1"):which(colnames(rep_data) == "trendRVP4")){
    y <- k+1 - which(colnames(rep_data) == "trendRVP1")
    if(max(x$RVP2_ind) == 1 & max(x$RFG_ind) == 
       0  & unique(x$censusRegion) == y){  rep_data[z,k] <- rep_data$time[z]}
  }
  for(k in which(colnames(rep_data) == "trendRFG1"):which(colnames(rep_data) == "trendRFG4")){
    y <- k+1 - which(colnames(rep_data) == "trendRFG1")
    if(max(x$RVP2_ind) == 0 & max(x$RFG_ind) == 
       1 & max(x$CARB_ind) == 
       0  & unique(x$censusRegion) == y){  rep_data[z,k] <- rep_data$time[z]}
  }
  for(k in which(colnames(rep_data) == "trendCARB1"):which(colnames(rep_data) == "trendCARB4")){
    y <- k+1 - which(colnames(rep_data) == "trendCARB1")
    if(  max(x$RFG_ind) ==   #TOOK OUT RVP2_IND to make it better here
         0 & max(x$CARB_ind) == 
         1 & unique(x$censusRegion) == y ){  rep_data[z,k] <- rep_data$time[z]}
  } 
  for(k in which(colnames(rep_data) == "trendRVPRFG1"):which(colnames(rep_data) == "trendRVPRFG4")){
    y <- k+1 - which(colnames(rep_data) == "trendRVPRFG1")
    if(max(x$RVP2_ind) == 1 & max(x$RFG_ind) == 
       1 & max(x$CARB_ind) == 
       0 & unique(x$censusRegion) == y ){  rep_data[z,k] <- rep_data$time[z]}
  } 
  for(k in which(colnames(rep_data) == "trendCARBRFG1"):which(colnames(rep_data) == "trendCARBRFG4")){
    y <- k+1 - which(colnames(rep_data) == "trendCARBRFG1")
    if( max(x$RFG_ind) == 
        1 & max(x$CARB_ind) == 
        1 & unique(x$censusRegion) == y ){  rep_data[z,k] <- rep_data$time[z]}
  }
}
for(i in which(colnames(rep_data) == "trendRVP1"):which(colnames(rep_data) == "trendCARBRFG4")){
  rep_data <- data.frame(rep_data, rep_data[i]^2)
  colnames(rep_data)[i+20] <- paste0(names(rep_data[i]),"2") 
}

#REG number 1 and 6 ozone max using two FE variables

reg_1 <- summary(felm(log(ozoneMax) ~ RVP1_ind + 
                        RVP2_ind +  RFG_ind +
                        CARB_ind | censusRegion:year +
                        fips:monitorID | 0 | 
                        state_year ,data = rep_data ))

reg_6 <- summary(felm(log(ozone8Hr) ~ RVP1_ind + 
                        RVP2_ind +  RFG_ind +
                        CARB_ind | censusRegion:year +
                        fips:monitorID | 0 | 
                        state_year ,data = rep_data ))



#Next 2 regressions add in a few more controls
reg_2 <- summary(felm(log(ozoneMax) ~ RVP1_ind + RVP2_ind + RFG_ind +  CARB_ind + 
                        rain + rain2 + snow + snow2 + tMin + tMax + 
                        tMin2 + tMax2 + tMin3 + tMax3 + tMinMax + rainMax + 
                        tMaxMinL1 + tMaxMaxL1 + tMindow1 + tMindow2 + tMindow3 +
                        tMindow4 + tMindow5 + tMindow6 + tMaxdow1 + tMaxdow2 +
                        tMaxdow3 + tMaxdow4 + tMaxdow5 + tMaxdow6 + raindow1 + 
                        raindow2 + raindow3 + raindow4 + raindow5 + raindow6 +
                        snowdow1 + snowdow2 + snowdow3 + snowdow4 + snowdow5 +
                        snowdow6 + doy_int1 + doy_int1 + doy_int2 + doy_int3 +
                        doy_int4 + doy_int5 + doy_int6 + doy_int7 + doy_int8 +
                        doy_int9 + doy_int10 + doy_int11 + doy_int12 + doy_int13 +
                        doy_int14
                      | fips:monitorID  + censusRegion:doy +censusRegion:dow + 
                        censusRegion:year | 0 |  state_year , data = rep_data ))

reg_7 <- summary(felm(log(ozone8Hr) ~ RVP1_ind + RVP2_ind + RFG_ind +  CARB_ind + 
                        rain + rain2 + snow + snow2 + tMin + tMax + 
                        tMin2 + tMax2 + tMin3 + tMax3 + tMinMax + rainMax + 
                        tMaxMinL1 + tMaxMaxL1 + tMindow1 + tMindow2 + tMindow3 +
                        tMindow4 + tMindow5 + tMindow6 + tMaxdow1 + tMaxdow2 +
                        tMaxdow3 + tMaxdow4 + tMaxdow5 + tMaxdow6 + raindow1 + 
                        raindow2 + raindow3 + raindow4 + raindow5 + raindow6 +
                        snowdow1 + snowdow2 + snowdow3 + snowdow4 + snowdow5 +
                        snowdow6 + doy_int1 + doy_int1 + doy_int2 + doy_int3 +
                        doy_int4 + doy_int5 + doy_int6 + doy_int7 + doy_int8 +
                        doy_int9 + doy_int10 + doy_int11 + doy_int12 + doy_int13 +
                        doy_int14
                      | fips:monitorID  + censusRegion:doy +censusRegion:dow + 
                        censusRegion:year | 0 |  state_year , data = rep_data ))



# regressions 3 add income to the previous regression only do this for ozonemax

reg_3 <- summary(felm(log(ozoneMax) ~ RVP1_ind + RVP2_ind + RFG_ind +  CARB_ind + 
                        rain + rain2 + snow + snow2 + tMin + tMax + 
                        tMin2 + tMax2 + tMin3 + tMax3 + tMinMax + rainMax + 
                        tMaxMinL1 + tMaxMaxL1 + tMindow1 + tMindow2 + tMindow3 +
                        tMindow4 + tMindow5 + tMindow6 + tMaxdow1 + tMaxdow2 +
                        tMaxdow3 + tMaxdow4 + tMaxdow5 + tMaxdow6 + raindow1 + 
                        raindow2 + raindow3 + raindow4 + raindow5 + raindow6 +
                        snowdow1 + snowdow2 + snowdow3 + snowdow4 + snowdow5 +
                        snowdow6 + doy_int1 + doy_int1 + doy_int2 + doy_int3 +
                        doy_int4 + doy_int5 + doy_int6 + doy_int7 + doy_int8 +
                        doy_int9 + doy_int10 + doy_int11 + doy_int12 + doy_int13 +
                        doy_int14 + income
                      | fips:monitorID  + censusRegion:doy +censusRegion:dow + 
                        censusRegion:year | 0 |  state_year , data = rep_data ))

## Regression 4 add in only linear interactions of region time and policy

reg_4 <- summary(felm(log(ozoneMax) ~ RVP1_ind + RVP2_ind + RFG_ind +  CARB_ind + 
                        rain + rain2 + snow + snow2 + tMin + tMax + 
                        tMin2 + tMax2 + tMin3 + tMax3 + tMinMax + rainMax + 
                        tMaxMinL1 + tMaxMaxL1 + tMindow1 + tMindow2 + tMindow3 +
                        tMindow4 + tMindow5 + tMindow6 + tMaxdow1 + tMaxdow2 +
                        tMaxdow3 + tMaxdow4 + tMaxdow5 + tMaxdow6 + raindow1 + 
                        raindow2 + raindow3 + raindow4 + raindow5 + raindow6 +
                        snowdow1 + snowdow2 + snowdow3 + snowdow4 + snowdow5 +
                        snowdow6 + doy_int1 + doy_int1 + doy_int2 + doy_int3 +
                        doy_int4 + doy_int5 + doy_int6 + doy_int7 + doy_int8 +
                        doy_int9 + doy_int10 + doy_int11 + doy_int12 + doy_int13 +
                        doy_int14 + income + trendRVP1 + trendRVP2 + trendRVP3 +
                        trendRVP4 + trendRFG1 + trendRFG2 + trendRFG3 + trendRFG4 +
                        trendCARB1 + trendCARB2 + trendCARB3 + trendCARB4 +
                        trendRVPRFG1 + trendRVPRFG2 + trendRVPRFG3 + trendRVPRFG4 +
                        +trendCARBRFG1 + trendCARBRFG2 + trendCARBRFG3 + trendCARBRFG4
                      | fips:monitorID  + censusRegion:doy +censusRegion:dow + 
                        censusRegion:year | 0 |  state_year , data = rep_data ))
#Regressions 5 and 8 include all previous controls as well as quadratic terms for interactions
#between region time and policy 
reg_5 <- summary(felm(log(ozoneMax) ~ RVP1_ind + RVP2_ind + RFG_ind +  CARB_ind + 
                        rain + rain2 + snow + snow2 + tMin + tMax + 
                        tMin2 + tMax2 + tMin3 + tMax3 + tMinMax + rainMax + 
                        tMaxMinL1 + tMaxMaxL1 + tMindow1 + tMindow2 + tMindow3 +
                        tMindow4 + tMindow5 + tMindow6 + tMaxdow1 + tMaxdow2 +
                        tMaxdow3 + tMaxdow4 + tMaxdow5 + tMaxdow6 + raindow1 + 
                        raindow2 + raindow3 + raindow4 + raindow5 + raindow6 +
                        snowdow1 + snowdow2 + snowdow3 + snowdow4 + snowdow5 +
                        snowdow6 + doy_int1 + doy_int1 + doy_int2 + doy_int3 +
                        doy_int4 + doy_int5 + doy_int6 + doy_int7 + doy_int8 +
                        doy_int9 + doy_int10 + doy_int11 + doy_int12 + doy_int13 +
                        doy_int14 + income + trendRVP1 + trendRVP2 + trendRVP3 +
                        trendRVP4 + trendRFG1 + trendRFG2 + trendRFG3 + trendRFG4 +
                        trendCARB1 + trendCARB2 + trendCARB3 + trendCARB4 +
                        trendRVPRFG1 + trendRVPRFG2 + trendRVPRFG3 + trendRVPRFG4 +
                        +trendCARBRFG1 + trendCARBRFG2 + trendCARBRFG3 + trendCARBRFG4+ 
                        trendRVP12 + trendRVP22 + trendRVP32 +
                        trendRVP42 + trendRFG12 + trendRFG22 + trendRFG32 + trendRFG42 +
                        trendCARB12 + trendCARB22 + trendCARB32 + trendCARB42 +
                        trendRVPRFG12 + trendRVPRFG22 + trendRVPRFG32 + trendRVPRFG42 +
                        +trendCARBRFG12 + trendCARBRFG22 + trendCARBRFG32 + trendCARBRFG42
                      | fips:monitorID  + censusRegion:doy +censusRegion:dow + 
                        censusRegion:year | 0 |  state_year , data = rep_data ))

reg_8 <- summary(felm(log(ozoneMax) ~ RVP1_ind + RVP2_ind + RFG_ind +  CARB_ind + 
                        rain + rain2 + snow + snow2 + tMin + tMax + 
                        tMin2 + tMax2 + tMin3 + tMax3 + tMinMax + rainMax + 
                        tMaxMinL1 + tMaxMaxL1 + tMindow1 + tMindow2 + tMindow3 +
                        tMindow4 + tMindow5 + tMindow6 + tMaxdow1 + tMaxdow2 +
                        tMaxdow3 + tMaxdow4 + tMaxdow5 + tMaxdow6 + raindow1 + 
                        raindow2 + raindow3 + raindow4 + raindow5 + raindow6 +
                        snowdow1 + snowdow2 + snowdow3 + snowdow4 + snowdow5 +
                        snowdow6 + doy_int1 + doy_int1 + doy_int2 + doy_int3 +
                        doy_int4 + doy_int5 + doy_int6 + doy_int7 + doy_int8 +
                        doy_int9 + doy_int10 + doy_int11 + doy_int12 + doy_int13 +
                        doy_int14 + income + trendRVP1 + trendRVP2 + trendRVP3 +
                        trendRVP4 + trendRFG1 + trendRFG2 + trendRFG3 + trendRFG4 +
                        trendCARB1 + trendCARB2 + trendCARB3 + trendCARB4 +
                        trendRVPRFG1 + trendRVPRFG2 + trendRVPRFG3 + trendRVPRFG4 +
                        +trendCARBRFG1 + trendCARBRFG2 + trendCARBRFG3 + trendCARBRFG4+ 
                        trendRVP12 + trendRVP22 + trendRVP32 +
                        trendRVP42 + trendRFG12 + trendRFG22 + trendRFG32 + trendRFG42 +
                        trendCARB12 + trendCARB22 + trendCARB32 + trendCARB42 +
                        trendRVPRFG12 + trendRVPRFG22 + trendRVPRFG32 + trendRVPRFG42 +
                        +trendCARBRFG12 + trendCARBRFG22 + trendCARBRFG32 + trendCARBRFG42
                      | fips:monitorID  + censusRegion:doy +censusRegion:dow + 
                        censusRegion:year | 0 |  state_year , data = rep_data ))

sig_1 <- ifelse(reg_1$coefficients[1:4,4] >
                  0.1,"",ifelse(reg_1$coefficients[1:4,4] >
                                  0.05,"*",ifelse(reg_1$coefficients[1:4,4] 
                                                  >0.01,"**","***")))
sig_2 <- ifelse(reg_2$coefficients[1:4,4] >
                  0.1,"",ifelse(reg_2$coefficients[1:4,4] >
                                  0.05,"*",ifelse(reg_2$coefficients[1:4,4] 
                                                  >0.01,"**","***")))
sig_3 <- ifelse(reg_3$coefficients[1:4,4] >
                  0.1,"",ifelse(reg_3$coefficients[1:4,4] >
                                  0.05,"*",ifelse(reg_3$coefficients[1:4,4] 
                                                  >0.01,"**","***")))
sig_4 <- ifelse(reg_4$coefficients[1:4,4] >
                  0.1,"",ifelse(reg_4$coefficients[1:4,4] >
                                  0.05,"*",ifelse(reg_4$coefficients[1:4,4] 
                                                  >0.01,"**","***")))
sig_5 <- ifelse(reg_5$coefficients[1:4,4] >
                  0.1,"",ifelse(reg_5$coefficients[1:4,4] >
                                  0.05,"*",ifelse(reg_5$coefficients[1:4,4] 
                                                  >0.01,"**","***")))
sig_6 <- ifelse(reg_6$coefficients[1:4,4] >
                  0.1,"",ifelse(reg_6$coefficients[1:4,4] >
                                  0.05,"*",ifelse(reg_6$coefficients[1:4,4] 
                                                  >0.01,"**","***")))
sig_7 <- ifelse(reg_7$coefficients[1:4,4] >
                  0.1,"",ifelse(reg_7$coefficients[1:4,4] >
                                  0.05,"*",ifelse(reg_7$coefficients[1:4,4] 
                                                  >0.01,"**","***")))
sig_8 <- ifelse(reg_8$coefficients[1:4,4] >
                  0.1,"",ifelse(reg_8$coefficients[1:4,4] >
                                  0.05,"*",ifelse(reg_8$coefficients[1:4,4] 
                                                  >0.01,"**","***")))
col_1 <- data.frame(paste0(round(reg_1$coefficients[,1],digits = 3),
                           " (", round(reg_1$coefficients[,2],digits =3), ")",sig_1))
col_2 <- data.frame(paste0(round(reg_2$coefficients[1:4,1],digits = 3),
                           " (", round(reg_2$coefficients[1:4,2],digits =3), ")",sig_2))
col_3 <- data.frame(paste0(round(reg_3$coefficients[1:4,1],digits = 3),
                           " (", round(reg_3$coefficients[1:4,2],digits =3), ")",sig_3))
col_4 <- data.frame(paste0(round(reg_4$coefficients[1:4,1],digits = 3),
                           " (", round(reg_4$coefficients[1:4,2],digits =3), ")",sig_4))
col_5 <- data.frame(paste0(round(reg_5$coefficients[1:4,1],digits = 3),
                           " (", round(reg_5$coefficients[1:4,2],digits =3), ")",sig_5))
col_6 <- data.frame(paste0(round(reg_6$coefficients[1:4,1],digits = 3),
                           " (", round(reg_6$coefficients[1:4,2],digits =3), ")",sig_6))
col_7 <- data.frame(paste0(round(reg_7$coefficients[1:4,1],digits = 3),
                           " (", round(reg_7$coefficients[1:4,2],digits =3), ")",sig_7))
col_8 <- data.frame(paste0(round(reg_8$coefficients[1:4,1],digits = 3),
                           " (", round(reg_8$coefficients[1:4,2],digits =3), ")",sig_8))

table_2 <- data.frame(col_1 , col_2 , col_3 , col_4 , col_5 , col_6, col_7 , col_8)

print(xtable(table_2),
      include.rownames = FALSE, include.colnames = FALSE)
saveRDS(rep_data,"rep_data_5.rds")
#================================================================================
# Question 6
#================================================================================
rm(list = ls())
#load the data 
rep_data <- readRDS("rep_data.rds")


##Generate the necessary indicator variables for each policy
rep_data$RVP2_ind <- ifelse(!is.na(rep_data$standard_RVP2) , 1 , 0)
rep_data$RFG_ind <- ifelse( rep_data$standard_RFG != 
                              "" & rep_data$standard_RFG != 
                              "CARB Phase 2", 1, 0)
rep_data$CARB_ind <- ifelse(rep_data$standard_RFG == 
                              "CARB Phase 2", 1, 0)


#Keep only RFG treated counties
for(i in unique(rep_data$fips)){
  x <- rep_data[rep_data$fips == i ,]
  if(max(x$RFG_ind) == 0 | max(x$RVP2_ind) == 
     1 | max(x$CARB_ind) == 
     1 ){rep_data <- rep_data[-which(rep_data$fips == i),]}
}
rm(x)
rm(i)


#### Drop all the pollution readings with less than 9 observations
rep_data <- rep_data[rep_data$pollutionReadings >= 9, ]


###Create season variable
rep_data$mon <- month(as.POSIXlt(rep_data$date, format="%Y-%m-%d"))
rep_data$season <- ifelse(rep_data$mon == 3 | rep_data$mon == 
                            4 | rep_data$mon == 5,2,1)
rep_data$season <- ifelse(rep_data$mon == 6 | rep_data$mon == 
                            7 | rep_data$mon == 8, 3 ,rep_data$season)
rep_data$season <-ifelse(rep_data$mon == 9 | rep_data$mon == 
                           10 | rep_data$mon == 11, 4 ,rep_data$season)

#Create day of year variables
rep_data$doy <- as.numeric(day(as.POSIXlt(rep_data$date, format="%Y-%m-%d")))

#Create new year variable with month 12 adding a year
rep_data$year <- ifelse(rep_data$mon == 12, rep_data$year + 1, rep_data$year)

#Drop the counties that have less than 69 days in a season
rep_data$yfips <- as.numeric(paste0(rep_data$year,rep_data$fips,rep_data$monitorID,rep_data$season))
x <- count(rep_data, "yfips")
rep_data <- merge(cbind(rep_data, X=rownames(rep_data)), cbind(x, variable=rownames(x)))
rep_data <- rep_data[-which(rep_data$freq < 69),]
rep_data$X <- NULL
rep_data$freq <- NULL
rep_data$variable <- NULL


##Observation count here is 3282342
saveRDS(rep_data, "rep_data_rd.rds")

rep_data <- readRDS("rep_data_rd.rds")



#Drop monitors that have less than 75% total days
start_date <- as.Date("1989-01-01", "%Y-%m-%d")
end_date <- as.Date("2003-11-30", "%Y-%m-%d")
ref  <- as.numeric(difftime(end_date,start_date ,units="days"))*0.75
x <- count(rep_data, c("fips","monitorID"))
rep_data <- merge(cbind(rep_data, X=rownames(rep_data)), cbind(x, variable=rownames(x)))
rep_data <- rep_data[-which(rep_data$freq <= ref),]



saveRDS(rep_data, "rep_data_rd.rds")
rep_data <- readRDS("rep_data_rd.rds")

# Create interactions between day of week and weather variables
rep_data$dow <- as.factor(weekdays(as.Date(rep_data$date, "%Y-%m-%d")))
x <- model.matrix(~(dow)^2,rep_data) #dow into dummy variables
x <- as.data.frame(x[,2:7]) #remove the intercept column
x$dowFriday <- rowSums(x[,1:6])
x$dowFriday <- ifelse(x$dowFriday == 1 , 0 , 1)
for(i in 1:7){
  y <- x[,i]*rep_data$tMax
  rep_data <- within(rep_data,assign(paste0("tMaxdow",i), y))
  y <- x[,i]*rep_data$tMin
  rep_data <- within(rep_data,assign(paste0("tMindow",i), y))
  y <- x[,i]*rep_data$rain
  rep_data <- within(rep_data,assign(paste0("raindow",i), y))
  y <- x[,i]*rep_data$snow
  rep_data <- within(rep_data,assign(paste0("snowdow",i), y))
}
# Create dummies for dow and season interaction
rep_data$dows <- paste0(rep_data$dow,rep_data$season)
x <- model.matrix(~(dows)^2 ,rep_data) #dow into dummy variables
x <- as.data.frame(x[,2:28])
x$last_dum <- rowSums(x[,1:27])
x$last_dum <- ifelse(x$last_dum == 1 , 0 , 1)
rep_data <- data.frame(rep_data, x)

#Create month dummies
rep_data$mon <- as.factor(rep_data$mon)
x <- model.matrix(~(mon)^2 ,rep_data) #dow into dummy variables
x <- as.data.frame(x[,2:12])
x$mon1 <- rowSums(x[,1:11])
x$mon1 <- ifelse(x$mon1 == 1 , 0 , 1)
rep_data <- data.frame(rep_data, x)

##Create weather variables
rep_data$yfips <- as.numeric(paste0(rep_data$fips,rep_data$monitorID))
rep_data <- setDT(rep_data)[, tMinL1:=c(NA , tMin[-.N]), by=yfips]
rep_data <- setDT(rep_data)[, tMaxL1:=c(NA , tMin[-.N]), by=yfips]
rep_data$rain1 <-  rep_data$rain
rep_data$rain2 <-  rep_data$rain^2 
rep_data$snow1<-  rep_data$snow
rep_data$snow2 <-  rep_data$snow^2 
rep_data$tMin1 <-  rep_data$tMin
rep_data$tMin2 <-  rep_data$tMin^2
rep_data$tMin3 <-  rep_data$tMin^3 
rep_data$tMax1 <- rep_data$tMax
rep_data$tMax2 <- rep_data$tMax^2 
rep_data$tMax3 <- rep_data$tMax^3 
rep_data$tMinMax <- rep_data$tMin*rep_data$tMax 
rep_data$rainMax <- rep_data$rain*rep_data$tMax 
rep_data$tMaxMinL1 <-  rep_data$tMax*rep_data$tMinL1
rep_data$tMaxMaxL1 <- rep_data$tMax*rep_data$tMaxL1

#Create season dummies 
rep_data$season <- as.factor(rep_data$season)
x <- model.matrix(~(season)^2 ,rep_data) #dow into dummy variables
x <- as.data.frame(x[,2:4])
x$season1 <- rowSums(x[,1:3])
x$season1 <- ifelse(x$season1 == 1 , 0 , 1)
rep_data <- data.frame(rep_data, x)

saveRDS(rep_data,"rep_data_rd_1.rds")

#read data file again
rep_data <- readRDS("rep_data_rd_1.rds")


#Create interactions between all these variables and season
for(i in which(colnames(rep_data) == "season2"):which(colnames(rep_data) == "season1")){
  y <- i + 1 - which(colnames(rep_data) == "season2")
  for(k in which(colnames(rep_data) == "tMinL1"):which(colnames(rep_data) == "tMaxMaxL1")){
    rep_data <- within(rep_data,assign(paste0("season",k,y), rep_data[,i]*rep_data[,k]))
  }
}



## Create time trend variables
startdate <- as.Date("1960-01-01", "%Y-%m-%d")
rep_data$time  <- as.numeric(difftime(as.Date(rep_data$date),startdate ,units="days"))/365
maxdate <- max(rep_data$time)
mindate <- min(rep_data$time)
Z <- 2 * (rep_data$time - mindate) / (maxdate - mindate) - 1 
rep_data$time1 <- Z
rep_data$time2 <-  2*Z^2 - 1
rep_data$time3 <- 4*Z^3 - 3*Z
rep_data$time4 <- 2 * Z * rep_data$time3 - rep_data$time2
rep_data$time5 <- 2 * Z * rep_data$time4 - rep_data$time3
rep_data$time6 <- 2 * Z * rep_data$time5 - rep_data$time4
rep_data$time7 <- 2 * Z * rep_data$time6 - rep_data$time5
rep_data$time8 <- 2 * Z * rep_data$time7 - rep_data$time6
rep_data$time9 <- 2 * Z * rep_data$time8 - rep_data$time7
rep_data$time10 <- 2 * Z * rep_data$time9 - rep_data$time8
rep_data$time <- NULL

#Create the cluster variables
rep_data$yearmonth <- rep_data$year * 100 + as.numeric(rep_data$mon)
rep_data$yearseason <- rep_data$year * 100 + as.numeric(rep_data$season)
rep_data$logozone <- log(rep_data$ozoneMax)
rep_data$dows <- NULL
saveRDS(rep_data,"rep_data_rd_reg.rds")


#read data file again
rep_data <- readRDS("rep_data_rd_reg.rds")
#Create the dataset for the regressions
rep_data <- rep_data[which(rep_data$monitorID == 1001 & rep_data$fips == 34007),]
rep_data <- rep_data[order(rep_data$date),]
rep_data <-rep_data[-which(is.na(rep_data$tMinL1)),]
reg_data <- data.frame( rep_data$RFG_ind,
                        rep_data[which(colnames(rep_data) == 
                                         "tMaxdow1"):which(colnames(rep_data) ==
                                                             "logozone")])

#Run the rd regression
rd_reg <- lm(reg_data$logozone ~ .  
             , data = reg_data[,1:163] )
#Get residuals and summarize 
residual <- resid(rd_reg)
pred <- data.frame(predict(rd_reg, se.fit = TRUE))
rd_reg <- summary(rd_reg, cluster = c(reg_data$yearmonth,reg_data$yearseason))


#Merge the datasets that give the variables we want 
rd_reg_1 <- data.frame(t(rd_reg$coefficients[2,]))
rd_reg_2 <- data.frame(rd_reg$coefficients[115:124,])
rd_reg <- rbind(rd_reg_1,rd_reg_2)

#Generate the plot line and it's residuals
plot_line <- rd_reg[1,1]*reg_data$rep_data.RFG_ind +
  rd_reg[2,1]*reg_data$time1 + rd_reg[3,1]*reg_data$time2 + 
  rd_reg[4,1]*reg_data$time3 + rd_reg[5,1]*reg_data$time4 + 
  rd_reg[6,1]*reg_data$time5 + rd_reg[7,1]*reg_data$time6 +
  rd_reg[8,1]*reg_data$time7 + rd_reg[9,1]*reg_data$time8 
plot_residual <- residual + plot_line

#Create upper and lower bounds
up_bound <- plot_line + pred[,2]*1.96
low_bound <- plot_line - pred[,2]*1.96
#change the variables of plot line and residuals
plot_residual <- plot_residual - mean(plot_residual)
plot_line <- plot_line - mean(plot_line)


#Create the plot
plot(rep_data$date,plot_residual,ylim=c(-0.5,0.5),ylabel = "Daily residuals of log(ozone max)" )
lines(rep_data$date, plot_line, col = "red")
lines(rep_data$date, up_bound,col = "blue")
lines(rep_data$date, low_bound, col = "blue")

