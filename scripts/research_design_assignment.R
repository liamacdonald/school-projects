
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

setwd("C:/Users/liama/Dropbox/school/assignments/research/ps2")







############################################  QUESTION 3 ####################################################
df <-  read.csv("finalanalysisdata.csv")
###Use only panchayats which have some people paid according to daily wages

df <- df[df$anydw == 1,]

###This changes the shock_day variable to be in a better form for using it in the regression
df$time_int <- 0
df$time_int[df$shock > 0] <- df$shock_day[df$shock > 0] - 121


#####################  QUESTION 3(a)
#Create table 3 from the paper
#Column 1a
m1a <- felm( daysdwoff ~ shock + day
    + holiday + res_gender + res_sbc + res_tribe 
    + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3 + daysdw | 0 | 0 |upid + day , data = df)


#Column 2a
m2a <- felm( daysdwoff ~ shock + day
    + holiday + res_gender + res_sbc + res_tribe 
    + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3 + daysdw |  did | 0 | upid + day, data = df)

#Column 3a

m3a <- felm( daysdwoff ~ shock   + time_int + day
               + holiday + res_gender + res_sbc + res_tribe 
               + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3 + daysdw  |  did | 0 | upid + day, data = df)

#Column 4a
 m4a <- felm( daysdwoff  ~ shock + shock_alwaysdw + alwaysdw
 + day+ holiday + res_gender + res_sbc + res_tribe + 
   season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3 +daysdw | 0 | 0 | upid + day , data = df)

 #Column 5a
 m5a <- felm( daysdwoff  ~  shock + shock_alwaysdw + alwaysdw
             + day+ holiday + res_gender + res_sbc + res_tribe + 
               season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3 + daysdw | did | 0 | upid + day , data = df)

#Column 6a
 m6a <- felm( daysdwoff ~ shock  + shock_alwaysdw + alwaysdw + time_int + day
             + holiday + res_gender + res_sbc + res_tribe 
             + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3 + daysdw  |  did | 0 | upid + day, data = df) 

 #Print the table 
stargazer(m1a,m2a,m3a, m4a , m5a, m6a , keep = c("shock", "shock_alwaysdw" , "alwaysdw"),omit = c("shock_day"),  dep.var.labels = "", 
          dep.var.caption = "" , title= "Wage Shock Effects on Daily Wage Reports: Panel A",covariate.labels = 
            c("Shock","Shock x AlwaysDW" , "AlwaysDW"), css.cell = "padding: 0px 10px 0px;")


##################### QUESTION 3(c)


###First generate some variables were going to need for this question


### Calculate the midpoint of the shockdays
ind <- median(df$shock_day[df$shock != 0])

###Create a dummy for the second half of the shock period
df$shock2 <- 0 
df$shock2[df$shock_day >= ind] <- 1
### Then the dummy for the first half of the period
df$shock1 <- df$shock - df$shock2


### Time trends for each of these shocks 
df$time_int1 <- df$time_int*df$shock1
df$time_int2 <- (df$day - ind)*df$shock2

#Create interaction terms for each period with alwaysdw 
df$shock1_alwaysdw <- df$shock_alwaysdw*df$shock1
df$shock2_alwaysdw <- df$shock_alwaysdw*df$shock2



#Column 1a
m1c <- felm( daysdwoff ~ shock1 + shock2 + day
            + holiday + res_gender + res_sbc + res_tribe 
            + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3 + daysdw | 0 | 0 |upid + day , data = df)


#Column 2a
m2c <- felm( daysdwoff ~ shock1 + shock2 + day
            + holiday + res_gender + res_sbc + res_tribe 
            + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3 + daysdw |  did | 0 | upid + day, data = df)

#Column 3a

m3c <- felm( daysdwoff ~ shock1 + shock2 + time_int1 + time_int2 + day
            + holiday + res_gender + res_sbc + res_tribe 
            + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3 + daysdw  |  did | 0 | upid + day, data = df)

#Column 4a
m4c <- felm( daysdwoff  ~ shock1 + shock2 + shock1_alwaysdw  + shock2_alwaysdw  + alwaysdw
            + day+ holiday + res_gender + res_sbc + res_tribe + 
              season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3 +daysdw | 0 | 0 | upid + day , data = df)

#Column 5a
m5c <- felm( daysdwoff  ~  shock1 + shock2 + shock1_alwaysdw  + shock2_alwaysdw + alwaysdw
            + day+ holiday + res_gender + res_sbc + res_tribe + 
              season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3 + daysdw | did | 0 | upid + day , data = df)

#Column 6a
m6c <- felm( daysdwoff ~ shock1 + shock2  + shock1_alwaysdw + shock2_alwaysdw + alwaysdw + time_int1 + time_int2 + day
            + holiday + res_gender + res_sbc + res_tribe 
            + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3 + daysdw  |  did | 0 | upid + day, data = df) 

#Print the table 
stargazer(m1c,m2c,m3c, m4c , m5c, m6c , keep = c("shock1", "shock2" , "shock_alwaysdw" , "alwaysdw"),omit = c("shock_day"),  
          dep.var.labels = "",  dep.var.caption = "" , title= "Wage Shock Effects on Daily Wage Reports: Panel B",
          covariate.labels = c("Period 1 Shock","Period 2 Shock", "Period 1 Shock x AlwaysDW" , 
                               "Period 2 Shock x AlwaysDW" , "AlwaysDW"), css.cell = "padding: 0px 10px 0px;")

rm(list = ls())


##################################### QUESTION 4 ##################################################
df1 <-  read.dta13("BRFSS.dta")
df2 <- read.dta13("temp_clean.dta")




##### Part (a)
#fips is already in the data?  Just merge data
df <- merge(df1,df2, by = "fips")

####Part(b) created summary statistics for well being and graph distribution
stargazer(df, title= "Summary Statistics",keep =c("lsatisfy_all"))

png(filename = "plot1.png")
barplot(table(df$lsatisfy_all),ylab = "Number of Individuals",xlab = "Self Reported Health")
dev.off()

#Normalize variable
df$norm_satisfy <- (df$lsatisfy_all - mean(df$lsatisfy_all))/sqrt(var(df$lsatisfy_all))



#### Part (c) Create log population growth
df$log_growth <- df$lpop2000 - df$lpop1950
df$log_growth2 <- df$log_growth^2

#Remove NA values so that residuals line up with correct observations later
df <- df[complete.cases(df),]


#### Part (d) Check for concavity of population growth using a number of different models
#linear regression of log growth
m1d <- lm(lsatisfy_all ~ log_growth , data =df)

#Quadratic model of log growth

m2d <- lm(lsatisfy_all ~ log_growth + log_growth2, data =df)


# linear spline model
m3d <- lm(lsatisfy_all ~ bs(log_growth,degree = 1, knots = c(median(log_growth))) , data =df)

summary(m3d)
#interaction of above median 
df$above_median <- 0
df$above_median[df$log_growth > median(df$log_growth)] <- 1
df$int <- df$log_growth*df$above_median

m4d <- lm(lsatisfy_all ~ log_growth + int, data =df)

#Make a table 
stargazer(m1d,m2d,m3d, m4d ,  dep.var.labels = "", 
          dep.var.caption = "" , title= "Population Growth Effects on Self-Reported Wellbeing",covariate.labels = 
            c("Growth","Growth2", "Growth b-spline 1" , "Growth b-spline 2","Growth x Above Median"), css.cell = "padding: 0px 10px 0px;")
#Store residuals from all regressions
df$resid1 <- m1d$residuals
df$resid2 <- m2d$residuals
df$resid3 <- m3d$residuals
df$resid4 <- m4d$residuals
test <- reshape2::melt(df[,c("log_growth","resid1","resid2","resid3","resid4")], id.var = 'log_growth')
### Part (e) 
m2e <- felm(lsatisfy_all ~ log_growth + log_growth2 | 0 | 0 | pmsa , data =df)
se1 <- sqrt(diag(m2e$vcv))
se2 <- sqrt(diag(m2e$robustvcv))
se3 <- sqrt(diag(m2e$clustervcv))

#Put the different standard errors into a table
stargazer(m2e, m2e,m2e,  dep.var.labels = "", 
          dep.var.caption = "" , title= "Population Growth Quadratic Model with Different Standard Errors",covariate.labels = 
            c("Growth","Growth2"), css.cell = "padding: 0px 10px 0px;",
          se = list(se1,se2,se3))

##### Part (f) 
df$"Model 1: Linear" <- m1d$residuals
df$"Model 2: Quadratic" <- m2d$residuals
df$"Model 3: Spline"  <- m3d$residuals
df$"Model 4: Above Median Interaction"  <- m4d$residuals
graph <- reshape2::melt(df[,c("log_growth","Model 1: Linear" ,"Model 2: Quadratic" ,"Model 3: Spline" ,"Model 4: Above Median Interaction" )], id.var = 'log_growth')

### Make the graph
png(filename = "plot2.png")
ggplot(graph, aes(x = log_growth , y = value, shape = variable)) + 
stat_binmean(n=100) + labs(shape = "Legend",y = "Residuals",x = "Log Population Growth") +theme(legend.position="bottom")
dev.off()



