
# Replication bit
rm(list=ls())
# Load packages
library(foreign)
library(stargazer)
library(tidyverse)
library(Hmisc)
library(readstata13)
library(haven)
library(texreg)
library(xtable)
library(knitr)
library(vtable)
library(plm)
library(dplyr)
library(magrittr)
## New Method
#install.packages("devtools")
#devtools::install_github("pedrohcgs/DRDID")
library(DRDID)
#devtools::install_github("bcallaway11/did")
library(did)
#install.packages("writexl")
library("writexl")


# Set directory
#setwd("/Users/yangshuya/Desktop/Jasmine Yang/PhD/Econometrics 2/Replication/Replication/data")
setwd("C:\\Users\\darra\\Dropbox\\UCD PhD\\PhD\\Econo 2\\project")

# Starting by looking at the yearly-obs data for summary characteristics

# Strategy 2 regression
## https://psantanna.com/DRDID/reference/drdid.html
################################## the above only works with pre and post.. 
##below has twfe.. treat at different times.
## https://rdrr.io/github/bcallaway11/did/f/README.md
## https://bcallaway11.github.io/did/articles/did-basics.html
#library(did)

##################################################
## Our context

# Strategy 1
# Load data into dataframe 'DF2'
DF2 <- read.dta13("pres-elec.dta")

DF2v1 <- DF2 %>% select(county, year,ms, mse,mspost, popsd,stateyear,repshare)

DF2v1$year1 <- ifelse(DF2v1$year == 2000, 1, 0)
DF2v1$year1 <- ifelse(DF2v1$year == 2004, 2, DF2v1$year1)
DF2v1$year1 <- ifelse(DF2v1$year == 2008, 3, DF2v1$year1)
DF2v1$year1 <- ifelse(DF2v1$year == 2012, 4, DF2v1$year1)
DF2v1$year1 <- ifelse(DF2v1$year == 2016, 5, DF2v1$year1)

DF2v1$treatedyear <- ifelse(DF2v1$ms == 1, DF2v1$year1, 0)

DF2v1 <- DF2v1 %>% 
  group_by(county) %>% 
  mutate(treatedfirst = min(ifelse(treatedyear == 0, 9999, treatedyear)))

DF2v1$treatedfirst <- ifelse(DF2v1$treatedfirst == 9999, 0, DF2v1$treatedfirst)

#####################################################

#https://bcallaway11.github.io/did/reference/att_gt.html
#https://rstudio-pubs-static.s3.amazonaws.com/927985_a19e1dddee2b474a81db02aca462dd4e.html

DF2v1$year1 <-as.integer(DF2v1$year1)
DF2v1$county <-as.numeric(DF2v1$county)
DF2v1$treatedfirst <-as.numeric(DF2v1$treatedfirst)

outcome1 <- att_gt(yname = "repshare",
              tname = "year1",
              idname = "county",
              gname = "treatedfirst",
              xformla = ~ 1 +  popsd,
              data = DF2v1,
              panel = TRUE, 
              control_group = "nevertreated",
              est_method = "dr",
              base_period = "universal",
)


outcome1
outsimple1 <- aggte(outcome1, type="simple", na.rm = TRUE)
summary(outsimple1)

## Making an event study out of it 
es1 <- aggte(outcome1, type = "dynamic")
summary(es1)
ggdid(es1)


############################################################################
## not yet treated

outcome2 <- att_gt(yname = "repshare",
                   tname = "year1",
                   idname = "county",
                   gname = "treatedfirst",
                   xformla = ~ 1 +  popsd,
                   data = DF2v1,
                   panel = TRUE, 
                   control_group = "notyettreated",
                   est_method = "dr",
                   base_period = "universal",
)


outcome2
outsimple2 <- aggte(outcome2, type="simple", na.rm = TRUE)
summary(outsimple2)

## Making an event study out of it 
es2 <- aggte(outcome2, type = "dynamic")
summary(es2)
ggdid(es2)

df <- as.data.frame(es1$egt)
df$attNever <- es1$att.egt
df$seNever <- es1$se
df$attNotYet <- es2$att.egt
df$seNotYet <- es2$se

stargazer(df, type = "latex", header = FALSE, title = "Doubly Robust Gorup ATT's",
          column.labels = c("Never treated", "Not yet treated"),
                            row.labels = c("time", "ATT", "std. error")
                            )

stargazer(summary(es2), type = "latex", header = FALSE, title = "Doubly Robust Gorup ATT's",
          column.labels = c("Not yet treated"))


stargazer(reg_results, type = "latex", header = FALSE, title = "County Characteristics",
          column.labels = c("Republican share", "Turnout", 
                            "State capitol",
                            "Log (median HH income)", 
                            "Poverty rate","Inequality (Gini Index)", 
                            "Racial diversity HHI", 
                            "Proportion with mental issues", 
                            "Population 15 to 39 years", "Single",
                            "College dropouts", "Log (homicides by gun)", 
                            "Log (suicides by Gun)","Log (violent crime)",
                            "Log (property crime)"), 
          row.labels = c("Constant", "MS", "Population"),
          flip = TRUE)





########################################
## Messing with spec ever so slightly 

outcome2 <- att_gt(yname = "repshare",
                   tname = "year1",
                   idname = "county",
                   gname = "treatedfirst",
                   xformla = ~ 1 +  popsd,
                   data = DF2v1,
                   panel = TRUE, 
                   control_group = "nevertreated",
                   est_method = "dr",
                   base_period = "universal",
                   clustervars = "county"
)

## gets errors 

outcome2
outsimple2 <- aggte(outcome2, type="simple", na.rm = TRUE)
summary(outsimple2)

stargazer(outsimple2, type = "latex", header = FALSE, title = "Doubly Robust Gorup ATT's")


          column.labels = c("Republican share", "Turnout", 
                            "State capitol",
                            "Log (median HH income)", 
                            "Poverty rate","Inequality (Gini Index)", 
                            "Racial diversity HHI", 
                            "Proportion with mental issues", 
                            "Population 15 to 39 years", "Single",
                            "College dropouts", "Log (homicides by gun)", 
                            "Log (suicides by Gun)","Log (violent crime)",
                            "Log (property crime)"), 
          row.labels = c("Constant", "MS", "Population"),
          flip = TRUE)


#########################################################################################################################################################
## Strategy 2
# Load data into dataframe 'DF3'
DF3 <- read.dta13("pres-elec-succfail.dta")

DF3v1 <- DF3 %>% select(county, year,mse,failmse, postattack, successful, mspost,ms, popsd,stateyear,repshare)

DF3v1$year1 <- ifelse(DF3v1$year == 2000, 1, 0)
DF3v1$year1 <- ifelse(DF3v1$year == 2004, 2, DF3v1$year1)
DF3v1$year1 <- ifelse(DF3v1$year == 2008, 3, DF3v1$year1)
DF3v1$year1 <- ifelse(DF3v1$year == 2012, 4, DF3v1$year1)
DF3v1$year1 <- ifelse(DF3v1$year == 2016, 5, DF3v1$year1)

DF3v1$treatedyear <- ifelse(DF3v1$mse == 1 |DF3v1$failmse == 1 & DF3v1$successful == 1 & DF3v1$postattack == 1, DF3v1$year1, 0)

#DF3v1 <- pdata.frame(DF3v1, index=c("county","year"), drop.index=FALSE, row.names=TRUE)

DF3v1 <- DF3v1 %>% 
  group_by(county) %>% 
  mutate(treatedfirst = min(ifelse(treatedyear == 0, 9999, treatedyear)))

DF3v1$treatedfirst <- ifelse(DF3v1$treatedfirst == 9999, 0, DF3v1$treatedfirst)

#####################################################
## outcome version 2


DF3v1$year1 <-as.integer(DF3v1$year1)
DF3v1$county <-as.numeric(DF3v1$county)
DF3v1$treatedfirst <-as.numeric(DF3v1$treatedfirst)

outcome3 <- att_gt(yname = "repshare",
                   tname = "year1",
                   idname = "county",
                   gname = "treatedfirst",
                   xformla = ~ 1,
                   data = DF3v1,
                   panel = TRUE, 
                   control_group = "nevertreated",
                   est_method = "dr",
                   base_period = "universal",
)


outcome3
outsimple3 <- aggte(outcome3, type="simple", na.rm = TRUE)
summary(outsimple3)

## Making an event study out of it 
es2 <- aggte(outcome3, type = "dynamic")
summary(es3)
ggdid(es3)

