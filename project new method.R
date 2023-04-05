
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

# Set directory
#setwd("/Users/yangshuya/Desktop/Jasmine Yang/PhD/Econometrics 2/Replication/Replication/data")
setwd("C:\\Users\\darra\\Dropbox\\UCD PhD\\PhD\\Econo 2\\project")

# Starting by looking at the yearly-obs data for summary characteristics

# Strategy 2 regression
## https://psantanna.com/DRDID/reference/drdid.html
#library(DRDID)
# Load data in long format that comes in the DRDID package
data(nsw_long)
# Form the Lalonde sample with CPS comparison group
eval_lalonde_cps <- subset(nsw_long, nsw_long$treated == 0 | nsw_long$sample == 2)

out <- drdid(yname = "re", tname = "year", idname = "id", dname = "experimental",
             xformla= ~ age + educ + black + married + nodegree + hisp + re74,
             data = eval_lalonde_cps, panel = TRUE)
summary(out)
out
################################## the above only works with pre and post.. 
##below has twfe.. treat at different times.
## https://rdrr.io/github/bcallaway11/did/f/README.md
## https://bcallaway11.github.io/did/articles/did-basics.html
#library(did)
data(mpdta)

out <- att_gt(yname = "lemp",
              gname = "first.treat",
              idname = "countyreal",
              tname = "year",
              xformla = ~1,
              data = mpdta,
              est_method = "reg",
              
)

summary(out)
ggdid(out)
##################################################
## Our context

# Strategy 1
# Load data into dataframe 'DF2'
DF2 <- read.dta13("pres-elec.dta")
## first we should panel set the data 
## next find the value it first goes to one 

library(dplyr)
#DF2v1 <- data.frame(DF2$county, DF2$year, DF2$mspost)
#DF2v1 <- DF2v1 %>%
  #group_by(DF2v1$DF2.county) %>% 
  #mutate(treatedfirst = ifelse(DF2.mspost == 1 & lag(DF2.mspost == 0), DF2v1$DF2.year, NA))

DF2v1 <- DF2 %>% select(county, year,mse,mspost, popsd,stateyear,repshare)
DF2v1$treatedyear <- ifelse(DF2v1$mspost == 1, DF2v1$year, 0)
#DF2v1 <- pdata.frame(DF2v1, index=c("county","year"), drop.index=FALSE, row.names=TRUE)
## For some reason when I dont panel set the data it works up until a point 

DF2v1 <- DF2v1 %>% 
  group_by(county) %>% 
  mutate(treatedfirst = min(ifelse(treatedyear == 0, 9999, treatedyear)))

DF2v1$treatedfirst <- ifelse(DF2v1$treatedfirst == 9999, 0, DF2v1$treatedfirst)


#####################################################
## outcome version 1
#outcome1 <- drdid(yname = "repshare", tname = "year", idname = "county", dname = "mspost",
                  #xformla= ~  mspost + popsd + stateyear+ county+ year + year:popsd,
                  #data = DF2, panel = TRUE)
#summary(outcome1)

#https://bcallaway11.github.io/did/reference/att_gt.html
#https://rstudio-pubs-static.s3.amazonaws.com/927985_a19e1dddee2b474a81db02aca462dd4e.html


DF2v1$year <-as.numeric(DF2v1$year)
DF2v1$county <-as.numeric(DF2v1$county)
DF2v1$treatedfirst <-as.numeric(DF2v1$treatedfirst)

outcome1 <- att_gt(yname = "repshare",
              tname = "year",
              idname = "county",
              gname = "treatedfirst",
              xformla = ~ popsd + stateyear+ county+ year + year:popsd,
              data = DF2v1,
              panel = TRUE, 
              control_group = c("notyettreated"),
              anticipation = 0,
              est_method = "dr",
              base_period = "universal",
)

outsimple1 <- aggte(outcome1, type="simple")
summary(outsimple1)

# No pre-treatment periods to test

outcome1 <- att_gt(yname = "repshare",
                   tname = "year",
                   idname = "county",
                   gname = "treatedfirst",
                   xformla = ~ popsd + stateyear+ county+ year + year:popsd,
                   data = DF2v1,
                   panel = TRUE, 
                   control_group = c("nevertreated"),
                   anticipation = 0,
                   est_method = "dr",
                   base_period = "universal",
)

# No pre-treatment periods to test

outsimple1 <- aggte(outcome1, type="simple")
summary(outsimple1)




##No pre-treatment periods to test

summary(outcome1)
ggdid(outcome1)

## Making an event study out of it 
es1 <- aggte(outcome1, type = "dynamic")
summary(es1)
ggdid(es1)














outcome1 <- att_gt(yname = "repshare",
                   tname = "year",
                   idname = "county",
                   gname = "treatedfirst",
                   xformla = ~ popsd + stateyear+ county+ year + year:popsd,
                   data = DF2v1,
                   est_method = "reg",
                   panel = TRUE, 
                   allow_unbalanced_panel = FALSE,
                   control_group = c("nevertreated", "notyettreated"),
                   anticipation = 0,
                   weightsname = NULL,
                   alp = 0.05,
                   bstrap = TRUE,
                   cband = TRUE,
                   biters = 1000,
                   clustervars = NULL,
                   est_method = "dr",
                   base_period = "varying",
                   print_details = FALSE,
                   pl = FALSE,
                   cores = 1
)

outsimple1 <- aggte(outcome1, type="simple")
summary(outsimple1)

##No pre-treatment periods to test

summary(outcome1)
ggdid(outcome1)

## Making an event study out of it 
es1 <- aggte(outcome1, type = "dynamic")
summary(es1)
ggdid(es1)

## Strategy 2
# Load data into dataframe 'DF3'
DF3 <- read.dta13("pres-elec-succfail.dta")

#####################################################
## outcome version 2
#outcome2 <- #drdid(yname = "repshare", tname = "year", idname = "county", dname = "successful",
                  #xformla= ~  popsd +stateyear+ county+ year+ mspost,
                  #data = DF3, panel = TRUE)
#summary(outcome2)



outcome1 <- att_gt(yname = "repshare",
                   tname = "year",
                   idname = "county",
                   gname = "mspost",
                   xformla = ~ popsd + stateyear+ county+ year + year:popsd,
                   data = DF3,
                   est_method = "reg"
)

summary(outcome2)
ggdid(outcome2)

## Making an event study out of it 
es2 <- aggte(outcome2, type = "dynamic")
summary(es2)
ggdid(es2)
