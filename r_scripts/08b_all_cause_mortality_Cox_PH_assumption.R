###******************************************************************************
# Name: examine mortality 
# Created by: Ania Z
#******************************************************************************


# Housekeeping =========================
rm(list=ls())
gc()

#path location
setwd(" ")

path <- " "
data_dir <- " "
master_dir <- " "
outputs <- " "
results <- " "


## Load packages  ==============================================================
library(dplyr)
library(survival)
library(survminer) # for plots
library(ggplot2)
library(broom) # tidy output
library(data.table)


# LOAD DATA   ==========================================================

## load pupil data ==================
pupils <- fread(paste0(outputs, "/02_year10_cohort.csv"))

## formatting model variables --------------------
pupils <- pupils %>% mutate( # leave first year out as baseline:
  school_year_end2011 = ifelse(school_year_end==2011, 1, 0),
  school_year_end2012 = ifelse(school_year_end==2012, 1, 0),
  school_year_end2013 = ifelse(school_year_end==2013, 1, 0),
  school_year_end2014 = ifelse(school_year_end==2014, 1, 0),
  school_year_end2015 = ifelse(school_year_end==2015, 1, 0),
  sen_only = ifelse(sen_csc_flag== "SEN only", 1, 0),
  csc_only  = ifelse(sen_csc_flag=="CSC only", 1, 0),
  sen_csc  = ifelse(sen_csc_flag=="SEN and CSC", 1, 0)) 

pupils$sex.f <- factor(pupils$sex)
table(pupils$sex.f, pupils$sex)

## collapse data to make more computationally managable: ==========

# check if collapsing works
pupils_collapse <- pupils %>% 
  mutate(tmp=1) %>%
  group_by(school_year_end2011, school_year_end2012, school_year_end2013, school_year_end2014,
           school_year_end2015, sen_only, csc_only, sen_csc, sex.f, dth_followup_boo, fup_length) %>%
  summarise(tmp = sum(tmp))

# results from original data:
c0_girls_full_data <- coxph(Surv(fup_length, dth_followup_boo) ~ sen_only +
                        csc_only + sen_csc + 
                        school_year_end2011 +
                        school_year_end2012 + school_year_end2013 +
                        school_year_end2014 +school_year_end2015,  data=pupils[sex.f=="F"] )
summary(c0_girls_full_data)
cox.zph(c0_girls_full_data)
par(mfrow=c(3,3))
plot(cox.zph(c0_girls_full_data))

# GIRLS ===========================

## TEST 1: ==============

c1_girls_time1 <- coxph(Surv(fup_length, dth_followup_boo) ~ sen_only +
                        csc_only + sen_csc + tt(sen_csc) +
                        school_year_end2011 +
                        school_year_end2012 + school_year_end2013 +
                        school_year_end2014 +school_year_end2015 +
                        tt(sen_csc),
                      data=pupils_collapse[sex.f=="F"],
                      weights = tmp,
                      tt=function(x,t,...)x*log(t))
summary(c1_girls_time1)

c1_girls_time2 <- coxph(Surv(fup_length, dth_followup_boo) ~ sen_only +
                          csc_only + sen_csc + tt(sen_csc) +
                          school_year_end2011 +
                          school_year_end2012 + school_year_end2013 +
                          school_year_end2014 +school_year_end2015 +
                          tt(sen_csc),
                        data=pupils_collapse[sex.f=="F"],
                        weights = tmp,
                        tt=function(x,t,...)x*t)
summary(c1_girls_time2)

AIC(c0_girls_collapsed, c1_girls_time1, c1_girls_time2)
BIC(c0_girls_collapsed, c1_girls_time1, c1_girls_time2)

## TEST 2 - split follow-up ================

girls_tmp <- pupils[sex=="F"] 

# need to include all covariates - this creates a new dataset with all variables split
girls_time_split <-  survSplit(Surv(fup_length, dth_followup_boo) ~ sen_only +
                           csc_only + sen_csc +
                           school_year_end2011 +
                           chronic_condition_pre14 +
                           school_year_end2012 + school_year_end2013 +
                           school_year_end2014 +school_year_end2015 + chronic_condition_pre14 ,
                         data=girls_tmp, 
                         cut=c(1:9), 
                         episode="time_group",
                         id="id")

rm(girls_tmp)
gc()
head(girls_time_split)

# collapse:
girls_time_split <- girls_time_split %>%  mutate(tmp=1) %>%
  group_by(school_year_end2011, school_year_end2012, school_year_end2013, school_year_end2014,
           school_year_end2015, sen_only, csc_only, sen_csc, dth_followup_boo, fup_length,
           chronic_condition_pre14, 
           tstart,time_group ) %>%
  summarise(tmp = sum(tmp))

# check gives same results are previously
c0_girls_time_split <- coxph(Surv(fup_length, dth_followup_boo) ~ sen_only +
                         csc_only + sen_csc + 
                         school_year_end2011 +
                         school_year_end2012 + school_year_end2013 +
                         school_year_end2014 +school_year_end2015,
                       data=girls_time_split,
                       weights = tmp)
summary(c0_girls_time_split)
summary(c0_girls_full_data)

# ok, test interaction:

# model with an interaction between year of fup and SEN_CSC indicator
c1_girls_time3 <- coxph(Surv(tstart ,fup_length, dth_followup_boo) ~ 
                                sen_only +
                                csc_only +  sen_csc:strata(time_group) +
                                school_year_end2011 +
                                school_year_end2012 + school_year_end2013 +
                                school_year_end2014 +school_year_end2015 ,
                              data=girls_time_split,
                              weights = tmp)
summary(c1_girls_time3)

AIC(c0_girls_collapsed, c1_girls_time1, c1_girls_time2)
AIC(c1_girls_time3)
BIC(c0_girls_collapsed, c1_girls_time1, c1_girls_time2)
BIC(c1_girls_time3)

# BOYS ===========================

# results from original data:
c0_boys_full_data <- coxph(Surv(fup_length, dth_followup_boo) ~ sen_only +
                              csc_only + sen_csc + 
                              school_year_end2011 +
                              school_year_end2012 + school_year_end2013 +
                              school_year_end2014 +school_year_end2015,  data=pupils[sex.f=="M"] )
summary(c0_boys_full_data)
cox.zph(c0_boys_full_data)
par(mfrow=c(3,3))
plot(cox.zph(c0_boys_full_data))

# results from collapsed data
setDT(pupils_collapse)
c0_boys_collapsed <- coxph(Surv(fup_length, dth_followup_boo) ~ sen_only +
                              csc_only + sen_csc + 
                              school_year_end2011 +
                              school_year_end2012 + school_year_end2013 +
                              school_year_end2014 +school_year_end2015,
                            data=pupils_collapse[sex.f=="M"],
                            weights = tmp)
summary(c0_boys_full_data)
summary(c0_boys_collapsed)


## TEST 1:  ===============
c1_boys_time1 <- coxph(Surv(fup_length, dth_followup_boo) ~ sen_only +
                          csc_only + sen_csc + tt(sen_csc) +
                          school_year_end2011 +
                          school_year_end2012 + school_year_end2013 +
                          school_year_end2014 +school_year_end2015 +
                          tt(sen_csc),
                        data=pupils_collapse[sex.f=="M"],
                        weights = tmp,
                        tt=function(x,t,...)x*log(t))
summary(c1_boys_time1)

c1_boys_time2 <- coxph(Surv(fup_length, dth_followup_boo) ~ sen_only +
                          csc_only + sen_csc + tt(sen_csc) +
                          school_year_end2011 +
                          school_year_end2012 + school_year_end2013 +
                          school_year_end2014 +school_year_end2015 +
                          tt(sen_csc),
                        data=pupils_collapse[sex.f=="M"],
                        weights = tmp,
                        tt=function(x,t,...)x*t)
summary(c1_boys_time2)

AIC(c0_boys_collapsed, c1_boys_time1, c1_boys_time2)
BIC(c0_boys_collapsed, c1_boys_time1, c1_boys_time2)

## TEST 2 - split follow-up ================

boys_tmp <- pupils[sex=="M"] 

# need to include all covariates - this creates a new dataset with all variables split
boys_time_split <-  survSplit(Surv(fup_length, dth_followup_boo) ~ sen_only +
                                 csc_only + sen_csc +
                                 school_year_end2011 +
                                 chronic_condition_pre14 +
                                 school_year_end2012 + school_year_end2013 +
                                 school_year_end2014 +school_year_end2015 + chronic_condition_pre14 ,
                               data=boys_tmp, 
                               cut=c(1:9), 
                               episode="time_group",
                               id="id")

rm(boys_tmp)
gc()
head(boys_time_split)

# collapse:
boys_time_split <- boys_time_split %>%  mutate(tmp=1) %>%
  group_by(school_year_end2011, school_year_end2012, school_year_end2013, school_year_end2014,
           school_year_end2015, sen_only, csc_only, sen_csc, dth_followup_boo, fup_length,
           chronic_condition_pre14, 
           tstart,time_group ) %>%
  summarise(tmp = sum(tmp))

# check gives same results are previously
c0_boys_time_split <- coxph(Surv(fup_length, dth_followup_boo) ~ sen_only +
                               csc_only + sen_csc + 
                               school_year_end2011 +
                               school_year_end2012 + school_year_end2013 +
                               school_year_end2014 +school_year_end2015,
                             data=boys_time_split,
                             weights = tmp)
summary(c0_boys_time_split)
summary(c0_boys_full_data)

# ok, test interaction:

# model with an interaction between year of fup and SEN_CSC indicator
c1_boys_time3 <- coxph(Surv(tstart ,fup_length, dth_followup_boo) ~ 
                          sen_only +
                          csc_only +  sen_csc:strata(time_group) +
                          school_year_end2011 +
                          school_year_end2012 + school_year_end2013 +
                          school_year_end2014 +school_year_end2015 ,
                        data=boys_time_split,
                        weights = tmp)
summary(c1_boys_time3)

AIC(c0_boys_collapsed, c1_boys_time1, c1_boys_time2)
AIC(c1_boys_time3)
BIC(c0_boys_collapsed, c1_boys_time1, c1_boys_time2)
BIC(c1_boys_time3)

