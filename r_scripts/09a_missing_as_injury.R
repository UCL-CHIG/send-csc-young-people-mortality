#09. cause specific


# HOUSEKEEPING ======================

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
library(tidycmprsk)
library(data.table)
# library(cmprsk)  # 
# library(ggsurvfit)


## load pupil data ==================
pupils <- fread(paste0(outputs, "/02_year10_cohort.csv"))
head(pupils)

## drop what not needed and generate ID: ------------
pupils <- pupils %>% select(encrypted_hesid, school_year_end,
                            sex, csc, sen_csc_flag, csc2, sen_csc_sens,
                            chronic_condition_pre14,dth_followup_boo, death_adv, fup_length_days,
                            fup_length ) 
gc()
pupils$id <- seq_len(nrow(pupils))  # need for multistate cox

## formatting SEN / CSC===================

# SEN / CSC
pupils <- pupils %>% mutate( # leave first year out as baseline:
  school_year_end2011 = ifelse(school_year_end==2011, 1, 0),
  school_year_end2012 = ifelse(school_year_end==2012, 1, 0),
  school_year_end2013 = ifelse(school_year_end==2013, 1, 0),
  school_year_end2014 = ifelse(school_year_end==2014, 1, 0),
  school_year_end2015 = ifelse(school_year_end==2015, 1, 0),
  sen_only = ifelse(sen_csc_flag== "SEN only", 1, 0),
  csc_only  = ifelse(sen_csc_flag=="CSC only", 1, 0),
  sen_csc  = ifelse(sen_csc_flag=="SEN and CSC", 1, 0)) 


# SEN / CSC sensitivity
pupils <- pupils %>% 
  mutate(sen_only_sens = ifelse(sen_csc_sens== "SEN only", 1, 0),
         csc_only_sens  = ifelse(sen_csc_sens=="CSC only", 1, 0),
         sen_and_csc_sens  = ifelse(sen_csc_sens=="SEN and CSC", 1, 0)) 

pupils$sen_csc_sens <- factor(pupils$sen_csc_sens, levels = c("None", "SEN only", "CSC only", "SEN and CSC"))
table(pupils$sen_csc_flag, pupils$sen_csc_sens, exclude=NULL)

## deaths by cause ============================

# need one factor variable

pupils <- pupils %>%
  mutate(death_factor = case_when(death_adv=="Accidental injury" | death_adv=="Adversity-related injury" |
                                    death_adv=="Missing underlying cause" ~ 1,
                                death_adv=="Non injury" ~ 2,
                                death_adv=="" ~ 0 ),  #0 needs to be censoring state
         death_factor = factor(death_factor, levels=c(0:2), labels=c("Alive", "Injury", "Non-injury")))

table(pupils$death_factor, exclude=NULL)
class(pupils$death_factor)

# cumulative incidence: --------- 

## 1 - Competing risk  ------------------------------------

# main 

setDT(pupils)
pupils$sex <- factor(pupils$sex, levels = c("F", "M"))
table(pupils$sex)

# using tidycmprsk command & FUP in days - might be easier to get estimates at specific times
# no difference - still cannot estimate specific times, unless event happened then
girls <- tidycmprsk::cuminc( Surv(fup_length_days , death_factor) ~ sen_csc_flag, data=pupils[sex=="F"] )
girls_summary <- girls %>% tidy(time=max(pupils[sex=="F"]$fup_length_days)) 
girls_summary$gender <- "F"

# using tidycmprsk command & FUP in days - might be easier to get estimates at specific times
boys <- tidycmprsk::cuminc( Surv(fup_length_days , death_factor) ~ sen_csc_flag, data=pupils[sex=="M"] )
boys_summary <- boys %>% tidy(time=max(pupils[sex=="F"]$fup_length_days)) 
boys_summary$gender <- "M"

cuminc_summary <- rbind(girls_summary, boys_summary)
cuminc_summary <- cuminc_summary %>% select(-time, -time_max)
fwrite(cuminc_summary, paste0(results, "tmp/cause_spec_sens_cum_inc.csv"))

# sensitivity 
# no difference - still cannot estimate specific times, unless event happened then
girls <- tidycmprsk::cuminc( Surv(fup_length_days , death_factor) ~ sen_csc_sens, data=pupils[sex=="F"] )
girls_summary <- girls %>% tidy(time=max(pupils[sex=="F"]$fup_length_days)) 
girls_summary$gender <- "F"

# using tidycmprsk command & FUP in days - might be easier to get estimates at specific times
boys <- tidycmprsk::cuminc( Surv(fup_length_days , death_factor) ~ sen_csc_sens, data=pupils[sex=="M"] )
boys_summary <- boys %>% tidy(time=max(pupils[sex=="F"]$fup_length_days)) 
boys_summary$gender <- "M"

cuminc_summary <- rbind(girls_summary, boys_summary)
cuminc_summary <- cuminc_summary %>% select(-time, -time_max)
fwrite(cuminc_summary, paste0(results, "tmp/cause_spec_sens_cum_inc_sens.csv"))

rm(boys_summary, boys, girls_summary, girls)

# Regressions ======================================

## 1 - multistate cox ---------------

### girls =================

# unadjuted
cox_multi_state <- coxph(Surv(fup_length, death_factor) ~ sen_only +
                       csc_only + sen_csc + 
                       school_year_end2011 +
                       school_year_end2012 + school_year_end2013 +
                       school_year_end2014 +school_year_end2015,
                       data=pupils[sex=="F"],
                       id=id)  # id needed for the multistate; and death_factor being a factor variable

model_summary <- summary(cox_multi_state)

cox_multi_state_summary <- data.frame(
  coef = rownames(model_summary$coefficients),
  estimate = model_summary$coefficients[,1],
  se.robust =  model_summary$coefficients[,3],
  HR = model_summary$conf.int[,1],
  lci = model_summary$conf.int[,3],
  uci = model_summary$conf.int[,4],
  gender = "girls",
  model = "unadjusted")

###  adjusted
cox_multi_state_adj <- coxph(Surv(fup_length, death_factor) ~ sen_only +
                           csc_only + sen_csc + 
                           school_year_end2011 +
                           school_year_end2012 + school_year_end2013 +
                           school_year_end2014 +school_year_end2015 +
                           chronic_condition_pre14 ,
                         data=pupils[sex=="F"],
                         id=id)  # id needed for the multistate; and death_factor being a factor variable

model_summary <- summary(cox_multi_state_adj)

cox_multi_state_summary_adj <- data.frame(
  coef = rownames(model_summary$coefficients),
  estimate = model_summary$coefficients[,1],
  se.robust =  model_summary$coefficients[,3],
  HR = model_summary$conf.int[,1],
  lci = model_summary$conf.int[,3],
  uci = model_summary$conf.int[,4],
  gender = "girls",
  model = "adjusted")

cox_multi_state_summary <- rbind(cox_multi_state_summary, cox_multi_state_summary_adj)
rm(cox_multi_state_summary_adj, model_summary)
rm(cox_multi_state_adj, cox_multi_state)

### boys=================
# unadjuted
cox_multi_state <- coxph(Surv(fup_length, death_factor) ~ sen_only +
                           csc_only + sen_csc + 
                           school_year_end2011 +
                           school_year_end2012 + school_year_end2013 +
                           school_year_end2014 +school_year_end2015,
                         data=pupils[sex=="M"],
                         id=id)  # id needed for the multistate; and death_factor being a factor variable

model_summary <- summary(cox_multi_state)

tmp <- data.frame(
  coef = rownames(model_summary$coefficients),
  estimate = model_summary$coefficients[,1],
  se.robust =  model_summary$coefficients[,3],
  HR = model_summary$conf.int[,1],
  lci = model_summary$conf.int[,3],
  uci = model_summary$conf.int[,4],
  gender = "boys",
  model = "unadjusted")

cox_multi_state_summary <- rbind(cox_multi_state_summary, tmp)
rm(tmp, model_summary)


# adjusted
cox_multi_state <- coxph(Surv(fup_length, death_factor) ~ sen_only +
                               csc_only + sen_csc + 
                               school_year_end2011 +
                               school_year_end2012 + school_year_end2013 +
                               school_year_end2014 +school_year_end2015 +
                               chronic_condition_pre14 ,
                             data=pupils[sex=="M"],
                             id=id)  # id needed for the multistate; and death_factor being a factor variable

model_summary <- summary(cox_multi_state)

tmp <- data.frame(
  coef = rownames(model_summary$coefficients),
  estimate = model_summary$coefficients[,1],
  se.robust =  model_summary$coefficients[,3],
  HR = model_summary$conf.int[,1],
  lci = model_summary$conf.int[,3],
  uci = model_summary$conf.int[,4],
  gender = "boys",
  model = "adjusted")

cox_multi_state_summary <- rbind(cox_multi_state_summary, tmp)
rm(tmp, model_summary)

fwrite(cox_multi_state_summary, paste0(results, "tmp/multistate_cox_results_sens.csv"))


