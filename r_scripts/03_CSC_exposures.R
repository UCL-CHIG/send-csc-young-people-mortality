# Flagging having past record of CIN/CLA
# AZ - revised May 2025

# Housekeeping =========================
rm(list=ls())
gc()

library(tidyverse) 
library(data.table)

#path location
setwd(" ")

path <- " "
data_dir <- " "
master_dir <- " "
outputs <- " "

# load pupil data
pupils <- fread(paste0(outputs, "/01de_dup_pupils.csv"))
glimpse(pupils)

# CSC data #############

## CLA ##############

# load and keep only relevant IDs

cla <- fread(paste0(master_dir, "csc/cla.csv"))
nrow(cla)
glimpse(cla)

cla <- cla %>%
  filter(!is.na(pupilmatchingrefanonymous) & pupilmatchingrefanonymous!="")
nrow(cla)

cla <- cla %>% subset(pupilmatchingrefanonymous %in% pupils$PupilMatchingRefAnonymous) 

# link relevant info from pupil cohort:
tmp <- pupils %>% select(PupilMatchingRefAnonymous, start_date, ydob, mdob ) %>%
  rename(school_start_date =start_date ) %>%
  filter(PupilMatchingRefAnonymous %in% cla$pupilmatchingrefanonymous)

cla <- merge(cla, tmp, all.x=F, all.y=F, by.x="pupilmatchingrefanonymous", by.y="PupilMatchingRefAnonymous")
glimpse(cla)

# filter only episodes before Y10 start:
# poc_start: Period of care start date
# A POC consists of one or more episodes. 
# An episode is a period of being looked after for at least 24 hours.
# A new episode is started if there is a change in placement or legal status

cla <- cla %>% filter(poc_start<school_start_date)
nrow(cla)

# check inconsistencies between birthdates
cla <- cla %>% 
  mutate(drop_record = case_when(ydob != year_of_birth  ~ 1, 
                                 mdob != month_of_birth  ~ 1,
                                 TRUE ~ 0))
table(cla$drop_record, exclude = NULL)

cla <- cla %>% filter(drop_record!=1)

# save short breaks separately - code from Matt
# 2025-08: this should be included! refine the cohort later on
setDT(cla)
stb <- cla[legal_status %in% c("V3", "V4")]
cla <- cla[!(legal_status %in% c("V3", "V4"))]

pupils <- pupils %>%
  mutate(cla_ever_flag = ifelse(PupilMatchingRefAnonymous %in% cla$pupilmatchingrefanonymous, 1, 0),
         cla_stb_flag = ifelse(PupilMatchingRefAnonymous %in% stb$pupilmatchingrefanonymous, 1, 0))
table(pupils$cla_ever_flag, exclude = NULL)
table(pupils$cla_stb_flag, exclude = NULL)


##SENSITIVITY for both CLA and CIN is just aged 12-14
cla <- cla %>% filter(date_episode_started<school_start_date)
cla <- cla %>% mutate( tmp = school_start_date %m-% years(2) )
cla <- cla %>% filter(date_episode_started>=tmp )

stb <- stb %>% filter(date_episode_started<school_start_date)
stb <- stb %>% mutate( tmp = school_start_date %m-% years(2) )
stb <- stb %>% filter(date_episode_started>=tmp )

pupils <- pupils %>%
  mutate(cla_sensitivity_flag = ifelse(PupilMatchingRefAnonymous %in% cla$pupilmatchingrefanonymous, 1, 0),
         cla_stb_sensitivity_flag = ifelse(PupilMatchingRefAnonymous %in% stb$pupilmatchingrefanonymous, 1, 0))
table(pupils$cla_sensitivity_flag, exclude = NULL)
table(pupils$cla_stb_sensitivity_flag, exclude = NULL)

# save
fwrite(pupils, paste0(outputs, "/01de_dup_pupils.csv"))
rm(cla, tmp, stb)
gc()

dups <- duplicated(pupils[, "PupilMatchingRefAnonymous"])
table(dups)

## CIN ###############

cin <- fread(paste0(master_dir, "csc/cin.csv"))

cin <- cin %>%
  filter(!is.na(pupilmatchingrefanonymous) & pupilmatchingrefanonymous!="")

cin <- cin %>%
  subset(pupilmatchingrefanonymous %in% pupils$PupilMatchingRefAnonymous)

#from matts 01_cin.R code this is need_episode variable
cin <- cin %>%
  filter(need_episode == T)

# link relevant info from pupil cohort:
tmp <- pupils %>% select(PupilMatchingRefAnonymous, start_date, ydob, mdob ) %>%
  rename(school_start_date =start_date ) %>%
  filter(PupilMatchingRefAnonymous %in% cin$pupilmatchingrefanonymous)

cin <- merge(cin, tmp, all.x=F, all.y=F, by.x="pupilmatchingrefanonymous", by.y="PupilMatchingRefAnonymous")
glimpse(cin)

# check inconsistencies between birthdates
head(cin)
cin <- cin %>% 
  mutate(notional_dob = as.Date(notional_dob),
    drop_record = case_when(ydob != year(notional_dob)  ~ 1, 
                                 mdob != month(notional_dob)  ~ 1,
                                 TRUE ~ 0))
table(cin$drop_record, exclude = NULL)
cin <- cin %>% filter(drop_record!=1)

# From documentation i think referrals can go back max -7*31 (during pregnancy) so drop other
hist(cin$age_at_ref_yrs)
cin <- cin %>% mutate(dob_flag = ifelse(age_at_ref_yrs < -(7*31)/365.25, T, F))
table(cin$dob_flag)
cin <- cin %>% filter(dob_flag == F) #2456860 

# Basing exclusions on dates
cin <- cin %>% filter(referraldate<school_start_date)
hist(cin$age_at_ref_yrs)
setDT(cin)

pupils <- pupils %>%
  mutate(cin_ever_flag = ifelse(PupilMatchingRefAnonymous %in% cin$pupilmatchingrefanonymous, 1, 0),
         cin_cpp = ifelse(PupilMatchingRefAnonymous %in% cin[cpp==T & cppstartdate<school_start_date]$pupilmatchingrefanonymous, 1, 0))
table(pupils$cin_ever_flag, exclude = NULL)
table(pupils$cin_ever_flag, pupils$cin_cpp, exclude = NULL)

# sensitivity - any contact aged 12-14
##SENSITIVITY for both CLA and CIN is just aged 12-14
cin <- cin %>% mutate( school_start_date = as.Date(school_start_date),
  tmp=as.Date( paste0(year(school_start_date) - 2 , "-09-01")) )

cin <- cin %>% filter( referraldate>=tmp | (cppstartdate>=tmp & !is.na(cppstartdate) ) )

pupils <- pupils %>%
  mutate(cin_sensitivity = ifelse(PupilMatchingRefAnonymous %in% cin$pupilmatchingrefanonymous, 1, 0),
         cin_cpp_sens = ifelse(PupilMatchingRefAnonymous %in% cin[cpp==T & cppstartdate<school_start_date]$pupilmatchingrefanonymous, 1, 0))

table(pupils$cin_ever_flag, pupils$cin_sensitivity, exclude = NULL)
table(pupils$cin_cpp, pupils$cin_cpp_sens, exclude = NULL)

# save
fwrite(pupils, paste0(outputs, "/01de_dup_pupils.csv"))
rm(cin, tmp)
gc()

dups <- duplicated(pupils[, "PupilMatchingRefAnonymous"])
table(dups)

table(pupils$cla_ever_flag, exclude=NULL)


##AGGREGATION OF EXPOSURE FLAGS 
pupils <- pupils %>%  
  mutate(csc = case_when(cla_ever_flag==1 ~ 3,
                         cin_cpp==1 ~ 2,
                         cin_ever_flag==1 | cla_stb_flag ~1,
                         TRUE ~ 0 ))

# sense check it did what i wanted:
table(pupils$csc, pupils$cla_ever_flag, exclude=NULL)
table(pupils$cin_ever_flag, pupils$cla_ever_flag, exclude=NULL)
table(pupils$cin_cpp, pupils$cla_ever_flag, exclude=NULL)
table(pupils$csc, pupils$cin_cpp, exclude=NULL)
table(pupils$csc, pupils$cla_stb_flag, exclude=NULL)

table(pupils$sen_provision, exclude=NULL)

pupils <- pupils %>% 
  mutate(sen_csc_flag = case_when(sen_provision!=0 & csc!=0 ~ "SEN and CSC",
                                  sen_provision == 2 & csc==0 ~ "EHCP only",
                                  sen_provision == 1 & csc==0 ~ "SEN only",
                                  sen_provision==0 & csc!=0 ~ "CSC only",
                                  sen_provision==0 & csc==0 ~ "None",
                                  TRUE ~ "Oops")) 

pupils %>% group_by(sen_csc_flag) %>% count()

fwrite(pupils, paste0(outputs, "/01de_dup_pupils.csv"))


