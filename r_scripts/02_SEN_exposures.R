#identifying SEN/EHCP 
# AZ - May 2025

# Housekeeping =========================
rm(list=ls())
gc()

library(tidyverse) 
library(lubridate)
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

pupils$sen_sensitivity <- NULL
pupils$sen_provision <- NULL


# sen provision ====================================
sen_data <- fread(paste0(master_dir,"NPD_SEN_provision_data_clean.csv"))
sen_data <- sen_data %>% filter(PupilMatchingRefAnonymous %in% pupils$PupilMatchingRefAnonymous)
head(sen_data)

tmp <- pupils %>% select(PupilMatchingRefAnonymous, school_year_end) %>%
  rename(year9_end=school_year_end)

sen_data <- merge(sen_data, 
                  tmp,
                  all.x=F, all.y=F, by="PupilMatchingRefAnonymous")

sen_data <- sen_data %>% filter(school_year_end<=year9_end)
sen_data <- sen_data %>% filter(school_year_end> (year9_end-7) )

# sense check:
table(sen_data$AgeAtStartOfAcademicYear, sen_data$NCyearActual, exclude = NULL)

#add in sensitivity here with one year look back - but that's 2 years :)
sen_data_sensitivity <- sen_data %>%
  filter( school_year_end==year9_end | school_year_end==(year9_end-1) ) %>% 
  group_by(PupilMatchingRefAnonymous) %>% 
  mutate(sen_sensitivity = max(sen_provision)) %>%
  ungroup() %>% 
  select(PupilMatchingRefAnonymous, sen_sensitivity) %>% 
  distinct(PupilMatchingRefAnonymous, .keep_all = T)

pupils <- merge(pupils, sen_data_sensitivity, all.x=T, all.y=F, by="PupilMatchingRefAnonymous")
rm(sen_data_sensitivity)
gc()

table(pupils$sen_sensitivity, exclude=NULL)
pupils <- pupils %>% mutate(sen_sensitivity = ifelse(is.na(sen_sensitivity), 0, sen_sensitivity) )

sen_data <- sen_data %>%
  group_by(PupilMatchingRefAnonymous) %>% 
  mutate(sen_provision = max(sen_provision)) %>%
  ungroup() %>% 
  select(PupilMatchingRefAnonymous, sen_provision) %>% 
  distinct()

dups <- duplicated(pupils[, "PupilMatchingRefAnonymous"])
table(dups)

dups <- duplicated(sen_data[, "PupilMatchingRefAnonymous"])
table(dups)

pupils <- merge(pupils, sen_data, all.x=T, all.y=F, by="PupilMatchingRefAnonymous")

table(pupils$sen_provision, pupils$sen_sensitivity, exclude=NULL)
pupils <- pupils %>% mutate(sen_provision = ifelse(is.na(sen_provision), 0, sen_provision) )

fwrite(pupils, paste0(outputs, "/01de_dup_pupils.csv"))

rm(sen_data)
gc()


# Indication of need =================

# type of need
sen_need <- fread(paste0(master_dir,"NPD_SEN_Type_of_Need_clean.csv"))
sen_need <- sen_need %>% filter(PupilMatchingRefAnonymous %in% pupils$PupilMatchingRefAnonymous)
head(sen_need)

sen_need <- merge(sen_need, 
                  tmp,
                  all.x=F, all.y=F, by="PupilMatchingRefAnonymous")

rm(tmp)
gc()

# only most recent:
sen_need <- sen_need %>% filter(school_year_end<=year9_end)
sen_need <- sen_need %>% filter(school_year_end> (year9_end-7) )

sen_need <- sen_need %>%
  mutate(asd = ifelse(PrimarySENtype=="ASD" | SecondarySENtype=="ASD" , 1, 0),
         pmld = ifelse(PrimarySENtype=="PMLD" | SecondarySENtype=="PMLD" , 1, 0),
         sld = ifelse(PrimarySENtype=="SLD" | SecondarySENtype=="SLD" , 1, 0),
         any_ld = ifelse(PrimarySENtype=="SLD" | SecondarySENtype=="SLD" | PrimarySENtype=="PMLD" | SecondarySENtype=="PMLD", 1, 0),
         mld = ifelse(PrimarySENtype=="MLD" | SecondarySENtype=="MLD" , 1, 0),
         vi = ifelse(PrimarySENtype=="VI" | SecondarySENtype=="VI" | PrimarySENtype=="MSI" | SecondarySENtype=="MSI" , 1, 0),
         hi = ifelse(PrimarySENtype=="HI" | SecondarySENtype=="HI" | PrimarySENtype=="MSI" | SecondarySENtype=="MSI", 1, 0),
         pd = ifelse(PrimarySENtype=="PD" | SecondarySENtype=="PD" , 1, 0),
         slcn = ifelse(PrimarySENtype=="SLCN" | SecondarySENtype=="SLCN" , 1, 0),
         besd = ifelse(PrimarySENtype=="BESD" | SecondarySENtype=="BESD" , 1, 0),
         semh = ifelse(PrimarySENtype=="SEMH" | SecondarySENtype=="SEMH" , 1, 0),
         spld = ifelse(PrimarySENtype=="SPLD" | SecondarySENtype=="SPLD" , 1, 0)
  ) 

# how many times each mentioned
sen_need <- sen_need %>% select(-NCyearActual, -AgeAtStartOfAcademicYear, -sen_provision, -school_year_start,
                                -school_year_end, -census, -PrimarySENtype, -SecondarySENtype ) %>%
  group_by(PupilMatchingRefAnonymous) %>%
  mutate( across(asd:spld, ~ max(.x) )) %>% ungroup() %>% distinct()

fwrite(sen_need, paste0(outputs, "/sen_types_need_ever_y3y9.csv"))

table(sen_need$asd, exclude=NULL)

sen_need <- sen_need %>% filter(asd==1)

pupils <- pupils %>% mutate(asd = ifelse(PupilMatchingRefAnonymous %in% sen_need$PupilMatchingRefAnonymous, 1, 0) )
table(pupils$sex_y9, pupils$asd, exclude=NULL)

fwrite(pupils, paste0(outputs, "/01de_dup_pupils.csv"))

rm(sen_need)
gc()
