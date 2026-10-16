## 05a_cohort_finalise_flow_chart.R


# Housekeeping =========================
rm(list=ls())
gc()

library(tidyverse) 
library(data.table)
library(lubridate)

#path location
setwd(" ")

path <- " "
data_dir <- " "
master_dir <- " "
outputs <- " "


# load pupil data -------------------------------
pupils <- fread(paste0(outputs, "/01de_dup_pupils.csv"))
glimpse(pupils)

# 1) flowchart numbers =======================================================
tmp1 <- c(table(pupils$sex_y9, exclude = NULL), "initial cohort") # overall

# drop missing sex:
pupils <- pupils %>% filter(sex_y9!="")

# drop people with ages at start of Y10 outside expected values
pupils <- pupils %>%   mutate(# school_year_end = as.integer(school_year_end),
                              acadydob = ydob + (mdob >= 9) ,
                              age_y10 = as.integer(school_year_end - acadydob) )
table(pupils$age_y10)

pupils <- pupils %>% filter(age_y10>=13 & age_y10<=15)
tmp2 <- c("", table(pupils$sex_y9, exclude = NULL), "Aged 13-15" )
tmp1 <- rbind(tmp1, tmp2)

# no linked HESID 
pupils <- pupils %>% filter(encrypted_hesid!="")
tmp2 <- c("", table(pupils$sex_y9, exclude = NULL), "Linked HES" )
tmp1 <- rbind(tmp1, tmp2)

# inconsistent DOD
table(pupils$hes_dob_inconsist, exclude=NULL)
pupils <- pupils %>% filter(hes_dob_inconsist!=1) %>% select(-hes_dob_inconsist)
tmp2 <- c("", table(pupils$sex_y9, exclude = NULL), "Remove conflicing DOB in HES and NPD" )
tmp1 <- rbind(tmp1, tmp2)
tmp1

fwrite(tmp1, paste0(results, "tmp/flow_chart.csv"))
rm(tmp1, tmp2)


# extra data cleaning --------------------
glimpse(pupils)

## gender --- 
pupils <- pupils %>% rename(sex=sex_y9)

## chronic conditions ---
pupils <- pupils %>%
  mutate( across(mh_behav:musc_skel, ~ ifelse(is.na(.x), 0, .x)),
          across(mh_behav:musc_skel, ~ ifelse(.x>=1, 1, .x)) )

pupils <- pupils %>%
  mutate(chronic_condition_pre14 = ifelse(mh_behav == 1 | cardiovasc == 1 |
                                            neuro == 1 |  cc_other == 1 |
                                            resp == 1 | cancer == 1 |
                                            metab_gastr == 1 | musc_skel, 1, 0)) 

## idaci -----
pupils <- pupils %>%
  mutate(idaci_rename = case_when(idaci_y9 == 1 ~ "Q1", 
                                  idaci_y9 == 2 ~ "Q1",
                                  idaci_y9 == 3 ~ "Q2",
                                  idaci_y9 == 4 ~ "Q2",
                                  idaci_y9 == 5 ~ "Q3",
                                  idaci_y9 == 6 ~ "Q3",
                                  idaci_y9 == 7 ~ "Q4",
                                  idaci_y9 == 8 ~ "Q4",
                                  idaci_y9 == 9 ~ "Q5",
                                  idaci_y9 == 10 ~ "Q5",
                                  TRUE ~ "Missing"),
         idaci_rename = factor(idaci_rename , levels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Missing") ) )
table(pupils$idaci_y9, pupils$idaci_rename, exclude=NULL) 

pupils <- pupils %>% select(-idaci_y9, -overall_imd_y9)

## FSM ------------
table(pupils$fsm_y9, exclude=NULL)
table(pupils$fsm_y9, pupils$fsm_evereligible, exclude=NULL)

pupils <- pupils %>% 
  mutate(fsm_rename = case_when(fsm_evereligible == 1 ~ "Ever", 
                                 TRUE ~ "Never") ) %>%
  select(-fsm_evereligible)
                            
                            
## Ethnic group ------------- 
pupils <- pupils %>% 
  mutate(ethnic_rename = case_when(ethnic_gr_y9 == "AOEG" ~ "Any other ethnic group", 
                                   ethnic_gr_y9 == "ASIA" ~ "Asian",
                                   ethnic_gr_y9 == "BLAC" ~ "Black",
                                   ethnic_gr_y9 == "CHIN" ~ "Any other ethnic group",
                                   ethnic_gr_y9 == "MIXD" ~ "Mixed",
                                   ethnic_gr_y9 == "WHIT" ~ "White",
                                   TRUE ~ "Unknown"),
         ethnic_rename = factor(ethnic_rename, levels = c("White", "Asian",  "Black", "Mixed", "Any other ethnic group", "Unknown")) )

table(pupils$ethnic_rename, pupils$ethnos_mode, exclude=NULL)
table(pupils$ethnic_rename, pupils$ethnic_gr_y9, exclude=NULL)
pupils <- pupils %>% select(-ethnic_gr_y9, -ethnos_mode )

## SEN / CSC flag -------------------
pupils <- pupils %>% 
  mutate(sen_csc_flag = case_when(sen_provision!=0 & csc!=0 ~ "SEN and CSC",
                                  sen_provision == 2 & csc==0 ~ "SEN only",
                                  sen_provision == 1 & csc==0 ~ "SEN only",
                                  sen_provision==0 & csc!=0 ~ "CSC only",
                                  sen_provision==0 & csc==0 ~ "None",
                                  TRUE ~ "Oops")) 

pupils$sen_csc_flag <- factor(pupils$sen_csc_flag, levels = c("None", "SEN only", "CSC only", "SEN and CSC"))
table(pupils$sen_csc_flag )

pupils <- pupils %>% 
  mutate(csc2 = case_when(cla_sensitivity_flag==1 ~ 3,
                          cin_cpp_sens==1 ~ 2,
                          cin_sensitivity==1 | cla_stb_sensitivity_flag==1 ~1,
                          TRUE ~ 0 ),
         sen_csc_sens = case_when(sen_sensitivity!=0 & csc2!=0 ~ "SEN and CSC",
                                  sen_sensitivity == 2 & csc2==0 ~ "SEN only",
                                  sen_sensitivity == 1 & csc2==0 ~ "SEN only",
                                  sen_sensitivity==0 & csc2!=0 ~ "CSC only",
                                  sen_sensitivity==0 & csc2==0 ~ "None",
                                  TRUE ~ "Oops"))
table(pupils$sen_csc_sens)         

## month of birth --------------------
pupils <- pupils %>% 
  mutate( mdob_cat =case_when(
    mdob==9 | mdob==10             ~ "Sep-Oct",
    mdob==11 | mdob==12            ~ "Nov-Dec" ,
    mdob==1 | mdob==2              ~ "Jan-Feb",  
    mdob==3 | mdob==4              ~  "Mar-Apr"  ,
    mdob==6 | mdob==5              ~ "May-Jun",
    mdob==7 | mdob==8               ~ "Jul-Aug") ,
    mdob_cat = factor(mdob_cat, levels=c("Sep-Oct", "Nov-Dec" , "Jan-Feb", "Mar-Apr", "May-Jun", "Jul-Aug")) )
table(pupils$mdob_cat, exclude = NULL)

## Save cohort for rest of analyses
fwrite(pupils, paste0(outputs, "/02_year10_cohort.csv"))
