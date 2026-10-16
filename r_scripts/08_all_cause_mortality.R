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


## load pupil data ==================
pupils <- fread(paste0(outputs, "/02_year10_cohort.csv"))

## formatting ===================

## model variables
pupils <- pupils %>% mutate( # leave first year out as baseline:
  # school_year_end2010 = ifelse(school_year_end==2010, 1, 0),
  school_year_end2011 = ifelse(school_year_end==2011, 1, 0),
  school_year_end2012 = ifelse(school_year_end==2012, 1, 0),
  school_year_end2013 = ifelse(school_year_end==2013, 1, 0),
  school_year_end2014 = ifelse(school_year_end==2014, 1, 0),
  school_year_end2015 = ifelse(school_year_end==2015, 1, 0),
  sen_only = ifelse(sen_csc_flag== "SEN only", 1, 0),
  csc_only  = ifelse(sen_csc_flag=="CSC only", 1, 0),
  sen_csc  = ifelse(sen_csc_flag=="SEN and CSC", 1, 0)) 

## SEN / CSC sensitivity

# pupils <- pupils %>% 
#   mutate(csc2 = case_when(cla_sensitivity_flag==1 ~ 3,
#                           cin_cpp_sens==1 ~ 2,
#                           cin_sensitivity==1 | cla_stb_sensitivity_flag==1 ~1,
#                           TRUE ~ 0 ),
#          sen_csc_sens = case_when(sen_sensitivity!=0 & csc2!=0 ~ "SEN and CSC",
#                                   sen_sensitivity == 2 & csc2==0 ~ "SEN only",
#                                   sen_sensitivity == 1 & csc2==0 ~ "SEN only",
#                                   sen_sensitivity==0 & csc2!=0 ~ "CSC only",
#                                   sen_sensitivity==0 & csc2==0 ~ "None",
#                                   TRUE ~ "Oops"))
# table(pupils$sen_csc_sens)         

pupils <- pupils %>% 
  mutate(sen_only_sens = ifelse(sen_csc_sens== "SEN only", 1, 0),
         csc_only_sens  = ifelse(sen_csc_sens=="CSC only", 1, 0),
         sen_and_csc_sens  = ifelse(sen_csc_sens=="SEN and CSC", 1, 0)) 

pupils$sen_csc_sens <- factor(pupils$sen_csc_sens, levels = c("None", "SEN only", "CSC only", "SEN and CSC"))
table(pupils$sen_csc_flag, pupils$sen_csc_sens, exclude=NULL)


## checks

table(pupils$dth_followup_boo, exclude=NULL)
table(pupils$age_tod, exclude=NULL)

table(pupils$dth_followup_boo, pupils$sen_csc_flag, exclude=NULL)
table(pupils$age_tod, pupils$sen_csc_flag, exclude=NULL)

glimpse(pupils)


# Cummulative incidence ----------------

## girls ================
girls <- survfit( Surv(fup_length, dth_followup_boo) ~ sen_csc_flag, data=pupils[sex=="F"] )

# Save cumulative incidence (i.e failure) at specific time points:
options(digits = 8)
options(scipen = n)
summary(girls, times=seq(0,13, by=0.5))

tmp <- summary(girls)
tmp.df <- as.data.frame(tmp[c("strata",  "time", "n.risk", "n.event", "surv", "std.err",
                              "lower", "upper")])

girls_summary <- summary(girls, times=c(0:13))
girls_summary <- as.data.frame(girls_summary[c("strata",  "time", "n.risk", "n.event", "surv", "std.err",
                              "lower", "upper")])

girls_summary <- girls_summary %>% mutate(cum_inc = (1 - surv),
                            lci = 1 -upper,
                            uci = 1-lower,
                            age = time+14)

glimpse(girls_summary)

fwrite(girls_summary, paste0(results, "tmp/KM_girls_exposure.csv"))



## boys ================
boys <- survfit( Surv(fup_length, dth_followup_boo) ~ sen_csc_flag, data=pupils[sex=="M"] )

boys_summary <- summary(boys, times=c(0:13))
boys_summary <- as.data.frame(boys_summary[c("strata",  "time", "n.risk", "n.event", "surv", "std.err",
                                               "lower", "upper")])

boys_summary <- boys_summary %>% mutate(cum_inc = (1 - surv),
                                          lci = 1 -upper,
                                          uci = 1-lower,
                                          age = time+14)

glimpse(boys_summary)
fwrite(boys_summary, paste0(results, "tmp/KM_boys_exposure.csv"))


# KM plots ===============================

## girls ==============

# Save the plot 
ggplot(girls_summary, aes(x = age, y = cum_inc, color = strata)) + 
  geom_point() + 
  geom_line() + 
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.1) + 
  xlab("Age (years)") + 
  ylab("Cummulative risk of death") +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(14,25)) 
ggsave( paste0(results, "for export/KM_girls.jpeg"))


# complete KM - to check that the "limited" figure below is ok
ggsurvplot(girls, data=pupils[sex=="F"], 
           combine = T,
           fun = "event", # this turns it into event rate rather than failure
           censor = F,
           conf.int = T,
           xlab = "Time from Y10 (years)",
           ylab = "Cummulative risk of death")

## boys =======================

ggplot(boys_summary, aes(x = age, y = cum_inc, color = strata)) + 
  geom_point() + 
  geom_line() + 
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.1) + 
  xlab("Age (years)") + 
  ylab("Cummulative risk of death") +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(14,25)) 
ggsave( paste0(results, "for export/KM_boys.jpeg"))

# for checking
ggsurvplot(boys, data=pupils[sex=="M"], 
           combine = T,
           fun = "event", # this turns it into event rate rather than failure
           censor = F,
           conf.int = T,
           xlab = "Time from Y10 (years)",
           ylab = "Cummulative risk of death")


# cum inc - sensitivity ---------------------------------

## girls ================
girls <- survfit( Surv(fup_length, dth_followup_boo) ~ sen_csc_sens, data=pupils[sex=="F"] )

girls_summary <- summary(girls, times=c(0:13))
girls_summary <- as.data.frame(girls_summary[c("strata",  "time", "n.risk", "n.event", "surv", "std.err",
                                               "lower", "upper")])

girls_summary <- girls_summary %>% mutate(cum_inc = (1 - surv),
                                          lci = 1 -upper,
                                          uci = 1-lower,
                                          age = time+14)

glimpse(girls_summary)
fwrite(girls_summary, paste0(results, "tmp/KM_girls_exposure_sens.csv"))


## boys ================
boys <- survfit( Surv(fup_length, dth_followup_boo) ~ sen_csc_sens, data=pupils[sex=="M"] )

boys_summary <- summary(boys, times=c(0:13))
boys_summary <- as.data.frame(boys_summary[c("strata",  "time", "n.risk", "n.event", "surv", "std.err",
                                             "lower", "upper")])

boys_summary <- boys_summary %>% mutate(cum_inc = (1 - surv),
                                        lci = 1 -upper,
                                        uci = 1-lower,
                                        age = time+14)

glimpse(boys_summary)

fwrite(boys_summary, paste0(results, "tmp/KM_boys_exposure_sens.csv"))



# cum inc - detailed exposure ---------------------------------
glimpse(pupils)

table(pupils$csc, exclude = NULL)
table(pupils$sen_provision, exclude = NULL)

pupils <- pupils %>% mutate(csc = ifelse(csc==3, 2, csc),
  sen_csc_det = case_when(sen_provision == 2 & csc==2 ~ "EHCP & CLA/CPP",
                          sen_provision == 2 & csc==1 ~ "EHCP & CiN",
                          sen_provision == 2 & csc==0 ~ "EHCP only",
                          sen_provision == 1 & csc==2 ~ "SEN & CLA/CPP",
                          sen_provision == 1 & csc==1 ~ "SEN & CiN",
                          sen_provision == 1 & csc==0 ~ "SEN only",
                          sen_provision == 0 & csc==2 ~ "CLA/CPP",
                          sen_provision == 0 & csc==1 ~ "CiN",
                          sen_provision == 0 & csc==0 ~ "none",
                                                    TRUE ~ "Oops") )

## girls ================
girls <- survfit( Surv(fup_length, dth_followup_boo) ~ sen_csc_det, data=pupils[sex=="F"] )

girls_summary <- summary(girls, times=10)
girls_summary <- as.data.frame(girls_summary[c("strata",  "time", "n.risk", "n.event", "surv", "std.err",
                                               "lower", "upper")])

girls_summary <- girls_summary %>% mutate(cum_inc = (1 - surv),
                                          lci = 1 -upper,
                                          uci = 1-lower,
                                          age = time+14)

glimpse(girls_summary)
fwrite(girls_summary, paste0(results, "tmp/KM_girls_exposure_det.csv"))



## boys ================
boys <- survfit( Surv(fup_length, dth_followup_boo) ~ sen_csc_det, data=pupils[sex=="M"] )

boys_summary <- summary(boys, times=10)
boys_summary <- as.data.frame(boys_summary[c("strata",  "time", "n.risk", "n.event", "surv", "std.err",
                                               "lower", "upper")])

boys_summary <- boys_summary %>% mutate(cum_inc = (1 - surv),
                                          lci = 1 -upper,
                                          uci = 1-lower,
                                          age = time+14)

glimpse(boys_summary)
fwrite(boys_summary, paste0(results, "tmp/KM_boys_exposure_det.csv"))

