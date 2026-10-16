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

# formatting ===================

## model variables --------------------
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


## SEN / CSC sensitivity -------------------

pupils <- pupils %>% 
  mutate(csc2 = case_when(cla_sensitivity_flag==1 ~ 3,
                          cin_cpp_sens==1 ~ 2,
                          cin_sensitivity==1 | cla_stb_sensitivity_flag ~1,
                          TRUE ~ 0 ),
         sen_csc_sens = case_when(sen_sensitivity!=0 & csc2!=0 ~ "SEN and CSC",
                                  sen_sensitivity == 2 & csc2==0 ~ "SEN only",
                                  sen_sensitivity == 1 & csc2==0 ~ "SEN only",
                                  sen_sensitivity==0 & csc2!=0 ~ "CSC only",
                                  sen_sensitivity==0 & csc2==0 ~ "None",
                                  TRUE ~ "Oops"))
table(pupils$sen_csc_sens)         

pupils <- pupils %>% 
  mutate(sen_only_sens = ifelse(sen_csc_sens== "SEN only", 1, 0),
         csc_only_sens  = ifelse(sen_csc_sens=="CSC only", 1, 0),
         sen_and_csc_sens  = ifelse(sen_csc_sens=="SEN and CSC", 1, 0)) 

pupils$sen_csc_sens <- factor(pupils$sen_csc_sens, levels = c("None", "SEN only", "CSC only", "SEN and CSC"))
table(pupils$sen_csc_flag, pupils$sen_csc_sens, exclude=NULL)

pupils <- pupils %>% mutate(sens_exclude_hist = 
                              ifelse(sen_csc_flag!="None" & sen_csc_sens=="None", 1, 0))

table(pupils$sen_and_csc_sens, pupils$sens_exclude_hist, exclude=NULL)


# COX PH ================================

## Checks ===========================
# 
# # compare to poisson - equivalent if constant hazard
# c0_girls_tmp <-glm(dth_followup_boo ~ sen_only +
#                         csc_only + sen_csc + 
#                         school_year_end2011 +
#                         school_year_end2012 + school_year_end2013 +
#                         school_year_end2014 +school_year_end2015, 
#                    family=poisson, 
#                    offset = log(fup_length/100 ), data=pupils[sex=="F"] )
# summary(c0_girls_tmp)

## Girls ===============
# to check that my indicator variables work:
c0_girls_tmp <- coxph(Surv(fup_length, dth_followup_boo) ~ sen_only +
                        csc_only + sen_csc + 
                        school_year_end2011 +
                        school_year_end2012 + school_year_end2013 +
                        school_year_end2014 +school_year_end2015,  data=pupils[sex=="F"] )

summary(c0_girls_tmp)
cox_girls <- tidy(c0_girls_tmp, exponentiate = T, conf.int = T)
cox_girls
cox_girls$model <- "unadjusted"

table(pupils$chronic_condition_pre14, exclude=NULL)
# to check that my indicator variables work:
c0_girls_tmp <- coxph(Surv(fup_length, dth_followup_boo) ~ sen_only +
                        csc_only + sen_csc + 
                        chronic_condition_pre14 +
                        school_year_end2011 +
                        school_year_end2012 + school_year_end2013 +
                        school_year_end2014 +school_year_end2015,  data=pupils[sex=="F"] )
summary(c0_girls_tmp)
cox_girls2 <- tidy(c0_girls_tmp, exponentiate = T, conf.int = T)
cox_girls2
cox_girls2$model <- "adjusted"
cox_girls <- rbind(cox_girls, cox_girls2)

# sensitivity
c0_girls_tmp <- coxph(Surv(fup_length, dth_followup_boo) ~ sen_only_sens +
                        csc_only_sens + sen_and_csc_sens + 
                        school_year_end2011 +
                        school_year_end2012 + school_year_end2013 +
                        school_year_end2014 +school_year_end2015,  data=pupils[sex=="F"] )
summary(c0_girls_tmp)
cox_girls2 <- tidy(c0_girls_tmp, exponentiate = T, conf.int = T)
cox_girls2
cox_girls2$model <- "unadjusted_sens"
cox_girls <- rbind(cox_girls, cox_girls2)

# sensitivity - save using shared comparison of those with no history of CSC:
c0_girls_tmp <- coxph(Surv(fup_length, dth_followup_boo) ~ sen_only_sens +
                        csc_only_sens + sen_and_csc_sens + 
                        school_year_end2011 +
                        school_year_end2012 + school_year_end2013 +
                        school_year_end2014 +school_year_end2015,  data=pupils[sex=="F" & sens_exclude_hist==0] )
summary(c0_girls_tmp)
cox_girls2 <- tidy(c0_girls_tmp, exponentiate = T, conf.int = T)
cox_girls2
cox_girls2$model <- "unadjusted_sens2"
cox_girls <- rbind(cox_girls, cox_girls2)


# to check that my indicator variables work:
c0_girls_tmp <- coxph(Surv(fup_length, dth_followup_boo) ~ sen_only_sens +
                        csc_only_sens + sen_and_csc_sens + 
                        chronic_condition_pre14 +
                        school_year_end2011 +
                        school_year_end2012 + school_year_end2013 +
                        school_year_end2014 +school_year_end2015,  data=pupils[sex=="F"] )
summary(c0_girls_tmp)
cox_girls2 <- tidy(c0_girls_tmp, exponentiate = T, conf.int = T)
cox_girls2
cox_girls2$model <- "adjusted_sens"
cox_girls <- rbind(cox_girls, cox_girls2)


# to check that my indicator variables work:
c0_girls_tmp <- coxph(Surv(fup_length, dth_followup_boo) ~ sen_only_sens +
                        csc_only_sens + sen_and_csc_sens + 
                        chronic_condition_pre14 +
                        school_year_end2011 +
                        school_year_end2012 + school_year_end2013 +
                        school_year_end2014 +school_year_end2015,  data=pupils[sex=="F"  & sens_exclude_hist==0] )
summary(c0_girls_tmp)
cox_girls2 <- tidy(c0_girls_tmp, exponentiate = T, conf.int = T)
cox_girls2
cox_girls2$model <- "adjusted_sens2"
cox_girls <- rbind(cox_girls, cox_girls2)

fwrite(cox_girls, paste0(results, "tmp/Cox_girls_all_cause.csv"))


## boys ===============
# to check that my indicator variables work:
c0_boys_tmp <- coxph(Surv(fup_length, dth_followup_boo) ~ sen_only +
                        csc_only + sen_csc + 
                        school_year_end2011 +
                        school_year_end2012 + school_year_end2013 +
                        school_year_end2014 +school_year_end2015,  data=pupils[sex=="M"] )
summary(c0_boys_tmp)
cox_boys <- tidy(c0_boys_tmp, exponentiate = T, conf.int = T)
cox_boys
cox_boys$model <- "unadjusted"

c0_boys_tmp <- coxph(Surv(fup_length, dth_followup_boo) ~ sen_only +
                        csc_only + sen_csc + 
                        chronic_condition_pre14 +
                        school_year_end2011 +
                        school_year_end2012 + school_year_end2013 +
                        school_year_end2014 +school_year_end2015,  data=pupils[sex=="M"] )
summary(c0_boys_tmp)
cox_boys2 <- tidy(c0_boys_tmp, exponentiate = T, conf.int = T)
cox_boys2
cox_boys2$model <- "adjusted"
cox_boys <- rbind(cox_boys, cox_boys2)

# to check that my indicator variables work:
c0_boys_tmp <- coxph(Surv(fup_length, dth_followup_boo) ~ sen_only_sens +
                        csc_only_sens + sen_and_csc_sens + 
                        school_year_end2011 +
                        school_year_end2012 + school_year_end2013 +
                        school_year_end2014 +school_year_end2015,  data=pupils[sex=="M"] )
summary(c0_boys_tmp)
cox_boys2 <- tidy(c0_boys_tmp, exponentiate = T, conf.int = T)
cox_boys2
cox_boys2$model <- "unadjusted_sens"
cox_boys <- rbind(cox_boys, cox_boys2)

c0_boys_tmp <- coxph(Surv(fup_length, dth_followup_boo) ~ sen_only_sens +
                        csc_only_sens + sen_and_csc_sens + 
                        chronic_condition_pre14 +
                        school_year_end2011 +
                        school_year_end2012 + school_year_end2013 +
                        school_year_end2014 +school_year_end2015,  data=pupils[sex=="M"] )
summary(c0_boys_tmp)
cox_boys2 <- tidy(c0_boys_tmp, exponentiate = T, conf.int = T)
cox_boys2
cox_boys2$model <- "adjusted_sens"
cox_boys <- rbind(cox_boys, cox_boys2)
fwrite(cox_boys, paste0(results, "tmp/Cox_boys_all_cause.csv"))

# to check that my indicator variables work:
c0_boys_tmp <- coxph(Surv(fup_length, dth_followup_boo) ~ sen_only_sens +
                       csc_only_sens + sen_and_csc_sens + 
                       school_year_end2011 +
                       school_year_end2012 + school_year_end2013 +
                       school_year_end2014 +school_year_end2015,  data=pupils[sex=="M" & sens_exclude_hist==0] )
summary(c0_boys_tmp)
cox_boys2 <- tidy(c0_boys_tmp, exponentiate = T, conf.int = T)
cox_boys2
cox_boys2$model <- "unadjusted_sens2"
cox_boys <- rbind(cox_boys, cox_boys2)

c0_boys_tmp <- coxph(Surv(fup_length, dth_followup_boo) ~ sen_only_sens +
                       csc_only_sens + sen_and_csc_sens + 
                       chronic_condition_pre14 +
                       school_year_end2011 +
                       school_year_end2012 + school_year_end2013 +
                       school_year_end2014 +school_year_end2015,  data=pupils[sex=="M" & sens_exclude_hist==0] )
summary(c0_boys_tmp)
cox_boys2 <- tidy(c0_boys_tmp, exponentiate = T, conf.int = T)
cox_boys2
cox_boys2$model <- "adjusted_sens2"
cox_boys <- rbind(cox_boys, cox_boys2)
fwrite(cox_boys, paste0(results, "tmp/Cox_boys_all_cause.csv"))




## Figure -------------------------------

cox_girls %>% 
  mutate(exposurenames = case_when(term == "sen_only" | term == "sen_only_sens" ~ "1. SEN only", 
                                   term == "csc_only" | term == "csc_only_sens" ~ "2. CSC only",
                                   term == "sen_and_csc_sens" | term=="sen_csc" ~ "3. CSC & SEN"),
         model_names = case_when(model=="unadjusted" ~ "1. unadjusted",
                                 model=="adjusted" ~ "2. adjusted",
                                 model=="unadjusted_sens" ~ "3. sens: unadjusted",
                                 model=="adjusted_sens" ~ "4. sens: adjusted",
                                 model=="unadjusted_sens2" ~ "5. sens: unadjusted2",
                                 model=="adjusted_sens2" ~ "6. sens: adjusted2")) %>%
  filter(!is.na(exposurenames)) %>%
  ggplot(aes(x = estimate, y = exposurenames )) +
  geom_point(aes(colour = model_names), size = 2, position = position_dodge(-.5)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high, colour = model_names), position = position_dodge(-.5), width = 0.2) + 
  ylab("") + 
  coord_trans(x = "log") + 
  xlab("Hazard ratio (log scale)") + 
  theme_bw() +
  geom_vline(aes(xintercept = 1))
ggsave(paste0(results, "tmp/HR_girls_all_cause.jpeg"))


cox_boys %>% 
  mutate(exposurenames = case_when(term == "sen_only" | term == "sen_only_sens" ~ "1. SEN only", 
                                   term == "csc_only" | term == "csc_only_sens" ~ "2. CSC only",
                                   term == "sen_and_csc_sens" | term=="sen_csc" ~ "3. CSC & SEN"),
         model_names = case_when(model=="unadjusted" ~ "1. unadjusted",
                                 model=="adjusted" ~ "2. adjusted",
                                 model=="unadjusted_sens" ~ "3. sens: unadjusted",
                                 model=="adjusted_sens" ~ "4. sens: adjusted",
                                 model=="unadjusted_sens2" ~ "5. sens: unadjusted2",
                                 model=="adjusted_sens2" ~ "6. sens: adjusted2")) %>%
  filter(!is.na(exposurenames)) %>%
  ggplot(aes(x = estimate, y = exposurenames )) +
  geom_point(aes(colour = model_names), size = 2, position = position_dodge(-.5)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high, colour = model_names), position = position_dodge(-.5), width = 0.2) + 
  ylab("") + 
  coord_trans(x = "log") + 
  xlab("Hazard ratio (log scale)") + 
  theme_bw() +
  geom_vline(aes(xintercept = 1))
ggsave(paste0(results, "tmp/HR_boys_all_cause.jpeg"))


