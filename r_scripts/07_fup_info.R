# FUP

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
results <- " "

## load pupil data ==================
pupils <- fread(paste0(outputs, "/02_year10_cohort.csv"))
glimpse(pupils)

table(pupils$sen_csc_flag, exclude=NULL)

### FUP END: death, 10yr since school start, end of study ============================
pupils <- pupils %>% 
  mutate(fup_end_tmp = as.Date( paste(school_year_end + 10, "09", "15", sep="-")) )

# View(pupils[fup_end_tmp<dod & !is.na(dod)])
pupils <- pupils %>% 
  mutate(fup_end = fup_end_tmp,
         fup_end = ifelse(fup_end > dod & !is.na(dod), dod, fup_end ),
         tmp =  as.Date("2020-12-31"),  
         fup_end = ifelse(fup_end> tmp , tmp, fup_end ),
         fup_end = as.Date(fup_end, origin="1970-01-01")) %>% select(-tmp, -fup_end_tmp)

glimpse(pupils)

# derive FUP length: 
pupils <- pupils %>% mutate(fup_length_days := as.integer(difftime(fup_end, start_date, units = "days")),
                            fup_length = time_length(interval(start_date, fup_end), "years" ),
                            fup_length_yr = floor(fup_length) ) 

range(pupils$fup_length_days)
range(pupils$fup_length)

# # make sure those with event on same day count as half a day 
# pupils <- pupils %>% mutate(fup_length_days = ifelse(fup_length_days==0, 0.5, fup_length_days),
#                             fup_length = ifelse(fup_length==0, 0.5/365, fup_length) )

fwrite(pupils, paste0(outputs, "/02_year10_cohort.csv"))


# median FUP -----------------------------------
pupils <- fread(paste0(outputs, "/02_year10_cohort.csv"))

fup_summary <- pupils %>% # filter(sex=="F") %>%
  summarise(mean=mean(fup_length), 
            st_dev = sd(fup_length),
            median=median(fup_length), 
            q1 = quantile(fup_length, p=0.25),
            q3 = quantile(fup_length, p=0.75)
  )

extra_nrs <- pupils %>% # filter(sex=="F") %>%
  mutate(nr_median = ifelse(fup_length == fup_summary$median, 1, 0),
         nr_q1 = ifelse(fup_length == fup_summary$q1, 1, 0),
         nr_q3 = ifelse(fup_length == fup_summary$q3, 1, 0),
         tmp=1) %>%
  summarise(nr_median = sum(nr_median),
            nr_q1 = sum(nr_q1),
            nr_q3 = sum(nr_q3),
            nr_all = sum(tmp))

fup_summary <- cbind(fup_summary, extra_nrs)
fup_summary$sex <- "TOTAL"

tmp <- pupils %>%  filter(sex=="F") %>%
  summarise(mean=mean(fup_length), 
            st_dev = sd(fup_length),
            median=median(fup_length), 
            q1 = quantile(fup_length, p=0.25),
            q3 = quantile(fup_length, p=0.75)
  )

extra_nrs <- pupils %>%  filter(sex=="F") %>%
  mutate(nr_median = ifelse(fup_length == tmp$median, 1, 0),
         nr_q1 = ifelse(fup_length == tmp$q1, 1, 0),
         nr_q3 = ifelse(fup_length == tmp$q3, 1, 0),
         tmp=1) %>%
  summarise(nr_median = sum(nr_median),
            nr_q1 = sum(nr_q1),
            nr_q3 = sum(nr_q3),
            nr_all = sum(tmp))
tmp <- cbind(tmp, extra_nrs)
tmp$sex <- "girls"
fup_summary <- rbind(fup_summary, tmp)

tmp <- pupils %>%  filter(sex=="M") %>%
  summarise(mean=mean(fup_length), 
            st_dev = sd(fup_length),
            median=median(fup_length), 
            q1 = quantile(fup_length, p=0.25),
            q3 = quantile(fup_length, p=0.75)
  )

extra_nrs <- pupils %>%  filter(sex=="M") %>%
  mutate(nr_median = ifelse(fup_length == tmp$median, 1, 0),
         nr_q1 = ifelse(fup_length == tmp$q1, 1, 0),
         nr_q3 = ifelse(fup_length == tmp$q3, 1, 0),
         tmp=1) %>%
  summarise(nr_median = sum(nr_median),
            nr_q1 = sum(nr_q1),
            nr_q3 = sum(nr_q3),
            nr_all = sum(tmp))

tmp <- cbind(tmp, extra_nrs)
tmp$sex <- "boys"
fup_summary <- rbind(fup_summary, tmp)

fwrite(fup_summary, paste0(results, "tmp/median_fup.csv"))

rm(list=ls())
gc()
