# cleaning of cause of death data
# 

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

## load pupil data ==================
pupils <- fread(paste0(outputs, "/02_year10_cohort.csv"))
glimpse(pupils)

pupils <- pupils %>% select(-dod, -age_tod, -hosp_death, 
                            -dth_followup_boo, -no_cause_death, -suicide_flag,
                            -homicide_flag, -drugalc_flag, -accident_flag, -death_adv, 
                            -death_hier)

#check
dups <- duplicated(pupils[, "encrypted_hesid"])
table(dups)

dups <- duplicated(pupils[, "PupilMatchingRefAnonymous"])
table(dups)

# CLEANING deaths ========================================

dths <- fread(paste0(master_dir, "HES_Death_all_data_wide_v2.csv"))
nrow(dths)

# subset in our data
dths <- dths %>%
  subset(encrypted_hesid %in% pupils$encrypted_hesid)
nrow(dths)

sample_flow <- c(nrow(dths), "initial number of deaths")

# merge date of birth and start of secondary school date to calcuate age at death
dths <- merge(dths, pupils[, c("encrypted_hesid", "dob", "start_date")], 
                    all.x=T, all.y=F, by="encrypted_hesid")

dths <- dths %>% 
  mutate(age_death = floor(time_length(interval(dob, dod), "years" ) ) )
table(dths$age_death, exclude = NULL)
# there are a few wrong links, but mostly sensible

range(dths[!is.na(dor)]$dor)
range(dths[!is.na(dod)]$dod)
# lastest date: "2021-07-15"

# check ages that dont make sense (younger than inception ie <11yo)
table(dths$age_death, dths$match_rank, exclude = NULL)
# majority at age 0 have match rank=8
# but there are some clearly missed matches

# drop deaths before start of FUP (start of Y10)
dths <- dths %>% filter(start_date<=dod) 
table(dths$age_death, exclude = NULL)
# ok, this deals with all of these issues

tmp <- c(nrow(dths), "drop deaths before FUP start")
sample_flow <- rbind(sample_flow, tmp)

# drop those with poor match rank
table(dths$match_rank, exclude = NULL)
dths <- dths %>% filter(match_rank!=8)

tmp <- c(nrow(dths), "drop deaths with poor match rank")
sample_flow <- rbind(sample_flow, tmp)

# drop if subsequent activity
nrow(dths)
dths <- dths %>%
  mutate(subs_act1 = ifelse(!is.na(nr_subseq_adm) , 1, 0),
         subs_act2 = ifelse(!is.na(hes_dod_latest_adm) & hes_dod_latest_adm>=3, 1, 0) )

table(dths$dod_year, dths$hosp_death, exclude = NULL)
table(dths$dod_year, dths$subs_act1, exclude = NULL)
table(dths$dod_year, dths$subs_act2, exclude = NULL)
table(dths$subs_act2, dths$subs_act1, exclude = NULL)
table(dths$dod_year, dths$subsequent_activity, exclude = NULL)

summary(dths$time_to_latest_adm)
summary(dths$hes_dod_latest_adm)
summary(dths$nr_subseq_adm)

dths <- dths %>% filter(subs_act1!=1 & subs_act2!=1)

tmp <- c(nrow(dths), "drop if subsequent activity")
sample_flow <- rbind(sample_flow, tmp)

# check year of death and just drop any that are before FUP start and in last year of data
table(dths$dod_year)

tmp <- dths %>% group_by(cause_of_death) %>% count()
View(tmp)

dups <- duplicated(dths[, c("encrypted_hesid")])
table(dups)

dths <- dths %>%
  # select(encrypted_hesid, dod, age_death, hes_dod , hosp_death, dob
  #        contains("cause_of_death_non"), cause_of_death ) %>%
  mutate(dod_compl = ifelse(!is.na(hes_dod), hes_dod, dod),
         dod_compl = as.Date(dod_compl, origin="1970-01-01"))
dths <- dths %>% mutate( age_death = floor(time_length(interval(dob, dod_compl), "years" ) ) )
table(dths$age_death, exclude=NULL)

# deaths more than 10 years since FUP start: 
dths <- dths %>% 
  mutate(fup_tmp = as.integer( floor(time_length(interval(start_date, dod), "years" ) ) ) )
table(dths$fup_tmp, exclude = NULL)

fwrite(dths, paste0(outputs, "ONS_dths_with_causes.csv"))


# mergin info on deaths: ========================================

dths <- fread(paste0(outputs, "ONS_dths_with_causes.csv"))

dths <- dths %>%
  mutate(dod = ifelse(is.na(dod), dod_compl, dod),
         dod = as.Date(dod, origin="1970-01-01"))
  # select(encrypted_hesid, dod, age_death, hes_dod , hosp_death, fup_tmp,
  #        cause_of_death,
  #        contains("cause_of_death_non"))
  

dths <- dths %>% filter(dod<as.Date("2020-12-30")) %>%
  mutate(dod_after_covid = ifelse(dod>=as.Date("2020-03-01"), 1, 0))
head(dths)
table(dths$age_death, exclude=NULL)
table(dths$fup_tmp, exclude=NULL)
table(dths$age_death, dths$dod_after_covid, exclude=NULL)

## save flow chart ====
tmp <- c(nrow(dths), "drop deaths after 2020-12-30")
sample_flow <- rbind(sample_flow, tmp)

# i think this should be longer, but paper said this:
dths <- dths %>% filter(fup_tmp<10)
head(dths)
table(dths$age_death, exclude=NULL)
table(dths$fup_tmp, exclude=NULL)

## save flow chart ====
tmp <- c(nrow(dths), "drop deaths after more than 10 years")
sample_flow <- rbind(sample_flow, tmp)

fwrite(sample_flow,  paste0(results, "tmp/deaths_flow_chart.csv"))

dths <- dths %>%
  select(encrypted_hesid, dod, age_death, hes_dod , hosp_death, fup_tmp,dod_after_covid,
       cause_of_death,
       contains("cause_of_death_non"))

## merge info ====
dths2 <- dths %>%
  select(encrypted_hesid, dod, age_death, hosp_death, dod_after_covid)

head(pupils)
pupils <- merge(pupils, dths2, all.x=T, all.=F, by="encrypted_hesid")
pupils <- pupils %>% mutate(died=ifelse(!is.na(dod), 1, 0))
rm(dths2)

table(pupils$died, exclude = NULL)
table(pupils$died, pupils$hosp_death, exclude = NULL)

rm(dths2, tmp)
gc()


# Prepare causes of death -----------------------------------------------------

## recode missing death data ---------------
head(dths)
dths <- dths %>%
  mutate(across(cause_of_death:cause_of_death_non_neonatal_15, ~ifelse(.x=="", NA, .x)))

## create indicator for those with no cause of death =====
dths <- dths %>%
  mutate(no_cause_death = ifelse(is.na(cause_of_death), 1, 0) ) 
table(dths$no_cause_death, exclude = NULL)
table(dths$no_cause_death,dths$dod_after_covid, exclude = NULL)


## Injury vs non-injury ========================================================

### load Herbert adversity-related and accidental death codes ------------
# herbert_adv_acc <- fread(paste0(outputs, "lookups/icd10_adv_acc_herbert.csv"))
# update:
herbert_adv_acc <- fread(paste0(outputs, "lookups/icd10_ari_herbert_3char.csv"))

glimpse(herbert_adv_acc)
table(herbert_adv_acc$group, exclude=NULL)

## reshape the data - Annie's paper shows no hierarchy for deaths (just for admissons by type)
deaths_tmp <- dths %>%
  select(encrypted_hesid, starts_with("cause_of_death")) %>%
  pivot_longer(
    cols=c( starts_with("cause_of_death")),
    names_to = "variable", 
    values_to = "diag"
    ) %>%
  select(-variable) %>%
  filter(diag!="")

setDT(deaths_tmp)

### suicides: ------------------
code_list <- herbert_adv_acc[group=="suicide"]

# 2 character codes:
subgroup_codes2 <- code_list[nchar(diag)==2,] 
diag_cc2 <-   deaths_tmp[substr(diag, 1, 2) %in% subgroup_codes2$diag]

# 3 character codes:
subgroup_codes3 <- code_list[nchar(diag)==3,] 
diag_cc3 <-   deaths_tmp[substr(diag, 1, 3) %in% subgroup_codes3$diag]

# 4 letter codes:
subgroup_codes4 <- code_list[nchar(diag)==4,] 
diag_cc4 <-   deaths_tmp[substr(diag, 1, 4) %in% subgroup_codes4$diag]

# combine
diag_cc <- rbind(diag_cc4, diag_cc3, diag_cc2)
diag_cc <- diag_cc %>% distinct()

# generate flag: 
dths <- dths %>% mutate(suicide_flag = ifelse(encrypted_hesid %in% diag_cc$encrypted_hesid, 1, 0))


### homicide: ------------------
table(herbert_adv_acc$group)
code_list <- herbert_adv_acc[group=="homicide"]

# 2 character codes:
subgroup_codes2 <- code_list[nchar(diag)==2,] 
diag_cc2 <-   deaths_tmp[substr(diag, 1, 2) %in% subgroup_codes2$diag]

# 3 character codes:
subgroup_codes3 <- code_list[nchar(diag)==3,] 
diag_cc3 <-   deaths_tmp[substr(diag, 1, 3) %in% subgroup_codes3$diag]

# 4 letter codes:
subgroup_codes4 <- code_list[nchar(diag)==4,] 
diag_cc4 <-   deaths_tmp[substr(diag, 1, 4) %in% subgroup_codes4$diag]

# combine
diag_cc <- rbind(diag_cc4, diag_cc3, diag_cc2)
diag_cc <- diag_cc %>% distinct()

# generate flag: 
dths <- dths %>% mutate(homicide_flag = ifelse(encrypted_hesid %in% diag_cc$encrypted_hesid, 1, 0))


### drug and alcohol: ------------------
table(herbert_adv_acc$group)
code_list <- herbert_adv_acc[group=="drugalcohol"]

# 2 character codes:
subgroup_codes2 <- code_list[nchar(diag)==2,] 
diag_cc2 <-   deaths_tmp[substr(diag, 1, 2) %in% subgroup_codes2$diag]

# 3 character codes:
subgroup_codes3 <- code_list[nchar(diag)==3,] 
diag_cc3 <-   deaths_tmp[substr(diag, 1, 3) %in% subgroup_codes3$diag]

# 4 letter codes:
subgroup_codes4 <- code_list[nchar(diag)==4,] 
diag_cc4 <-   deaths_tmp[substr(diag, 1, 4) %in% subgroup_codes4$diag]

# combine
diag_cc <- rbind(diag_cc4, diag_cc3, diag_cc2)
diag_cc <- diag_cc %>% distinct()

# generate flag: 
dths <- dths %>% mutate(drugalc_flag = ifelse(encrypted_hesid %in% diag_cc$encrypted_hesid, 1, 0))


### accidental : ------------------
table(herbert_adv_acc$group)
code_list <- herbert_adv_acc[group=="accidental"]

# 2 character codes:
subgroup_codes2 <- code_list[nchar(diag)==2,] 
diag_cc2 <-   deaths_tmp[substr(diag, 1, 2) %in% subgroup_codes2$diag]

# 3 character codes:
subgroup_codes3 <- code_list[nchar(diag)==3,] 
diag_cc3 <-   deaths_tmp[substr(diag, 1, 3) %in% subgroup_codes3$diag]

# 4 letter codes:
subgroup_codes4 <- code_list[nchar(diag)==4,] 
diag_cc4 <-   deaths_tmp[substr(diag, 1, 4) %in% subgroup_codes4$diag]

# combine
diag_cc <- rbind(diag_cc4, diag_cc3, diag_cc2)
diag_cc <- diag_cc %>% distinct()

# generate flag: 
dths <- dths %>% mutate(accident_flag = ifelse(encrypted_hesid %in% diag_cc$encrypted_hesid, 1, 0))



### create adversity-related vs accidental injury death indicator -------------
dths <- dths %>% 
  rowwise() %>% 
  mutate(death_adv = case_when(is.na(cause_of_death) ~ "Missing underlying cause", 
                               suicide_flag == 1 | 
                                 homicide_flag == 1 | 
                                 drugalc_flag == 1 ~ "Adversity-related injury", 
                               accident_flag == 1 ~ "Accidental injury", 
                               TRUE ~ "Non injury"))
dths %>% group_by(death_adv) %>% count()

table(dths$accident_flag, exclude = NULL)
table(dths$suicide_flag, exclude = NULL)
table(dths$homicide_flag, exclude = NULL)
table(dths$drugalc_flag, exclude = NULL)

dths <- dths %>% 
  rowwise() %>% 
  mutate(death_hier = case_when(suicide_flag == 1 ~ "Suicide",
                                homicide_flag == 1 ~ "Homicide",
                                drugalc_flag == 1 ~ "Drug_alcohol",
                                accident_flag ==1 ~ "Accidental",
                                is.na(cause_of_death) ~ "Missing underlying cause",
                                TRUE ~ "Non Adveristy"))

dths <- dths %>%
  select(no_cause_death, suicide_flag, homicide_flag, drugalc_flag, accident_flag, death_hier, death_adv, encrypted_hesid)

pupils <- merge(pupils, dths, all.x=T, all.=F, by="encrypted_hesid")


### tidy death vars in cohort ------------------------
pupils <- pupils %>% 
  rename(age_tod = age_death,
         dth_followup_boo = died )

head(pupils)
fwrite(pupils, paste0(outputs, "/02_year10_cohort.csv"))

rm(code_list, combined_table)
rm(deaths_tmp)
rm(diag_cc, diag_cc2, diag_cc3, diag_cc4)
rm(herbert_adv_acc)
rm(sample_flow)
rm(subgroup_codes2, subgroup_codes3, subgroup_codes4)
rm(dths)
