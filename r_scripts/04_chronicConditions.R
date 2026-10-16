# Flagging history of chronic conditions
#  
# revised Ania Z 

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

# load the data with all diagnoses -----------------------------
diags <- fread( paste0(master_dir, "HES_APC_DIAG_combined.csv"))
head(diags)

# make the data smaller if possible: 
diags <- diags %>% filter(encrypted_hesid %in% pupils$encrypted_hesid) 

# also, they are not going to be any older than 15
diags <- diags %>% filter(startage>7000 | startage<17) 
nrow(diags)

diags <- merge(diags, pupils[, c("encrypted_hesid", "start_date")],
               all.x=F, all.y=F, by="encrypted_hesid")

diags <- diags %>% filter(epistart<start_date)
table(diags$startage)
gc()

# Hardelid - chronic conditions code list
hardelid <- read.csv(paste0(outputs, "lookups/hardelid.csv"))
head(hardelid)

hardelid <- hardelid %>% select(code, type, category, flag) %>%
  rename(diag=code, hardelid_type=type, hardelid_cat=category) %>%
  mutate(hardelid_type=as.factor(hardelid_type))


# function to derive indicators based on diagnostic  (from ania)
indicate_icd  <- function(diag_data, code_list, diag) {
  # diag_data --> operation or diagnostic dataset
  # code list --> list of ICD-10 codes
  # diag --> field to specify if we want to use diagnoses or operations
  
  # clean the code list
  code_list <- data.table(code_list)
  code_list$diag <- gsub("\\.","", code_list$diag )
  code_list[, diag := substr(diag, 1, 4)]
  
  # 3 character codes:
  subgroup_codes3 <- code_list[nchar(diag)==3,] 
  diag_cc3 <-   diag_data[substr(diag, 1, 3) %in% subgroup_codes3$diag]
  
  # 4 letter codes:
  subgroup_codes4 <- code_list[nchar(diag)==4,] 
  diag_cc4 <-   diag_data[substr(diag, 1, 4) %in% subgroup_codes4$diag]
  
  # combine
  diag_cc <- rbind(diag_cc4, diag_cc3)
  return(diag_cc)
}


# indicating all mentions of a code:
diag_cc <- indicate_icd(diags, hardelid)

rm(diags)
gc()

table(diag_cc$startage)

#only keep those who link to my cohort
diag_cc <- diag_cc %>% 
  filter(encrypted_hesid %in% pupils$encrypted_hesid) %>% 
  rename(age = startage)
nrow(diag_cc)

diag_cc <- diag_cc %>% mutate(age=ifelse(age>7000, 0, age))

tmp <- hardelid %>% filter(hardelid_type=="Cancer/Blood")
cancer <- indicate_icd(diag_cc, tmp) %>% 
  select(encrypted_hesid, age) %>%
  distinct() %>%
  group_by(encrypted_hesid) %>%
  mutate(age = min(age) ) %>% 
  rename(cancer = age) %>%
  ungroup() %>%
  distinct()

tmp <- hardelid %>% filter(hardelid_type=="Cardiovascular")
cardiovasc <- indicate_icd(diag_cc, tmp) %>% 
  select(encrypted_hesid, age) %>%
  distinct() %>%
  group_by(encrypted_hesid) %>%
  mutate(age = min(age) ) %>% 
  rename(cardiovasc = age) %>%
  ungroup() %>%
  distinct()

tmp <- hardelid %>% filter(hardelid_type=="Neurological")
neuro <- indicate_icd(diag_cc, tmp) %>% 
  select(encrypted_hesid, age) %>%
  distinct() %>%
  group_by(encrypted_hesid) %>%
  mutate(age = min(age) ) %>% 
  rename(neuro = age) %>%
  ungroup() %>%
  distinct()

tmp <- hardelid %>% filter(hardelid_type=="Respiratory")
respiratory <- indicate_icd(diag_cc, tmp) %>% 
  select(encrypted_hesid, age) %>%
  distinct() %>%
  group_by(encrypted_hesid) %>%
  mutate(age = min(age) ) %>% 
  rename(resp = age) %>%
  ungroup() %>%
  distinct()

tmp <- hardelid %>% filter(hardelid_type=="Musculoskeletal/skin")
musc_skel <- indicate_icd(diag_cc, tmp) %>% 
  select(encrypted_hesid, age) %>%
  distinct() %>%
  group_by(encrypted_hesid) %>%
  mutate(age = min(age) ) %>% 
  rename(musc_skel = age) %>%  
  ungroup() %>%
  distinct()

tmp <- hardelid %>% filter(hardelid_type=="Metabolic/endocrine/digestive/renal/genitourinary")
metab_gastr <- indicate_icd(diag_cc, tmp) %>% 
  select(encrypted_hesid, age) %>%
  distinct() %>%
  group_by(encrypted_hesid) %>%
  mutate(age = min(age) ) %>% 
  rename(metab_gastr = age) %>%  
  ungroup() %>%
  distinct()

tmp <- hardelid %>% filter(hardelid_type=="Mental health/behavioural")
mh_behav <- indicate_icd(diag_cc, tmp) %>% 
  select(encrypted_hesid, age) %>%
  distinct() %>%
  group_by(encrypted_hesid) %>%
  mutate(age = min(age) ) %>% 
  rename(mh_behav = age) %>%
  ungroup() %>%
  distinct()

tmp <- hardelid %>% filter(hardelid_type=="Chronic infections" | hardelid_type=="Codes indicating non-specific chronic condition")
other <- indicate_icd(diag_cc, tmp) %>% 
  select(encrypted_hesid, age) %>%
  distinct() %>%
  group_by(encrypted_hesid) %>%
  mutate(age = min(age) ) %>% 
  rename(cc_other = age) %>%  
  ungroup() %>%
  distinct()



## link conditions onto  information ===========================================================
pupils <- merge(pupils, mh_behav, all.x=T, all.y=F, by="encrypted_hesid")
table(pupils$mh_behav, exclude = NULL)
rm(mh_behav)

pupils <- merge(pupils, cardiovasc, all.x=T, all.y=F, by="encrypted_hesid")
rm(cardiovasc)

pupils <- merge(pupils, neuro, all.x=T, all.y=F, by="encrypted_hesid")
rm(neuro)

pupils <- merge(pupils, other, all.x=T, all.y=F, by="encrypted_hesid")
rm(other)

pupils <- merge(pupils, respiratory, all.x=T, all.y=F, by="encrypted_hesid")
rm(respiratory)

pupils <- merge(pupils, cancer, all.x=T, all.y=F, by="encrypted_hesid")
rm(cancer)

pupils <- merge(pupils, metab_gastr, all.x=T, all.y=F, by="encrypted_hesid")
rm(metab_gastr)

pupils <- merge(pupils, musc_skel, all.x=T, all.y=F, by="encrypted_hesid")
rm(musc_skel)

gc()

fwrite(pupils, paste0(outputs, "/01de_dup_pupils.csv"))
