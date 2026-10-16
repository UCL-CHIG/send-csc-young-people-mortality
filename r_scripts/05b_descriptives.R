## 05b_descriptives.R

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
pupils <- fread(paste0(outputs, "/02_year10_cohort.csv"))
glimpse(pupils)


# extra data cleaning --------------------
glimpse(pupils)
## formatting ---
pupils <- pupils %>%
  mutate( idaci_rename = factor(idaci_rename , levels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Missing") ),
          ethnic_rename = factor(ethnic_rename, levels = c("White", "Asian",  "Black", "Mixed", "Any other ethnic group", "Unknown")),
          sen_csc_flag = factor(sen_csc_flag, levels = c("None", "SEN only", "CSC only", "SEN and CSC")),
          mdob_cat = factor(mdob_cat, levels=c("Sep-Oct", "Nov-Dec" , "Jan-Feb", "Mar-Apr", "May-Jun", "Jul-Aug")) )

# Table 1 - descriptive ---------------------
table(pupils$sex)

## Girls ----------------------------
pupils_tmp <- pupils %>% filter(sex=="F")
tmp1 <- table(pupils_tmp$age_y10, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp2 <- table(pupils_tmp$mdob_cat, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp3 <- table(pupils_tmp$school_year_end, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp4 <- table(pupils_tmp$fsm_rename, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp5 <- table(pupils_tmp$ethnic_rename, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp6 <- table(pupils_tmp$idaci_rename , pupils_tmp$sen_csc_flag, exclude=NULL)
tmp7 <- table(pupils_tmp$chronic_condition_pre14, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp8 <- table(pupils_tmp$mh_behav, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp9 <- table(pupils_tmp$cardiovasc, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp10 <- table(pupils_tmp$neuro, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp11 <- table(pupils_tmp$metab_gastr, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp12 <- table(pupils_tmp$resp, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp13 <- table(pupils_tmp$cancer, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp14 <- table(pupils_tmp$cc_other, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp15 <- table(pupils_tmp$csc, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp16 <- table(pupils_tmp$sen_provision, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp17 <- table(pupils_tmp$asd, pupils_tmp$sen_csc_flag, exclude=NULL)

# this just combines the above tables
combined_table <- do.call(rbind, list(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10,
                                      tmp11, tmp12, tmp13, tmp14, tmp15, tmp16, tmp17))
View(combined_table) # see what data tou're saving

# this adds some labels (not great, but better than nothing):
combined_table <- cbind(rownames(combined_table),combined_table)
View(combined_table)

# save as csv to avoid having to copy and paste:
fwrite(combined_table,  paste0(results, "tmp/girls_chars.csv"))


## Boys -------------------
pupils_tmp <- pupils %>% filter(sex=="M")
tmp1 <- table(pupils_tmp$age_y10, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp2 <- table(pupils_tmp$mdob_cat, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp3 <- table(pupils_tmp$school_year_end, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp4 <- table(pupils_tmp$fsm_rename, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp5 <- table(pupils_tmp$ethnic_rename, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp6 <- table(pupils_tmp$idaci_rename , pupils_tmp$sen_csc_flag, exclude=NULL)
tmp7 <- table(pupils_tmp$chronic_condition_pre14, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp8 <- table(pupils_tmp$mh_behav, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp9 <- table(pupils_tmp$cardiovasc, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp10 <- table(pupils_tmp$neuro, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp11 <- table(pupils_tmp$metab_gastr, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp12 <- table(pupils_tmp$resp, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp13 <- table(pupils_tmp$cancer, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp14 <- table(pupils_tmp$cc_other, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp15 <- table(pupils_tmp$csc, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp16 <- table(pupils_tmp$sen_provision, pupils_tmp$sen_csc_flag, exclude=NULL)
tmp17 <- table(pupils_tmp$asd, pupils_tmp$sen_csc_flag, exclude=NULL)

# this just combines the above tables
combined_table <- do.call(rbind, list(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10,
                                      tmp11, tmp12, tmp13, tmp14, tmp15, tmp16, tmp17))
View(combined_table) # see what data tou're saving

# this adds some labels (not great, but better than nothing):
combined_table <- cbind(rownames(combined_table),combined_table)
View(combined_table)

# save as csv to avoid having to copy and paste:
fwrite(combined_table,  paste0(results, "tmp/boys_chars.csv"))


rm(pupils_tmp)
rm(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10,
   tmp11, tmp12, tmp13, tmp14, tmp15, tmp16, tmp17)
gc()


# 2) SEN / CSC provision ----------------------------------
glimpse(pupils)

tmp1 <- table(pupils[sen_provision==0]$csc, pupils[sen_provision==0]$sex, exclude=NULL)
tmp2 <- table(pupils[sen_provision==1]$csc, pupils[sen_provision==1]$sex, exclude=NULL)
tmp3 <- table(pupils[sen_provision==2]$csc, pupils[sen_provision==2]$sex, exclude=NULL)
combined_table <- do.call(rbind, list(tmp1, tmp2, tmp3))

# this adds some labels (not great, but better than nothing):
combined_table <- cbind(rownames(combined_table),combined_table)

# save as csv to avoid having to copy and paste:
fwrite(combined_table,  paste0(results, "tmp/detailed exposures.csv"))

tmp2 <- fread(paste0(results, "tmp/detailed exposures.csv"))

