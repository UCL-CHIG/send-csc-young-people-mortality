#01_create_cohort.R
#
# 
#08/23
#Adapted from Ania Z

# Ania edited 09/05/25

# Housekeeping =============================================

rm(list=ls())
gc()

library(tidyverse) 
library(RODBC)
library(lubridate)
library(data.table)

#path location
setwd(" ")

path <- " "
data_dir <- " "
master_dir <- " "
outputs <- " "

source(paste0(path, "_functions.R"))


# Get data from school censuses ==============================================

## SQL connect -------
# Connecting to the SQL database which stores the NPD dataset
dbhandle <- odbcDriverConnect( )

# The following lines of code isolate the Spring, Autumn and Summer NPD censuses
tables <- subset(sqlTables(dbhandle), TABLE_SCHEM == "dbo")
tables_census <- subset(tables, grepl("_Census_", TABLE_NAME) | grepl("PLASC", TABLE_NAME))
tables_census <- subset(tables_census, grepl("Autumn", TABLE_NAME) | grepl("Spring", TABLE_NAME) | grepl("Summer", TABLE_NAME) | grepl("PLASC", TABLE_NAME))
tables_census <- subset(tables_census, !grepl("Disability", TABLE_NAME))

tables_census <- subset(tables_census, # grepl("2009", TABLE_NAME) | # don't need this year:
                          grepl("2010", TABLE_NAME)
                        | grepl("2011", TABLE_NAME) | grepl("2012", TABLE_NAME)
                        | grepl("2013", TABLE_NAME) | grepl("2014", TABLE_NAME)
                        | grepl("2015", TABLE_NAME))

rm(tables)
tables_census <- tables_census %>% select(TABLE_NAME)
tables_census


## Spring Census ==========================================================

# pick only spring censuses - they have more information that the other ones
tables_census_tmp <- subset(tables_census, grepl("Spr",  TABLE_NAME)) 
tables_census_tmp


for(table in unique(tables_census_tmp$TABLE_NAME) ) {
  
  # the loop needs to start with checking the names and getting the TERMYY ending:  
  temp <- sqlQuery(dbhandle, paste0("SELECT TOP 10 * FROM ", table) )
  
  # get the TERMYY eg AUT10
  term_year <- colnames(temp)
  term_year <- term_year[grepl("PupilMatchingRefAnonymous_", term_year)]
  term_year <- gsub("PupilMatchingRefAnonymous_", "", term_year)
  term_year
  
  # year 9 or aged 13 and not following national curriculum
  temp <- sqlQuery(dbhandle, paste0("SELECT * FROM ", table, 
                                    " WHERE [NCyearActual_", term_year, "]='X' OR [AgeAtStartOfAcademicYear_", term_year,
                                    "]=13 OR [NCyearActual_", term_year, "]='9' "))
  
  # Fix all names
  colnames(temp) <- gsub(paste0("_",term_year), "", colnames(temp))
  
  head(temp)

  temp <- temp %>% select(PupilMatchingRefAnonymous, AcademicYear, FSMeligible, Gender, AgeAtStartOfAcademicYear, 
                          YearOfBirth, MonthOfBirth, NCyearActual, EthnicGroupMajor, LanguageGroupMajor,
                          starts_with("overall_imd"), starts_with("idaci_20"), CensusTerm, URN  )  
  
  temp <- temp %>% 
    filter(NCyearActual=="9" | (AgeAtStartOfAcademicYear==13 & NCyearActual=="X")) %>%
    mutate(cen_yr = term_year)
  
  fwrite(temp, paste0(data_dir, term_year,  "_analytical.csv"))
  
  rm(temp)
  
}


## Autumn and Summer censuses: --------------------------------------------------------

# select Autumn censuses:
tables_census <- subset(tables_census, !grepl("Spr", TABLE_NAME))
tables_census

for(table in unique(tables_census$TABLE_NAME) ) {
  
  # the loop needs to start with checking the names and getting the TERMYY ending:  
  temp <- sqlQuery(dbhandle, paste0("SELECT TOP 10 * FROM ", table) )
  
  # get the TERMYY eg AUT10
  term_year <- colnames(temp)
  term_year <- term_year[grepl("PupilMatchingRefAnonymous_", term_year)]
  term_year <- gsub("PupilMatchingRefAnonymous_", "", term_year)
  term_year
  
  # year 9 or aged 13 and not following national curriculum
  temp <- sqlQuery(dbhandle, paste0("SELECT * FROM ", table, 
                                    " WHERE [NCyearActual_", term_year, "]='X' OR [AgeAtStartOfAcademicYear_", term_year,
                                    "]=13 OR [NCyearActual_", term_year, "]='9' "))
  
  # Fix all names
  colnames(temp) <- gsub(paste0("_",term_year), "", colnames(temp))
  
  head(temp)

  temp <- temp %>% select(PupilMatchingRefAnonymous, AcademicYear, CensusTerm, LA, 
                          URN, Gender, AgeAtStartOfAcademicYear,
                          YearOfBirth, MonthOfBirth, FSMeligible,
                          NCyearActual,
                          starts_with("overall_imd"), starts_with("idaci_20") )
  

  temp <- temp %>% 
    filter(NCyearActual=="9" | (AgeAtStartOfAcademicYear==13 & NCyearActual=="X")) %>%
    mutate(cen_yr = term_year)
  
  fwrite(temp, paste0(data_dir, term_year,  "_analytical.csv"))
  
  rm(temp)
  
}

## AP census: --------------------------------------------------------

# The following lines of code isolate the Spring, Autumn and Summer NPD censuses
tables <- subset(sqlTables(dbhandle), TABLE_SCHEM == "dbo")
tables_census <- subset(tables, grepl("_Census_", TABLE_NAME) )
tables_census <- subset(tables_census, grepl("PRU", TABLE_NAME) | grepl("AP", TABLE_NAME))

rm(tables)
tables_census <- tables_census %>% select(TABLE_NAME)
tables_census

#adding in sample flow
AP <- sqlQuery(dbhandle, paste0("SELECT * FROM ", subset(tables_census, grepl("AP", TABLE_NAME))))

#select those in AP_YR 10-15, AP_Age_Start are 13 (no NC data)
AP <- AP %>% 
  filter(grepl("2010|2011|2012|2013|2014|2015", AP_Year)) %>%
  filter(grepl("13", AP_Age_Start))

# AZ: Fix all names
colnames(AP) <- gsub(paste0("AP_"), "", colnames(AP))
head(AP)

AP <- AP %>% rename(AcademicYear=ACADYR , # school year ending
                    LA=LANumber,
                    AgeAtStartOfAcademicYear=Age_Start) %>% 
  mutate(CensusTerm="AP") %>% 
  select(PupilMatchingRefAnonymous, AcademicYear, CensusTerm, LA, Gender, AgeAtStartOfAcademicYear,
         YearOfBirth, MonthOfBirth, FSMeligible, EthnicGroupMajor) 
# note: no IMD variables and no NCyearActual and no URN

AP <- AP %>% mutate(cen_yr = "AP")

#de duplicate - I want to keep the spring census row if exists as this has most info 
table(AP$Gender)

AP <- AP %>% mutate(Gender= case_when(Gender==1 ~ "M",
                                      Gender==2 ~ "F",
                                  Gender!=1 & Gender!=2 ~ NA))

fwrite(AP, paste0(data_dir, "/AP_clean_data.csv"))

## PRU ========================================

#select those in PRU_AcademicYear (take last 4 items in string) 10-15, PRU_AgeAtStartOfAcademicYear, PRU_NCyearActual

PRU <- sqlQuery(dbhandle, paste0("SELECT * FROM ", subset(tables_census, grepl("PRU", TABLE_NAME))))

PRU <- PRU %>% 
  mutate(PRU_yrend = substr(PRU_AcademicYear, 6, 10)) %>%
  filter(grepl("2010|2011|2012|2013|2014|2015", PRU_yrend)) %>%
  filter(grepl("13", PRU_AgeAtStartOfAcademicYear) | grepl("9", PRU_NCyearActual))

# AZ: Fix all names
head(PRU)
colnames(PRU) <- gsub(paste0("PRU_"), "", colnames(PRU))
head(PRU)

table(PRU$Gender, exclude = NULL)

PRU <- PRU %>% select(PupilMatchingRefAnonymous, AcademicYear, CensusTerm, 
                      LA, URN, 
                      Gender, AgeAtStartOfAcademicYear,
                      YearOfBirth, MonthOfBirth, FSMeligible,
                      NCyearActual,EthnicGroupMajor,
                      starts_with("overall_imd"), starts_with("idaci_20") )

PRU <- PRU %>% mutate(cen_yr = "PRU")
table(PRU$Gender, exclude=NULL)

fwrite(PRU, paste0(data_dir, "/PRU_clean_data.csv"))

rm(PRU, AP)
rm(tables_census, tables_census_tmp)
rm(table, term_year)
rm(dbhandle)
odbcCloseAll()
gc()


# Combine and clean data =============================================

data_files <- list.files(path = paste0(data_dir), pattern = "*.csv", full.names = TRUE) 
data_files

files_join <- do.call("bind_rows", lapply(data_files, fread, colClasses="character"))

table(files_join$AcademicYear)
table(files_join$AgeAtStartOfAcademicYear, exclude=NULL)
head(files_join)

gc()

table(files_join$Gender)
table(files_join$MonthOfBirth, exclude = NULL)
table(files_join$YearOfBirth, exclude = NULL)

## cohort CORE: ==============================================

pupils <- files_join %>%
  select(PupilMatchingRefAnonymous, YearOfBirth, MonthOfBirth, AcademicYear, Gender, cen_yr) %>%
  dplyr::rename( ydob=YearOfBirth, mdob=MonthOfBirth ) %>%
  mutate( Gender = toupper(Gender), 
          sex= as.factor(Gender),
          school_year_end=substr(AcademicYear, 6, 10) ) %>%
  select( -AcademicYear, -Gender) %>%
  group_by(PupilMatchingRefAnonymous) %>%
  mutate(school_year_end = min(school_year_end),
          sex=mode_fun(sex),
          mdob=mode_fun(mdob),
          ydob=mode_fun(ydob)) %>%
  ungroup() 

pupils <- pupils %>% select(-cen_yr ) %>% distinct()

table(pupils$sex, exclude=NULL)
table(pupils$mdob, exclude=NULL)
table(pupils$ydob, exclude=NULL)

dups <- duplicated(pupils["PupilMatchingRefAnonymous"])
table(dups)
rm(dups)

nrow(pupils)

pupils <- pupils %>%  
  mutate( dob = as.Date( paste(ydob ,mdob, "15",sep = "-")) ) 
head(pupils)

# CHECK duplicates
dups <- duplicated(pupils[, "PupilMatchingRefAnonymous"])
table(dups)

fwrite(pupils, paste0(outputs, "/01de_dup_pupils.csv"))

gc()

## clean IMD data =======================================

imd_data <- files_join %>%
  mutate(overall_imd_y9 = ifelse( !is.na(overall_imd_2010_rank ), overall_imd_2010_rank , overall_imd_2007_rank),
         idaci_y9 = ifelse(!is.na(idaci_2010_rank), idaci_2010_rank, idaci_2007_rank) ) %>%
  select(overall_imd_y9, idaci_y9, PupilMatchingRefAnonymous, cen_yr)

table(imd_data$overall_imd, exclude=NULL)
table(imd_data$overall_imd, imd_data$cen_yr, exclude=NULL)

imd_data <- imd_data %>% 
  filter(!is.na(overall_imd_y9) | !is.na(idaci_y9)) %>% 
  group_by(PupilMatchingRefAnonymous) %>%
  mutate(overall_imd_y9=mode_fun(overall_imd_y9),
         idaci_y9=mode_fun(idaci_y9)) %>%
  ungroup() %>% select(-cen_yr ) %>% distinct()

dups <- duplicated(imd_data["PupilMatchingRefAnonymous"])
table(dups)
rm(dups)

pupils <- merge(pupils, imd_data, all.x=T, all.y=F, by="PupilMatchingRefAnonymous")
rm(imd_data)
gc()
fwrite(pupils, paste0(outputs, "/01de_dup_pupils.csv"))

## FSM eligibility data  ============================================
head(files_join)
table(files_join$FSMeligible, exclude = NULL)
View(files_join[FSMeligible==""])

fsm <- files_join %>% select(PupilMatchingRefAnonymous, FSMeligible) %>%
 filter(FSMeligible!="") %>% distinct() 

# ever FSM elig in Year 9
fsm <- fsm %>% group_by(PupilMatchingRefAnonymous) %>%
  mutate(fsm_y9 = max(FSMeligible) ) %>% ungroup() %>% select(-FSMeligible) %>% distinct()

dups <- duplicated(fsm[,"PupilMatchingRefAnonymous"])
table(dups)
rm(dups)

pupils <- merge(pupils, fsm, all.x=T, all.y=F, by="PupilMatchingRefAnonymous")
rm(fsm)
gc()
fwrite(pupils, paste0(outputs, "/01de_dup_pupils.csv"))

## EthnicGroupMajor =============================================
table(files_join$EthnicGroupMajor, exclude=NULL)

files_join <- files_join %>%
  select(PupilMatchingRefAnonymous, EthnicGroupMajor) %>%
  filter(EthnicGroupMajor!="" & !is.na(EthnicGroupMajor))

files_join <- files_join %>%
  mutate( EthnicGroupMajor = toupper(EthnicGroupMajor), 
          EthnicGroupMajor= as.factor(EthnicGroupMajor) ) %>%
  group_by(PupilMatchingRefAnonymous) %>%
  mutate(EthnicGroupMajor =mode_fun(EthnicGroupMajor)) %>%
  ungroup() %>%
  rename(ethnic_gr_y9 = EthnicGroupMajor)

# ever FSM elig in Year 9
files_join <- files_join %>% distinct()
table(files_join$ethnic_gr_y9, exclude=NULL)

dups <- duplicated(files_join[,"PupilMatchingRefAnonymous"])
table(dups)
rm(dups)

pupils <- merge(pupils, files_join, all.x=T, all.y=F, by="PupilMatchingRefAnonymous")
rm(files_join)
gc()
fwrite(pupils, paste0(outputs, "/01de_dup_pupils.csv"))


# Link HES-NPD linkage spine ------------------------------------------

#read in NPD-HES spine from master common space
npd_spine <- fread(paste0(master_dir,"NPD_HES_Linkage_Spine.csv"))
head(npd_spine)

npd_spine <- npd_spine %>% 
  filter(PupilMatchingRefAnonymous %in% pupils$PupilMatchingRefAnonymous) %>%
  select(PupilMatchingRefAnonymous, encrypted_hesid)

# keep only those that link to SPINE
pupils <- merge(pupils, npd_spine, all.x=T, all.y=F, by="PupilMatchingRefAnonymous")
rm(npd_spine)
gc()

#check
dups <- duplicated(pupils[, "encrypted_hesid"])
table(dups)

dups <- duplicated(pupils[, "PupilMatchingRefAnonymous"])
table(dups)
rm(dups)

setDT(pupils)

sample_flow <- c(nrow(pupils), "Nr pupils in the cohort")
sample_flow <- sample_flow %>% 
  rbind(c(nrow(pupils[!is.na(encrypted_hesid)]), "Total count of pupils which join to linked spine"))

pupils <- pupils %>% 
  mutate(start_date = case_when(school_year_end == "2009" ~ "2009-09-01",
                                school_year_end == "2010" ~ "2010-09-01",
                                school_year_end == "2011" ~ "2011-09-01",
                                school_year_end == "2012" ~ "2012-09-01",
                                school_year_end == "2013" ~ "2013-09-01",
                                school_year_end == "2014" ~ "2014-09-01",
                                school_year_end == "2015" ~ "2015-09-01"))

## flag inconsistent YDOB in HES and NPD ==================================

# read in HES DOB data and drop records where the DOB is v different to NPD
mydob_det  <- read_csv(paste0(master_dir, "/HES_APC_DOB_combined.csv"))
head(mydob_det)

mydob_det <- mydob_det %>% filter(encrypted_hesid %in% pupils$encrypted_hesid)
head(mydob_det)

mydob_det <- mydob_det %>%
  dplyr::rename(bday_hes = bday)

mydob_det <- mydob_det %>%
  mutate(bday_hes=as.Date(bday_hes, origin="1970-01-01"), ydob_hes=year(bday_hes))

#join HES dob with pupils spine
pupils <- pupils %>%
  left_join(mydob_det, by="encrypted_hesid")
rm(mydob_det)
gc()

pupils <- pupils %>% 
  mutate(ydob_hes = year(bday_hes),
         mdob_hes = month(bday_hes),
         ydob = year(dob),
         mdob = month(dob)) %>%
  mutate(drop_record = case_when(ydob != ydob_hes ~ 1, 
                                 mdob != mdob_hes ~ 1,
                                 TRUE ~ 0))

pupils <- pupils %>% select( # -HES_MATCHSTEP , -PDS_MATCHSTEP,
                            - ydob_hes, -mdob_hes) %>%
  rename(hes_dob_inconsist = drop_record)

pupils <- pupils %>% rename(sex_y9 = sex)

fwrite(pupils, paste0(outputs, "/01de_dup_pupils.csv"))

## IMD --------------------------------

table(pupils$idaci_y9, exclude=NULL)
pupils <- pupils %>% mutate(idaci_y9 = ifelse(idaci_y9=="", NA, idaci_y9),
                            overall_imd_y9 = ifelse(overall_imd_y9=="", NA, overall_imd_y9))


# CHECK duplicates!!
dups <- duplicated(pupils[, "PupilMatchingRefAnonymous"])
table(dups)

#write linked spine
fwrite(pupils, paste0(outputs, "/01de_dup_pupils.csv"))

rm(list=ls())
gc()


