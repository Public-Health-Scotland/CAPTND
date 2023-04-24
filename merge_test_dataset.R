###############################################################.
### Merge individual test data files into one large dataset ###.
###############################################################.


#1 Load packages####
library(dplyr)
library(readr)
library(stringr)
library(janitor)

#2 Set path####
p='../../../data/test_dataset_separate_issues/'

#3 Read files from location####
filenames <- list.files(p, pattern="*.csv", full.names=FALSE)

#4 Make empty lists to store info####
lfiles=list()
lnames=list()

#5 Iteratively read all files and save df and file name into lists####
for(i in 1:length(filenames)){
  df=read_csv(paste0(p,filenames[[i]]),
              col_select = 1:51,
              cols(
                   DATASET=col_character(),
                   HB=col_character(),
                   CHI=col_character(),
                   UCPN=col_character(),
                   UPI=col_character(),
                   POSTCODE=col_character(),
                   SEX=col_character(),
                   DOB=col_date(),
                   ETHNICITY=col_character(),
                   REF_DATE=col_date(),
                   PROTECTION=col_character(),
                   LAC=col_character(),
                   RECEIVED_DATE=col_date(),
                   REF_SOURCE=col_character(),
                   REF_REASON=col_character(),
                   ACCEPTED=col_character(),
                   REJ_DATE=col_date(),
                   REJ_REASON=col_character(),
                   REJ_ACTIONS=col_character(),
                   APP_DATE=col_date(),
                   APP_PURPOSE=col_character(),
                   ATT_STATUS=col_character(),
                   UNA_START_DATE=col_date(),
                   UNA_END_DATE=col_date(),
                   NUM_DAYS=col_double(),
                   UNA_REASON=col_character(),
                   ATT_CATEGORY=col_character(),
                   PROF_GROUP=col_character(),
                   LOCATION=col_character(),
                   DIAG1=col_character(),
                   DIAG2=col_character(),
                   DIAG3=col_character(),
                   TREAT1=col_character(),
                   TREAT2=col_character(),
                   TREAT3=col_character(),
                   GRPIND1=col_character(),
                   GRPIND2=col_character(),
                   GRPIND3=col_character(),
                   START_DATE=col_date(),
                   MEASURES1=col_character(),
                   MEASURES2=col_character(),
                   MEASURES3=col_character(),
                   DISCHARGE_DATE=col_date(),
                   VETERAN=col_character(),
                   PPMH_REFERRAL=col_character(),
                   CODE_SENT_DATE=col_date(),
                   SUB_SOURCE=col_character(),
                   FILE_ID=col_character(),
                   LINE_NO=col_character(),
                   HEADER_DATE=col_character(),
                   PPMH=col_character()
              )) %>% 
    row_to_names(row_number = 1)
  lfiles[[i]]=df
  lnames[[i]]=str_remove(filenames[[i]], '.csv')
}

#6 Name dataframe list with file names####
names(lfiles)=lnames

#7 Filter rows with no HB info####
dfFinal=bind_rows(lfiles, .id = "ISSUE") %>% 
  filter(!is.na(HB))

#8 Save data####
write_csv(dfFinal,'../../../data/testDataset.csv')
