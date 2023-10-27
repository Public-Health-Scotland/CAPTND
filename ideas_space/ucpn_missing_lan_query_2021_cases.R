# Description: Extract Lanarkshire referrals with no UCPNs from May to Oct 2021 (inclusive).

# Issues:
# ~ need to use 'captnd_all' version of data as this time period spans move to SWIFT
# ~ need to use 'captnd_all' as it's before any cleaning would remove cases with missing UCPN 


#Run to line 337 to get 'captnd_all' data
# 1 - Establish Connection ------------------------------------------------


if( ! exists("con")){
  con <- dbConnect(odbc::odbc(),
                   dsn = "CAPTND",
                   uid = rstudioapi::askForPassword("Enter network username:"), 
                   pwd = rstudioapi::askForPassword("Enter network password:"))
} 


# load functions
source("./Script/Functions/fn_fill_down_vals.R")
source("./Script/Functions/dataCompletionTool_demographics.R")
source("./Script/Functions/fn_add_batch_month.R")
source("./Script/Functions/fn_add_leading_zeros.R")
source("./Script/Functions/fn_tidy_hb_names.R")
source("../../../CAPTND/CAPTND_shorewise/scripts/charlie/CAPTND/setup/save_df_as_parquet.R")


# set file path for outputs
output_folder <- paste0("./Output/", today("GMT"), "/") 


# 2 - Create Working DF for Analysis -------------------------------------------


# 2.1 - SWIFT DATA --------------------------------------------------------
# SWIFT CAMHS
swift_camhs <- as.data.frame(tbl(con, in_schema("CAPTND", "CAPTND_CAMHS"))) %>% 
  rename(DATASET = DATA_TYPE, # rename vars to match pre-swift var names
         HB = HB_NAME,
         UCPN = UPCN, # TYPO IN SWIFT
         UPI = UNIQUE_PAT_ID,
         REF_DATE = REFERRAL_DATE,
         PROTECTION = CHILD_PROTECTION,
         LAC = LOOKED_AFTER_CHILD,
         RECEIVED_DATE = REFERRAL_RECEIVED_DATE,
         REF_SOURCE = REFERRAL_SOURCE,
         REF_REASON = REFERRAL_REASON,
         ACCEPTED = REFERRAL_ACCEPTED, 
         REJ_DATE = REFERRAL_REJECTED_DATE,
         REJ_REASON = REFERRAL_REJECTED_REASON,
         REJ_ACTIONS = REFERRAL_REJECTED_ACTIONS,
         APP_DATE = DATE_OF_APPOINTMENT,
         APP_PURPOSE = APPOINTMENT_PURPOSE,
         ATT_STATUS = ATTENDENCE_STATUS, # TYPO IN SWIFT VAR
         UNA_START_DATE  = UNAVAILABILITY_START_DATE,
         UNA_END_DATE = UNAVAILABILITY_END_DATE,
         NUM_DAYS = NO_OF_DAYS_UNAVAILABLE,
         UNA_REASON = UNAVAILABILITY_REASON,
         ATT_CATEGORY = ATTENDANCE_CATEGORY,
         PROF_GROUP = PROFESSIONAL_GROUP,
         LOCATION = CARE_CONTACT_LOCATION,
         DIAG1 = DIAGNOSIS_1,
         DIAG2 = DIAGNOSIS_2,
         DIAG3 = DIAGNOSIS_3,
         TREAT1 = TREATMENT_INTERVENTION_1,
         TREAT2 = TREATMENT_INTERVENTION_2,
         TREAT3 = TREATMENT_INTERVENTION_3,
         GRPIND1 = GRP_IND_TREAT_INTERVENTION_1,
         GRPIND2 = GRP_IND_TREAT_INTERVENTION_2,
         GRPIND3 = GRP_IND_TREAT_INTERVENTION_3,
         START_DATE = TREAT_INTERVENTION_DATE, 
         MEASURES1 = CLINICAL_OUTCOME_1, 
         MEASURES2 = CLINICAL_OUTCOME_2,
         MEASURES3 = CLINICAL_OUTCOME_3,
         DISCHARGE_DATE = DATE_CASE_CLOSED_OR_DOD,
         HEADER_DATE = HEADER_REF_DATE) # new submission date marker - only accurate for SWIFT subs from June 2022 (use lookup for months prior)


# SWIFT PT
swift_pt <- as.data.frame(tbl(con, in_schema("CAPTND", "CAPTND_PT"))) %>% 
  rename(DATASET = DATA_TYPE, 
         HB = HB_NAME, 
         UCPN = UPCN, # TYPO IN SWIFT
         UPI = UNIQUE_PAT_ID, 
         #POSTCODE, 
         #SEX, 
         #DOB, 
         #ETHNICITY, 
         #CHI, 
         REF_DATE = REFERRAL_DATE, 
         PROTECTION = ADULT_PROTECTION, 
         VETERAN = VETERAN_ARMED_FORCES, 
         PPMH = PREGNANT_PERINATAL_MH, 
         RECEIVED_DATE = REFERRAL_RECEIVED_DATE, 
         REF_SOURCE = REFERRAL_SOURCE,
         REF_REASON = REFERRAL_REASON,
         ACCEPTED = REFERRAL_ACCEPTED, 
         REJ_DATE = REFERRAL_REJECTED_DATE,
         REJ_REASON = REFERRAL_REJECTED_REASON,
         REJ_ACTIONS = REFERRAL_REJECTED_ACTIONS,
         APP_DATE = DATE_OF_APPOINTMENT,
         APP_PURPOSE = APPOINTMENT_PURPOSE,
         ATT_STATUS = ATTENDENCE_STATUS, # TYPO IN SWIFT VAR
         UNA_START_DATE  = UNAVAILABILITY_START_DATE,
         UNA_END_DATE = UNAVAILABILITY_END_DATE,
         NUM_DAYS = NO_OF_DAYS_UNAVAILABLE,
         UNA_REASON = UNAVAILABILITY_REASON,
         ATT_CATEGORY = ATTENDANCE_CATEGORY,
         CODE_SENT_DATE = ACTIVATION_CODE_SENT_DATE, 
         PROF_GROUP = PROFESSIONAL_GROUP,
         LOCATION = CARE_CONTACT_LOCATION, 
         DIAG1 = DIAGNOSIS_1,
         DIAG2 = DIAGNOSIS_2,
         DIAG3 = DIAGNOSIS_3,
         TREAT1 = TREATMENT_INTERVENTION_1,
         TREAT2 = TREATMENT_INTERVENTION_2,
         TREAT3 = TREATMENT_INTERVENTION_3,
         GRPIND1 = GRP_IND_TREAT_INTERVENTION_1,
         GRPIND2 = GRP_IND_TREAT_INTERVENTION_2,
         GRPIND3 = GRP_IND_TREAT_INTERVENTION_3,
         START_DATE = TREAT_INTERVENTION_DATE, 
         MEASURES1 = CLINICAL_OUTCOME_1, 
         MEASURES2 = CLINICAL_OUTCOME_2,
         MEASURES3 = CLINICAL_OUTCOME_3,
         DISCHARGE_DATE = DATE_CASE_CLOSED_OR_DOD,
         HEADER_DATE = HEADER_REF_DATE) 




# join swift subs
swift_master <- rbind.fill(swift_camhs, swift_pt) %>% 
  mutate(UCPN_og = UCPN,
         UCPN = if_else(UCPN %in% c("NULL", "0"), NA_character_, UCPN), 
         SUB_SOURCE = "SWIFT") %>% 
  tidy_hb_names()

#save_as_parquet(df = swift_master, 
#                path = paste0(output_folder, "captnd_raw"))

rm(swift_camhs, swift_pt)



# 2.2 - Glob CAMHS --------------------------------------------------------

glob_camhs_ref <- as.data.frame(tbl(con, in_schema("CAPTND", "CAMHS_REFERRAL_STAGE"))) %>% 
  mutate(UCPN_og = UCPN, .before = UCPN,
         UCPN = if_else(UCPN %in% c("NULL", "0"), NA_character_, UCPN),
         UPI = if_else(UPI %in% c("NULL", "0"), NA_character_, UPI),
         CHI = if_else(CHI %in% c("NULL", "0"), NA_character_, CHI),
         UCPN = coalesce(UCPN, UPI, CHI)) #%>% 
  #filter(!is.na(REF_DATE) | !is.na(RECEIVED_DATE)) # check impact of these filters


glob_camhs_new <- as.data.frame(tbl(con, in_schema("CAPTND", "CAMHS_NEW_STAGE"))) %>% 
  mutate(UCPN_og = UCPN, .before = UCPN,
         UCPN = if_else(UCPN %in% c("NULL", "0"), NA_character_, UCPN),
         UPI = if_else(UPI %in% c("NULL", "0"), NA_character_, UPI),
         CHI = if_else(CHI %in% c("NULL", "0"), NA_character_, CHI),
         UCPN = coalesce(UCPN, UPI, CHI)) %>% 
  select(-c(FILENAME, LOADDATE, HB, UPI, CHI)) %>% 
  filter(!is.na(APP_DATE))


glob_camhs_ret <- as.data.frame(tbl(con, in_schema("CAPTND", "CAMHS_RETURN_STAGE"))) %>%
  mutate(UCPN_og = UCPN, .before = UCPN,
         UCPN = if_else(UCPN %in% c("NULL", "0"), NA_character_, UCPN),
         UPI = if_else(UPI %in% c("NULL", "0"), NA_character_, UPI),
         CHI = if_else(CHI %in% c("NULL", "0"), NA_character_, CHI),
         UCPN = coalesce(UCPN, UPI, CHI)) %>% 
  select(-c(FILENAME, LOADDATE, HB, UPI, CHI)) %>% 
  filter(!is.na(APP_DATE))


glob_camhs_diag <- as.data.frame(tbl(con, in_schema("CAPTND", "CAMHS_DIAGNOSIS_STAGE"))) %>%
  mutate(UCPN_og = UCPN, .before = UCPN,
         UCPN = if_else(UCPN %in% c("NULL", "0"), NA_character_, UCPN),
         UPI = if_else(UPI %in% c("NULL", "0"), NA_character_, UPI),
         CHI = if_else(CHI %in% c("NULL", "0"), NA_character_, CHI),
         UCPN = coalesce(UCPN, UPI, CHI)) %>% 
  select(-c(FILENAME, LOADDATE, HB, UPI, CHI)) %>% 
  filter(!is.na(DIAG1))


glob_camhs_out = as.data.frame(tbl(con, in_schema("CAPTND", "CAMHS_OUTCOMES_STAGE"))) %>% 
  mutate(UCPN_og = UCPN, .before = UCPN,
         UCPN = if_else(UCPN %in% c("NULL", "0"), NA_character_, UCPN),
         UPI = if_else(UPI %in% c("NULL", "0"), NA_character_, UPI),
         CHI = if_else(CHI %in% c("NULL", "0"), NA_character_, CHI),
         UCPN = coalesce(UCPN, UPI, CHI)) %>% 
  select(-c(FILENAME, LOADDATE, HB, UPI, CHI)) %>% 
  filter(!is.na(MEASURES1)) 


glob_camhs_dis <- as.data.frame(tbl(con, in_schema("CAPTND", "CAMHS_DISCHARGE_STAGE"))) %>% 
  mutate(UCPN_og = UCPN, .before = UCPN,
         UCPN = if_else(UCPN %in% c("NULL", "0"), NA_character_, UCPN),
         UPI = if_else(UPI %in% c("NULL", "0"), NA_character_, UPI),
         CHI = if_else(CHI %in% c("NULL", "0"), NA_character_, CHI),
         UCPN = coalesce(UCPN, UPI, CHI)) %>% 
  select(-c(FILENAME, LOADDATE, HB, UPI, CHI)) %>% 
  rename(DISCHARGE_DATE = END_DATE) %>% 
  filter(!is.na(DISCHARGE_DATE))


# Combine appointment info
glob_camhs_apps <- rbind(glob_camhs_new, glob_camhs_ret) %>% 
  #mutate(UCPN = ifelse(UCPN == "0", NA_character_, UCPN)) %>% 
  rename(UNA_START_DATE = START_DATE,
         UNA_END_DATE = END_DATE,
         APP_PURPOSE = PURPOSE,
         ATT_STATUS = STATUS,
         UNA_REASON = REASON, 
         ATT_CATEGORY = CATEGORY)


# Join all dfs into master df
glob_camhs_master <- glob_camhs_ref %>% 
  left_join(., glob_camhs_apps, by = c("UCPN", "UCPN_og"), na_matches = "never") %>% # check joins are appropriate
  left_join(., glob_camhs_diag, by = c("UCPN", "UCPN_og"), na_matches = "never") %>%
  left_join(., glob_camhs_out, by = c("UCPN", "UCPN_og"), na_matches = "never") %>%
  left_join(., glob_camhs_dis, by = c("UCPN", "UCPN_og"), na_matches = "never") %>%
  select(-c(LOADDATE)) %>%
  #distinct(.) %>% 
  mutate(DATASET = "CAMHS")


# 2.3 - Glob PT -------------------------------------------------------------

glob_pt_ref <- as.data.frame(tbl(con, in_schema("CAPTND", "PT_REFERRAL_STAGE"))) %>% 
  mutate(UCPN_og = UCPN, .before = UCPN,
         UCPN = if_else(UCPN %in% c("NULL", "0"), NA_character_, UCPN),
         UPI = if_else(UPI %in% c("NULL", "0"), NA_character_, UPI),
         CHI = if_else(CHI %in% c("NULL", "0"), NA_character_, CHI),
         UCPN = coalesce(UCPN, UPI, CHI)) %>% 
  rename(PPMH_REFERRAL = PPMH) #%>% 
  #filter(!is.na(REF_DATE) | !is.na(RECEIVED_DATE)) # check impact of filters


glob_pt_new <- as.data.frame(tbl(con, in_schema("CAPTND", "PT_NEW_STAGE"))) %>% 
  mutate(UCPN_og = UCPN, .before = UCPN,
         UCPN = if_else(UCPN %in% c("NULL", "0"), NA_character_, UCPN),
         UPI = if_else(UPI %in% c("NULL", "0"), NA_character_, UPI),
         CHI = if_else(CHI %in% c("NULL", "0"), NA_character_, CHI),
         UCPN = coalesce(UCPN, UPI, CHI)) %>% 
  select(-c(FILENAME, LOADDATE, HB, UPI, CHI)) %>% 
  filter(!is.na(APP_DATE))


glob_pt_ret <- as.data.frame(tbl(con, in_schema("CAPTND", "PT_RETURN_STAGE"))) %>%
  mutate(UCPN_og = UCPN, .before = UCPN,
         UCPN = if_else(UCPN %in% c("NULL", "0"), NA_character_, UCPN),
         UPI = if_else(UPI %in% c("NULL", "0"), NA_character_, UPI),
         CHI = if_else(CHI %in% c("NULL", "0"), NA_character_, CHI),
         UCPN = coalesce(UCPN, UPI, CHI)) %>% 
  select(-c(FILENAME, LOADDATE, HB, UPI, CHI)) %>% 
  rename(PPMH_RETURN = PPMH) %>% 
  select(-PPMH_RETURN) %>% 
  filter(!is.na(APP_DATE))


glob_pt_diag <- as.data.frame(tbl(con, in_schema("CAPTND", "PT_DIAGNOSIS_STAGE"))) %>%
  mutate(UCPN_og = UCPN, .before = UCPN,
         UCPN = if_else(UCPN %in% c("NULL", "0"), NA_character_, UCPN),
         UPI = if_else(UPI %in% c("NULL", "0"), NA_character_, UPI),
         CHI = if_else(CHI %in% c("NULL", "0"), NA_character_, CHI),
         UCPN = coalesce(UCPN, UPI, CHI)) %>% 
  select(-c(FILENAME, LOADDATE, HB, UPI, CHI)) %>% 
  filter(!is.na(DIAG1))


glob_pt_out = as.data.frame(tbl(con, in_schema("CAPTND", "PT_OUTCOMES_STAGE"))) %>% 
  mutate(UCPN_og = UCPN, .before = UCPN,
         UCPN = if_else(UCPN %in% c("NULL", "0"), NA_character_, UCPN),
         UPI = if_else(UPI %in% c("NULL", "0"), NA_character_, UPI),
         CHI = if_else(CHI %in% c("NULL", "0"), NA_character_, CHI),
         UCPN = coalesce(UCPN, UPI, CHI)) %>% 
  select(-c(FILENAME, LOADDATE, HB, UPI, CHI)) %>% 
  filter(!is.na(MEASURES1)) 


glob_pt_dis <- as.data.frame(tbl(con, in_schema("CAPTND", "PT_DISCHARGE_STAGE"))) %>% 
  mutate(UCPN_og = UCPN, .before = UCPN,
         UCPN = if_else(UCPN %in% c("NULL", "0"), NA_character_, UCPN),
         UPI = if_else(UPI %in% c("NULL", "0"), NA_character_, UPI),
         CHI = if_else(CHI %in% c("NULL", "0"), NA_character_, CHI),
         UCPN = coalesce(UCPN, UPI, CHI)) %>% 
  select(-c(FILENAME, LOADDATE, HB, UPI, CHI)) %>% 
  rename(DISCHARGE_DATE = END_DATE) %>% 
  filter(!is.na(DISCHARGE_DATE)) 



# Combine appointment info
glob_pt_apps <- rbind.fill(glob_pt_new, glob_pt_ret) %>%  
  #mutate(UCPN = ifelse(UCPN == "0", NA_character_, UCPN)) %>% 
  rename(UNA_START_DATE = START_DATE, # renaming needs done manually???
         UNA_END_DATE = END_DATE,
         APP_PURPOSE = PURPOSE,
         ATT_STATUS = STATUS,
         UNA_REASON = REASON,
         ATT_CATEGORY = CATEGORY) 



# Join all dfs into master df
glob_pt_master <- glob_pt_ref %>%
  left_join(., glob_pt_apps, by = c("UCPN", "UCPN_og"), na_matches = "never") %>%
  left_join(., glob_pt_diag, by = c("UCPN", "UCPN_og"), na_matches = "never") %>%
  left_join(., glob_pt_out, by = c("UCPN", "UCPN_og"), na_matches = "never") %>%
  left_join(., glob_pt_dis, by = c("UCPN", "UCPN_og"), na_matches = "never") %>%
  select(-c(LOADDATE)) %>%
  #distinct(.) %>% 
  mutate(DATASET = "PT")




# 3 - Stack all data ------------------------------------------------------

# combine globalscape records
master_glob <- rbind.fill(glob_camhs_master, glob_pt_master) %>%
  mutate(SUB_SOURCE = "GLOBALSCAPE")

rm(list=ls(pattern=c("^glob")))

glob_master <- master_glob

rm(master_glob)

# stack all data
captnd_all <- rbind.fill(glob_master, swift_master) %>% 
  select(DATASET, HB, CHI, UCPN_og, UCPN, everything()) 

captnd_all <- tidy_hb_names(captnd_all)



# NHS Lanarkshire data missing UCPN from May 2021 to Oct 2021
#First part is in globalscape May-July 2021, rest in SWIFT Aug to Oct 2021

#Filter out NHA Lanarkshire cases with no UCPN that have referral date
lan_ucpn_miss <- captnd_all %>%
  filter(HB == 'NHS Lanarkshire' & is.na(UCPN)) %>% 
  filter(!is.na(REF_DATE) | !is.na(RECEIVED_DATE))

#Get date from file name for globalscape cases
lan_ucpn_miss <- lan_ucpn_miss %>% 
  mutate(file_date = ymd(str_extract(lan_ucpn_miss$FILENAME, "(?<=l_)\\d+-\\d+-\\d+(?=.)"))) 

#Filter cases from May 2021 to Oct 2021
lan_ucpn_miss_2021 <- lan_ucpn_miss %>% 
 mutate(case_status = case_when(SUB_SOURCE == 'GLOBALSCAPE' & file_date >= '2021-05-01' ~ 'in range',
           SUB_SOURCE == 'SWIFT' & HEADER_DATE <= '2021-11-01' ~ 'in range',
           TRUE ~ 'not in range')) %>% 
  filter(case_status == 'in range')

