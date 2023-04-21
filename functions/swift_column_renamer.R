############################.
### Swift column renamer ###
############################.

#This function takes names from the master document with all column names for CAPTND and changes swift column names accordingly.


# 1 Load packages ---------------------------------------------------------

library(dplyr)
library(magrittr)


# 2 Source colnames objects -----------------------------------------------

source('scripts/setup/new_column_names_swift.R')


# 3 Function code ---------------------------------------------------------

rename_swift_columns <- function(data){
  
  data <- data %>% 
    rename(
      !!file_id_o := "FILE_ID",
      !!line_no_o := "LINE_NO",
      !!dataset_type_o := "DATA_TYPE",
      !!hb_name_o := "HB_NAME",
      !!ucpn_o := "UPCN",
      !!upi_o := "UNIQUE_PAT_ID",             
      !!postcode_o := "POSTCODE",
      !!sex_o := "SEX",
      !!dob_o := "DOB",
      !!ethnicity_o := "ETHNICITY",            
      !!chi_o := "CHI",
      !!ref_date_o := "REFERRAL_DATE",            
      !!ref_rec_date_o := "REFERRAL_RECEIVED_DATE",   
      !!ref_source_o := "REFERRAL_SOURCE",        
      !!ref_reason_o := "REFERRAL_REASON",            
      !!ref_acc_o := "REFERRAL_ACCEPTED",          
      !!ref_rej_date_o := "REFERRAL_REJECTED_DATE",       
      !!ref_rej_reason_o := "REFERRAL_REJECTED_REASON",   
      !!rej_rej_act_o := "REFERRAL_REJECTED_ACTIONS",    
      !!app_date_o := "DATE_OF_APPOINTMENT",        
      !!app_purpose_o := "APPOINTMENT_PURPOSE",       
      !!att_status_o := "ATTENDENCE_STATUS",        
      !!unav_date_start_o := "UNAVAILABILITY_START_DATE",
      !!unav_date_end_o :="UNAVAILABILITY_END_DATE",
      !!unav_days_no_o :="NO_OF_DAYS_UNAVAILABLE",
      !!unav_reason_o := "UNAVAILABILITY_REASON",  
      !!att_cat_o := "ATTENDANCE_CATEGORY",       
      !!prof_group_o := "PROFESSIONAL_GROUP",       
      !!location_o := "CARE_CONTACT_LOCATION",
      !!diag_1_o := "DIAGNOSIS_1",             
      !!diag_2_o :="DIAGNOSIS_2",
      !!diag_3_o := "DIAGNOSIS_3",          
      !!treat_1_o := "TREATMENT_INTERVENTION_1",
      !!treat_2_o := "TREATMENT_INTERVENTION_2",    
      !!treat_3_o := "TREATMENT_INTERVENTION_3", 
      !!treat_group_or_ind_1_o := "GRP_IND_TREAT_INTERVENTION_1",
      !!treat_group_or_ind_2_o := "GRP_IND_TREAT_INTERVENTION_2",
      !!treat_group_or_ind_3_o := "GRP_IND_TREAT_INTERVENTION_3",
      !!treat_start_date_o := "TREAT_INTERVENTION_DATE",
      !!measure_1_o := "CLINICAL_OUTCOME_1",          
      !!measure_2_o := "CLINICAL_OUTCOME_2",
      !!measure_3_o := "CLINICAL_OUTCOME_3",
      !!case_closed_date_o :=  "DATE_CASE_CLOSED_OR_DOD",
      !!header_date_o := "HEADER_REF_DATE")          

  #vector to evaluate if dataset is CAMHS or PT, in order to correctly rename   
  dataset_vector=data %>% pull(!!dataset_type_o)
  
  if('CAMHS' %in% dataset_vector){
    data <- data %>% rename(!!protection_o := "CHILD_PROTECTION",          
                         !!looked_after_c_o := "LOOKED_AFTER_CHILD")
  }else{
    data <- data %>% rename(!!protection_o := "ADULT_PROTECTION",       
                            !!vet_o := "VETERAN_ARMED_FORCES",    
                            !!preg_perinatal_o := "PREGNANT_PERINATAL_MH",
                            !!act_code_sent_date_o := "ACTIVATION_CODE_SENT_DATE") 
  }

  return(data)
}