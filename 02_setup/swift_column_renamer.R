############################.
### Swift column renamer ###
############################.

# Takes names from new_colnames.R and changes column names accordingly.
# source('./02_setup/new_colnames.R')


rename_swift_columns <- function(data){
  
  data <- data %>% 
    dplyr::rename(
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
      !!ref_rej_act_o := "REFERRAL_REJECTED_ACTIONS",    
      !!app_date_o := "DATE_OF_APPOINTMENT",        
      !!app_purpose_o := "APPOINTMENT_PURPOSE",       
      !!att_status_o := "ATTENDANCE_STATUS",        
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
      !!cgi_i_o := "CGI_I",
      !!pgi_i_o := "PGI_I",
      !!cgi_s_o := "CGI_S",
      !!case_closed_date_o :=  "DATE_CASE_CLOSED_OR_DOD",
      !!header_date_o := "HEADER_REF_DATE", 
      
      !!care_plan_inc_o := "CARE_PLAN_INCLUSION",
      !!cancellation_date_o := "DATE_OF_CANCELLATION",
      
      !!presenting_prob_1_o := "PRESENTING_PROBLEM_1",
      !!presenting_prob_2_o := "PRESENTING_PROBLEM_2",
      !!presenting_prob_3_o := "PRESENTING_PROBLEM_3",
      
      !!treat_reason_1_o := "REASON_FOR_TREATMENT_1",
      !!treat_reason_2_o := "REASON_FOR_TREATMENT_2",
      !!treat_reason_3_o := "REASON_FOR_TREATMENT_3"
      
      )          

  # vector to evaluate if dataset is CAMHS or PT, in order to correctly rename   
  dataset_vector = data %>% pull(!!dataset_type_o)
  
  if('CAMHS' %in% dataset_vector){
    
    data <- data %>% 
      rename(!!protection_o := "CHILD_PROTECTION",          
                         !!looked_after_c_o := "LOOKED_AFTER_CHILD")
    
  }else{
    
    data <- data %>% 
      rename(!!protection_o := "ADULT_PROTECTION",       
                            !!vet_o := "VETERAN_ARMED_FORCES",    
                            !!preg_perinatal_o := "PREGNANT_PERINATAL_MH",
                            !!act_code_sent_date_o := "ACTIVATION_CODE_SENT_DATE") 
    
  }
  
  return(data)

}

