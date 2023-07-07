#######################.
###   DF Splitter   ###
#######################.

#Splits large data frame into several smaller ones
#should be done AFTER data cleanup


split_df <- function(df){
  
  data_keys <- c(patient_id_o,
                 ucpn_o,
                 dataset_type_o,
                 hb_name_o)
  
  df_index <- df %>% dplyr::select(!!data_keys) %>% 
    distinct()
  
  
  
  df_patient_personal_info <- df %>% 
    dplyr::select(!!data_keys,
                  !!chi_o,
                  !!chi_valid_o,
                  !!upi_o,
                  !!postcode_o,
                  !!dob_o,
                  !!ethnicity_o,
                  !!ethnicity_edited_o,
                  !!ethnicity_edited_counts_o,
                  !!ethnicity_evaluation_o,
                  !!sex_o,
                  !!sex_from_chi_o,
                  !!sex_recorded_matches_chi_o) %>%
    filter_at(vars(-all_of(data_keys)), all_vars(!is.na(.))) %>% 
    distinct()
  
  
  df_data_info <- df %>% 
    dplyr::select(!!data_keys,
                  !!file_id_o,
                  !!line_no_o,
                  !!header_date_o) %>% 
    filter_at(vars(-all_of(data_keys)), all_vars(!is.na(.))) %>% 
    distinct()
  
  
  df_ref_info <- df %>%
    dplyr::select(!!data_keys,
                  !!ref_date_o,           
                  !!ref_rec_date_o,   
                  !!ref_source_o,
                  !!ref_reason_o,          
                  !!ref_acc_o,
                  !!ref_rej_date_o,      
                  !!ref_rej_reason_o,
                  !!rej_rej_act_o,
                  !!act_code_sent_date_o) %>% 
    filter_at(vars(-all_of(data_keys)), all_vars(!is.na(.))) %>% 
    distinct()
  
  df_appt_info <- df %>% 
    dplyr::select(!!data_keys,
                  !!app_date_o,
                  !!app_purpose_o,
                  !!att_status_o,
                  !!unav_date_start_o,
                  !!unav_date_end_o,
                  !!unav_days_no_o,
                  !!unav_reason_o,  
                  !!att_cat_o,
                  !!prof_group_o,         
                  !!location_o,
                  !!diag_1_o,        
                  !!diag_2_o,
                  !!diag_3_o,            
                  !!treat_1_o, 
                  !!treat_2_o,  
                  !!treat_3_o, 
                  !!treat_group_or_ind_1_o, 
                  !!treat_group_or_ind_2_o,
                  !!treat_group_or_ind_3_o,
                  !!treat_start_date_o,
                  !!measure_1_o,
                  !!measure_2_o,
                  !!measure_3_o) %>%  
    filter_at(vars(-all_of(data_keys)), all_vars(!is.na(.))) %>% 
    distinct()
  
  
  df_closed_case_info <- df %>% 
    dplyr::select(!!data_keys,
                  !!case_closed_date_o) %>% 
    filter_at(vars(-all_of(data_keys)), all_vars(!is.na(.))) %>% 
    distinct()
  
  
  df_list=list('index'=df_index, 
               'patient_info'=df_patient_personal_info, 
               'data_info'=df_data_info, 
               'ref_info'=df_ref_info, 
               'appt_info'=df_appt_info,
               'closed_case_info'=df_closed_case_info)
  
  return(df_list)
  
}





          






