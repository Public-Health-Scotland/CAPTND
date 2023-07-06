#######################.
###   DF Splitter   ###
#######################.

#Splits large data frame into several smaller ones
#should be done AFTER data cleanup


split_df <- function(df){
  
  df_index <- df %>% dplyr::select(!!ucpn_o,
                                !!dataset_type_o,
                                !!chi_o,
                                !!hb_name_o) %>% 
    distinct()
  
  
  
  df_patient_personal_info <- df %>% dplyr::select(!!ucpn_o,
                                                   !!dataset_type_o,
                                                   !!chi_o,
                                                   !!hb_name_o,
                                                   !!postcode_o,
                                                   !!dob_o,
                                                   !!ethnicity_o,
                                                   !!ethnicity_edited_o,
                                                   !!ethnicity_edited_counts_o,
                                                   !!ethnicity_evaluation_o,
                                                   !!sex_o) %>%
    distinct()
  
  df_data_info <- df %>% dplyr::select(!!ucpn_o,
                                       !!dataset_type_o,
                                       !!chi_o,
                                       !!hb_name_o,
                                       !!file_id_o,
                                       !!line_no_o,
                                       !!header_date_o) %>% 
    distinct()
  
  df_ref_info <- df %>% dplyr::select(!!ucpn_o,
                                 !!dataset_o,
                                 !!chi_o,
                                 !!hb_name_o,
                                 !!ref_date_o,           
                                 !!ref_rec_date_o,   
                                 !!ref_source_o,
                                 !!ref_reason_o,          
                                 !!ref_acc_o,
                                 !!ref_rej_date_o,      
                                 !!ref_rej_reason_o,
                                 !!rej_rej_act_o) %>% 
    distinct()

  
  df_appt_info <- df %>% dplyr::select(!!ucpn_o,
                                  !!dataset_o,
                                  !!chi_o,
                                  !!hb_name_o,
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
    if_all(c(!!app_date_o,
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
             !!measure_3_o), is.na) %>% 
    distinct()
  
  df_closed_case_info <- df %>% dplyr::select(!!ucpn_o,
                                          !!dataset_o,
                                          !!chi_o,
                                          !!hb_name_o,
                                          !!case_closed_date_o) %>% 
    distinct()
   
  df_list=list(df_index, df_patient_personal_info, df_data_info, df_ref, df_appt,df_closed_case)
  
  return(df_list)
  
}





          






