
##################################.
### Set column order in CAPTND ###
##################################.

# Purpose: Set order of column in CAPTND

# Author: Charlie Smith
# Date: 2023-06-30



set_column_order <- function(df){
  
  x <- df %>% 
    select(
      
      # 1 - metadata
      sym(sub_source_o),
      sym(header_date_o),
      sym(file_id_o),
      #sym(line_no_o),
      
      # 2 - Patient info
      sym(dataset_type_o),
      sym(chi_o),
      sym(upi_o), 
      sym(patient_id_o),
      sym(ucpn_o),
      sym(hb_name_o),
      sym(sex_o),
      sym(dob_o),
      sym(ethnicity_o),
      sym(postcode_o),
      sym(protection_o),
      sym(looked_after_c_o),
      sym(vet_o),
      sym(preg_perinatal_o),
      
      # 3 - Ref info
      sym(ref_date_o),
      sym(ref_rec_date_o),
      sym(ref_source_o),
      sym(ref_reason_o),
      sym(ref_acc_o),
      sym(ref_rej_date_o),
      sym(ref_rej_reason_o),
      sym(ref_rej_act_o),
      
      # 4 - App info
      sym(record_type_o),
      sym(act_code_sent_date_o), # is this an okay place?
      sym(app_date_o),
      sym(att_status_o),
      sym(att_cat_o),
      sym(app_purpose_o),
      sym(prof_group_o),
      sym(location_o),
      
      sym(unav_date_start_o),
      sym(unav_date_end_o),
      sym(unav_days_no_o),
      sym(unav_reason_o),
    
      # 5 - Diagnosis info
      sym(diag_1_o),
      sym(diag_2_o),
      sym(diag_3_o),
      sym(treat_1_o),
      sym(treat_2_o),
      sym(treat_3_o),
      sym(treat_group_or_ind_1_o),
      sym(treat_group_or_ind_2_o),
      sym(treat_group_or_ind_3_o),
      sym(treat_start_date_o), # maybe include in app info?
      
      # 6 - Outcome measures used
      sym(measure_1_o),
      sym(measure_2_o),
      sym(measure_3_o),
      
      # 7 - Case closed info
      sym(case_closed_date_o)
    )
  
  return(x)
  
}
