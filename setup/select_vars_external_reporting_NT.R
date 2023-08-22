
#####################################################################.
### Select Variables for Internal Analysis and External Reporting ###
#####################################################################.

# Author: Charlie Smith
# Date: 2023-08-22

# df = df_glob_swift_refs2
select_vars_external_reporting <- function(df){
  
  df_selected <- df %>% 
    select(!!!syms(c( # very cool!
      
      sub_source_o,
      dataset_type_o,
      hb_name_o,
      ucpn_o,
      patient_id_o,
      upi_o,
      chi_o,
      
      sex_o, # STILL TO UPDATE - Joana working on
      dob_verified_o, 
      # dob_verified_o,
      # age_at_ref_rec_o,
      # age_group_o,
      ethnicity_last_reported_o,
      protection_o,
      looked_after_c_edited_o,
      vet_edited_o,
      preg_perinatal_o,
      postcode_last_reported_o,
      # simd_vigintile_o,
      # simd_decile_o,
      simd_quintile_o,
      
      ref_date_o,
      ref_rec_date_o,
      ref_source_o,
      ref_reason_o,
      ref_acc_o, # feel like this could be more explicit
      ref_rej_date_o,
      ref_rej_reason_o,
      ref_rej_act_o,
      
      app_date_o,
      act_code_sent_date_o,
      app_purpose_o,
      att_status_o,
      unav_date_start_o,
      unav_date_start_o,
      unav_days_no_o,
      unav_reason_o,
      att_cat_o,
      prof_group_o,
      location_o,
      
      diag_1_o,
      diag_2_o,
      diag_3_o,
      treat_1_o,
      treat_2_o,
      treat_3_o,
      treat_group_or_ind_1_o,
      treat_group_or_ind_2_o,
      treat_group_or_ind_3_o,
      treat_start_date_o,
      
      measure_1_o,
      measure_2_o,
      measure_3_o,
      
      case_closed_date_o)
      )
     )
  
  return(df_selected)
  
}



