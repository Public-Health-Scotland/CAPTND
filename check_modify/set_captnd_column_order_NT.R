
###################################.
### Set all CAPTND column order ###
###################################.

# Author: Charlie Smith 
# Date: 2023-09-07

set_captnd_column_order <- function(df){
  
  df_order <- df %>% 
    select(!!!syms(
      
      # 1 - Metadata -------------------------------------------------------
      sub_source_o,
      submission_date_o,
      dataset_type_o,
      record_type_o,
      data_keys,
      chi_valid_o,
      
      # 2 - Demographics ---------------------------------------------------
      vec_demographic_cols,
      dob_from_chi_o,
      sex_from_chi_o,
      dob_recorded_matches_chi_o,
      sex_recorded_matches_chi_o,
      sex_reported_o,
      dob_verified_o,
      age_at_ref_rec_o,
      age_group_o, 
      ethnicity_edited_o,
      ethnicity_counts_o, 
      ethnicity_edited_counts_o,
      ethnicity_evaluation_o,
      ethnicity_last_reported_o,
      vet_edited_o,
      looked_after_c_edited_o,
      postcode_last_reported_o,
      simd_quintile_o,
      simd_decile_o,
      simd_vigintile_o,

      # 3 - Referral columns -----------------------------------------------
      vec_referral_cols,
      ref_rec_date_opti_o,
    
      # 4 - App columns ----------------------------------------------------
      vec_app_cols,
    
      # 5 - Diagnosis columns ----------------------------------------------
      vec_diag_cols,
    
      # 6 - Treatment columns ----------------------------------------------
      vec_treat_cols,
    
      # 7 - Outcome columns ------------------------------------------------
      vec_outcome_cols,
    
      # 8 - Case closed columns --------------------------------------------
      vec_case_closed_cols,
    
      # 9 - Misc. Columns --------------------------------------------------
      total_rows_o
      ))
}


