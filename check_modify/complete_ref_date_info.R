
################################################################.
###  Complete ref info for all records of the same pathway   ###
################################################################.

#IMPORTANT
#Needs each unique record (keys: patient_id, ucpn, hb_name and dataset_type)
#to have only one ref rec date.


# 1 Load packages ---------------------------------------------------------



# 2 Function --------------------------------------------------------------

complete_ref_date_info <- function(df) {
  df_completed <- df %>%
    mutate(!!ref_rec_date_opti_o := case_when( # optimised referral received date 
      !is.na(!!sym(ref_rec_date_o)) ~ !!sym(ref_rec_date_o),
      is.na(!!sym(ref_rec_date_o)) ~ !!sym(ref_date_o),
      TRUE ~ NA_Date_)) %>% 
    group_by(across(all_of(data_keys))) %>% 
    fill(all_of(vec_referral_cols), .direction = "downup") %>% 
    ungroup() 
  
  return(df_completed)
  
}





