
################################################################.
###  Complete ref info for all records of the same pathway   ###
################################################################.

#IMPORTANT
#Needs each unique record (keys: patient_id, ucpn, hb_name and dataset_type)
#to have only one ref rec date.


# 1 Load packages ---------------------------------------------------------

conflict_prefer('arrange','dplyr')


# 2 List of referrals columns ---------------------------------------------

# ref_cols <- c(ref_date_o, ref_rec_date_o, ref_source_o, ref_reason_o, ref_acc_o, # better to use vector specified in new_colnames.R?
#               ref_rej_date_o, ref_rej_reason_o, ref_rej_act_o, act_code_sent_date_o)


# 3 Function --------------------------------------------------------------

complete_ref_date_info <- function(df) {
  df_completed <- df %>% 
    group_by(across(all_of(data_keys))) %>% 
    fill(vec_referral_cols, .direction = "downup") %>% 
    ungroup() %>% 
    mutate(!!sym(ref_rec_date_opti_o) := case_when( # optimised referral received date 
      !is.na(sym(ref_rec_date_o)) ~ sym(ref_rec_date_o),
      is.na(sym(ref_rec_date_o)) ~ sym(ref_date_o),
      TRUE ~ NA_Date_))
  
  return(df_completed)
  
}

df_captnd <- read_parquet("../../../output/df_glob_merged_cleaned.parquet") 

test <- df_captnd %>% 
  select(data_keys, vec_referral_cols) %>% 
  complete_ref_date_info()









