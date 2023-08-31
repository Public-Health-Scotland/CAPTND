
################################################################.
###  Complete ref info for all records of the same pathway   ###
################################################################.

#IMPORTANT
#Needs each unique record (keys: patient_id, ucpn, hb_name and dataset_type)
#to have only one ref rec date.


# 1 Load packages ---------------------------------------------------------

conflict_prefer('arrange','dplyr')


# 2 List of referrals columns ---------------------------------------------

ref_cols <- c(ref_date_o, ref_rec_date_o, ref_source_o, ref_reason_o, ref_acc_o, 
              ref_rej_date_o, ref_rej_reason_o, ref_rej_act_o, act_code_sent_date_o)


# 3 Function --------------------------------------------------------------

complete_ref_date_info <- function(df) {
  df_completed <- df %>% 
    group_by(across(all_of(data_keys))) %>% 
    fill(ref_cols, .direction="downup") %>% 
    #mutate(!!sym(ref )) # complete ref rec date...
    ungroup()
  
  
  
  
  return(df_completed)
}











