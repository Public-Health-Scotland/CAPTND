
########################################################################.
###  Complete diagnosis info for appointments with the same file id  ###
########################################################################.

#IMPORTANT
#Not all diagnosis will be matched to an appointment


# 1 Load packages ---------------------------------------------------------

#conflict_prefer('arrange','dplyr')


# 2 List of referrals columns ---------------------------------------------

#take vector from here
grouping_cols=c(data_keys,file_id_o)

# 3 Function --------------------------------------------------------------

complete_diag_outc_appt <- function(df) {
  df_completed <- df %>% 
    group_by(across(all_of(grouping_cols))) %>% 
    fill(all_of(c(vec_diag_cols, vec_treat_cols)), .direction="downup") %>% 
    fill(all_of(vec_outcome_cols), .direction="downup") %>% 
    ungroup()
  
  return(df_completed)
}









