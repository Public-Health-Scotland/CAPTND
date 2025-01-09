
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
    fill(all_of(setdiff(vec_outcome_cols, c("cgi_i", "pgi_i", "cgi_s"))), .direction="downup") %>% # excluded new outcome measures as they do not need filled
    ungroup()
  
  message('Diagnosis, treatment & outcome columns completed\n')
  
  return(df_completed)
}









