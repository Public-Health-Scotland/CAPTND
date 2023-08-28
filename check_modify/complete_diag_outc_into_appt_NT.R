
########################################################################.
###  Complete diagnosis info for appointments with the same file id  ###
########################################################################.

#IMPORTANT
#Not all diagnosis will be matvhed to an appointment


# 1 Load packages ---------------------------------------------------------

conflict_prefer('arrange','dplyr')


# 2 List of referrals columns ---------------------------------------------

#take vector from here
source('config/new_colnames.R')
grouping_cols=c(data_keys,file_id_o)

# 3 Function --------------------------------------------------------------

complete_diag_outc_into_appt <- function(df) {
  df_completed <- df %>% 
    group_by(across(all_of(grouping_cols))) %>% 
    fill(all_of(vec_diag_cols), .direction="downup") %>% 
    fill(all_of(vec_outcome_cols), .direction="downup") %>% 
    #fill(all_of(vec_app_cols), .direction="downup") %>% 
    ungroup()
  
  return(df_completed)
}









