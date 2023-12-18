
##########################################################.
###  Complete case closed and started treatment dates  ###
##########################################################.

#IMPORTANT


# 1 Function --------------------------------------------------------------

complete_case_closed_treat_start_date <- function(df) {
  df_completed <- df %>% 
    group_by(across(all_of(data_keys))) %>% 
    fill(!!case_closed_date_o, .direction="downup") %>% 
    fill(!!treat_start_date_o, .direction="downup") %>%
    distinct() %>%
    ungroup()
  
  return(df_completed)
}


