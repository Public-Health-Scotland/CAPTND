
##########################################################.
###  Complete case closed and started treatment dates  ###
##########################################################.

#IMPORTANT


# 1 Function --------------------------------------------------------------

complete_case_closed_treat_start_date <- function(df) {
  df_completed <- df %>% 
    group_by(across(all_of(data_keys))) %>% 
    fill(!!case_closed_date_o, .direction="downup") %>% 
    #fill(!!treat_start_date_o, .direction="downup") %>% ##treat_start_date refers to the start date of an individual treatment e.g. CBT, not overall treatment start, could be multiple per pathway
    distinct() %>%
    ungroup()
  
  message('Data undiagonalised! Hurray!!\n')
  return(df_completed)
}


