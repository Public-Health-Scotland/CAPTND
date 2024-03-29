###########################.
###  Complete Postcode ###
###########################.


# 1 Housekeeping ----------------------------------------------------------

#Comppletes records with latest reported postcode
# library(dplyr)
# library(tidyr)


# 2 Function --------------------------------------------------------------

complete_postcode <- function(df){
  
  df_completed <- df %>%
    group_by(across(all_of(data_keys))) %>% 
    mutate(!!postcode_last_reported_o := last(!!sym(postcode_o),order_by=!!sym(header_date_o), na_rm = TRUE),
           .after = !!postcode_o) %>% 
    ungroup()
  
  message('Postcode completed\n')

  return(df_completed)
}
