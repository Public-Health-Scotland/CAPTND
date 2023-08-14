###########################.
###  Complete Postcode ###
###########################.


# 1 Housekeeping ----------------------------------------------------------

#Comppletes records with latest reported postcode
source('config/new_colnames.R')
library(dplyr)
library(tidyr)


# 2 Function --------------------------------------------------------------

complete_postcode <- function(df){
  
  df_completed <- df %>%
    group_by(across(all_of(data_keys))) %>% 
    mutate(!!postcode_last_reported_o := last(!!sym(postcode_o),order_by=!!sym(header_date_o), na_rm = TRUE),
           .after = !!postcode_o) %>% 
    ungroup()
  

  return(df_completed)
}
