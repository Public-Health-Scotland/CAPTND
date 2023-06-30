############################.
###  Complete LAC status ###
############################.


# 1 Housekeeping ----------------------------------------------------------

#Completes missing records of lac status
#if unknown, status is set as not being a lac in case a further record 
#indicates that the person is not a lac
#if unknown, status is set as being a lac in case a past record indicates
#that person is a lac
source('setup/new_column_names_swift.R')
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)


# 2 Function --------------------------------------------------------------

complete_lac_status <- function(df){
 
  df_completed <- df %>%
    group_by(!!sym(chi_o)) %>% 
    mutate(!!looked_after_c_edited_o := case_when(
      (is.na(!!sym(looked_after_c_o)) | !!sym(looked_after_c_o) %in% c(98, 99)) & 
        lead(!!sym(looked_after_c_o), order_by=!!sym(header_date_o)) == 1 ~ 1,
      (is.na(!!sym(looked_after_c_o)) | !!sym(looked_after_c_o) %in% c(98, 99)) & 
        lag(!!sym(looked_after_c_o),order_by=!!sym(header_date_o)) == 2 ~ 2,
      TRUE ~ !!sym(looked_after_c_o)),
      .after=!!looked_after_c_o)
  
  
  return(df_completed)
}

 