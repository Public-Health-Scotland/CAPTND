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


# 2 Function --------------------------------------------------------------

complete_lac_status <- function(df){
 
  df_completed <- df %>%
    group_by(!!sym(chi_o)) %>% 
    mutate(!!lac_edited_o := case_when(
      (is.na(!!sym(lac_o)) | !!sym(lac_o) %in% c(98, 99)) & 
        lead(!!sym(lac_o), order_by=!!sym(header_date_o)) == 1 ~ 1,
      (is.na(!!sym(lac_o)) | !!sym(lac_o) %in% c(98, 99)) & 
        lag(!!sym(lac_o),order_by=!!sym(header_date_o)) == 2 ~ 2,
      TRUE ~ !!sym(lac_o)),
      .after=!!lac_o)
  
  
  return(df_completed)
}



