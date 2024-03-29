############################.
###  Complete LAC status ###
############################.


# 1 Housekeeping ----------------------------------------------------------

#Completes missing records of lac status
#if unknown, status is set as not being a lac in case a further record 
#indicates that the person is not a lac
#if unknown, status is set as being a lac in case a past record indicates
#that person is a lac
# library(dplyr)
# library(tidyr)
# library(lubridate)


# 2 Function --------------------------------------------------------------

complete_lac_status <- function(df){
 
  df_completed <- df %>%
    group_by(!!sym(patient_id_o)) %>% 
    mutate(!!looked_after_c_edited_o := case_when(
      (is.na(!!sym(looked_after_c_o)) | !!sym(looked_after_c_o) %in% c(98, 99)) & 
        lead(!!sym(looked_after_c_o), order_by=!!sym(header_date_o)) == 1 ~ 1,
      (is.na(!!sym(looked_after_c_o)) | !!sym(looked_after_c_o) %in% c(98, 99)) & 
        lag(!!sym(looked_after_c_o),order_by=!!sym(header_date_o)) == 2 ~ 2,
      TRUE ~ !!sym(looked_after_c_o)),
      .after=!!looked_after_c_o)%>% 
    ungroup()
  
  message('Looked after child status completed\n')
  
  return(df_completed)
}

 