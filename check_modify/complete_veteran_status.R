################################.
###  Complete veteran status ###
################################.


# 1 Housekeeping ----------------------------------------------------------

#Completes missing records of veteran status
#if unknown, status is set as not being a veteran in case a further record 
#indicates that the person is not a veteran
#if unknown, status is set as being a veteran in case a past record indicates
#that person is a veteran

source('config/new_colnames.R')
# library(dplyr)
# library(tidyr)
# library(lubridate)


# 2 Function --------------------------------------------------------------

complete_veteran_status <- function(df){
  
  df_completed <- df %>%
    group_by(!!sym(patient_id_o)) %>% 
    mutate(!!vet_edited_o := case_when(
      (is.na(!!sym(vet_o)) | !!sym(vet_o) %in% c(98, 99)) & 
        lead(!!sym(vet_o), order_by=!!sym(header_date_o)) == 1 ~ 1,
      (is.na(!!sym(vet_o)) | !!sym(vet_o) %in% c(98, 99)) & 
        lag(!!sym(vet_o),order_by=!!sym(header_date_o)) == 2 ~ 2,
                                       TRUE ~ !!sym(vet_o)),
      .after=!!vet_o)%>% 
    ungroup()
     

  return(df_completed)
}


