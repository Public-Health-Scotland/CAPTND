#######################################.
###   Complete DOB from patient ID  ###
#######################################.


# 1 Housekeeping ----------------------------------------------------------

#Fills NA DOBs with DOB found for the same patient using patient ID
source('config/new_colnames.R')
library(dplyr)
library(lubridate)


# 2 Function --------------------------------------------------------------
complete_dob_from_patient_id <- function(df){
  
  df_dob_pat_id <- df %>%
    group_by(!!(patient_id_o)) %>% 
    mutate(!!dob_o := case_when(
      !!sym(dob_o) > today() ~ NA_Date_,
      TRUE ~ !!sym(dob_o)),
      !!last_dob_from_pat_id_o := last(
        !!sym(dob_o), order_by=!!sym(header_date_o), na_rm = TRUE),
      !!dob_from_pat_id_matches_record := case_when(
        !!sym(last_dob_from_pat_id_o)==!!sym(dob_o) ~ TRUE,
        TRUE ~ FALSE),
      .after = !!dob_o)
  
  df_dob_conflicting <- df_dob %>% filter(!!sym(dob_from_pat_id_matches_record)==FALSE)
  
  #write_csv(df_dob_conflicting, paste0('../../../output/dob_conflicting_', today(), '.csv'))
  
  return(df_dob)
}

