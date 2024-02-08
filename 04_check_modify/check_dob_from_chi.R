##############################.
###   Compare DOB and CHI  ###
##############################.


# 1 Housekeeping ----------------------------------------------------------

#Compares DOB recorded to DOB from CHI
source('reporting/report_dob_conflicting.R')
# library(dplyr)
# library(phsmethods)
# library(lubridate)
# library(tidyr)


# 2 Function --------------------------------------------------------------
check_dob_from_chi <- function(df){
  
  df_dob <- df %>%
    mutate(!!chi_o := as.character(!!sym(chi_o)),
           !!dob_from_chi_o := dob_from_chi(!!sym(chi_o), min_date = ymd(19200101), max_date = today()),
           !!dob_recorded_matches_chi_o := case_when(is.na(!!sym(dob_from_chi_o)) ~ 'no dob from chi',
                                                     !!sym(dob_from_chi_o) == !!sym(dob_o) ~ 'match',
                                                     !!sym(dob_from_chi_o) != !!sym(dob_o) ~ 'no match',
                                                     TRUE ~ 'no reported dob'),
           !!dob_verified_o := case_when(!!sym(dob_recorded_matches_chi_o) == 'no dob from chi' ~ !!sym(dob_o),
                                         TRUE ~ !!sym(dob_from_chi_o)),
            .after = !!dob_o
            ) %>%
    group_by(!!sym(patient_id_o)) %>%
    fill(!!sym(dob_verified_o), .direction = "downup") %>%
    ungroup()
  
  report_dob_conflicting(df_dob)
  
  return(df_dob)
}

