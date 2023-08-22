##############################.
###   Compare DOB and CHI  ###
##############################.


# 1 Housekeeping ----------------------------------------------------------

#Compares DOB recorded to DOB from CHI
source('config/new_colnames.R')
library(dplyr)
library(phsmethods)
library(lubridate)
library(tidyr)


# 2 Function --------------------------------------------------------------
check_dob_from_chi <- function(df){
  
  df_dob <- df %>%
    mutate(!!chi_o := as.character(!!sym(chi_o)),
           !!dob_from_chi_o := dob_from_chi(!!sym(chi_o), min_date = ymd(19200101), max_date = today()),
           !!dob_recorded_matches_chi_o := case_when(!!sym(dob_from_chi_o) == !!sym(dob_o) ~ 'match',
                                                     !!sym(dob_from_chi_o) != !!sym(dob_o) ~ 'no match',
                                                     TRUE ~ 'no dob from chi'),
           !!dob_verified_o := case_when(!!sym(dob_recorded_matches_chi_o) == 'match' ~ !!sym(dob_from_chi_o),
                                         !!sym(dob_recorded_matches_chi_o) == 'no match' ~ !!sym(dob_from_chi_o),
                                         !!sym(dob_recorded_matches_chi_o) == 'no dob from chi' ~ !!sym(dob_o)),
            .after = !!dob_o
            ) %>%
    group_by(!!sym(patient_id_o)) %>%
    fill(!!sym(dob_verified_o), .direction = "downup") %>%
    ungroup()
  
  
  
  # df_dob_conflicting <- df_dob %>% 
  #   filter(!!sym(dob_recorded_matches_chi_o)!='match') %>% 
  #   select(data_keys,!!chi_o,!!dob_from_chi_o,!!dob_o,!!dob_recorded_matches_chi_o,!!dob_verified_o) %>% 
  #   distinct()

  
  
  #write_csv(df_dob_conflicting, paste0('../../../output/dob_conflicting_', today(), '.csv'))
  
  return(df_dob)
}

