###########################.
###  Check sex from CHI ###
###########################.


# 1 Housekeeping ----------------------------------------------------------

#Compares DOB recorded to DOB from CHI
source('config/new_colnames.R')
library(dplyr)
library(phsmethods)
library(lubridate)
library(tidyr)


# 2 Function --------------------------------------------------------------

check_sex_from_chi <- function(df){
  
  df_sex <- df %>%
    group_by(!!sym(patient_id_o)) %>%
    fill(!!sym(sex_o), .direction = "downup") %>%
    ungroup() %>% 
    mutate(!!chi_o := as.character(!!sym(chi_o)),
           !!sex_o := as.numeric(!!sym(sex_o)),
           !!sex_from_chi_o := sex_from_chi(!!sym(chi_o)),
           !!sex_recorded_matches_chi_o := case_when(is.na(!!sym(sex_from_chi_o)) ~ 'no sex from chi info',
                                                     !!sym(sex_from_chi_o) == !!sym(sex_o) ~ 'match',
                                                     !!sym(sex_from_chi_o) != !!sym(sex_o) ~ 'no match',
                                                     TRUE ~ 'no og sex info'),
           !!sex_reported_o := case_when(!!sym(sex_recorded_matches_chi_o) == 'no og sex info' ~ !!sym(sex_from_chi_o),
                                         TRUE ~ !!sym(sex_o)),
           .after = !!sex_o) 
    
   
  
  return(df_sex)
}

