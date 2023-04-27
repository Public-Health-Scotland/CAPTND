##############################.
###   Compare DOB and CHI  ###
##############################.


# 1 Housekeeping ----------------------------------------------------------

#Compares DOB recorded to DOB from CHI
source('setup/new_column_names_swift.R')
library(dplyr)
library(phsmethods)


# 2 Function --------------------------------------------------------------

check_dob_from_chi <- function(df){
  
  df_dob=df %>%
    mutate(dob_recorded_matches_chi = case_when(dob_from_chi(!!chi_o)==!!sym(chi_o) ~ TRUE,
                                                dob_from_chi(!!chi_o)==!!sym(chi_o) ~ FALSE) )
  
  return(df_dob)
}

