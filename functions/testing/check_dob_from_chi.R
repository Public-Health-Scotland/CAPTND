##############################.
###   Compare DOB and CHI  ###
##############################.


# 1 Housekeeping ----------------------------------------------------------

#Compares DOB recorded to DOB from CHI
source('setup/new_column_names_swift.R')
library(dplyr)
library(phsmethods)
library(lubridate)


# 2 Function --------------------------------------------------------------

check_dob_from_chi <- function(df){
  
  stamp_dob = stamp("30/10/2018")
  
  df_dob = df %>%
    mutate(!!chi_o := as.character(!!sym(chi_o)),
           !!dob_from_chi_o := dob_from_chi(!!sym(chi_o)),
           !!dob_recorded_matches_chi_o := case_when(!!sym(dob_from_chi_o) == !!sym(dob_o) ~ TRUE,
                                                     !!sym(dob_from_chi_o) != !!sym(dob_o) ~ FALSE),
           .after = !!dob_o)
  
  return(df_dob)
}

