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
    mutate(!!chi_o := as.character(!!sym(chi_o)),
           dob_recorded_matches_chi = case_when(ymd(dob_from_chi(!!sym(chi_o)))==dmy(!!sym(dob_o)) ~ TRUE,
                                                ymd(dob_from_chi(!!sym(chi_o)))!=dmy(!!sym(dob_o)) ~ FALSE),
           .after = !!dob_o)
  
  return(df_dob)
}

