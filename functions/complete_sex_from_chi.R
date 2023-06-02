##############################.
###  Complete sex from CHI ###
##############################.


# 1 Housekeeping ----------------------------------------------------------

#Compares DOB recorded to DOB from CHI
source('setup/new_column_names_swift.R')
library(dplyr)
library(phsmethods)
library(lubridate)


# 2 Function --------------------------------------------------------------

complete_sex_chi <- function(df){
  
  df_sex <- df %>%
    mutate(!!chi_o := as.character(!!sym(chi_o)),
           !!sex_from_chi_o := sex_from_chi(!!sym(chi_o)),
           !!sex_o := case_when(is.na(!!sym(sex_o)) ~ !!sym(sex_from_chi_o),
                                TRUE ~ !!sym(sex_o)),
           !!sex_recorded_matches_chi_o := case_when(!!sym(sex_from_chi_o) == !!sym(sex_o) ~ TRUE,
                                                     !!sym(sex_from_chi_o) != !!sym(sex_o) ~ FALSE),
           .after = !!sex_o
    )
  
  
  return(df_sex)
}

