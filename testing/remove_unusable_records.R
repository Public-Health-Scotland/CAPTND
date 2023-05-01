################################.
###  remove unusable records ###
################################.

#Removes recprds that don't have one or more of the following:
#UCPN, CHI, DATASET, HEALTH BOARD


# 1 Load libraries and col names ------------------------------------------

source('setup/new_column_names_swift.R')
library(dplyr)
library(readr)
library(lubridate)


# 2 Function --------------------------------------------------------------

remove_unusable_records <- function(df){
  
  df_clean=df %>% filter(!is.na(!!sym(dataset_type_o)) &
                           !is.na(!!sym(chi_o)) &
                           !is.na(!!sym(ucpn_o)) &
                           !is.na(!!sym(hb_name_o)))
  
  df_removed= setdiff(df,df_clean)
  
  df_removed_location=paste0('../../../output/removed/',
                             'remove_unusable_records_',
                             today(),
                             '.csv')
  
  write_csv(df_removed, df_removed_location)
  usable_records=nrow(df_clean)
  unusable_records=nrow(df_removed)
  
  message(paste(unusable_records, 
                'records were removed and saved to\n',
                df_removed_location))
  
  return(df_clean)
}

