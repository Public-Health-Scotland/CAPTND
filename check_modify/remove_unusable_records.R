################################.
###  remove unusable records ###
################################.

#Removes recprds that don't have one or more of the following:
#UCPN, CHI, DATASET, HEALTH BOARD


# 1 Load libraries and col names ------------------------------------------

source("config/new_colnames.R")
source("setup/save_df_as_parquet.R")
source("reporting/report_unusable_records.R")

library(dplyr)
library(readr)
library(lubridate)


# 2 Function --------------------------------------------------------------

remove_unusable_records <- function(df, stage_name){
  
  df_clean=df %>% filter(!is.na(!!sym(dataset_type_o)) &
                           !is.na(!!sym(patient_id_o)) &
                           !is.na(!!sym(ucpn_o)) &
                           !is.na(!!sym(hb_name_o)))
  
  df_removed= anti_join(df,df_clean, by=data_keys) %>% 
    select(all_of(data_keys),!!header_date_o) %>% 
    mutate(issue='missing UCPN or UPI/CHI') %>% 
    distinct()
  
  write_csv(df_removed,
            paste0('../../../output/removed/details_removed/',
            stage_name,
            '_details_removed_missing_ucpn_chi_upi_',
            DATA_FOLDER_LATEST,
            '.csv'))
 
  report_unusable_records(df, stage_name)
  
  return(df_clean)
}

