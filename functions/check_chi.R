###################.
### Check CHI #####
###################.

#Pads all CHIs with 9 digits using chi_pad from phsmethods
#Checks all CHIs in df using chi_check from phsmethods
#Removes records with invalid CHIs to output file

# Author: Maria Gannon
# Date: 19/04/2023

# 1 Load sources and libraries --------------------------------------------------------
source('setup/new_column_names_swift.R')
source("./functions/save_df_as_parquet.R")
library(dplyr)
library(phsmethods)
library(readr)
library(lubridate)

# 2 chi_check -----------------------------------------------------

check_chi_captnd <- function(df){
  
  df_padded <- df %>% 
    mutate(!!chi_o := chi_pad(!!sym(chi_o)),
           validity = chi_check(!!sym(chi_o)),
           .after=chi) 
  
  df_check_chi <- df_padded %>%
    filter(validity == 'Valid CHI') %>% 
    select(-validity)
  
  df_removed <- df_padded %>% filter(validity != 'Valid CHI') %>% 
    select(-validity)
  
  df_removed_location=paste0('../../../output/removed/',
                             'remove_unusable_chi_',
                             today())
  
  save_as_parquet(df_removed, df_removed_location)
  usable_records=nrow(df_check_chi)
  unusable_records=nrow(df_removed)
  
  message(paste(unusable_records, 
                'records were removed and saved to\n',
                df_removed_location, ".parquet"))
  
  return(df_check_chi)
}



