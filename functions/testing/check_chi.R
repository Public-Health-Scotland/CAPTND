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
library(dplyr)
library(phsmethods)
library(readr)
library(lubridate)

# 2 chi_check -----------------------------------------------------

check_chi_captnd <- function(df){
  
  df_check_chi <- df %>% 
    mutate(!!chi_o := chi_pad(!!sym(chi_o)),
           validity = chi_check(!!sym(chi_o))) %>%
    filter(validity == 'Valid CHI') %>% 
    select(-validity)
  
  return(df_check_chi)
}



