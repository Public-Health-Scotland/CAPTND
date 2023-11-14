###################.
### Check CHI #####
###################.

#Pads all CHIs with 9 digits using chi_pad from phsmethods
#Checks all CHIs in df using chi_check from phsmethods
#Removes records with invalid CHIs to output file

# Author: Maria Gannon
# Date: 19/04/2023

# 1 Load sources and libraries --------------------------------------------------------
source('config/new_colnames.R')
source("setup/save_df_as_parquet.R")
library(dplyr)
library(phsmethods)
library(readr)
library(lubridate)

# 2 chi_check -----------------------------------------------------

check_chi_captnd <- function(df){
  
  df_checked <- df %>% 
    mutate(!!chi_valid_o := chi_check(!!sym(chi_o)),
           .after= !!chi_o) 
  
  return(df_checked)
}



