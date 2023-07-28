
########################################################.
### Controller script for Consolidating Globalscape  ###
########################################################.

#This script makes connection to database, loads SWIFT data, merges CAMHS and PT,
#renames columns.

############## *IMPORTANT* #############
##     Needs at least 5Gb memory      ##
# 1 - Housekeeping --------------------------------------------------------
# 1.1 - Load packages -----------------------------------------------------

library(conflicted)
library(odbc)
library(rstudioapi)
library(dbplyr)
library(purrr)
library(stringr)

# 1.2 Source functions --------------------------------------
source('globalscape_prep/globalscape_column_renamer.R')
source('globalscape_prep/globalscape_data_loader.R')
source('setup/save_df_as_parquet.R')
source('globalscape_prep/save_globalscape_parquet.R')
source('setup/null_to_na.R')
source('check_modify/correct_HB_names.R')
source('check_modify/check_chi.R')
source('check_modify/remove_unusable_records.R')
source('check_modify/pad_chi.R')
source('globalscape_prep/access_glob_parquet_files.R')
source('setup/set_col_data_types.R')
source('check_modify/complete_sex_from_chi.R')
source('check_modify/complete_ethnicity.R')
source('check_modify/check_dob_from_chi_NT.R')
source('check_modify/append_simd_ranks.R')
source('check_modify/complete_lac_status.R')
source('check_modify/complete_veteran_status.R')
source('setup/add_patient_id.R')
source('check_modify/extract_chi_upi_pat_id.R')
source('check_modify/remove_multi_ref_pathways.R')
source('reporting/report_multiple_ethnicities_NT.R')
library(plyr)
library(dplyr)


# 1.3 - Deal with package conflicts ---------------------------------------
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::case_when)
conflicts_prefer(dplyr::order_by)
conflicts_prefer(dplyr::lag)
conflicts_prefer(dplyr::lead)
conflicts_prefer(dplyr::first)

# 2 - Gather globalscape --------------------------------------------------
#to create the parquet files, we use:
# source("./globalscape_prep/globalscape_pull_save_parquet.R")

#load saved parquet files
df_glob_raw <- load_glob_parquet_dfs()


 cleaning_fun <- list(null_to_na, correct_hb_names, pad_chi, add_patient_id, check_chi_captnd)

 df_glob_clean <- df_glob_raw %>% 
   map(cleaning_fun) %>%
   map2(., names(.), remove_unusable_records) %>%
   #map(~select(.x, -!!sym(upi_o))) %>%
   map(~ .x %>% mutate(across(where(is.character), trimws))) 
 
df_chi_upi_patID <- df_glob_clean %>% 
  map(extract_chi_upi_pat_id) %>% 
  bind_rows(.) %>% 
  distinct()
 

#what to do with chi and upi?
df_glob_merged <- df_glob_clean %>% 
  map(~left_join(.x,df_chi_upi_patID)) %>% 
  bind_rows(.) %>% 
  mutate(!!chi_o:=str_replace_all(!!sym(chi_o), " ", ""),
         !!patient_id_o:=str_replace_all(!!sym(patient_id_o), " ", ""))
  
  # reduce(full_join, by = c(ucpn_o, 
  #                          upi_o,
  #                          chi_o,
  #                          chi_valid_o,
  #                          patient_id_o, 
  #                          hb_name_o, 
  #                          dataset_type_o,
  #                          sub_source_o, 
  #                          file_id_o,
  #                          header_date_o,
  #                          record_type_o,
  #                          preg_perinatal_o)) 


df_glob_merged_cleaned <- df_glob_merged %>% 
  set_col_data_types() %>%
  check_dob_from_chi() %>% # need to work on min and max DOBs to help with DOB allocation
  complete_sex_from_chi() %>% 
  complete_ethnicity() %>% 
  complete_veteran_status() %>% 
  complete_lac_status() %>% 
  append_postcode_lookup() %>% 
  remove_multi_ref_pathways()
  
save_as_parquet(df_glob_merged,'../../../output/df_glob_merged')
save_as_parquet(df_glob_merged_cleaned,'../../../output/df_glob_merged_cleaned')

rm(cleaning_fun, df_glob_clean, df_glob_raw, df_glob_merged, df_glob_merged_cleaned,df_chi_upi_patID)  


