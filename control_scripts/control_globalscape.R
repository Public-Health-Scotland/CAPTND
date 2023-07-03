
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
source('./functions/swift_column_renamer.R')
source('functions/globalscape_column_renamer.R')
source('functions/globalscape_data_loader.R')
source('functions/save_df_as_parquet.R')
source('functions/save_globalscape_parquet.R')
source('functions/null_to_na.R')
source('functions/correct_HB_names.R')
source('functions/check_chi.R')
source('functions/remove_unusable_records.R')
source('functions/pad_chi.R')
source('functions/access_glob_parquet_files.R')
source('functions/set_col_data_types.R')
source('functions/complete_sex_from_chi.R')
source('functions/complete_ethnicity.R')
source('functions/check_dob_from_chi.R')
#source('functions/not_tested/load_test_data.R')
source('functions/append_simd_ranks.R')
source('functions/complete_lac_status.R')
source('functions/complete_veteran_status.R')
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
#to create the parquet files, we used the pull_globalscape_save_parquet.R 
#in the control scripts folder

#load saved parquet files
df_glob_raw <- load_glob_parquet_dfs()

# try functions against test data
 # df_glob_raw <- list(read_csv("../../../data/testDataset_lowercase.csv"))
 # names(df_glob_raw)=c('test')

 cleaning_fun <- list(null_to_na, correct_hb_names, pad_chi)

 df_glob_clean <- df_glob_raw %>% 
   map(cleaning_fun) %>%
   map2(.,names(.), check_chi_captnd) %>%
   map2(., names(.), remove_unusable_records) %>%
   map(~select(.x, -!!sym(upi_o))) %>%
   map(~ .x %>% mutate(across(where(is.character), trimws))) 


df_glob_merged <- df_glob_clean %>% 
  reduce(full_join, by = c(ucpn_o, 
                           chi_o, 
                           hb_name_o, 
                           dataset_type_o,
                           sub_source_o, 
                           file_id_o,
                           header_date_o,
                           record_type_o,
                           preg_perinatal_o)) 


df_glob_merged_cleaned <- df_glob_merged %>% 
  set_col_data_types() %>%
  complete_ethnicity() %>% 
  complete_veteran_status() %>%
  complete_lac_status() %>%
  append_postcode_lookup()

#takes ~ 5 min

rm(cleaning_fun, df_glob_clean, df_glob_raw)  


