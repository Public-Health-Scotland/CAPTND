
########################################################.
### Controller script for Consolidating Swift  ###
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
source('functions/add_patient_id.R')
source('functions/functions_not_tested_yet/report_multiple_ethnicities.R')
library(plyr)
library(dplyr)


# 1.3 - Deal with package conflicts ---------------------------------------
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::case_when)
conflicts_prefer(dplyr::lag)
conflicts_prefer(dplyr::lead)
conflicts_prefer(dplyr::first)
conflicts_prefer(dplyr::last)


# 2 - Gather globalscape --------------------------------------------------
#to create the parquet files, we used the pull_globalscape_save_parquet.R 
#in the control scripts folder

#load saved parquet files
df_swift_raw <- read_parquet("../../../output/swift.parquet")


df_swift_clean <- df_swift_raw %>%
  null_to_na() %>% 
  correct_hb_names() %>% 
  pad_chi() %>% 
  add_patient_id() %>% 
  check_chi_captnd() %>% 
  remove_unusable_records(., "swift") %>% 
  mutate(across(where(is.character), trimws))
  



df_swift_clean2 <- df_swift_clean %>% 
  set_col_data_types() %>%
  #check_dob_from_chi() %>% # need to work on min and max DOBs to help with DOB allocation
  complete_sex_from_chi() %>% 
  complete_ethnicity() %>% 
  complete_veteran_status() %>% 
  complete_lac_status() %>% 
  append_postcode_lookup()



rm(cleaning_fun, df_glob_clean, df_glob_raw)  



