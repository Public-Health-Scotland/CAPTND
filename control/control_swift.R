
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
source('setup/swift_column_renamer.R')
source('setup/save_df_as_parquet.R')
source('setup/null_to_na.R')
source('check_modify/correct_HB_names.R')
source('check_modify/check_chi.R')
source('check_modify/remove_unusable_records.R')
source('check_modify/pad_chi.R')
source('setup/set_col_data_types.R')
source('check_modify/complete_sex_from_chi.R')
source('check_modify/complete_ethnicity.R')
source('check_modify/check_dob_from_chi_NT.R')
source('check_modify/append_simd_ranks.R')
source('check_modify/complete_lac_status.R')
source('check_modify/complete_veteran_status.R')
source('setup/add_patient_id.R')
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
  



df_swift_clean_completed <- df_swift_clean %>% 
  set_col_data_types() %>%
  #check_dob_from_chi() %>% # need to work on min and max DOBs to help with DOB allocation
  complete_sex_from_chi() %>% 
  complete_ethnicity() %>% 
  complete_veteran_status() %>% 
  complete_lac_status() %>% 
  append_postcode_lookup()



rm(df_swift_raw, df_swift_clean)  



