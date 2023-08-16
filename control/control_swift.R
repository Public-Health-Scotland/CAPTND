
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
source('check_modify/check_dob_from_chi.R')
source('check_modify/append_simd_ranks.R')
source('check_modify/complete_lac_status.R')
source('check_modify/complete_veteran_status.R')
source('setup/add_patient_id.R')
source('reporting/report_multiple_ethnicities.R')
source('check_modify/remove_multi_ref_pathways.R')
source('check_modify/complete_ref_date_info.R')
source('check_modify/remove_pat_upi_mult_chi.R')
source('check_modify/complete_postcode_NT.R')
source("setup/load_swift_latest_NT.R")
source('reporting/report_removed_upi_mult_chi_NT.R')
source('reporting/report_multiple_ref_per_journey_NT.R')

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
conflicts_prefer(dplyr::filter)


# 2 - Load SWIFT data --------------------------------------------------

# pull swift data from database (run everytime updated data required)
# source("./setup/swift_pull_save_parquet.R")

# load saved parquet files
df_swift_raw <- load_swift_latest_NT()

# clean swift data
df_swift_clean <- df_swift_raw %>%
  null_to_na() %>% 
  correct_hb_names() %>% 
  pad_chi() %>% 
  add_patient_id() %>% 
  check_chi_captnd() %>% 
  filter_non_unique_upi(., "swift") %>% 
  remove_unusable_records(., "swift") %>% 
  remove_multi_ref_pathways(., "swift") %>% 
  mutate(across(where(is.character), trimws))
  
# complete swift data (as far as possible)
df_swift_clean_completed <- df_swift_clean %>% 
  set_col_data_types() %>%
  check_dob_from_chi() %>% # need to work on min and max DOBs to help with DOB allocation
  complete_sex_from_chi() %>% 
  complete_ethnicity() %>% 
  complete_veteran_status() %>% 
  complete_lac_status() %>% 
  complete_postcode() %>% 
  append_postcode_lookup() 

save_as_parquet(df_swift_clean,'../../../output/df_swift_clean')
save_as_parquet(df_swift_clean_completed,'../../../output/df_swift_clean_completed')

rm(df_swift_raw, df_swift_clean)  

df_swift_clean_completed <- read_parquet('../../../output/df_swift_clean_completed.parquet') %>% 
  mutate(!!sym(sub_source_o) := 'swift',
         !!sym(record_type_o) := NA_character_)

glob_ready <- read_parquet('../../../output/df_glob_merged_cleaned.parquet') %>% 
  mutate(!!sym(sub_source_o) := 'globalscape',
         !!sym(line_no_o) := NA_real_)

df_glob_swift <- bind_rows(df_swift_clean_completed, glob_ready) 

rm(df_swift_clean_completed,glob_ready)

df_glob_swift_refs <- complete_ref_date_info(df_glob_swift)

df_glob_swift_refs2 <- complete_diag_outc_into_appt(df_glob_swift_refs)
save_as_parquet(df_glob_swift_refs2,'../../../output/df_glob_swift_refs2')

# df_glob_swift_refs2 <- read_parquet('../../../output/df_glob_swift_refs2.parquet') # load CAPTND quickly (placeholder)


