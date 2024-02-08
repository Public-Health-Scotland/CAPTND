
########################################################.
### Controller script for Consolidating Globalscape  ###
########################################################.

#This script makes connection to database, loads SWIFT data, merges CAMHS and PT,
#renames columns.

############## *IMPORTANT* #############
##     Needs at least 5Gb memory      ##
# 1 - Housekeeping --------------------------------------------------------
# 1.1 - Load packages -----------------------------------------------------

# library(conflicted)
# library(odbc)
# library(rstudioapi)
# library(dbplyr)
# library(purrr)
# library(stringr)

# 1.2 Source functions --------------------------------------
source('03_globalscape_prep/globalscape_column_renamer.R')
source('03_globalscape_prep/globalscape_data_loader.R')
source('02_setup/save_df_as_parquet.R')
source('03_globalscape_prep/save_globalscape_parquet.R')
source('02_setup/null_to_na.R')
source('04_check_modify/correct_HB_names.R')
source('04_check_modify/check_chi.R')
source('04_check_modify/remove_unusable_records.R')
source('04_check_modify/pad_chi.R')
source('03_globalscape_prep/access_glob_parquet_files.R')
source('02_setup/set_col_data_types.R')
source('04_check_modify/check_sex_from_chi.R')
source('04_check_modify/complete_ethnicity.R')
source('04_check_modify/check_dob_from_chi.R')
source('04_check_modify/append_simd_ranks.R')
source('04_check_modify/complete_lac_status.R')
source('04_check_modify/complete_veteran_status.R')
source('02_setup/add_patient_id.R')
source('04_check_modify/extract_chi_upi_pat_id.R')
source('04_check_modify/remove_multi_ref_pathways.R')
source('05_data_quality/report_multiple_ethnicities.R')
# library(plyr)
# library(dplyr)


# 1.3 - Deal with package conflicts ---------------------------------------
# conflicts_prefer(dplyr::rename)
# conflicts_prefer(dplyr::filter)
# conflicts_prefer(dplyr::select)
# conflicts_prefer(dplyr::mutate)
# conflicts_prefer(dplyr::summarise)
# conflicts_prefer(dplyr::case_when)
# conflicts_prefer(dplyr::order_by)
# conflicts_prefer(dplyr::lag)
# conflicts_prefer(dplyr::lead)
# conflicts_prefer(dplyr::first)

# 2 - Gather globalscape --------------------------------------------------
#to create the parquet files, we use:
# source("./03_globalscape_prep/globalscape_pull_save_parquet.R")

#load saved parquet files
df_glob_raw <- load_glob_parquet_dfs()


 cleaning_fun <- list(null_to_na, correct_hb_names, 
                      remove_spaces_data_keys, pad_chi, 
                      add_patient_id, check_chi_captnd)

 df_glob_clean <- df_glob_raw %>% 
   map(cleaning_fun) %>%
   map2(., names(.), remove_unusable_records) %>%
   map(~ .x %>% mutate(across(where(is.character), trimws))) 
 
df_chi_upi_patID <- df_glob_clean %>% 
  map(extract_chi_upi_pat_id) %>% 
  bind_rows(.) %>% 
  distinct()
 
df_glob_merged <- df_glob_clean %>% 
  map(~left_join(.x,df_chi_upi_patID)) %>% 
  bind_rows(.)

save_as_parquet(df_glob_merged,'../../../output/globalscape_final_data/df_glob_merged')

