
########################################################.
### Controller script for Consolidating Swift  ###
########################################################.

#This script makes connection to database, loads SWIFT data, merges CAMHS and PT,
#renames columns.

############## *IMPORTANT* #############
##     Needs at least 12GiB memory      ##
# 1 - Housekeeping --------------------------------------------------------
# 1.1 - Load packages -----------------------------------------------------

# library(conflicted)
# library(odbc)
# library(rstudioapi)
# library(dbplyr)
# library(purrr)
# library(stringr)
# library(plyr)
# library(dplyr)
# library(crayon)
# library(beepr)

  # 1.2 Source functions --------------------------------------
  source('setup/swift_column_renamer.R')
  source('setup/save_df_as_parquet.R')
  source('setup/null_to_na.R')
  source('check_modify/correct_HB_names.R')
  source('check_modify/remove_spaces_data_keys.R')
  source('check_modify/check_chi.R')
  source('check_modify/remove_unusable_records.R')
  source('check_modify/pad_chi.R')
  source('setup/set_col_data_types.R')
  source('check_modify/check_sex_from_chi.R')
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
  source('check_modify/complete_postcode.R')
  source('check_modify/complete_diag_outc_appt.R')
  source('check_modify/append_age_variables.R')
  source('reporting/report_removed_rows.R')
  source('reporting/report_details_removed_rows.R')
  source('check_modify/add_started_treat_status.R')
  source('check_modify/append_local_authority_res.R')
  source('check_modify/add_ref_appt_discharge_month.R')
  source('check_modify/add_rtt_eval.R')
  source('check_modify/add_sub_source_eval.R')
  
  
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
  
read_clean_captnd_data <- function() {
    
  cat(green('CAPTND data will e read and cleaned.\nThis is a huge dataset, so be patient.\nTakes about 30 minutes.\n\n'))
  cat(green('Pay attention to messages.\n\n')) 
  # 2 - Load SWIFT data --------------------------------------------------
  # measure run time
  start_time <- Sys.time()
  
  # pull swift data from database (run every time updated data required)
  #source("./setup/swift_pull_save_parquet.R")
  
  # load saved parquet files
 
  df_swift_raw <- read_parquet(paste0(root_dir, "/swift_extract.parquet"))
  
  # clean swift data
  df_swift_clean <- df_swift_raw %>%
    null_to_na() %>% 
    correct_hb_names() %>% 
    remove_spaces_data_keys() %>% 
    pad_chi() %>% 
    add_patient_id() %>% 
    check_chi_captnd() %>% 
    filter_non_unique_upi(., "swift") %>% 
    remove_unusable_records(., "swift") %>% 
    remove_multi_ref_pathways(., "swift") %>% 
    mutate(!!sym(sub_source_o) := 'swift',
           !!sym(record_type_o) := NA_character_,
           !!sym(file_id_o) := as.character(!!sym(file_id_o)))
  
  #For reporting on removed rows run the following
  report_removed_rows_details()
  report_removed_rows()
    
  
  df_glob_clean <- read_parquet(paste0('../../../output/globalscape_final_data/df_glob_merged.parquet')) %>% 
    mutate(!!sym(sub_source_o) := 'globalscape',
           !!sym(line_no_o) := NA_real_,
           !!sym(unav_days_no_o) := as.numeric(!!sym(unav_days_no_o)))
  
  
  df_glob_swift <- bind_rows(df_swift_clean, df_glob_clean) 
  
  save_as_parquet(df_glob_swift,paste0(root_dir,'/swift_glob_merged'))
  
  rm(df_swift_raw,df_swift_clean, df_glob_clean)
   
  
  # complete swift data 
  df_glob_swift_completed <- df_glob_swift %>% 
    set_col_data_types() %>%
    check_dob_from_chi() %>% # speak to chili team about ambiguous birth year
    check_sex_from_chi() %>% 
    complete_ethnicity() %>% 
    complete_veteran_status() %>% 
    complete_lac_status() %>%
    complete_postcode() %>% 
    append_postcode_lookup() %>% 
    # append_local_authority_res() |> # still to test
    complete_ref_date_info() %>% 
    complete_diag_outc_appt() %>% 
    append_age_vars() %>% 
    filter(!!sym(ref_rec_date_opti_o) > ymd(20190601)) %>% 
    add_sub_source_eval() %>% 
    add_ref_appt_discharge_month()
  
  save_as_parquet(df_glob_swift_completed, paste0(root_dir,'/swift_glob_completed'))
  
  #add RTT evaluation
  df_glob_swift_completed_rtt <- add_rtt_eval(df_glob_swift_completed, evalAllData=FALSE)
  
  #add column with info on 'had first treat appt'
  #df_glob_swift_completed_rtt <- add_started_treat_status(df_glob_swift_completed_rtt)
  
  save_as_parquet(df_glob_swift_completed_rtt, paste0(root_dir,'/swift_glob_completed_rtt'))
  
  end_time <- Sys.time()
  
  duration=end_time - start_time
  
  cat(green('CAPTND data read and cleaned! \nThis process took', format(duration,usetz = TRUE), '\n\n'))
}

#takes about 30 minutes and 12 GiB

read_clean_captnd_data()






