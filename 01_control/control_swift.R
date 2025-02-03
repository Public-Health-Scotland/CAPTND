
##############################################################.
### Controller script for consolidating Swift submissions  ###
##############################################################.

# This script makes connection to database, loads SWIFT data, merges CAMHS and PT,
# and renames columns.

# NB Needs at least 17GB memory! Always request 10-20% extra


# 1 - Housekeeping --------------------------------------------------------

# 1.2 Source functions --------------------------------------
source('02_setup/swift_column_renamer.R')
source('02_setup/save_df_as_parquet.R')
source('02_setup/null_to_na.R')
source('04_check_modify/correct_HB_names.R')
source('04_check_modify/correct_hb_names_simple.R')
source('04_check_modify/remove_spaces_data_keys.R')
source('04_check_modify/check_chi.R')
source('04_check_modify/remove_unusable_records.R')
source('04_check_modify/pad_chi.R')
source('02_setup/set_col_data_types.R')
source('02_setup/set_column_order.R')
source('04_check_modify/check_sex_from_chi.R')
source('04_check_modify/complete_ethnicity.R')
source('04_check_modify/check_dob_from_chi.R')
source('04_check_modify/append_simd_ranks.R')
source('04_check_modify/complete_lac_status.R')
source('04_check_modify/complete_veteran_status.R')
source('02_setup/add_patient_id.R')
source('05_data_quality/report_multiple_ethnicities.R')
source('04_check_modify/remove_multi_ref_pathways.R')
source('04_check_modify/complete_ref_date_info.R')
source('04_check_modify/filter_non_unique_upi.R')
source('04_check_modify/complete_postcode.R')
source('04_check_modify/complete_diag_outc_appt.R')
source('04_check_modify/append_age_variables.R')
source('05_data_quality/report_removed_rows.R')
source('05_data_quality/report_details_removed_rows.R')
source("05_data_quality/report_missing_referrals.R")
source("05_data_quality/report_apps_after_ref_rej.R")
#source('04_check_modify/add_started_treat_status.R')
source('04_check_modify/append_local_authority_res.R')
source('04_check_modify/add_ref_appt_discharge_month.R')
source('04_check_modify/add_rtt_eval.R')
source('04_check_modify/add_sub_source_eval.R')
source('04_check_modify/complete_case_closed_start_treat_date.R')
source('04_check_modify/add_new_return_apps.R')
source('04_check_modify/id_app_after_case_closed.R')
source('05_data_quality/flag_data_after_subm_date.R')
source('04_check_modify/add_urban_rural_class.R')
source('04_check_modify/d&g_ucpn_update_fix.R')
source('04_check_modify/remove_lead0s_treat_int.R')
source('04_check_modify/add_optimised_ref_acceptance.R')


# 1.3 - Set preamble -------------------------------------------------------
  
read_clean_captnd_data <- function() {
    
cat(green('CAPTND data will be read and cleaned.\nThis is a huge dataset, so be patient.\nTakes about 90 minutes.\n\n'))
cat(green('Pay attention to messages.\n\n')) 

# 2 - Load SWIFT data ------------------------------------------------------
# measure run time
start_time <- Sys.time()
start_time

# pull swift data from database (run every time updated data required)
source("./02_setup/swift_pull_save_parquet.R") # where directory structure is applied

rm(swift_all)
gc()
# load saved parquet files

df_swift_clean <- read_parquet(paste0(root_dir, "/swift_extract.parquet")) |> 

# clean swift data
#df_swift_clean <- df_swift_raw %>%
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
         !!sym(file_id_o) := as.character(!!sym(file_id_o))) |> 
  remove_lead_0s_treat_int()
  
df_glob_clean <- read_parquet(paste0('../../../output/globalscape_final_data/df_glob_merged.parquet')) %>% 
  mutate(!!sym(sub_source_o) := 'globalscape',
         !!sym(line_no_o) := NA_real_,
         !!sym(unav_days_no_o) := as.numeric(!!sym(unav_days_no_o)))


df_glob_swift <- bind_rows(df_swift_clean, df_glob_clean) 
 

df_glob_swift_data_types_set <- df_glob_swift %>% 
  set_col_data_types() |> 
  set_column_order() |> 

  save_as_parquet(paste0(root_dir,'/swift_glob_merged'))

rm(df_swift_clean, df_glob_clean, 
   df_glob_swift, df_glob_swift_data_types_set)
gc()
#For 05_data_quality data after submission date
#df_glob_swift_data_types_set <- read_parquet(paste0(root_dir,'/swift_glob_merged.parquet'))
#flag_data_after_subm_date(df_glob_swift_data_types_set)


# complete swift data 
df_glob_swift_completed_rtt <- read_parquet(paste0(root_dir,'/swift_glob_merged.parquet')) %>%
  dumfries_ucpn_fix() %>%
  complete_ref_date_info() %>% 
  filter(!!sym(ref_rec_date_opti_o) >= ymd(20190601) | 
           is.na(!!sym(ref_rec_date_opti_o))) %>% # na inclusion added 30/1/25 to prevent exclusion of pathways missing ref info
  check_dob_from_chi() %>% # speak to chili team about ambiguous birth year
  check_sex_from_chi() %>%
  complete_ethnicity() %>%
  complete_veteran_status() %>%
  complete_lac_status() %>%
  complete_postcode() %>%
  append_postcode_lookup() %>%
  #append_local_authority_res() %>% # not really needed
  complete_diag_outc_appt() %>% 
  complete_case_closed_treat_start_date() %>% #now only completes case_closed_date
  append_age_vars() %>%
  add_sub_source_eval() %>%
  add_ref_appt_discharge_month() %>%
  add_rtt_eval(., evalAllData=FALSE) %>% 
  add_new_return_apps() %>%
  add_urban_rural_class() |> 
  add_optimised_ref_acceptance()

# For complete data including globalscape and swift entries, please run the 
#former scripts again with add_rtt_eval(., evalAllData=TRUE)

# data quality checks
report_missing_referrals(df_glob_swift_completed_rtt)
report_apps_after_ref_rej(df_glob_swift_completed_rtt)
#For 05_data_quality on removed rows run the following
#report_removed_rows_details()
report_removed_rows()


save_as_parquet(df_glob_swift_completed_rtt, paste0(root_dir,'/swift_glob_completed_rtt'))

end_time <- Sys.time()

duration = end_time - start_time

cat(green('CAPTND data read and cleaned! \nThis process took', format(duration,usetz = TRUE), '\n\n'))
rm(con)
}

# takes about 1hr 20 minutes and 14.5 GiB

read_clean_captnd_data()


