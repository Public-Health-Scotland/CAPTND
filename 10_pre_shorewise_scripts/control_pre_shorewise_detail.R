
#########################################################.
### Control pre-shorewise CAPTND processes and ouputs ###
#########################################################.

# Author: Charlie Smith
# Date: 2024-04-01

# Purpose: Refactor original CAPTND data pull, data quality and publication prep, 
# and publication analyses using updated coding practices

# NB this is an updated version of the old (pre-shorewise) CAPTND processes and 
# are distinct from the Shorewise methodology.



# Outline:
# 1. Pull, check, save data
# 2. Create dq reports


# 0 - Start timer ---------------------------------------------------------

tictoc::tic()

# 1 - Load functions ------------------------------------------------------
source('02_setup/save_df_as_parquet.R')
source('02_setup/swift_column_renamer.R')
source('02_setup/null_to_na.R')
source('04_check_modify/correct_hb_names_simple.R')

source('10_pre_shorewise_scripts/pull_captnd_from_db.R')
source('10_pre_shorewise_scripts/set_preg_perinatal_stage.R')
source('10_pre_shorewise_scripts/fix_dob_issue.R')
source('10_pre_shorewise_scripts/format_dates.R')
source('10_pre_shorewise_scripts/save_captnd_raw.R')
source('10_pre_shorewise_scripts/save_captnd_checked.R')

source('10_pre_shorewise_scripts/assess_variables_demo.R')
source('10_pre_shorewise_scripts/assess_variables_ref.R')
source('10_pre_shorewise_scripts/assess_variables_ref2.R')
source('10_pre_shorewise_scripts/assess_variables_apps.R')
source('10_pre_shorewise_scripts/assess_variables_unav.R')
source('10_pre_shorewise_scripts/assess_variables_diag.R')
source('10_pre_shorewise_scripts/assess_variables_dis.R')
source('10_pre_shorewise_scripts/fix_treat_lead_zeros.R')

source('./10_pre_shorewise_scripts/dq_reporting/load_functions.R')

# 2 - Set constants -------------------------------------------------------

source("10_pre_shorewise_scripts/dq_reporting/set_constants.R")

message('Pulling the data and creating the report takes ~15 mins. Get the kettle on!')

# 3 - Pull, check, save output data ---------------------------------------
df_captnd_raw <- pull_captnd_from_db() |> 
  null_to_na() |> 
  set_preg_perinatal_stage() |> 
  fix_dob_issue() |> 
  format_dates() |> 
  correct_hb_names_simple() |> 
  fix_treatment_leadings_zeros() |> 
  save_captnd_raw() 
  
rm(df_captnd_raw)
gc()


# for each stage: split into treatment stages and run checks
df <- read_parquet(paste0(data_prep_dir, '/captnd_raw.parquet'))

  # df_checked_demo <- assess_variables_demo(df)
  # df_checked_ref <- assess_variables_ref(df) 
  assess_variables_ref2(df) |> save_as_parquet(paste0(data_prep_dir, '/assess_refs'))
  assess_variables_apps(df) |> save_as_parquet(paste0(data_prep_dir, '/assess_apps'))
  assess_variables_unav(df) |> save_as_parquet(paste0(data_prep_dir, '/assess_unav'))
  assess_variables_diag(df) |> save_as_parquet(paste0(data_prep_dir, '/assess_diag'))
  assess_variables_dis(df) |> save_as_parquet(paste0(data_prep_dir, '/assess_dis'))

rm(df)
gc()
    
# combine and save
df_captnd_checked <- rbind.fill(
  # df_checked_demo,
  df_checked_ref <- read_parquet(paste0(data_prep_dir, '/assess_refs.parquet')),
  df_checked_apps <- read_parquet(paste0(data_prep_dir, '/assess_apps.parquet')),
  df_checked_unav <- read_parquet(paste0(data_prep_dir, '/assess_unav.parquet')),
  df_checked_diag <- read_parquet(paste0(data_prep_dir, '/assess_diag.parquet')),
  df_checked_dis <- read_parquet(paste0(data_prep_dir, '/assess_dis.parquet'))) |> 
  save_captnd_checked()
  

rm(#df_checked_demo,
   df_checked_ref,
   df_checked_apps,
   df_checked_unav,
   df_checked_diag,
   df_checked_dis, 
   df_captnd_checked)

gc()

# 4 - Create DQ heatmap reports -------------------------------------------

suppressWarnings(source('./10_pre_shorewise_scripts/dq_reporting/create_dq_report.R'))

# 5 - End timer -----------------------------------------------------------

tictoc::toc()
