
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
# 1. Pull, check, save data - DONE
# 2. Create dq reports - DOING
# 3. Get publication figures - TO DO


# 1 - Load functions ------------------------------------------------------
source('02_setup/save_df_as_parquet.R')
source('02_setup/swift_column_renamer.R')
source('02_setup/null_to_na.R')
#source('02_setup/set_dir_structure.R')
#source('02_setup/create_directory_structure.R')
source('04_check_modify/correct_hb_names_simple.R')

source('10_pre_shorewise_scripts/pull_captnd_from_db.R')
source('10_pre_shorewise_scripts/set_preg_perinatal_stage.R')
source('10_pre_shorewise_scripts/fix_dob_issue.R')
source('10_pre_shorewise_scripts/format_dates.R')
source('10_pre_shorewise_scripts/save_captnd_raw.R')
source('10_pre_shorewise_scripts/save_captnd_checked.R')

source('10_pre_shorewise_scripts/assess_variables_demo.R')
source('10_pre_shorewise_scripts/assess_variables_ref.R')
source('10_pre_shorewise_scripts/assess_variables_apps.R')
source('10_pre_shorewise_scripts/assess_variables_unav.R')
source('10_pre_shorewise_scripts/assess_variables_diag.R')
source('10_pre_shorewise_scripts/assess_variables_dis.R')


# 2 - Set constants -------------------------------------------------------
source("10_pre_shorewise_scripts/set_constants.R")


# 3 - Pull, check, save output data ---------------------------------------
df_captnd_raw <- pull_captnd_from_db() |> 
  null_to_na() |> 
  set_preg_perinatal_stage() |> 
  fix_dob_issue() |> 
  format_dates() |> 
  correct_hb_names_simple() |> 
  save_captnd_raw() 
  
rm(df_captnd_raw)


# for each stage: split into treatment stages and run checks
df <- read_parquet(paste0(data_prep_dir, '/captnd_raw.parquet')) 

  df_checked_demo <- assess_variables_demo(df)
  df_checked_ref <- assess_variables_ref(df) 
  df_checked_apps <- assess_variables_apps(df)
  df_checked_unav <- assess_variables_unav(df)
  df_checked_diag <- assess_variables_diag(df)
  df_checked_dis <- assess_variables_dis(df)
  
# combine and save
df_captnd_checked <- rbind.fill(
  df_checked_demo,
  df_checked_ref,
  df_checked_apps,
  df_checked_unav,
  df_checked_diag,
  df_checked_dis) |> 
  save_captnd_checked()
  


# 4 - Create DQ heatmap reports -------------------------------------------

source('./10_pre_shorewise_scripts/dq_reporting/create_dq_report.R')




































