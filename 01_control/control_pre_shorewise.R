
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
# 3. Get publication figures 


# 1 - Load functions ------------------------------------------------------
source('02_setup/save_df_as_parquet.R')
source('02_setup/swift_column_renamer.R')
source('02_setup/null_to_na.R')
source('02_setup/set_dir_structure.R')
source('04_check_modify/correct_hb_names_simple.R')

source('10_pre_shorewise_scripts/pull_captnd_from_db.R')
source('10_pre_shorewise_scripts/set_preg_perinatal_stage.R')
source('10_pre_shorewise_scripts/fix_dob_issue.R')
source('10_pre_shorewise_scripts/format_dates.R')
source('10_pre_shorewise_scripts/save_captnd_raw.R')
source('10_pre_shorewise_scripts/assess_variables_demo.R')


# 2 - Set constants -------------------------------------------------------
source("10_pre_shorewise_scripts/set_constants.R")


# 3 - Pull, check, save output data ---------------------------------------
df_captnd_raw <- pull_captnd_from_db() |> 
  null_to_na() |> 
  set_preg_perinatal_stage() |> 
  fix_dob_issue() |> 
  format_dates() |> 
  correct_hb_names_simple() |> 
  save_captnd_raw() |> 
  rm()



# for each stage: split into treatment stages X run checks
df <- read_parquet(paste0(data_prep_dir, '/captnd_raw.parquet')) 

  df_checked_demo <- assess_variables_demo(df)

  
# combine
# save

