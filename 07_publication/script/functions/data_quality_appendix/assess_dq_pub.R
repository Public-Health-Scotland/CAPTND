
####################################.
### Shorewise data DQ assessment ###
####################################.

# Author: Charlie Smith
# Date: 2024-11-27

# Purpose: requirement for separate DQ assessment on optimised data for publication

# Outline:
# Load optimsied data
# Filter for period
# Split into stages, run checks
# Save output


assess_dq_shorewise <- function(){
  
  source('10_pre_shorewise_dq/set_preg_perinatal_stage.R')
  
  # get opti data and filter for latest month
  #if( file.exists(paste0(data_prep_dir, '/captnd_shorewise_dq.parquet')) != TRUE){
  
  df_test <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |> 
      filter(header_month %in% date_range) |> # month end is a control_publication.R constant
      set_preg_perinatal_stage() |> 
      save_as_parquet(paste0(data_prep_dir, '/captnd_shorewise_dq'))
  
    #}
  
  # assess variables by stages (not sure if best course, given that CAPTND opti is undiagonalised)
  
  # updated assessment functions (for additional vars)
  source('./07_publication/script/functions/data_quality_appendix/assess_variables_demo_pub.R')
  source('./07_publication/script/functions/data_quality_appendix/assess_variables_ref_pub.R')
  source('./07_publication/script/functions/data_quality_appendix/assess_variables_apps_pub.R')
  source('./07_publication/script/functions/data_quality_appendix/assess_variables_unav_pub.R')
  source('./07_publication/script/functions/data_quality_appendix/assess_variables_diag_pub.R')
  source('./07_publication/script/functions/data_quality_appendix/assess_variables_dis_pub.R')
  
  # assess demos
  #if( file.exists(paste0(data_prep_dir, '/assess_demo_opti.parquet')) != TRUE){
    read_parquet(paste0(data_prep_dir, '/captnd_shorewise_dq.parquet')) |> 
      assess_variables_demo_pub() |> 
      save_as_parquet(paste0(data_prep_dir, '/assess_demo_opti'))
  ##}
  
  # assess referrals
  #if( file.exists(paste0(data_prep_dir, '/assess_refs_opti.parquet')) != TRUE){
    read_parquet(paste0(data_prep_dir, '/captnd_shorewise_dq.parquet')) |> 
      assess_variables_ref_pub() |> 
      save_as_parquet(paste0(data_prep_dir, '/assess_refs_opti'))
  #}
  
  # assess apps
  #if( file.exists(paste0(data_prep_dir, '/assess_apps_opti.parquet')) != TRUE){
    read_parquet(paste0(data_prep_dir, '/captnd_shorewise_dq.parquet')) |> 
      assess_variables_apps_pub() |> 
      save_as_parquet(paste0(data_prep_dir, '/assess_apps_opti'))
  #}
    
  # assess unav
  #if( file.exists(paste0(data_prep_dir, '/assess_unav_opti.parquet')) != TRUE){
    read_parquet(paste0(data_prep_dir, '/captnd_shorewise_dq.parquet')) |>
      assess_variables_unav_pub() |> 
      save_as_parquet(paste0(data_prep_dir, '/assess_unav_opti'))
  #}
  
  # assess diag
  #if( file.exists(paste0(data_prep_dir, '/assess_diag_opti.parquet')) != TRUE){
    read_parquet(paste0(data_prep_dir, '/captnd_shorewise_dq.parquet')) |>
      assess_variables_diag_pub() |> 
      save_as_parquet(paste0(data_prep_dir, '/assess_diag_opti'))
  #}
  
  # assess discharge
  #if( file.exists(paste0(data_prep_dir, '/assess_dis_opti.parquet')) != TRUE){
    read_parquet(paste0(data_prep_dir, '/captnd_shorewise_dq.parquet')) |>
      assess_variables_dis_pub() |> 
      save_as_parquet(paste0(data_prep_dir, '/assess_dis_opti'))
  #}
  
  
  # combine and save
  rbind.fill(
    read_parquet(paste0(data_prep_dir, '/assess_demo_opti.parquet')),
    read_parquet(paste0(data_prep_dir, '/assess_refs_opti.parquet')),
    read_parquet(paste0(data_prep_dir, '/assess_apps_opti.parquet')),
    read_parquet(paste0(data_prep_dir, '/assess_unav_opti.parquet')),
    read_parquet(paste0(data_prep_dir, '/assess_diag_opti.parquet')),
    read_parquet(paste0(data_prep_dir, '/assess_dis_opti.parquet'))) |> 
    save_as_parquet(paste0(data_prep_dir, '/captnd_checked_opti'))
  
  
  # wrangle and get counts
  
  source('./10_pre_shorewise_dq/dq_reporting/filter_wrangle_data.R') # move these to functions script
  source('./10_pre_shorewise_dq/dq_reporting/get_dq_counts.R')
  source('./10_pre_shorewise_dq/dq_reporting/add_scotland_totals.R')
  source('./10_pre_shorewise_dq/dq_reporting/complete_absent_vars_na.R')
  source('./10_pre_shorewise_dq/dq_reporting/append_submission_status.R')
  source('./10_pre_shorewise_dq/dq_reporting/get_dq_proportions.R')
  source('./10_pre_shorewise_dq/dq_reporting/label_impossible_combis_na.R')
  source('./10_pre_shorewise_dq/dq_reporting/label_retired_variables_na.R')
  source('./04_check_modify/correct_hb_names_simple.R')
  source('./10_pre_shorewise_dq/dq_reporting/append_patient_management_system.R')
  source('./10_pre_shorewise_dq/dq_reporting/append_variable_categories.R')
  source('./10_pre_shorewise_dq/dq_reporting/arrange_dq_df.R')
  
  
  df2 <- read_parquet(paste0(data_prep_dir, '/captnd_checked_opti.parquet')) #|> filter(ucpn == "100061825") |> 
    mutate(header_date_month = floor_date(!!sym(header_date_o), unit = "month"), .after = !!sym(header_date_o)) |> 
    filter(header_date_month == month_end) |> 
    #filter(ucpn == "100061825") |> 
    filter_wrangle_data() |> 
    get_dq_counts() |> 
    add_scotland_totals() |> 
    group_by(header_date_month, dataset_type, 
             #record_type, # TEST
             hb_name, variable) |> 
    mutate(total = sum(count), 
           proportion = round(count / total * 100, digits = 1), 
           hb_name = factor(hb_name, levels = hb_vector),
           variable = factor(variable, levels = vec_vars)
           ) |> 
    filter(value == "known") |>  
    arrange(header_date_month, dataset_type, hb_name, variable) |> 
    save_as_parquet(paste0(data_prep_dir, '/captnd_counts_opti'))
  
  

}


test <- anti_join(df2, df)
