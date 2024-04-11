
#####################################.
### Assess Appointments Variables ###
#####################################.

# Author: Bex Madden
# Date: 2024-04-10

assess_variables_apps <- function(df){
  
  source('10_pre_shorewise_scripts/assess_ucpn.R')
  source('10_pre_shorewise_scripts/assess_upi.R')
  source('10_pre_shorewise_scripts/assess_chi.R')
  
  source('10_pre_shorewise_scripts/assess_app_date.R')
  source('10_pre_shorewise_scripts/assess_app_purpose.R')
  source('10_pre_shorewise_scripts/assess_att_status.R')
  source('10_pre_shorewise_scripts/assess_att_cat.R')
  source('10_pre_shorewise_scripts/assess_preg_perinatal_app.R')
  source('10_pre_shorewise_scripts/assess_prof_group.R')
  source('10_pre_shorewise_scripts/assess_app_location.R')
  
    
    # appointment variables to assess
  vars_apps <- c(header_date_o, dataset_type_o, hb_name_o, ucpn_o, upi_o, chi_o,
                 app_date_o, app_purpose_o, att_status_o, att_cat_o, 
                 preg_perinatal_app_o, prof_group_o, location_o)
  
  
  df_apps <- df |> 
    select(all_of(vars_apps)) |> # select appointment vars 
    filter(!is.na(!!sym(app_date_o)) | 
             !is.na(!!sym(app_purpose_o)) | 
             !is.na(!!sym(att_status_o)) | 
             !is.na(!!sym(att_cat_o)) | 
             !is.na(!!sym(preg_perinatal_app_o)) | 
             !is.na(!!sym(prof_group_o)) | 
             !is.na(!!sym(location_o))) |> 
    distinct() |>  
    mutate(!!record_type_o := "appointments",
           !!sub_source_o := "swift")
  
  # assess variables
  df_apps_checked <- df_apps |> 
    assess_ucpn() |> 
    assess_upi() |>
    assess_chi() |>
    assess_app_date() |>
    assess_app_purpose() |>
    assess_att_status() |>
    assess_att_cat() |>
    assess_preg_perinatal_app() |>
    assess_prof_group() |>
    assess_app_location()
  
  return(df_apps_checked)
  
}
