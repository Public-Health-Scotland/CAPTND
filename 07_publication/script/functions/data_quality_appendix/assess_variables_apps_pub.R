
#####################################.
### Assess Appointments Variables ###
#####################################.

# Author: Bex Madden
# Date: 2024-04-10

assess_variables_apps_pub <- function(df){
  
  source('10_pre_shorewise_dq/assess_ucpn.R')
  source('10_pre_shorewise_dq/assess_upi.R')
  source('10_pre_shorewise_dq/assess_chi.R')
  source('./07_publication/script/functions/data_quality_appendix/assess_patient_id.R') # patient_id
  
  source('10_pre_shorewise_dq/assess_app_date.R')
  source('./07_publication/script/functions/data_quality_appendix/assess_app_purpose_pub.R') 
  source('./07_publication/script/functions/data_quality_appendix/assess_att_status_pub.R') 
  source('./07_publication/script/functions/data_quality_appendix/assess_att_cat_pub.R')
  source('./07_publication/script/functions/data_quality_appendix/assess_preg_perinatal_app_pub.R')
  source('./07_publication/script/functions/data_quality_appendix/assess_prof_group_pub.R')
  source('./07_publication/script/functions/data_quality_appendix/assess_app_location_pub.R')
  
  
  # appointment variables to assess
  vars_apps <- c(header_date_o, dataset_type_o, hb_name_o, ucpn_o, upi_o, chi_o, patient_id_o,
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
    assess_patient_id() |> 
    assess_app_date() |>
    assess_app_purpose_pub() |>
    assess_att_status_pub() |>
    assess_att_cat_pub() |>
    assess_preg_perinatal_app_pub() |>
    assess_prof_group_pub() |>
    assess_app_location_pub()
  
  return(df_apps_checked)
  
}