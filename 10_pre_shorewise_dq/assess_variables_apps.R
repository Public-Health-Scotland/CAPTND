
#####################################.
### Assess Appointments Variables ###
#####################################.

# Author: Bex Madden
# Date: 2024-04-10

assess_variables_apps <- function(df){
  
  source('10_pre_shorewise_dq/assess_ucpn.R')
  source('10_pre_shorewise_dq/assess_upi.R')
  source('10_pre_shorewise_dq/assess_chi.R')
  
  source('10_pre_shorewise_dq/assess_app_date.R')
  source('10_pre_shorewise_dq/assess_app_purpose.R')
  source('10_pre_shorewise_dq/assess_att_status.R')
  source('10_pre_shorewise_dq/assess_att_cat.R')
  source('10_pre_shorewise_dq/assess_preg_perinatal_app.R')
  source('10_pre_shorewise_dq/assess_prof_group.R')
  source('10_pre_shorewise_dq/assess_app_location.R')
  
  source('10_pre_shorewise_dq/assess_cancellation_date.R')
  source('10_pre_shorewise_dq/assess_presenting_problem.R')
  source('10_pre_shorewise_dq/assess_treat_reason.R')
  
    
    # appointment variables to assess
  vars_apps <- c(header_date_o, dataset_type_o, hb_name_o, ucpn_o, upi_o, chi_o,
                 app_date_o, app_purpose_o, att_status_o, att_cat_o, 
                 preg_perinatal_app_o, prof_group_o, location_o, 
                 
                 cancellation_date_o, 
                 presenting_prob_1_o, presenting_prob_2_o, presenting_prob_3_o, 
                 treat_reason_1_o, treat_reason_2_o, treat_reason_3_o)
  
  
  df_apps <- df |> 
    select(all_of(vars_apps)) |> # select appointment vars 
    filter(!is.na(!!sym(app_date_o)) | 
             !is.na(!!sym(app_purpose_o)) | 
             !is.na(!!sym(att_status_o)) | 
             !is.na(!!sym(att_cat_o)) | 
             !is.na(!!sym(preg_perinatal_app_o)) | 
             !is.na(!!sym(prof_group_o)) | 
             !is.na(!!sym(location_o)) | 
             !is.na(!!sym(cancellation_date_o)) | # check against att_status - if not cancelled, 'missing but valid'
             !is.na(!!sym(presenting_prob_1_o)) | 
             !is.na(!!sym(presenting_prob_2_o)) | 
             !is.na(!!sym(presenting_prob_3_o)) | 
             !is.na(!!sym(treat_reason_1_o)) | 
             !is.na(!!sym(treat_reason_2_o)) |
             !is.na(!!sym(treat_reason_3_o))) |> 
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
    assess_app_location() |> 
    assess_cancellation_date() |> 
    assess_presenting_prob() |> 
    assess_treat_reason()
  
  return(df_apps_checked)
  
}
