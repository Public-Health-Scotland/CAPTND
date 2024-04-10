
#####################################.
### Assess appointments variables ###
#####################################.

# Author: Bex Madden
# Date: 2024-04-10

assess_variables_apps <- function(df){
  
  source('10_pre_shorewise_scripts/assess_ucpn.R')
  source('10_pre_shorewise_scripts/assess_upi.R')
  source('10_pre_shorewise_scripts/assess_chi.R')
  
  source('10_pre_shorewise_scripts/assess_app_date.R')
  
  # diagnosis variables to assess
  vars_apps <- c(header_date_o, dataset_type_o, hb_name_o, ucpn_o, upi_o, chi_o,
                 app_date_o, app_purpose_o, att_status_o, att_cat_o, prof_group_o,
                 location_o, preg_perinatal_app_o)
  
  
  df_apps <- df %>% 
    select(all_of(vars_apps)) %>% # select diag vars 
    filter(!is.na(!!sym(diag_1_o)) | 
             !is.na(!!sym(diag_2_o)) | 
             !is.na(!!sym(diag_3_o)) | 
             !is.na(!!sym(treat_1_o)) | 
             !is.na(!!sym(treat_2_o)) | 
             !is.na(!!sym(treat_3_o)) | 
             !is.na(!!sym(treat_group_or_ind_1_o)) |
             !is.na(!!sym(treat_group_or_ind_2_o)) |
             !is.na(!!sym(treat_group_or_ind_3_o)) | 
             !is.na(!!sym(treat_start_date_o))) |> 
    distinct() %>%  
    mutate(!!record_type_o := "appointments",
           !!sub_source_o := "swift")
  
  # assess variables
  df_apps_checked <- df_apps |> 
    assess_ucpn() |> 
    assess_upi() |>
    assess_chi() |>
    assess_app_date()
  
}
