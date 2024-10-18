
##################################.
### Assess diagnosis variables ###
##################################.

# Author: Bex Madden
# Date: 2024-04-10

assess_variables_diag <- function(df){
  
  source('10_pre_shorewise_scripts/assess_ucpn.R')
  source('10_pre_shorewise_scripts/assess_upi.R')
  source('10_pre_shorewise_scripts/assess_chi.R')
  
  
  source('10_pre_shorewise_scripts/assess_diagnoses.R')  # does this need an icd code lookup added?
  source('10_pre_shorewise_scripts/assess_treatments.R') 
  source('10_pre_shorewise_scripts/assess_group_or_inds.R') 
  source('10_pre_shorewise_scripts/assess_treat_start_date.R')

  
  # diagnosis variables to assess
    vars_diag <- c(header_date_o, dataset_type_o, hb_name_o, ucpn_o, upi_o, chi_o,
                 diag_1_o, diag_2_o, diag_3_o, treat_1_o, treat_2_o, treat_3_o,
                 treat_group_or_ind_1_o, treat_group_or_ind_2_o, treat_group_or_ind_3_o,
                 treat_start_date_o)
  
  
  df_diag <- df %>% 
    select(all_of(vars_diag)) %>% # select diag vars 
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
    mutate(!!record_type_o := "diagnosis",
           !!sub_source_o := "swift")
  
  # assess variables
  df_diag_checked <- df_diag |> 
    assess_ucpn() |>
    assess_upi() |>
    assess_chi() |>
    assess_diagnoses() |>
    assess_treatments() |>  
    assess_group_or_inds() |> 
    assess_treat_start_date()
  
  return(df_diag_checked)

}

