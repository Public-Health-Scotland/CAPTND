
##################################.
### Assess diagnosis variables ###
##################################.

# Author: Bex Madden
# Date: 2024-04-10

assess_variables_diag <- function(df){
  
  source('10_pre_shorewise_dq/assess_ucpn.R')
  source('10_pre_shorewise_dq/assess_upi.R')
  source('10_pre_shorewise_dq/assess_chi.R')
  
  
  #source('10_pre_shorewise_dq/assess_diagnoses.R')  # does this need an icd code lookup added?
  source('10_pre_shorewise_dq/assess_treatments.R') 
  source('10_pre_shorewise_dq/assess_group_or_inds.R') 
  source('10_pre_shorewise_dq/assess_global_impressions.R') 
  source('10_pre_shorewise_dq/assess_treat_start_date.R')
  source('10_pre_shorewise_dq/assess_treat_reason.R')
  
  # diagnosis variables to assess
    vars_diag <- c(header_date_o, dataset_type_o, hb_name_o, ucpn_o, upi_o, chi_o,
                 #diag_1_o, diag_2_o, diag_3_o, # removed December 2024
                 treat_1_o, treat_2_o, treat_3_o,
                 treat_group_or_ind_1_o, treat_group_or_ind_2_o, treat_group_or_ind_3_o,
                 treat_start_date_o, treat_reason_1_o, treat_reason_2_o, treat_reason_3_o)
  
  
  df_diag <- df %>% 
    select(all_of(vars_diag)) %>% # select diag vars 
    filter(
      # !is.na(!!sym(diag_1_o)) | # removed December 2024
      #        !is.na(!!sym(diag_2_o)) | 
      #        !is.na(!!sym(diag_3_o)) | 
             !is.na(!!sym(treat_1_o)) | 
             !is.na(!!sym(treat_2_o)) | 
             !is.na(!!sym(treat_3_o)) | 
             !is.na(!!sym(treat_group_or_ind_1_o)) |
             !is.na(!!sym(treat_group_or_ind_2_o)) |
             !is.na(!!sym(treat_group_or_ind_3_o)) | 
             !is.na(!!sym(treat_start_date_o)) |
             !is.na(!!sym(treat_reason_1_o)) |
             !is.na(!!sym(treat_reason_2_o)) |
             !is.na(!!sym(treat_reason_3_o))) |>
    distinct() %>%  
    mutate(!!record_type_o := "diagnosis",
           !!sub_source_o := "swift")
  
  # assess variables
  df_diag_checked <- df_diag |> 
    assess_ucpn() |>
    assess_upi() |>
    assess_chi() |>
    # assess_diagnoses() |> # removed December 2024
    assess_treatments() |>  
    assess_group_or_inds() |> 
    assess_treat_start_date() |>
   assess_treat_reason()
  
  return(df_diag_checked)

}

