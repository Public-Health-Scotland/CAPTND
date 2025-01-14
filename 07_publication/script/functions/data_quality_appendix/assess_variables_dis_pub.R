
##################################.
### Assess discharge variables pub ###
##################################.

# Author: Bex Madden
# Date: 2024-12-05


assess_variables_dis_pub <- function(df){
  
  source('10_pre_shorewise_dq/assess_ucpn.R')
  source('10_pre_shorewise_dq/assess_upi.R')
  source('10_pre_shorewise_dq/assess_chi.R')
  source('./07_publication/script/functions/data_quality_appendix/assess_patient_id.R') # patient_id
  source('10_pre_shorewise_dq/assess_case_closed_date.R')
  
  # discharge variables to assess
  vars_dis <- c(header_date_o, dataset_type_o, hb_name_o, ucpn_o, upi_o, chi_o, patient_id_o,
                case_closed_date_o)
  
  
  df_dis <- df %>% 
    select(all_of(vars_dis)) %>% # select demo vars 
    filter(!is.na(!!sym(case_closed_date_o))) |> # Review logic of whether to filter
    distinct() %>%  
    mutate(!!record_type_o := "discharge",
           !!sub_source_o := "swift")
  
  # assess variables
  df_dis_checked <- df_dis |> 
    assess_ucpn() |>
    assess_upi() |>
    assess_chi() |>
    assess_patient_id() |> 
    assess_case_closed_date()
  
  return(df_dis_checked)
  
}
