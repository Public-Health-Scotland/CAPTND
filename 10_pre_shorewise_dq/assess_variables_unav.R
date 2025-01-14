
#######################################.
### Assess unavailability variables ###
#######################################.

# Author: Bex Madden
# Date: 2024-04-10

assess_variables_unav <- function(df){
  
  source('10_pre_shorewise_dq/assess_ucpn.R')
  source('10_pre_shorewise_dq/assess_upi.R')
  source('10_pre_shorewise_dq/assess_chi.R')
  
  source('10_pre_shorewise_dq/assess_unav_dates.R')
  source('10_pre_shorewise_dq/assess_unav_reason.R')  
  
  # diagnosis variables to assess
  
  vars_unav <- c(header_date_o, dataset_type_o, hb_name_o, ucpn_o, upi_o, chi_o, 
                unav_date_start_o, unav_date_end_o, unav_days_no_o, unav_reason_o)
  
  df_unav <- df %>% #filter(hb_name == "NHS Greater Glasgow and Clyde" & dataset_type == "PT") |> 
    select(all_of(vars_unav)) %>% # select diag vars 
    filter(!is.na(!!sym(unav_date_start_o)) | 
             !is.na(!!sym(unav_date_end_o)) | 
             !is.na(!!sym(unav_days_no_o)) | 
             !is.na(!!sym(unav_reason_o))) |> 
    distinct() %>%  
    mutate(!!record_type_o := "unavailability",
           !!sub_source_o := "swift")
    
  # assess variables
  df_unav_checked <- df_unav |> 
    assess_ucpn() |>
    assess_upi() |>
    assess_chi() |>
    assess_unav_dates() |>
    assess_unav_reason()  
  
  return(df_unav_checked)
  
}
