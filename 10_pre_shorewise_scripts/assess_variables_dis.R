
##################################.
### Assess discharge variables ###
##################################.

# Author: Bex Madden
# Date: 2024-04-10


assess_variables_dis <- function(df){
  
  source('10_pre_shorewise_scripts/assess_ucpn.R')
  source('10_pre_shorewise_scripts/assess_upi.R')
  source('10_pre_shorewise_scripts/assess_chi.R')
  
  source('10_pre_shorewise_scripts/assess_case_closed_date.R')
  
  # discharge variables to assess
  vars_dis <- c(header_date_o, dataset_type_o, hb_name_o, ucpn_o, upi_o, chi_o,
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
    assess_case_closed_date()
  
  return(df_dis_checked)
  
}