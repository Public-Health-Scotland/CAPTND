
##########################################.
### Assess global impression variables ###
##########################################.

# Author: Charlie Smith
# Date: 2025-11-27

assess_variables_global_impressions <- function(df){
  
  source('10_pre_shorewise_dq/assess_ucpn.R')
  source('10_pre_shorewise_dq/assess_upi.R')
  source('10_pre_shorewise_dq/assess_chi.R')
  
  source('10_pre_shorewise_dq/assess_global_impressions.R') 


  # global impression variables to assess
  vars_glob <- c(header_date_o, dataset_type_o, hb_name_o, ucpn_o, upi_o, chi_o,
                 cgi_i_o, pgi_i_o, cgi_s_o)
  
  
  df_diag <- df %>% 
    select(all_of(vars_glob)) %>% # select glob vars 
    filter(!is.na(!!sym(cgi_i_o)) |
            !is.na(!!sym(pgi_i_o)) |
            !is.na(!!sym(cgi_s_o))) |> 
    distinct() %>%  
    mutate(!!record_type_o := "global impressions",
           !!sub_source_o := "swift")
  
  # assess variables
  df_glob_checked <- df_diag |> 
    assess_ucpn() |>
    assess_upi() |>
    assess_chi() |>
    assess_global_impressions()
  
  return(df_glob_checked)
  
}
