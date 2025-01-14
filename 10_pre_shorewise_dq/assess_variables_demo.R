
#############################.
### Assess variables demo ###
#############################.

# Author: Charlie Smith
# Date: 2024-04-05

assess_variables_demo <- function(df){
  
  source('10_pre_shorewise_dq/assess_ucpn.R')
  source('10_pre_shorewise_dq/assess_upi.R')
  source('10_pre_shorewise_dq/assess_chi.R')
  
  source('10_pre_shorewise_dq/assess_postcode.R')
  source('10_pre_shorewise_dq/assess_sex.R')
  source('10_pre_shorewise_dq/assess_dob.R')
  source('10_pre_shorewise_dq/assess_ethnicity.R')
  source('10_pre_shorewise_dq/assess_lac_status.R')
  source('10_pre_shorewise_dq/assess_vet_status.R')
  source('10_pre_shorewise_dq/assess_preg_perinatal_ref.R')
  source('10_pre_shorewise_dq/assess_protection.R')
  
  # demographic variables to assess
  vars_demo <- c(header_date_o, dataset_type_o, hb_name_o, ucpn_o, upi_o, chi_o,
                 postcode_o, sex_o, dob_o, ethnicity_o, protection_o,
                 looked_after_c_o, vet_o, preg_perinatal_ref_o)
  
  df_demo <- df %>% 
    select(all_of(vars_demo)) %>% # select demo vars 
    filter(!is.na(!!sym(postcode_o)) | 
             !is.na(!!sym(sex_o)) | 
             !is.na(!!sym(dob_o)) | 
             !is.na(!!sym(ethnicity_o)) | 
             !is.na(!!sym(protection_o)) | 
             !is.na(!!sym(looked_after_c_o)) | 
             !is.na(!!sym(vet_o)) |
             !is.na(!!sym(preg_perinatal_ref_o))) |> 
    distinct() %>%  
    mutate(!!record_type_o := "demographics",
           !!sub_source_o := "swift")
  
  # assess variables
  df_demo_checked <- df_demo |> 
    assess_ucpn() |> 
    assess_upi() |> 
    assess_chi() |> 
    assess_postcode() |> 
    assess_sex() |> 
    assess_dob() |> 
    assess_ethnicity() |> 
    assess_lac_status() |> 
    assess_protection() |> 
    assess_vet_status() |> 
    assess_preg_perinatal_ref()
    
  return(df_demo_checked)
  
}

