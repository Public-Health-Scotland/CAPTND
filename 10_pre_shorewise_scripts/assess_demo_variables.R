
############################.
### Assess demo variables ###
############################.

# Author: Charlie Smith
# Date: 2024-04-05

assess_demo_variables <- function(df){
  
  source('10_pre_shorewise_scripts/assess_ucpn.R')
  source('10_pre_shorewise_scripts/assess_upi.R')
  
  source('10_pre_shorewise_scripts/assess_postcode.R')
  source('10_pre_shorewise_scripts/assess_sex.R')
  source('10_pre_shorewise_scripts/assess_dob.R')
  
  
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
    assess_postcode() |> 
    assess_sex() |> 
    assess_dob()
    # ethncity
    # protection
    # lac
  # vet
  # ppmh
  
}

