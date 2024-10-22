
################################################.
### Assess variables ref - demo and ref vars ###
################################################.

# Author: Charlie Smith
# Date: 2024-10-22

assess_variables_ref2 <- function(df){

  source('10_pre_shorewise_scripts/assess_ucpn.R')
  source('10_pre_shorewise_scripts/assess_upi.R')
  source('10_pre_shorewise_scripts/assess_chi.R')
  
  source('10_pre_shorewise_scripts/assess_postcode.R')
  source('10_pre_shorewise_scripts/assess_sex.R')
  source('10_pre_shorewise_scripts/assess_dob.R')
  source('10_pre_shorewise_scripts/assess_ethnicity.R')
  source('10_pre_shorewise_scripts/assess_lac_status.R')
  source('10_pre_shorewise_scripts/assess_vet_status.R')
  source('10_pre_shorewise_scripts/assess_preg_perinatal_ref.R')
  source('10_pre_shorewise_scripts/assess_protection.R')

  source('10_pre_shorewise_scripts/assess_ref_date.R')
  source('10_pre_shorewise_scripts/assess_ref_rec_date.R')
  source('10_pre_shorewise_scripts/assess_ref_source.R')
  source('10_pre_shorewise_scripts/assess_ref_reason.R')
  source('10_pre_shorewise_scripts/assess_ref_acc.R')
  source('10_pre_shorewise_scripts/assess_rej_date.R')
  source('10_pre_shorewise_scripts/assess_rej_reason.R')
  source('10_pre_shorewise_scripts/assess_rej_actions.R')
  source('10_pre_shorewise_scripts/assess_act_code_sent_date.R')  
  
  vars_ref <- c(header_date_o, dataset_type_o, hb_name_o, ucpn_o, upi_o, chi_o,
                
                postcode_o, sex_o, dob_o, ethnicity_o, protection_o,
                looked_after_c_o, vet_o, preg_perinatal_ref_o,
                
                ref_date_o, ref_rec_date_o, ref_source_o, ref_reason_o, ref_acc_o,
                ref_rej_date_o, ref_rej_reason_o, ref_rej_act_o, act_code_sent_date_o)
  
  df_ref <- df %>% 
    select(all_of(vars_ref)) %>% # select referral vars 
    filter(!is.na(!!sym(postcode_o)) | 
             !is.na(!!sym(sex_o)) | 
             !is.na(!!sym(dob_o)) | 
             !is.na(!!sym(ethnicity_o)) | 
             !is.na(!!sym(protection_o)) | 
             !is.na(!!sym(looked_after_c_o)) | 
             !is.na(!!sym(vet_o)) |
             !is.na(!!sym(preg_perinatal_ref_o)) |
      
      !is.na(!!sym(ref_date_o)) |
             !is.na(!!sym(ref_rec_date_o)) | 
             !is.na(!!sym(ref_source_o)) | 
             !is.na(!!sym(ref_reason_o)) | 
             !is.na(!!sym(ref_acc_o)) | 
             !is.na(!!sym(ref_rej_date_o)) | 
             !is.na(!!sym(ref_rej_reason_o)) | 
             !is.na(!!sym(ref_rej_act_o)) | 
             !is.na(!!sym(act_code_sent_date_o))) |>
    distinct() %>%  
    mutate(!!record_type_o := "referral",
           !!sub_source_o := "swift")
  
  
  df_ref_checked <- df_ref |> 
    
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
    assess_preg_perinatal_ref() |> 
    
    assess_ref_date() |> 
    assess_ref_rec_date() |> 
    assess_ref_source() |> 
    assess_ref_reason() |> 
    assess_ref_acc() |> 
    assess_rej_date() |> # revisit: potential for improvement
    assess_rej_reason() |> # revisit: potential for improvement
    assess_rej_actions() |> # revisit: potential for improvement
    assess_act_code_sent_date() # revisit: potential for improvement
  
  return(df_ref_checked)
  
}