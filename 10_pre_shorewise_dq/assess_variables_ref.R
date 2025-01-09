
############################.
### Assess variables ref ###
############################.

# Author: Charlie Smith
# Date: 2024-04-11


assess_variables_ref <- function(df){
  
  # load functions
  source('10_pre_shorewise_dq/assess_ucpn.R')
  source('10_pre_shorewise_dq/assess_upi.R')
  source('10_pre_shorewise_dq/assess_chi.R')
  
  source('10_pre_shorewise_dq/assess_ref_date.R')
  source('10_pre_shorewise_dq/assess_ref_rec_date.R')
  source('10_pre_shorewise_dq/assess_ref_source.R')
  source('10_pre_shorewise_dq/assess_ref_reason.R')
  source('10_pre_shorewise_dq/assess_ref_acc.R')
  source('10_pre_shorewise_dq/assess_rej_date.R')
  source('10_pre_shorewise_dq/assess_rej_reason.R')
  source('10_pre_shorewise_dq/assess_rej_actions.R')
  source('10_pre_shorewise_dq/assess_act_code_sent_date.R')
  
  # get treatment stage to check
  vars_ref <- c(header_date_o, dataset_type_o, hb_name_o, ucpn_o, upi_o, chi_o,
                ref_date_o, ref_rec_date_o, ref_source_o, ref_reason_o, ref_acc_o,
                ref_rej_date_o, ref_rej_reason_o, ref_rej_act_o, act_code_sent_date_o)
  
  df_ref <- df %>% 
    select(all_of(vars_ref)) %>% # select referral vars 
    filter(!is.na(!!sym(ref_date_o)) |
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
  
  
  # run checks
  df_ref_checked <- df_ref |> 
    assess_ucpn() |> # already checked in demo checks
    assess_upi() |>
    assess_chi() |>
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
