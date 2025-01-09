
#############################.
### Assess variables demo ###
#############################.

# Author: Charlie Smith
# Date: 2024-04-05

assess_variables_demo_pub <- function(df){
  
  source('10_pre_shorewise_dq/assess_ucpn.R')
  source('10_pre_shorewise_dq/assess_upi.R')
  source('10_pre_shorewise_dq/assess_chi.R')
  
  source('./07_publication/script/functions/data_quality_appendix/assess_patient_id.R') # patient_id
  source('10_pre_shorewise_dq/assess_postcode.R')
  source('./07_publication/script/functions/data_quality_appendix/assess_postcode_last_rep.R') # postcode last reported
  
  source('./07_publication/script/functions/data_quality_appendix/assess_simd_quintile.R') # simd_quintile
  source('./07_publication/script/functions/data_quality_appendix/assess_sex_pub.R')
  source('./07_publication/script/functions/data_quality_appendix/assess_sex_reported_pub.R') # sex reported
  source('10_pre_shorewise_dq/assess_dob.R')
  source('./07_publication/script/functions/data_quality_appendix/assess_dob_verified.R') # dob verified
  source('./07_publication/script/functions/data_quality_appendix/assess_age_at_ref_rec.R') # age at ref rec
  source('10_pre_shorewise_dq/assess_ethnicity.R')
  source('./07_publication/script/functions/data_quality_appendix/assess_ethnicity_last_reported.R') # ethnicity last reported
  source('./07_publication/script/functions/data_quality_appendix/assess_lac_status_pub.R')
  source('./07_publication/script/functions/data_quality_appendix/assess_looked_after_c_edited.R') # looked_after_c_edited
  source('./07_publication/script/functions/data_quality_appendix/assess_vet_status_pub.R') 
  source('./07_publication/script/functions/data_quality_appendix/assess_vet_status_edited.R') # vet_edited
  source('./07_publication/script/functions/data_quality_appendix/assess_preg_perinatal_ref_pub.R') # preg_perinatal
  source('./07_publication/script/functions/data_quality_appendix/assess_protection_pub.R')
  # protection_edited???
  
  # demographic variables to assess
  
  vars_demo <- c(header_date_o, hb_name_o, dataset_type_o,
                 ucpn_o, upi_o, chi_o, patient_id_o, postcode_o, postcode_last_reported_o,
                 simd_quintile_o, sex_o, sex_reported_o,
                 dob_o, dob_verified_o, age_at_ref_rec_o, ethnicity_o, ethnicity_last_reported_o,
                 protection_o, # protection_edited_o (doesn't exist!)
                 looked_after_c_o, looked_after_c_edited_o, vet_o, vet_edited_o, preg_perinatal_ref_o#,
                 # preg_perinatal_ref_edited_o (doesn't exist!)
                 )
  
  df_demo <- df %>% 
    select(all_of(vars_demo)) %>% # select demo vars 
    filter(!is.na(!!sym(postcode_o)) | 
             #!is.na(!!sym(postcode_last_reported_o)) | 
             #!is.na(!!sym(simd_quintile_o)) |  
             !is.na(!!sym(sex_o)) |
             #!is.na(!!sym(sex_reported_o)) |
             !is.na(!!sym(dob_o)) | 
             #!is.na(!!sym(dob_verified_o)) |
             #!is.na(!!sym(age_at_ref_rec_o)) |
             !is.na(!!sym(ethnicity_o)) | 
             #!is.na(!!sym(ethnicity_last_reported_o)) |
             !is.na(!!sym(protection_o)) | 
             !is.na(!!sym(looked_after_c_o)) | 
             #!is.na(!!sym(looked_after_c_edited_o)) | 
             !is.na(!!sym(vet_o)) |
             #!is.na(!!sym(vet_edited_o)) |
             !is.na(!!sym(preg_perinatal_ref_o))) |> 
    distinct() %>%  
    mutate(!!record_type_o := "demographics",
           !!sub_source_o := "swift")
  
  # assess variables
  df_demo_checked <- df_demo |> 
    assess_ucpn() |> 
    assess_upi() |> 
    assess_chi() |> 
    assess_patient_id() |> 
    assess_postcode() |> 
    assess_postcode_last_reported() |> 
    assess_simd_quintile() |> 
    assess_sex_pub() |> 
    assess_sex_reported() |> 
    assess_dob() |> 
    assess_dob_verified() |> 
    assess_age_at_ref_rec() |> # assess_age_ref_rec
    assess_ethnicity() |> 
    assess_ethnicity_last_reported() |> # assess_ethnicity_last_reported
    assess_lac_status_pub() |> 
    assess_lac_status_edited() |>  # assess_looked_after_c_edited
    assess_protection_pub() |> 
    assess_vet_status_pub() |> 
    assess_vet_status_edited() |> 
    assess_preg_perinatal_ref_pub()
    
  return(df_demo_checked)
  
}

