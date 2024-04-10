
##############################################.
### Split into treatment stages for checks ###
##############################################.

# Author: Charlie Smith
# Date: 2024-04-04

split_treat_stages <- function(){
  
  # vars to reference...
  vars_demo <- c(header_date_o, dataset_type_o, hb_name_o, ucpn_o, upi_o, chi_o,
                 postcode_o, sex_o, dob_o, ethnicity_o, protection_o,
                 looked_after_c_o, vet_o, preg_perinatal_ref_o)
  
  vars_ref <- c(header_date_o, dataset_type_o, hb_name_o, ucpn_o, upi_o, chi_o,
                ref_date_o, ref_rec_date_o, ref_source_o, ref_reason_o, ref_acc_o,
                ref_rej_date_o, ref_rej_reason_o, ref_rej_act_o, act_code_sent_date_o)
  
  vars_app <- c(header_date_o, dataset_type_o, hb_name_o, ucpn_o, upi_o, chi_o,
                app_date_o, app_purpose_o, att_status_o, att_cat_o, prof_group_o,
                location_o, preg_perinatal_app_o)
    
    
  vars_una <- c(header_date_o, dataset_type_o, hb_name_o, ucpn_o, upi_o, chi_o,
                unav_date_start_o, unav_date_end_o, unav_days_no_o, unav_reason_o)
  

  vars_diag <- c(header_date_o, dataset_type_o, hb_name_o, ucpn_o, upi_o, chi_o,
                 diag_1_o, diag_2_o, diag_3_o, treat_1_o, treat_2_o, treat_3_o,
                 treat_group_or_ind_1_o, treat_group_or_ind_2_o, treat_group_or_ind_3_o,
                 treat_start_date_o)
  
  vars_out <- c(header_date_o, dataset_type_o, hb_name_o, ucpn_o, upi_o, chi_o,
                measure_1_o, measure_2_o, measure_3_o)         # fields no longer in use?
  
  vars_dis <- c(header_date_o, dataset_type_o, hb_name_o, ucpn_o, upi_o, chi_o,
                 case_closed_date_o)
  
  # CONTINUE...
  
  df_demo <- df %>% 
    select(all_of(vars_demo)) %>% # select demo vars 
    filter(!is.na(POSTCODE) | 
             !is.na(SEX) | 
             !is.na(DOB) | 
             !is.na(ETHNICITY) | 
             !is.na(PROTECTION) | 
             !is.na(LAC) | 
             !is.na(VETERAN) |
             !is.na(PPMH_REF)) |> 
    distinct() %>%  
    mutate(REC_TYPE = "DEMOGRAPHICS",
           SUB_SOURCE = "SWIFT")
  
  swift_ref <- swift_master %>% 
    select(all_of(vars_ref)) %>% # select referral vars 
    filter(!is.na(REF_DATE) | 
             !is.na(RECEIVED_DATE) | 
             !is.na(REF_SOURCE) | 
             !is.na(REF_REASON) |
             !is.na(ACCEPTED) |  
             !is.na(REJ_DATE) |
             !is.na(REJ_REASON) |
             !is.na(REJ_ACTIONS) |
             !is.na(CODE_SENT_DATE)) |> 
    distinct() %>%  
    mutate(REC_TYPE = "REFERRAL",
           SUB_SOURCE = "SWIFT")
  
  swift_app <- swift_master %>% 
    select(all_of(vars_app)) %>% # select appointment vars 
    distinct() |> 
    filter(!is.na(APP_DATE) | 
             !is.na(APP_PURPOSE) | 
             !is.na(ATT_STATUS) | 
             !is.na(ATT_CATEGORY) |
             !is.na(PROF_GROUP) | 
             !is.na(LOCATION) |
             !is.na(PPMH_APP)) %>%  
    mutate(REC_TYPE = "APPS",
           SUB_SOURCE = "SWIFT")  
  
  swift_una <- swift_master %>% 
    select(all_of(vars_una)) %>% # select una vars 
    distinct() %>% 
    filter(!is.na(UNA_START_DATE) | 
             !is.na(UNA_END_DATE) |
             !is.na(NUM_DAYS) |
             !is.na(UNA_REASON)) %>%  
    mutate(REC_TYPE = "UNA",
           SUB_SOURCE = "SWIFT")
  
  
  swift_diag <- swift_master %>% # test: updating criteria
    select(all_of(vars_diag)) %>% # select diagnosis vars 
    distinct() %>% 
    filter(!is.na(DIAG1) | 
             !is.na(DIAG2) | 
             !is.na(DIAG3) | 
             !is.na(TREAT1) | 
             !is.na(TREAT2) | 
             !is.na(TREAT3) | 
             !is.na(GRPIND1) | 
             !is.na(GRPIND2) | 
             !is.na(GRPIND3) | 
             !is.na(START_DATE)) %>% # must have a diagnosis or treatment or start date
    mutate(REC_TYPE = "DIAGNOSIS",
           SUB_SOURCE = "SWIFT")
  
  
  swift_out <- swift_master %>% 
    select(all_of(vars_out)) %>% # select outcome vars 
    filter(!is.na(MEASURES1) |
             !is.na(MEASURES2) |
             !is.na(MEASURES3)) %>%  
    mutate(REC_TYPE = "OUTCOME",
           SUB_SOURCE = "SWIFT") |> 
    distinct() 
  
  
  
  
  
}

