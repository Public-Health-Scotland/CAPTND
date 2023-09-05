######################################################.
### Set colnames as objects - swif and globalscape ###
######################################################.


# Purpose: Uniformises column names for CAPTND and saves them as objects.
# N.B. all CAPTND scripts should use column names as objects.

# 1 ~Patient/record colnames ---------------------------------------------------
hb_name_o <- "hb_name"
ucpn_o <- "ucpn"
upi_o <- "upi" 
chi_o <- "chi"
file_id_o <- "file_id"
loaddate_o <- "loaddate" #globalscape only
line_no_o <- "line_no"
patient_id_o <- "patient_id"

postcode_o <-  "postcode"
sex_o <- "sex"
dob_o <- "dob"
ethnicity_o <- "ethnicity"   

protection_o <- "protection"       
looked_after_c_o <- "looked_after_c"
vet_o <- "vet"
preg_perinatal_o <- "preg_perinatal"  
act_code_sent_date_o <- "act_code_sent_date"

header_date_o <- "header_date"

# 2 Referrals colnames ---------------------------------------------------------

ref_date_o <- "ref_date"
ref_rec_date_o <- "ref_rec_date"
ref_source_o <- "ref_source"
ref_reason_o <- "ref_reason"           
ref_acc_o <- "ref_acc"    
ref_rej_date_o <- "ref_rej_date"      
ref_rej_reason_o <- "ref_rej_reason"
ref_rej_act_o <- "ref_rej_act"


# 3 Appointments colnames ------------------------------------------------------

app_date_o <-"app_date"
app_purpose_o <- "app_purpose"      
att_status_o <- "att_status"    
unav_date_start_o <- "unav_date_start"  
unav_date_end_o <-"unav_date_end"
unav_days_no_o <- "unav_days_no" 
unav_reason_o <- "unav_reason"   
att_cat_o <- "att_cat"    
prof_group_o <- "prof_group"         
location_o <- "location"

diag_1_o <- "diag_1"          
diag_2_o <- "diag_2"
diag_3_o <- "diag_3"             
treat_1_o <- "treat_1"
treat_2_o <- "treat_2"  
treat_3_o <- "treat_3" 
treat_start_date_o <- "treat_start_date" 

treat_group_or_ind_1_o <- "treat_group_or_ind_1"
treat_group_or_ind_2_o <- "treat_group_or_ind_2"
treat_group_or_ind_3_o <- "treat_group_or_ind_3"


# 4 - Outcome colnames ---------------------------------------------------------

measure_1_o <- "outcome_1"    
measure_2_o <- "outcome_2"
measure_3_o <- "outcome_3"



# 5 - Discharge colnames -------------------------------------------------------

case_closed_date_o <- "case_closed_date" 


# 6 New variables colnames -----------------------------------------------------

sub_source_o <- "sub_source"
dataset_type_o <- "dataset_type"
record_type_o <- "record_type"
dob_from_chi_o <-"dob_from_chi"
sex_from_chi_o <- "sex_from_chi"
dob_recorded_matches_chi_o <- "dob_recorded_matches_chi"
sex_recorded_matches_chi_o <- "sex_recorded_matches_chi"
ethnicity_o <- "ethnicity" 
ethnicity_edited_o <- "ethnicity_edited" 
ethnicity_counts_o <- "ethnicity_counts"
ethnicity_edited_counts_o <- "ethnicity_edited_counts"
ethnicity_evaluation_o <- "ethnicity_evaluation"
vet_edited_o <- "vet_edited"
looked_after_c_edited_o <- "looked_after_c_edited"
chi_valid_o <- "chi_valid"
simd_quintile_o <- "simd_quintile"
simd_decile_o <- "simd_decile"
simd_vigintile_o <- "simd_vigintile"
patient_id_o <- "patient_id"
ethnicity_last_reported_o <- "ethnicity_last_reported"
submission_date_o <- "submission_date"
total_rows_o <- "total_rows"
age_at_ref_rec_o <- "age_at_ref_rec"
age_group_o <- "age_group"
postcode_last_reported_o <- "postcode_last_reported"
dob_verified_o <- "dob_verified"
sex_reported_o <- "sex_reported"
ref_rec_date_opti_o <- "ref_rec_date_opti" # optimised ref rec date - if rec date is NA, use ref date instead if available

data_keys <- c(patient_id_o, # change name to vec_data_keys? (easier reference if consistent) 
               ucpn_o,
               dataset_type_o,
               hb_name_o)


vec_referral_cols <- c(ref_date_o, ref_rec_date_o, ref_source_o, ref_reason_o,
                       ref_acc_o,  ref_rej_date_o, ref_rej_reason_o, ref_rej_act_o,
                       ref_rec_date_opti_o)

vec_app_cols <- c(act_code_sent_date_o, app_date_o, att_status_o, att_cat_o,
                  app_purpose_o, prof_group_o, location_o, unav_date_start_o,
                  unav_date_end_o, unav_days_no_o, unav_reason_o)

vec_diag_cols <- c(diag_1_o, diag_2_o, diag_3_o)

vec_treat_cols <- c(treat_1_o, treat_2_o, treat_3_o,
                    treat_group_or_ind_1_o, treat_group_or_ind_2_o, 
                    treat_group_or_ind_3_o, treat_start_date_o)

vec_outcome_cols <- c(measure_1_o, measure_2_o, measure_3_o)

vec_case_closed_cols <- c(case_closed_date_o)

vec_rtt_adj_cols <- c(ref_rec_date_o, ref_acc_o, app_date_o, att_status_o, app_purpose_o,
                  unav_date_start_o, unav_date_end_o, unav_days_no_o)

vec_rtt_unadj_cols <- c(ref_rec_date_o, ref_acc_o, app_date_o, att_status_o, app_purpose_o)

# continue...

level_order <- c('NHS Ayrshire and Arran',
                 'NHS Borders',
                 'NHS Dumfries and Galloway',
                 'NHS Fife',
                 'NHS Forth Valley',
                 'NHS Grampian',
                 'NHS Greater Glasgow and Clyde',
                 'NHS Highland',
                 'NHS Lanarkshire',
                 'NHS Lothian',
                 'NHS Orkney',
                 'NHS Shetland',
                 'NHS Tayside',
                 'NHS Western Isles',
                 'NHS24',
                 'NHS Scotland')
