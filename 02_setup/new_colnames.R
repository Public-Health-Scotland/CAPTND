
#######################################################.
### Set colnames as objects - swift and globalscape ###
#######################################################.

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
preg_perinatal_ref_o <- "preg_perinatal_ref"
preg_perinatal_app_o <- "preg_perinatal_app"

ethnicity_o <- "ethnicity" 
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
care_plan_inc_o <- "care_plan_inc"


# 3 Appointments colnames ------------------------------------------------------

app_date_o <- "app_date"
app_purpose_o <- "app_purpose"      
att_status_o <- "att_status"    
unav_date_start_o <- "unav_date_start"  
unav_date_end_o <-"unav_date_end"
unav_days_no_o <- "unav_days_no" 
unav_reason_o <- "unav_reason"   
cancellation_date_o <- "cancellation_date"
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

presenting_prob_1_o <- "presenting_prob_1"
presenting_prob_2_o <- "presenting_prob_2"
presenting_prob_3_o <- "presenting_prob_3"

treat_reason_1_o <- "treat_reason_1"
treat_reason_2_o <- "treat_reason_2"
treat_reason_3_o <- "treat_reason_3"


# 4 - Outcome colnames ---------------------------------------------------------

measure_1_o <- "outcome_1"    
measure_2_o <- "outcome_2"
measure_3_o <- "outcome_3"

cgi_i_o <- "cgi_i"
pgi_i_o <- "pgi_i"
cgi_s_o <- "cgi_s"


# 5 - Discharge colnames -------------------------------------------------------

case_closed_date_o <- "case_closed_date" 


# 6 New variables colnames -----------------------------------------------------

sub_source_o <- "sub_source" #globalscape/swift - row evaluation
sub_source_eval_o <- "sub_source_eval" #globalscape/swift/both - data keys evaluation
dataset_type_o <- "dataset_type" #CAMHS/PT
record_type_o <- "record_type" # related to stage name in globalscape submission
dob_from_chi_o <-"dob_from_chi" # dob calculated from chi using phsmethods
sex_from_chi_o <- "sex_from_chi" # sex at birth from chi using phs methods
dob_recorded_matches_chi_o <- "dob_recorded_matches_chi" #TRUE/FALSE
sex_recorded_matches_chi_o <- "sex_recorded_matches_chi" #TRUE/FALSE
ethnicity_edited_o <- "ethnicity_edited" #removed 99 and replace with NA
ethnicity_counts_o <- "ethnicity_counts" # how many different ethnicities (excluding NA) are recorded per patient
ethnicity_edited_counts_o <- "ethnicity_edited_counts" # how many different ethnicities edited (excluding NA) are recorded per patient
ethnicity_evaluation_o <- "ethnicity_evaluation" # ok/multiple ethnicities
vet_edited_o <- "vet_edited" #vet status completed according to other records from patient
looked_after_c_edited_o <- "looked_after_c_edited" #lac status completed according to other records from patient
chi_valid_o <- "chi_valid" #TRUE/FALSE
simd_quintile_o <- "simd2020_quintile" #1/2/3/4/5
simd_decile_o <- "simd2020_decile" #1/2/3/4/5/6/7/8/9/10
simd_vigintile_o <- "simd2020_vigintile" #1/2/3/4/5/6/7/8/9/10/11/12/13/14/15/16/17/18/19/20
patient_id_o <- "patient_id" #primarily CHI, if CHI not available, UPI
ethnicity_last_reported_o <- "ethnicity_last_reported" # last ethnicity reported
submission_date_o <- "submission_date" #submission month derived from floor(header_date)
total_rows_o <- "total_rows" #total number of rows
age_at_ref_rec_o <- "age_at_ref_rec" #age of patient when referral was received
age_group_o <- "age_group" #5 year intevals until 90+
postcode_last_reported_o <- "postcode_last_reported" # last postocde reported by patient
dob_verified_o <- "dob_verified" #dob that has been confirmed by chi
sex_reported_o <- "sex_reported" # sex reported by patient/GP/consultant/etc
ref_rec_date_opti_o <- "ref_rec_date_opti" # optimised ref rec date - if rec date is NA, use ref date instead if available
rtt_eval_o <- "rtt_eval" #if it is possible to do RTT for record
had_first_treat_appt_o <- "had_first_treat_appt" # TRUE/FALSE if a pathway had a treatment appt that was attended
local_authority_name_o <- "local_authority_name"
app_month_o <- "app_month" #first day of the month of the appointment
header_month_o <- "header_month" # month of submission based on header_date
referral_month_o <- "referral_month" #first day of the month of referral rec date opti
att_cat_desc_o <- "att_cat_desc" # description of attendance category 
att_status_desc_o <- "att_status_desc" # description of attendance status 
case_closed_month_o <- "case_closed_month" #month of the recorded appt
app_month_o <- "app_month" #month of the recorded discharge date
ref_acc_last_reported_o <- "ref_acc_last_reported" #last reported referralb accepted status
first_treat_app_o <- "first_treat_app" #first app that the purpose was treatment and the patient attended it
new_or_return_app_o <- "new_or_return_app"

#data keys identify a unique patient pathway
data_keys <- c(dataset_type_o,
               hb_name_o,
               patient_id_o, 
               ucpn_o)

vec_demographic_cols <- c(postcode_o, 
                          sex_o,
                          dob_o,
                          ethnicity_o,
                          simd_vigintile_o,
                          local_authority_name_o,
                          protection_o,
                          looked_after_c_o,
                          vet_o,
                          preg_perinatal_o)

vec_referral_cols <- c(ref_date_o, ref_rec_date_o, ref_source_o, ref_reason_o,
                       ref_acc_o, ref_rej_date_o, ref_rej_reason_o, ref_rej_act_o,
                       ref_rec_date_opti_o, care_plan_inc_o)

vec_app_cols <- c(act_code_sent_date_o, app_date_o, att_status_o, att_cat_o,
                  app_purpose_o, prof_group_o, location_o, unav_date_start_o,
                  unav_date_end_o, unav_days_no_o, unav_reason_o, cancellation_date_o, 
                  presenting_prob_1_o, presenting_prob_2_o, presenting_prob_3_o,
                  treat_reason_1_o, treat_reason_2_o, treat_reason_3_o)

vec_diag_cols <- c(diag_1_o, diag_2_o, diag_3_o)

vec_treat_cols <- c(treat_1_o, treat_2_o, treat_3_o,
                    treat_group_or_ind_1_o, treat_group_or_ind_2_o, 
                    treat_group_or_ind_3_o, treat_start_date_o)

vec_outcome_cols <- c(measure_1_o, measure_2_o, measure_3_o, 
                      cgi_i_o, pgi_i_o, cgi_s_o)

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

level_order_hb <- level_order



