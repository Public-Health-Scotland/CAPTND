###############################.
### Apply record label type ###
###############################.

# Author: Luke Taylor
# Date: 2025-12-05

# This function applies a record label type to each row of the dataset

apply_record_label <- function(df){

df_apply_record_type <- df |>
  mutate(record_type_label = case_when(!is.na(ref_date) | !is.na(ref_rec_date) ~ 'Referral',
                                 !is.na(app_date) | !is.na(app_purpose) | !is.na(att_status) ~ 'Appointment',
                                 !is.na(case_closed_date) ~ 'Discharge',
                                 !is.na(cgi_i) | !is.na(pgi_i) | !is.na(cgi_s) ~ 'Clinical outcome',
                                 is.na(app_date) & !is.na(unav_date_start) | !is.na(unav_date_end) | 
                                   !is.na(unav_days_no) | !is.na(unav_reason) ~ 'Unavailability',
                                 !is.na(treat_1) | !is.na(treat_2) | !is.na(treat_3) | 
                                   !is.na(treat_reason_1) | !is.na(treat_reason_2) | !is.na(treat_reason_3) |
                                   !is.na(treat_group_or_ind_1) | !is.na(treat_group_or_ind_2) | !is.na(treat_group_or_ind_3) |
                                   !is.na(treat_start_date) ~ "Treatment",
                                 TRUE ~ 'Unknown')) 

return(df_apply_record_type)
}