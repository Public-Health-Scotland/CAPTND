
###########################################.
### Controller script for calculations  ###
###########################################.

#This takes the last rtt evaluated file and calculates different measure from it


# 1 - Source functions -------------------------------------------------
source('06_calculations/calculate_open_cases.R')
source('06_calculations/calculate_patients_waiting.R')
source('06_calculations/calculate_patients_waiting_monthly.R')
source('06_calculations/calculate_patients_seen.R')
source('06_calculations/calculate_referrals.R')
source('06_calculations/calculate_appointments.R')
source('06_calculations/calculate_attendance_status_rates.R')
source('06_calculations/calculate_first_contact.R')
source('05_data_quality/product1.R')
source('05_data_quality/product2.R')
source('04_check_modify/id_app_after_case_closed.R')
source('06_calculations/compare_pat_seen_aggregate_captnd.R')
source('06_calculations/compare_ref_aggregate_captnd.R')
source('06_calculations/compare_dna_aggregate_captnd.R')
source('06_calculations/compare_open_cases_aggregate_captnd.R')
source('06_calculations/calculate_first_treatment.R')
source('06_calculations/compare_first_contact_aggregate_captnd.R')
source('06_calculations/compare_dna_new_return_app.R')
source('06_calculations/compare_dna_aggregate_captnd_csrework.R')
#source('05_data_quality/product3.R')
source('02_setup/save_df_as_parquet.R')
source('06_calculations/compare_patients_waiting_monthly.R')
source('06_calculations/create_comparison_reports.R')
source('06_calculations/create_comparison_report_patient_data.R')
source('06_calculations/calculate_patient_turnover.R')
source('06_calculations/get_latest_month_end.R')
source("./05_data_quality/create_product_pack.R")
source("./05_data_quality/create_product_pack_mth.R")

# 2 - open most recent RTT eval file--------------------------------------

df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) 


# 2.1 Calculate variables -------------------------------------------------

most_recent_month_in_data <- get_lastest_month_end(df)

calculate_referrals(df, most_recent_month_in_data)
calculate_open_cases(df, most_recent_month_in_data)
calculate_patients_waiting(df, most_recent_month_in_data)
calculate_patients_seen(df)
calculate_appointments(df)
calculate_attendance_status_rates(df)

calculate_first_contact(df)
calculate_first_treatment(df)
calculate_pats_waiting_monthly(df)

calculate_patient_turnover(df)

# 2.2 Produce reports -----------------------------------------------------


id_app_after_case_closed(df)

make_product_1()
make_product_2(df, most_recent_month_in_data)
#make_product_3(df, most_recent_month_in_data, TRUE)
#make_product_3(df, most_recent_month_in_data, FALSE)

create_product_pack()
create_product_pack_mth() # with product 2 as a monthly not quarterly heatmap


# 2.3 Comparisons ---------------------------------------------------------

compare_ref_aggregate_captnd()
compare_first_contact_aggregate_captnd()
compare_pat_seen_aggregate_captnd()
compare_dna_aggregate_captnd() # updated from Joana's original
compare_open_cases_aggregate_captnd()
compare_dna_new_return_app() # did not run - new_or_return_app_o not in attendance_status_rates.csv, att_cat instead?
compare_patients_waiting_monthly() # unadjusted

create_comparison_reports()
create_comparison_reports_patient_data()
