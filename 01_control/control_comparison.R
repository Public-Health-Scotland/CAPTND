########################################################.
### Generate figures for CAPTND Aggregate Comparison ###
########################################################.

# Author: Luke Taylor
# Date: 2025-02-11

# 1 - Set most recent month in data ------------------------------------
most_recent_month_in_data <- as.Date("2025-09-01")
month_end <- "2025-09-01"

df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |> 
  filter(header_date <= most_recent_month_in_data)

# 2 - Source functions -------------------------------------------------
source("./07_publication/script/chapters/2_load_functions.R")
source("./07_publication/script/chapters/3_set_constants.R")
source('04_check_modify/correct_hb_names_simple.R')
source('02_setup/save_df_as_parquet.R')
source('06_calculations/compile_captnd_agg_comp_tables.R')

#calculations
source('06_calculations/calculate_referrals.R')
source('06_calculations/calculate_first_contact_dnas.R')
source('06_calculations/calculate_open_cases.R')
source('06_calculations/calculate_first_treatment.R')
source('06_calculations/calculate_first_contact.R')
source('06_calculations/calculate_patients_waiting_monthly.R')
source('07_publication/script/functions/summarise_patients_seen.R')
#source('06_calculations/calculate_patients_seen.R') #Old script
#source('06_calculations/calculate_appointments.R')
#source('06_calculations/calculate_attendance_status_rates.R')
#source('06_calculations/calculate_adjusted_rtt_waits.R')

#comparisons
source('06_calculations/compare_ref_aggregate_captnd.R')
source('06_calculations/compare_dna_aggregate_captnd_csrework.R')
source('06_calculations/compare_open_cases_aggregate_captnd.R')
source('06_calculations/compare_first_contact_aggregate_captnd.R')
source('06_calculations/compare_patients_waiting_monthly.R')
source('06_calculations/compare_pat_seen_unadjusted_agg_captnd.R')
source('06_calculations/compare_pat_seen_adjusted_agg_captnd.R')
#source('06_calculations/compare_pat_seen_aggregate_captnd.R')

# 3 Calculate variables -------------------------------------------------
calculate_referrals(df, most_recent_month_in_data)
calculate_first_contact_dnas(df)
calculate_open_cases(df, most_recent_month_in_data)
calculate_first_contact(df)
calculate_first_treatment(df)
calculate_pats_waiting_monthly(df)
#summarise_patients_seen() #only run if data is missing, requires 20GB/40 minutes to run

cat(green('Calculations Complete.\n\n')) 

# 3 Run comparison reports ----------------------------------------------
compare_ref_aggregate_captnd()
compare_dna_aggregate_captnd()
compare_open_cases_aggregate_captnd()
compare_first_contact_aggregate_captnd()
compare_patients_waiting_monthly() 
compare_pat_seen_adj_agg_captnd()
compare_pat_seen_unadj_agg_captnd()

cat(green('Comparisons Complete.\n\n')) 

# 4 Run reports --------------------------------------------------------
source('06_calculations/compile_captnd_agg_comp_tables.R')
source('06_calculations/compile_complete_captnd_agg_comp_tables.R')

#Create reports to send out to health boards
compile_captnd_agg_comp_tables("CAMHS")
compile_captnd_agg_comp_tables("PT")

#Create reports for internal review
compile_complete_captnd_agg_comp_tables("CAMHS")
compile_complete_captnd_agg_comp_tables("PT")
