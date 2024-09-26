
######################.
### Load functions ###
######################.

# Author: Charlie Smith
# Date: 2023-11-14



source('./02_setup/save_df_as_parquet.R')

source('./07_publication/script/functions/add_sex_description.R')
source('./07_publication/script/functions/append_quarter_ending.R')
source('./07_publication/script/functions/summarise_by_quarter.R')
source('./07_publication/script/functions/add_proportion_ds_hb.R')
source('./07_publication/script/functions/add_hb_region.R')
source("./07_publication/script/functions/get_appointments_df.R")
source("./07_publication/script/functions/get_basic_data_referrals_df.R")
source('./07_publication/script/functions/tidy_age_group_order.R')

source('./07_publication/script/functions/summarise_referrals.R')
source('./07_publication/script/functions/summarise_open_cases.R')
#source('./07_publication/script/functions/referrals_by_source.R')
source('./07_publication/script/functions/non_acceptance.R')
source('./07_publication/script/functions/non_acceptance_reason.R')
source('./07_publication/script/functions/non_acceptance_action.R')

source('./07_publication/script/functions/summarise_referrals_basic_opti.R')
source('./07_publication/script/functions/summarise_appointments_att.R')

source('./07_publication/script/functions/compile_referrals_excel_summary.R')
source('./07_publication/script/functions/compile_ref_source_excel_summary.R')
source('./07_publication/script/functions/compile_non_acceptance_summary.R')
source('./07_publication/script/functions/compile_non_acceptance_reason_summary.R')
source('./07_publication/script/functions/compile_non_acceptance_action_summary.R')
source('./07_publication/script/functions/compile_appointments_excel_summary.R')
source('./07_publication/script/functions/compile_basic_opti_excel_summary.R')
source('./07_publication/script/functions/compile_open_cases_summary.R')

source('./07_publication/script/functions/create_table_referrals_quarterly.R')
source('./07_publication/script/functions/create_table_acc_rate.R')
#source('./07_publication/script/functions/create_table_acc_reason_action.R')
source('./07_publication/script/functions/create_table_app_att.R')
source('./07_publication/script/functions/create_table_basic_opti.R')

source('./07_publication/script/functions/create_bar_chart_rej_reason.R')
source('./07_publication/script/functions/create_bar_chart_rej_action.R')
source('./07_publication/script/functions/create_bar_chart_dna_simd.R')
source('./07_publication/script/functions/create_trend_plot_dna_rate.R')

source('./07_publication/script/functions/create_pub_function.R')
source('./07_publication/script/functions/identify_next_publication_date.R')

# compile data tables functions
source('./07_publication/script/functions/compile_publication_data_tables.R')
source('./07_publication/script/functions/update_dt_wording.R')
source('./07_publication/script/functions/update_dt_values.R')
source('./07_publication/script/functions/protect_worksheets.R')

source('./07_publication/script/functions/summarise_referrals_sex_age.R')
source('./07_publication/script/functions/create_stemleaf_ref_sex_age.R')
source('./07_publication/script/functions/create_bar_chart_refs_simd.R')

source('./07_publication/script/functions/get_forpub_refs_agesex.R')



#### Functions for MMI only ----------------------------------------------------

source('./07_publication/script/functions/summarise_patients_seen.R')
source('./07_publication/script/functions/compile_pat_seen_adj_excel_summary.R')
source('./07_publication/script/functions/compile_pat_seen_unadj_excel_summary.R')
