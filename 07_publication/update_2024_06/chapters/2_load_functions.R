
######################.
### Load functions ###
######################.

# Author: Charlie Smith
# Date: 2023-11-14



source('./02_setup/save_df_as_parquet.R')

source('./07_publication/update_2024_06/functions/add_sex_description.R')
source('./07_publication/update_2024_06/functions/append_quarter_ending.R')
source('./07_publication/update_2024_06/functions/summarise_by_quarter.R')
source('./07_publication/update_2024_06/functions/add_proportion_ds_hb.R')


source('./07_publication/update_2024_06/functions/referrals_by_source.R')
source('./07_publication/update_2024_06/functions/non_acceptance.R')
source('./07_publication/update_2024_06/functions/non_acceptance_reason.R')
source('./07_publication/update_2024_06/functions/non_acceptance_action.R')


source('./07_publication/update_2024_06/functions/compile_ref_source_excel_summary.R')
source('./07_publication/update_2024_06/functions/compile_non_acceptance_summary.R')
source('./07_publication/update_2024_06/functions/compile_non_acceptance_reason_summary.R')
source('./07_publication/update_2024_06/functions/compile_non_acceptance_action_summary.R')


source('./07_publication/update_2024_06/functions/create_table_acc_rate.R')
