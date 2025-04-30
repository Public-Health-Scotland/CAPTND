
######################.
### Load Functions ###
######################.

# Author: Charlie Smith
# Date: 2024-05-07


source('02_setup/save_df_as_parquet.R')
source('./10_pre_shorewise_dq/dq_reporting/date_to_month_year_words.R')


# data prep functions
source('./10_pre_shorewise_dq/dq_reporting/load_dq_data.R')
source('./10_pre_shorewise_dq/dq_reporting/filter_wrangle_data.R')
source('./10_pre_shorewise_dq/dq_reporting/get_dq_counts.R')
source('./10_pre_shorewise_dq/dq_reporting/add_scotland_totals.R')
source('./04_check_modify/add_nhsscotland_label_factor.R')
source('./10_pre_shorewise_dq/dq_reporting/complete_absent_vars_na.R')
source('./10_pre_shorewise_dq/dq_reporting/append_submission_status.R')
source('./10_pre_shorewise_dq/dq_reporting/get_dq_proportions.R')
source('./10_pre_shorewise_dq/dq_reporting/label_impossible_combis_na.R')
source('./10_pre_shorewise_dq/dq_reporting/label_retired_variables_na.R')
source('./04_check_modify/correct_hb_names_simple.R')
source('./10_pre_shorewise_dq/dq_reporting/append_patient_management_system.R')
source('./10_pre_shorewise_dq/dq_reporting/append_variable_categories.R')            # - current variables - comment this out if replacing
# source('./10_pre_shorewise_dq/dq_reporting/append_variable_categories_March2025_001.R') # - alternative variables - un-comment out if applying
source('./10_pre_shorewise_dq/dq_reporting/arrange_dq_df.R')

source('./10_pre_shorewise_dq/dq_reporting/add_proportion_groups.R')
source('./10_pre_shorewise_dq/dq_reporting/create_heatmap_known.R')
source('./10_pre_shorewise_dq/dq_reporting/create_heatmap_missing.R')
source('./10_pre_shorewise_dq/dq_reporting/create_heatmap_not_known.R')
source('./10_pre_shorewise_dq/dq_reporting/create_heatmap_invalid.R')

source('./10_pre_shorewise_dq/dq_reporting/get_submission_detail_table.R')
source('./10_pre_shorewise_dq/dq_reporting/get_submission_summary_table.R')

source('./10_pre_shorewise_dq/dq_reporting/compile_dq_report.R')
source('./10_pre_shorewise_dq/dq_reporting/update_dq_wording.R')
source('./10_pre_shorewise_dq/dq_reporting/add_dq_heatmaps.R')
source('./10_pre_shorewise_dq/dq_reporting/update_dq_values.R')
source('./10_pre_shorewise_dq/dq_reporting/protect_dq_worksheets.R')
source('./10_pre_shorewise_dq/dq_reporting/update_formulas.R')
