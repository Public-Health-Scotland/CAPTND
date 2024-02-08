
######################.
### Load functions ###
######################.

# Author: Charlie Smith
# Date: 2023-11-14


source('./02_setup/save_df_as_parquet.R')
source("./07_publication/script/functions/retrieve_save_data.R")
source('./07_publication/script/functions/get_referrals_quarterly.R')
source('./07_publication/script/functions/get_referrals_monthly_sco.R')
source('./07_publication/script/functions/get_referrals_sex_age.R')
source('./07_publication/script/functions/get_referrals_simd.R')

source('./07_publication/script/functions/create_table_refs_quarterly.R')
source('./07_publication/script/functions/get_annual_summary_figures.R')

source('./07_publication/script/functions/make_chart_referrals_sco.R')
source('./07_publication/script/functions/make_chart_referrals_sex_age.R')
source('./07_publication/script/functions/make_chart_referrals_rate_simd.R')

source('./07_publication/script/functions/compile_excel_doc.R')
