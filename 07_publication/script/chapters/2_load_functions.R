
######################.
### Load functions ###
######################.

# Author: Charlie Smith
# Date: 2023-11-14


source('./setup/save_df_as_parquet.R')
source("./publication/script/functions/retrieve_save_data.R")
source('./publication/script/functions/get_referrals_quarterly.R')
source('./publication/script/functions/get_referrals_monthly_sco.R')
source('./publication/script/functions/get_referrals_sex_age.R')
source('./publication/script/functions/get_referrals_simd.R')

source('./publication/script/functions/create_table_refs_quarterly.R')
source('./publication/script/functions/get_annual_summary_figures.R')

source('./publication/script/functions/make_chart_referrals_sco.R')
source('./publication/script/functions/make_chart_referrals_sex_age.R')
source('./publication/script/functions/make_chart_referrals_rate_simd.R')

source('./publication/script/functions/compile_excel_doc.R')
