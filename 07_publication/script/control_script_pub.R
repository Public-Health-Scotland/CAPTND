
###############################################.
### Generate figures for CAPTND publication ###
###############################################.

# Author: Charlie Smith
# Date: 2023-11-14

# Note: recreate publication figures and charts using 'new' CAPTND (shorewise) data


# Step 1: Enter last month of data to include in publication --------------

month_end <- "2023-12-01"


# Step 2 - Run these scripts in sequence ----------------------------------

source("./07_publication/script/chapters/1_load_packages.R")
source("./07_publication/script/chapters/2_load_functions.R")
source("./07_publication/script/chapters/3_set_constants.R")

retrieve_save_data() 
get_referrals_quarterly()
get_referrals_monthly_sco()
get_referrals_sex_age() 
get_referrals_simd()


# make quarterly table
create_table_referrals_quarterly_hb(dataset_choice = "CAMHS")
create_table_referrals_quarterly_hb(dataset_choice = "PT")

# annual summaries (last 12 months)
get_annual_summary_figs()


# make charts
chart_monthly_refs(dataset_choice = "CAMHS")
chart_monthly_refs(dataset_choice = "PT")

make_chart_sex_age(dataset_choice = "CAMHS")
make_chart_sex_age(dataset_choice = "PT")

make_chart_ref_rate_simd(dataset_choice = "CAMHS")
make_chart_ref_rate_simd(dataset_choice = "PT")


# compile in excel workbook 
compile_excel_doc()

