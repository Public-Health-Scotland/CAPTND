
###############################################.
### Generate figures for CAPTND publication ###
###############################################.

# Author: Charlie Smith
# Date: 2023-11-14

# Note: recreate publication figures and charts using 'new' CAPTND (shorewise) data


# Step 1: Enter last month of data to include in publication --------------

month_end <- "2024-06-01"


# Step 2 - Run these scripts in sequence ----------------------------------

source("./07_publication/script/chapters/1_load_packages.R")
source("./07_publication/script/chapters/2_load_functions.R")
source("./07_publication/script/chapters/3_set_constants.R")
source("../../../data/secrets.R") # load passwords (saved here to avoid going to github)

# 3 - Analyse Data --------------------------------------------------------

summarise_referrals()
#summarise_referrals_by_ref_source()
summarise_referrals_sex_age()

summarise_non_acceptance()
summarise_non_acceptance_reason()
summarise_non_acceptance_action()

summarise_referrals_basic_opti()
summarise_appointments_att()
summarise_open_cases()

# 4 - Compile excel workbooks ---------------------------------------------

# part of measure scoping
# these create error messages but they run correctly despite them
compile_referrals_summary()
#compile_referrals_by_ref_source()

compile_non_acceptance_summary()
compile_non_acceptance_reason_summary()
compile_non_acceptance_action_summary()

compile_appointments_summary()
compile_basic_opti_summary()
compile_open_cases_summary()


# 5 - Create tables/charts for publication --------------------------------

# Data tables
create_table_referrals_quarterly()

create_table_acceptance_rate()
# create_table_acceptance_reason_action() # no longer required

create_table_app_att()
create_table_basic_opti()

# Charts

create_stemleaf_ref_sex_age(ds = "CAMHS")
create_stemleaf_ref_sex_age(ds = "PT")

create_bar_chart_refs_simd("CAMHS")
create_bar_chart_refs_simd("PT")

create_bar_chart_non_acceptance_reason(ds = "CAMHS") 
create_bar_chart_non_acceptance_reason(ds = "PT") 

create_bar_chart_non_acceptance_action(ds = "CAMHS") 
create_bar_chart_non_acceptance_action(ds = "PT") 

create_bar_chart_dna_simd("CAMHS")
create_bar_chart_dna_simd("PT")

create_trend_plot_dna_rate("CAMHS")
create_trend_plot_dna_rate("PT")

# Data for inline values
get_forpub_refs_agesex()


# 6 - Create report -------------------------------------------------------

# Render markdown document
create_pub_word_doc(dataset_choice = "PT")
create_pub_word_doc(dataset_choice = "CAMHS")


# 7 - Create data tables --------------------------------------------------
compile_pub_data_tables(dataset_choice = "CAMHS")
compile_pub_data_tables(dataset_choice = "PT")


# 8 - Create DQ appendix --------------------------------------------------



