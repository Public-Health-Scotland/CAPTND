###################################################.
### Generate figures for DNA CAPTND publication ###
###################################################.

# Author: Luke Taylor
# Date: 2026-06-26

# Step 1: Enter last month of data to include in publication --------------

month_end <- "2026-05-01"

# Step 2 - Run these scripts in sequence ----------------------------------
source("./07_publication/script/chapters/2_load_functions.R")
source("./07_publication/script/chapters/3_set_constants.R")

#source first contact DNA calculation scripts
source('./07_publication/investigations/dna_invest_pub/calculate_first_contact_df.R')
source('./07_publication/investigations/dna_invest_pub/dna_firstcon_rate_sex_avg.R')
source('./07_publication/investigations/dna_invest_pub/calculate_first_contact_dna_qt.R')
source('./07_publication/investigations/dna_invest_pub/calculate_first_contact_dna_qt_sex.R')
source('./07_publication/investigations/dna_invest_pub/calculate_first_contact_dna_qt_age.R')
source('./07_publication/investigations/dna_invest_pub/calculate_first_contact_dna_qt_agg_age_grp.R')
source('./07_publication/investigations/dna_invest_pub/calculate_first_contact_dna_qt_simd.R')
source('./07_publication/investigations/dna_invest_pub/calculate_first_contact_dna_qt_wait.R')
source('./07_publication/investigations/dna_invest_pub/calculate_first_contact_dna_qt_ur.R')
source('./07_publication/investigations/dna_invest_pub/calculate_first_contact_dna_qt_age_sex.R')

source('./07_publication/investigations/dna_invest_pub/calculate_firstcon_age_std_pop.R')
source('./07_publication/investigations/dna_invest_pub/calculate_firstcon_dna_age_std_simd_sex.R')
source('./07_publication/investigations/dna_invest_pub/calculate_firstcon_dna_age_std_ur_sex.R')

#source total DNA calculation scripts
source('./07_publication/investigations/dna_invest_pub/calculate_total_appts_df.R')
source('./07_publication/investigations/dna_invest_pub/dna_tot_rate_sex_avg.R')
source('./07_publication/investigations/dna_invest_pub/calculate_total_dna_qt.R')
source('./07_publication/investigations/dna_invest_pub/calculate_total_dna_qt_sex.R')
source('./07_publication/investigations/dna_invest_pub/calculate_total_dna_qt_age.R')
source('./07_publication/investigations/dna_invest_pub/calculate_total_dna_qt_agg_age_grp.R')
source('./07_publication/investigations/dna_invest_pub/calculate_total_dna_qt_simd.R')
source('./07_publication/investigations/dna_invest_pub/calculate_total_dna_qt_ur.R')
source('./07_publication/investigations/dna_invest_pub/calculate_total_dna_qt_age_sex.R')
source('./07_publication/investigations/dna_invest_pub/calculate_total_dna_qt_loc.R')
source('./07_publication/investigations/dna_invest_pub/calculate_total_dna_qt_prof.R')
source('./07_publication/investigations/dna_invest_pub/calculate_total_dna_qt_weekday.R')

source('./07_publication/investigations/dna_invest_pub/calculate_total_age_std_pop.R')
source('./07_publication/investigations/dna_invest_pub/calculate_total_dna_age_std_simd_sex.R')
source('./07_publication/investigations/dna_invest_pub/calculate_total_dna_age_std_ur_sex.R')

# 3 - Analyse Data --------------------------------------------------------

##first contact DNAs
firstcon_appt_dataframe()

df_tot_app_qt <- read_parquet(paste0(apps_firstcon_dir, "firstcon_dnas_tot_quarterly_appt_df.parquet"))
df_firstcon <- read_parquet(paste0(apps_firstcon_dir, "firstcon_dnas_all_first_appts_df.parquet"))

firstcon_appt_quarter(df_firstcon)
firstcon_appt_quarter_sex(df_firstcon)
firstcon_appt_quarter_age(df_firstcon)
firstcon_appt_quarter_agg_age(df_firstcon)
firstcon_appt_quarter_simd(df_firstcon)
firstcon_appt_quarter_wait(df_firstcon)
firstcon_appt_quarter_ur(df_firstcon)
firstcon_appt_quarter_age_sex(df_firstcon)

#age standardised rate
calculate_firstcon_age_std_pop(df_firstcon)

firstcon_std_pop <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_firstcon/firstcon_std_pop_weights.parquet"))

age_std_firstcon_appt_dna_simd_sex(df_firstcon, firstcon_std_pop)
age_std_firstcon_appt_dna_ur_sex(df_firstcon, firstcon_std_pop)

##total DNAs
total_appt_dataframe()

df_tot_app_qt <- read_parquet(paste0(apps_att_dir, "total_dnas_tot_quarterly_appt_df.parquet"))
df_tot <- read_parquet(paste0(apps_att_dir, "total_dnas_tot_appts_df.parquet"))

total_appts_quarter(df_tot)
total_appts_quarter_age(df_tot)
total_appts_quarter_age_sex(df_tot)
total_appt_quarter_agg_age(df_tot)
total_appts_quarter_loc(df_tot)
total_appts_quarter_prof(df_tot)
total_appts_quarter_sex(df_tot)
total_appts_quarter_simd(df_tot)
total_appts_quarter_ur(df_tot)
total_appts_quarter_weekday(df_tot)

#age standardised rate
calculate_total_age_std_pop(df_tot)

total_std_pop <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_att/total_std_pop_weights.parquet"))

age_std_tot_appt_dna_simd_sex(df_tot, total_std_pop)
age_std_tot_appt_dna_ur_sex(df_tot, total_std_pop)

# 4 - Create tables/charts for publication --------------------------------
#source first contact DNA chart scripts
source('./07_publication/investigations/dna_invest_pub/dna_tot_rate_sex_avg.R')
source('./07_publication/investigations/dna_invest_pub/dna_firstcon_rate_sex_avg.R')
source('./07_publication/investigations/dna_invest_pub/create_bar_chart_firstcon_dna_sex_age.R')
source('./07_publication/investigations/dna_invest_pub/create_bar_chart_firstcon_dna_simd.R')
source('./07_publication/investigations/dna_invest_pub/create_bar_chart_firstcon_dna_wait.R')
source('./07_publication/investigations/dna_invest_pub/create_bar_chart_firstcon_dna_simd_sex.R')
source('./07_publication/investigations/dna_invest_pub/create_bar_chart_firstcon_dna_ur.R')
source('./07_publication/investigations/dna_invest_pub/create_bar_chart_firstcon_dna_ur_sex.R')

source('./07_publication/investigations/dna_invest_pub/create_bar_chart_firstcon_age_std_dna_simd_sex.R')
source('./07_publication/investigations/dna_invest_pub/create_bar_chart_firstcon_age_std_dna_ur_sex.R')

#source total DNA chart scripts
source('./07_publication/investigations/dna_invest_pub/create_bar_chart_tot_dna_age_sex.R')
source('./07_publication/investigations/dna_invest_pub/create_bar_chart_tot_dna_simd.R')
source('./07_publication/investigations/dna_invest_pub/create_bar_chart_tot_dna_loc.R')
source('./07_publication/investigations/dna_invest_pub/create_bar_chart_tot_dna_prof.R')
source('./07_publication/investigations/dna_invest_pub/create_bar_chart_tot_dna_ur.R')
source('./07_publication/investigations/dna_invest_pub/create_bar_chart_tot_dna_weekday.R')
source('./07_publication/investigations/dna_invest_pub/create_bar_chart_tot_dna_simd_sex.R')
source('./07_publication/investigations/dna_invest_pub/create_bar_chart_tot_dna_ur_sex.R')

source('./07_publication/investigations/dna_invest_pub/create_bar_chart_tot_age_std_dna_simd_sex.R')
source('./07_publication/investigations/dna_invest_pub/create_bar_chart_tot_age_std_dna_ur_sex.R')

# Data tables
create_table_firstcon_att()
create_table_app_att()

# Charts
#first contact DNAs
create_bar_chart_dna_age_sex(dataset_choice = "PT")
create_bar_chart_dna_age_sex(dataset_choice = "CAMHS")

create_bar_chart_dna_simd(dataset_choice = "PT")
create_bar_chart_dna_simd(dataset_choice = "CAMHS")

create_bar_chart_dna_wait_length(dataset_choice = "PT")
create_bar_chart_dna_wait_length(dataset_choice = "CAMHS")

create_bar_chart_dna_simd_sex(dataset_choice = "PT")
create_bar_chart_dna_simd_sex(dataset_choice = "CAMHS")

create_bar_chart_dna_ur(dataset_choice = 'PT')
create_bar_chart_dna_ur(dataset_choice = 'CAMHS')

create_bar_chart_dna_ur_sex(dataset_choice = 'PT')
create_bar_chart_dna_ur_sex(dataset_choice = 'CAMHS')

create_bar_chart_firstcon_dna_ur_sex_std_age(dataset_choice = 'PT')
create_bar_chart_firstcon_dna_ur_sex_std_age(dataset_choice = 'CAMHS')

create_bar_chart_firstcon_dna_simd_sex_std_age(dataset_choice = 'PT')
create_bar_chart_firstcon_dna_simd_sex_std_age(dataset_choice = 'CAMHS')

#total DNAs
create_bar_chart_tot_dna_age_sex(dataset_choice = "PT")
create_bar_chart_tot_dna_age_sex(dataset_choice = "CAMHS")

create_bar_chart_tot_dna_loc(dataset_choice = "PT")
create_bar_chart_tot_dna_loc(dataset_choice = "CAMHS")

create_bar_chart_tot_dna_loc_simd(dataset_choice = "PT")
create_bar_chart_tot_dna_loc_simd(dataset_choice = "CAMHS")

create_bar_chart_tot_dna_prof(dataset_choice = "PT")
create_bar_chart_tot_dna_prof(dataset_choice = "CAMHS")

create_bar_chart_tot_dna_simd(dataset_choice = "PT")
create_bar_chart_tot_dna_simd(dataset_choice = "CAMHS")

create_bar_chart_tot_dna_simd_sex(dataset_choice = "PT")
create_bar_chart_tot_dna_simd_sex(dataset_choice = "CAMHS")

create_bar_chart_tot_dna_ur(dataset_choice = "PT")
create_bar_chart_tot_dna_ur(dataset_choice = "CAMHS")

create_bar_chart_tot_dna_ur_sex(dataset_choice = "PT")
create_bar_chart_tot_dna_ur_sex(dataset_choice = "CAMHS")

create_bar_chart_tot_dna_weekday(dataset_choice = "PT")
create_bar_chart_tot_dna_weekday(dataset_choice = "CAMHS")

create_bar_chart_tot_dna_simd_sex_std_age(dataset_choice = "PT")
create_bar_chart_tot_dna_simd_sex_std_age(dataset_choice = "CAMHS")

create_bar_chart_tot_dna_ur_sex_std_age(dataset_choice = "PT")
create_bar_chart_tot_dna_ur_sex_std_age(dataset_choice = "CAMHS")

# Data for inline values
get_forpub_refs_agesex()


# 6 - Create report -------------------------------------------------------
source('./07_publication/investigations/dna_invest_pub/compile_dna_pub_dt.R')
source('./07_publication/investigations/dna_invest_pub/create_dna_pub_function.R')
source('./07_publication/investigations/dna_invest_pub/compile_dna_pub_dt.R')
source('./07_publication/investigations/dna_invest_pub/update_dna_dt_values.R')
source('./07_publication/investigations/dna_invest_pub/update_dna_dt_wording.R')
source('./07_publication/investigations/dna_invest_pub/protect_dna_worksheets.R')

# Render markdown document
#create_dna_pub_word_doc(dataset_choice = "PT")
#create_dna_pub_word_doc(dataset_choice = "CAMHS")

# 7 - Create data tables --------------------------------------------------
compile_dna_pub_data_tables(dataset_choice = "CAMHS")
compile_dna_pub_data_tables(dataset_choice = "PT")


# 8 - Create summary docs -------------------------------------------------
# create_pub_sum_docs()

