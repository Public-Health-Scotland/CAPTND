###########################.
### Generate CAPTND MMI ###
###########################.

# Author: Charlie Smith & Bex Madden
# Date: 2024-09-18



# Step 1: Enter last month of data to include in MMI ----------------------

month_end <- "2024-08-01"


# Step 2 - Run these scripts in sequence ----------------------------------
# packages
source("./07_publication/script/chapters/1_load_packages.R")
# functions
source("./07_publication/script/chapters/2_load_functions.R")
# constants
source("./07_publication/script/chapters/3_set_constants.R")
#passwords
source("../../../data/secrets.R") # load passwords (saved here to avoid going to github)

# Step 3 - Analyse Data ---------------------------------------------------

summarise_referrals()
summarise_non_acceptance()
summarise_appointments_att() # key output "/appointments_att/apps_att_mth_hb.parquet"
summarise_ref_source()
summarise_open_cases()
summarise_patients_waiting()
summarise_patients_seen() # key outputs "/patients_seen/pat_seen_unadj_wait_grp_mth.parquet' and "/patients_seen/pat_seen_adj_wait_grp_mth.parquet'

# Step 4 - Excel summaries ------------------------------------------------# being used?

compile_referrals_summary()
compile_non_acceptance_summary()
compile_appointments_summary()

compile_pat_seen_adj_summary()
compile_pat_seen_unadj_summary()
compile_appointments_summary()

compile_open_cases_summary()
compile_pat_wait_unadj_summary()
compile_ref_source_summary()
# Step 5 - Create plots ---------------------------------------------------
#
#create_plots_patients_seen("PT")
#create_plots_patients_seen("CAMHS")


# Step 6 - Create MMI excel doc -------------------------------------------
compile_mmi_data_tables(dataset_choice = "CAMHS")
compile_mmi_data_tables(dataset_choice = "PT")



