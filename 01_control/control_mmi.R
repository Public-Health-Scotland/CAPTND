###########################.
### Generate CAPTND MMI ###
###########################.

# Author: Charlie Smith & Bex Madden
# Date: 2024-09-18

# Step 1: Enter last month of data to include in MMI ----------------------

month_end <- "2025-04-01"


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
summarise_non_acceptance_reason()
summarise_non_acceptance_action()
summarise_appointments_att() # key output "/appointments_att/apps_att_mth_hb.parquet"
summarise_appointments_firstcon() # used to be appointments_att
summarise_ref_source()
summarise_open_cases()
summarise_patients_waiting()
summarise_patients_seen() # key outputs "/patients_seen/pat_seen_unadj_wait_grp_mth.parquet' and "/patients_seen/pat_seen_adj_wait_grp_mth.parquet'
summarise_referral_demographics() 
summarise_appointments_att() # now uses total app attendance not first contact
summarise_appointment_location()
summarise_appointment_professional()

# Step 4 - Create MMI excel doc -------------------------------------------
compile_mmi_data_tables(dataset_choice = "CAMHS")
compile_mmi_data_tables(dataset_choice = "PT")



