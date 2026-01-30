###########################.
### Generate CAPTND MMI ###
###########################.

# Author: Charlie Smith & Bex Madden
# Date: 2024-09-18

# Step 1: Enter last month of data to include in MMI ----------------------

month_end <- "2025-12-01"


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
summarise_referral_demographics() 
summarise_appointment_location()
summarise_appointment_professional()
summarise_referrals_lac()
summarise_referrals_prot()
summarise_referrals_veteran()
summarise_referrals_care_plan()
summarise_presenting_prob()
summarise_treat_reason()
summarise_treat_intervention()
summarise_clinical_outcomes()
summarise_discharges()
summarise_referrals_ppmh()
summarise_treat_group_ind()

# Step 4 - Create MMI excel doc -------------------------------------------

source('07_publication/script/functions/compile_mmi_data_tables.R')
source('07_publication/script/functions/compile_mmi_comp_data_tables.R')

#Create reports to send out to health boards
compile_mmi_data_tables("CAMHS")
compile_mmi_data_tables("PT")

#Create reports with all HBs
compile_complete_mmi_data_tables(dataset_choice = "CAMHS")
compile_complete_mmi_data_tables(dataset_choice = "PT")

