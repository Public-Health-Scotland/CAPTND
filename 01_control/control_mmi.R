###########################.
### Generate CAPTND MMI ###
###########################.

# Author: Charlie Smith & Bex Madden
# Date: 2024-09-18

# Step 1: Enter last month of data to include in MMI ----------------------

month_end <- "2026-04-01"


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

df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |>
  filter(header_date <= month_end)

summarise_referrals(df)
summarise_non_acceptance(df)
summarise_non_acceptance_reason(df)
summarise_non_acceptance_action(df)
summarise_appointments_att(df) # key output "/appointments_att/apps_att_mth_hb.parquet"
summarise_appointments_firstcon(df) # used to be appointments_att
summarise_ref_source(df)
summarise_referral_demographics(df) 
summarise_appointment_location(df)
summarise_appointment_professional(df)
summarise_referrals_lac(df)
summarise_referrals_prot(df)
summarise_referrals_veteran(df)
summarise_referrals_care_plan(df)
summarise_presenting_prob(df)
summarise_treat_reason(df)
summarise_treat_intervention(df)
summarise_clinical_outcomes(df)
summarise_discharges(df)
summarise_referrals_ppmh(df)
summarise_treat_group_ind(df)

# Step 4 - Create MMI excel doc -------------------------------------------

source('07_publication/script/functions/compile_mmi_data_tables.R')
source('07_publication/script/functions/compile_mmi_comp_data_tables.R')

#Create reports to send out to health boards
compile_mmi_data_tables("CAMHS")
compile_mmi_data_tables("PT")

#Create reports with all HBs
compile_complete_mmi_data_tables(dataset_choice = "CAMHS")
compile_complete_mmi_data_tables(dataset_choice = "PT")

