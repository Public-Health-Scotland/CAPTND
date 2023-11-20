
###############################################.
### Generate figures for CAPTND publication ###
###############################################.

# Author: Charlie Smith
# Date: 2023-11-14

# Note: recreate publication figures and charts using 'new' CAPTND (shorewise) data


# Step 1: Enter last month of data to include in publication --------------

month_end <- "2023-06-01"


# Step 2 - Run these scripts in sequence ----------------------------------

source("./publication/script/chapters/1_load_packages.R")
source("./publication/script/chapters/2_load_functions.R")
source("./publication/script/chapters/3_set_constants.R")

source("./publication/script/chapters/4_retrieve_save_data.R")


# load_mutate_save

# referrals_hb
# referrals_sco
# ref_rate_agegrp_sex
# ref_rate_simd

# compile in excel workbook 